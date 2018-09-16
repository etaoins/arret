use std::collections::HashMap;
use std::os::raw::c_void;
use std::panic;
use std::rc::Rc;

use runtime;
use runtime::boxed;
use runtime::boxed::prelude::*;
use runtime::boxed::refs::Gc;

use runtime_syntax::reader;
use syntax::datum::Datum;
use syntax::span::Span;

use crate::codegen;
use crate::hir;
use crate::mir::builder::Builder;
use crate::mir::error::{Error, ErrorKind, Result};
use crate::mir::ops;
use crate::mir::value;
use crate::mir::{Expr, Value};
use crate::ty;

pub struct EvalHirCtx {
    runtime_task: runtime::task::Task,
    global_values: HashMap<hir::VarId, Value>,

    // This is important for drop order!
    rust_fun_portals: HashMap<*const c_void, codegen::portal::Portal>,
    portal_gen: codegen::CodegenCtx,
    portal_jit: codegen::jit::JITCtx,
}

pub struct DefCtx<'tvars> {
    tvars: &'tvars [ty::TVar],
    local_values: HashMap<hir::VarId, Value>,
}

impl<'tvars> DefCtx<'tvars> {
    pub fn new(tvars: &'tvars [ty::TVar]) -> DefCtx<'tvars> {
        DefCtx {
            tvars,
            local_values: HashMap::new(),
        }
    }
}

impl EvalHirCtx {
    pub fn new() -> EvalHirCtx {
        EvalHirCtx {
            runtime_task: runtime::task::Task::new(),
            global_values: HashMap::new(),

            rust_fun_portals: HashMap::new(),
            portal_gen: codegen::CodegenCtx::new(),
            portal_jit: codegen::jit::JITCtx::new(),
        }
    }

    fn destruc_scalar(
        var_values: &mut HashMap<hir::VarId, Value>,
        scalar: &hir::destruc::Scalar<ty::Poly>,
        value: Value,
    ) {
        if let Some(var_id) = scalar.var_id() {
            var_values.insert(*var_id, value);
        }
    }

    fn destruc_list(
        var_values: &mut HashMap<hir::VarId, Value>,
        list: &hir::destruc::List<ty::Poly>,
        value: &Value,
    ) {
        let mut iter = value.list_iter();

        for fixed_destruc in list.fixed() {
            Self::destruc_value(var_values, fixed_destruc, iter.next_unchecked().clone())
        }

        if let Some(rest_destruc) = list.rest() {
            Self::destruc_scalar(var_values, rest_destruc, iter.rest())
        }
    }

    fn destruc_value(
        var_values: &mut HashMap<hir::VarId, Value>,
        destruc: &hir::destruc::Destruc<ty::Poly>,
        value: Value,
    ) {
        use crate::hir::destruc::Destruc;

        match destruc {
            Destruc::Scalar(_, scalar) => Self::destruc_scalar(var_values, scalar, value),
            Destruc::List(_, list) => Self::destruc_list(var_values, list, &value),
        }
    }

    fn eval_ref(&self, dcx: &DefCtx<'_>, var_id: hir::VarId) -> Value {
        // Try local values first
        if let Some(value) = dcx.local_values.get(&var_id) {
            return value.clone();
        }

        self.global_values[&var_id].clone()
    }

    fn eval_do(
        &mut self,
        dcx: &mut DefCtx<'_>,
        b: &mut Option<Builder>,
        exprs: &[Expr],
    ) -> Result<Value> {
        let initial_value = Value::List(Box::new([]), None);

        // TODO: This needs to handle Never values once we can create them
        exprs
            .iter()
            .try_fold(initial_value, |_, expr| self.eval_expr(dcx, b, expr))
    }

    fn eval_let(
        &mut self,
        dcx: &mut DefCtx<'_>,
        b: &mut Option<Builder>,
        hir_let: &hir::Let<ty::Poly>,
    ) -> Result<Value> {
        let value = self.eval_expr(dcx, b, &hir_let.value_expr)?;
        Self::destruc_value(&mut dcx.local_values, &hir_let.destruc, value);

        self.eval_expr(dcx, b, &hir_let.body_expr)
    }

    fn eval_lit(&mut self, literal: &Datum) -> Value {
        match literal {
            Datum::List(_, elems) => {
                let elem_values: Vec<Value> =
                    elems.iter().map(|elem| self.eval_lit(elem)).collect();

                Value::List(elem_values.into_boxed_slice(), None)
            }
            other => {
                let boxed = reader::box_syntax_datum(self, other);
                Value::Const(boxed)
            }
        }
    }

    fn eval_closure_app(
        &mut self,
        dcx: &mut DefCtx<'_>,
        b: &mut Option<Builder>,
        closure: &value::Closure,
        fixed_args: &[Expr],
        rest_arg: Option<&Expr>,
    ) -> Result<Value> {
        let fun_expr = &closure.fun_expr;

        let fixed_values = fixed_args
            .iter()
            .map(|arg| self.eval_expr(dcx, b, arg))
            .collect::<Result<Vec<Value>>>()?;

        let rest_value = match rest_arg {
            Some(rest_arg) => Some(Box::new(self.eval_expr(dcx, b, rest_arg)?)),
            None => None,
        };

        let arg_list_value = Value::List(fixed_values.into_boxed_slice(), rest_value);
        Self::destruc_list(&mut dcx.local_values, &fun_expr.params, &arg_list_value);

        // Include any captured values from the closure in `local_values`
        dcx.local_values.extend(
            closure
                .captures
                .iter()
                .map(|(var_id, value)| (*var_id, value.clone())),
        );

        self.eval_expr(dcx, b, &fun_expr.body_expr)
    }

    fn eval_ty_pred_app(
        &mut self,
        dcx: &mut DefCtx<'_>,
        b: &mut Option<Builder>,
        test_poly: &ty::Poly,
        fixed_args: &[Expr],
        rest_arg: Option<&Expr>,
    ) -> Result<Value> {
        use crate::mir::value::poly_for_value;
        use crate::ty::pred::InterpretedPred;

        let value = if !fixed_args.is_empty() {
            self.eval_expr(dcx, b, &fixed_args[0])?
        } else if let Some(rest_arg) = rest_arg {
            let rest_value = self.eval_expr(dcx, b, rest_arg)?;
            rest_value.list_iter().next_unchecked().clone()
        } else {
            panic!("Unexpected arity for type predicate application");
        };

        let subject_poly = poly_for_value(self.runtime_task.heap().interner(), &value);

        match ty::pred::interpret_poly_pred(dcx.tvars, &subject_poly, test_poly) {
            InterpretedPred::Static(value) => {
                Ok(Value::Const(boxed::Bool::singleton_ref(value).as_any_ref()))
            }
            InterpretedPred::Dynamic(_, _) => unimplemented!("Runtime type check"),
        }
    }

    fn build_rust_fun_app(
        &mut self,
        b: &mut Builder,
        span: Span,
        rust_fun: &hir::rfi::Fun,
        fixed_values: &[Value],
        rest_value: Option<&Value>,
    ) -> Value {
        use crate::codegen::fun_abi::FunABI;
        use crate::mir::ops::*;
        use crate::mir::value::to_reg::{list_to_reg, value_to_reg};
        use runtime::abitype::{BoxedABIType, RetABIType};

        let mut arg_regs = vec![];

        if rust_fun.takes_task() {
            arg_regs.push(b.push_reg(span, OpKind::CurrentTask, ()));
        }

        let rust_fixed_len = rust_fun.params().len() - (rust_fun.has_rest() as usize);
        let (direct_fixed_values, rest_head_values) = fixed_values.split_at(rust_fixed_len);

        for (fixed_value, abi_type) in direct_fixed_values.iter().zip(rust_fun.params().iter()) {
            let reg_id = value_to_reg(b, span, fixed_value, abi_type);
            arg_regs.push(reg_id);
        }

        if rust_fun.has_rest() {
            let reg_id = list_to_reg(
                b,
                span,
                rest_head_values,
                rest_value,
                &BoxedABIType::List(&BoxedABIType::Any),
            );
            arg_regs.push(reg_id);
        };

        let abi = EntryPointABI {
            takes_task: rust_fun.takes_task(),
            params: rust_fun.params().to_owned().into(),
            ret: rust_fun.ret().clone(),
        };

        let fun_reg = b.push_reg(
            span,
            OpKind::ConstEntryPoint,
            ConstEntryPointOp {
                symbol: rust_fun.symbol(),
                abi: abi,
            },
        );

        let ret_reg = b.push_reg(
            span,
            OpKind::Call,
            CallOp {
                fun_reg,
                args: arg_regs.into_boxed_slice(),
            },
        );

        match rust_fun.ret() {
            RetABIType::Void => Value::List(Box::new([]), None),
            RetABIType::Never => Value::Divergent,
            RetABIType::Inhabited(abi_type) => {
                let reg_value = value::RegValue {
                    reg: ret_reg,
                    abi_type: abi_type.clone(),
                    arret_type: ty::Ty::Fun(Box::new(rust_fun.arret_fun_type().clone()))
                        .into_poly(),
                };

                Value::Reg(Rc::new(reg_value))
            }
        }
    }

    fn eval_rust_fun_app(
        &mut self,
        dcx: &mut DefCtx<'_>,
        b: &mut Option<Builder>,
        span: Span,
        rust_fun: &hir::rfi::Fun,
        fixed_args: &[Expr],
        rest_arg: Option<&Expr>,
    ) -> Result<Value> {
        use crate::codegen::portal::jit_portal_for_rust_fun;
        use crate::mir::intrinsic;
        use crate::mir::value::to_boxed::list_to_boxed;

        // TODO: Fix for polymorphism once it's supported
        let can_const_eval = b.is_none()
            || rust_fun.arret_fun_type().purity() == &ty::purity::Purity::Pure.into_poly();

        if let Some(intrinsic_name) = rust_fun.intrinsic_name() {
            // Attempt specialised evaluation
            if let Some(value) =
                intrinsic::try_eval(self, dcx, b, span, intrinsic_name, fixed_args, rest_arg)?
            {
                return Ok(value);
            }
        }

        let fixed_values = fixed_args
            .iter()
            .map(|arg| self.eval_expr(dcx, b, arg))
            .collect::<Result<Vec<Value>>>()?;

        let rest_value = match rest_arg {
            Some(rest_arg) => Some(self.eval_expr(dcx, b, rest_arg)?),
            None => None,
        };

        if can_const_eval {
            let boxed_arg_list = list_to_boxed(self, &fixed_values, rest_value.as_ref());

            if let Some(boxed_arg_list) = boxed_arg_list {
                // Create a dynamic portal to this Rust function if it doesn't exist
                let portal_gen = &mut self.portal_gen;
                let portal_jit = &mut self.portal_jit;
                let portal = self
                    .rust_fun_portals
                    .entry(rust_fun.entry_point())
                    .or_insert_with(|| jit_portal_for_rust_fun(portal_gen, portal_jit, rust_fun));

                let runtime_task = &mut self.runtime_task;

                // By convention convert string panics in to our `ErrorKind::Panic`
                return panic::catch_unwind(panic::AssertUnwindSafe(|| {
                    Value::Const(portal(runtime_task, boxed_arg_list))
                })).map_err(|err| {
                    let message = if let Some(message) = err.downcast_ref::<String>() {
                        message.clone()
                    } else {
                        "Unexpected panic type".to_owned()
                    };

                    Error::new(span, ErrorKind::Panic(message))
                });
            }
        }

        if let Some(b) = b {
            Ok(self.build_rust_fun_app(b, span, rust_fun, &fixed_values, rest_value.as_ref()))
        } else {
            panic!("Need builder for non-const function application");
        }
    }

    fn eval_value_app(
        &mut self,
        dcx: &mut DefCtx<'_>,
        b: &mut Option<Builder>,
        span: Span,
        fun_value: Value,
        fixed_args: &[Expr],
        rest_arg: Option<&Expr>,
    ) -> Result<Value> {
        match fun_value {
            Value::Closure(closure) => {
                self.eval_closure_app(dcx, b, &closure, fixed_args, rest_arg)
            }
            Value::RustFun(rust_fun) => {
                self.eval_rust_fun_app(dcx, b, span, rust_fun.as_ref(), fixed_args, rest_arg)
            }
            Value::TyPred(test_poly) => {
                self.eval_ty_pred_app(dcx, b, &test_poly, fixed_args, rest_arg)
            }
            _ => {
                unimplemented!("Unimplemented function value type");
            }
        }
    }

    fn eval_app(
        &mut self,
        dcx: &mut DefCtx<'_>,
        b: &mut Option<Builder>,
        span: Span,
        app: &hir::App<ty::Poly>,
    ) -> Result<Value> {
        let fun_value = self.eval_expr(dcx, b, &app.fun_expr)?;

        self.eval_value_app(
            dcx,
            b,
            span,
            fun_value,
            app.fixed_arg_exprs.as_slice(),
            app.rest_arg_expr.as_ref(),
        )
    }

    fn eval_cond(
        &mut self,
        dcx: &mut DefCtx<'_>,
        b: &mut Option<Builder>,
        cond: &hir::Cond<ty::Poly>,
    ) -> Result<Value> {
        let test_value = self.eval_expr(dcx, b, &cond.test_expr)?;

        let test_bool = match test_value {
            Value::Const(any_ref) => {
                let bool_ref = any_ref.downcast_ref::<boxed::Bool>().unwrap();
                bool_ref.as_bool()
            }
            _ => {
                unimplemented!("Non-constant condition");
            }
        };

        if test_bool {
            self.eval_expr(dcx, b, &cond.true_expr)
        } else {
            self.eval_expr(dcx, b, &cond.false_expr)
        }
    }

    fn eval_fun(&mut self, dcx: &mut DefCtx<'_>, fun_expr: Rc<hir::Fun<ty::Poly>>) -> Value {
        let mut captures = HashMap::new();

        // Only process captures if there are local values. This is to avoid visiting the function
        // body when capturing isn't possible.
        if !dcx.local_values.is_empty() {
            // Look for references to variables inside the function
            hir::visitor::visit_exprs(&fun_expr.body_expr, &mut |expr| {
                if let hir::Expr::Ref(_, var_id) = expr {
                    // Avoiding cloning the value if we've already captured
                    if !captures.contains_key(var_id) {
                        if let Some(value) = dcx.local_values.get(var_id) {
                            // Local value is referenced; capture
                            captures.insert(*var_id, value.clone());
                        }
                    }
                }
            });
        }

        Value::Closure(value::Closure { fun_expr, captures })
    }

    pub fn consume_def(&mut self, tvars: &[ty::TVar], def: hir::Def<ty::Poly>) -> Result<()> {
        let hir::Def {
            destruc,
            value_expr,
            ..
        } = def;

        let mut dcx = DefCtx::new(tvars);

        // Don't pass a builder; we should never generate ops based on a def
        let value = self.consume_expr(&mut dcx, &mut None, value_expr)?;

        Self::destruc_value(&mut self.global_values, &destruc, value);
        Ok(())
    }

    /// Collect any boxed values that are no longer reachable
    pub fn collect_garbage(&mut self) {
        use std::mem;
        let old_heap = mem::replace(self.runtime_task.heap_mut(), boxed::Heap::with_capacity(0));
        let mut collection = boxed::Collection::new(old_heap);

        // Move all of our global values to the new heap
        for value_ref in self.global_values.values_mut() {
            value::visit_value_root(&mut collection, value_ref);
        }

        *self.runtime_task.heap_mut() = collection.into_new_heap();
    }

    pub fn eval_expr(
        &mut self,
        dcx: &mut DefCtx<'_>,
        b: &mut Option<Builder>,
        expr: &Expr,
    ) -> Result<Value> {
        match expr {
            hir::Expr::Lit(literal) => Ok(self.eval_lit(literal)),
            hir::Expr::Do(exprs) => self.eval_do(dcx, b, &exprs),
            hir::Expr::Fun(_, fun_expr) => {
                Ok(self.eval_fun(dcx, Rc::new(fun_expr.as_ref().clone())))
            }
            hir::Expr::RustFun(_, rust_fun) => {
                Ok(Value::RustFun(Rc::new(rust_fun.as_ref().clone())))
            }
            hir::Expr::TyPred(_, test_poly) => Ok(Value::TyPred(Rc::new(test_poly.clone()))),
            hir::Expr::Ref(_, var_id) => Ok(self.eval_ref(dcx, *var_id)),
            hir::Expr::Let(_, hir_let) => self.eval_let(dcx, b, hir_let),
            hir::Expr::App(span, app) => self.eval_app(dcx, b, *span, app),
            hir::Expr::MacroExpand(span, expr) => self
                .eval_expr(dcx, b, expr)
                .map_err(|err| err.with_macro_invocation_span(*span)),
            hir::Expr::Cond(_, cond) => self.eval_cond(dcx, b, cond),
        }
    }

    pub fn consume_expr(
        &mut self,
        dcx: &mut DefCtx<'_>,
        b: &mut Option<Builder>,
        expr: Expr,
    ) -> Result<Value> {
        match expr {
            hir::Expr::Fun(_, fun_expr) => Ok(self.eval_fun(dcx, fun_expr.into())),
            hir::Expr::RustFun(_, rust_fun) => Ok(Value::RustFun(rust_fun.into())),
            hir::Expr::TyPred(_, test_poly) => Ok(Value::TyPred(Rc::new(test_poly))),
            other => self.eval_expr(dcx, b, &other),
        }
    }

    /// Evaluates the main function of a program
    ///
    /// This is intended for use by run-pass tests to avoid creating a temporary binary
    pub fn eval_main_fun(&mut self, tvars: &[ty::TVar], main_var_id: hir::VarId) -> Result<()> {
        use syntax::span::EMPTY_SPAN;

        let mut dcx = DefCtx::new(tvars);
        let main_value = self.eval_ref(&dcx, main_var_id);
        self.eval_value_app(&mut dcx, &mut None, EMPTY_SPAN, main_value, &[], None)?;

        Ok(())
    }

    /// Builds the main function of the program
    pub fn build_program(
        &mut self,
        tvars: &[ty::TVar],
        main_var_id: hir::VarId,
    ) -> Result<Vec<ops::Op>> {
        let mut dcx = DefCtx::new(tvars);
        let mut b = Some(Builder::new());

        let main_value = self.eval_ref(&dcx, main_var_id);

        let main_closure = if let Value::Closure(main_closure) = main_value {
            main_closure
        } else {
            unimplemented!("Non-closure main!");
        };

        self.eval_expr(&mut dcx, &mut b, &main_closure.fun_expr.body_expr)?;

        Ok(b.unwrap().into_ops())
    }

    pub fn value_to_boxed(&mut self, value: &Value) -> Option<Gc<boxed::Any>> {
        use crate::mir::value::to_boxed::value_to_boxed;
        value_to_boxed(self, value)
    }
}

impl Default for EvalHirCtx {
    fn default() -> EvalHirCtx {
        EvalHirCtx::new()
    }
}

impl AsHeap for EvalHirCtx {
    fn as_heap(&self) -> &boxed::Heap {
        self.runtime_task.heap()
    }

    fn as_heap_mut(&mut self) -> &mut boxed::Heap {
        self.runtime_task.heap_mut()
    }
}
