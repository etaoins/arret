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
use syntax::span::{Span, EMPTY_SPAN};

use crate::codegen;
use crate::hir;
use crate::mir::builder::Builder;
use crate::mir::error::{Error, ErrorKind, Result};
use crate::mir::ops;
use crate::mir::value;
use crate::mir::{Expr, Value};
use crate::ty;
use crate::ty::purity;

pub struct EvalHirCtx {
    runtime_task: runtime::task::Task,
    global_values: HashMap<hir::VarId, Value>,

    boxed_fun_values: HashMap<Gc<boxed::FunThunk>, Value>,

    // This is important for drop order!
    rust_fun_thunks: HashMap<*const c_void, boxed::ThunkEntry>,
    thunk_gen: codegen::CodegenCtx,
    thunk_jit: codegen::jit::JITCtx,
}

pub struct DefCtx {
    pvar_purities: HashMap<purity::PVarId, purity::Purity>,
    tvar_types: HashMap<ty::TVarId, ty::Ty<ty::Mono>>,

    tvars: ty::TVars,
    local_values: HashMap<hir::VarId, Value>,
}

pub struct BuiltProgram {
    pub main: ops::Fun,
    pub funs: Vec<(String, ops::Fun)>,
}

impl DefCtx {
    pub fn new() -> DefCtx {
        DefCtx {
            pvar_purities: HashMap::new(),
            tvar_types: HashMap::new(),

            tvars: ty::TVars::new(),
            local_values: HashMap::new(),
        }
    }
}

impl Default for DefCtx {
    fn default() -> DefCtx {
        DefCtx::new()
    }
}

impl EvalHirCtx {
    pub fn new() -> EvalHirCtx {
        EvalHirCtx {
            runtime_task: runtime::task::Task::new(),
            global_values: HashMap::new(),

            boxed_fun_values: HashMap::new(),

            rust_fun_thunks: HashMap::new(),
            thunk_gen: codegen::CodegenCtx::new(),
            thunk_jit: codegen::jit::JITCtx::new(),
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
        b: &mut Option<Builder>,
        span: Span,
        var_values: &mut HashMap<hir::VarId, Value>,
        list: &hir::destruc::List<ty::Poly>,
        value: Value,
    ) {
        let mut iter = value.into_list_iter();

        for fixed_destruc in list.fixed() {
            let guide_type = iter.next_unchecked(b, span).clone();
            Self::destruc_value(b, var_values, fixed_destruc, guide_type);
        }

        if let Some(rest_destruc) = list.rest() {
            Self::destruc_scalar(var_values, rest_destruc, iter.into_rest())
        }
    }

    fn destruc_value(
        b: &mut Option<Builder>,
        var_values: &mut HashMap<hir::VarId, Value>,
        destruc: &hir::destruc::Destruc<ty::Poly>,
        value: Value,
    ) {
        use crate::hir::destruc::Destruc;

        match destruc {
            Destruc::Scalar(_, scalar) => Self::destruc_scalar(var_values, scalar, value),
            Destruc::List(span, list) => Self::destruc_list(b, *span, var_values, list, value),
        }
    }

    fn eval_ref(&self, dcx: &DefCtx, var_id: hir::VarId) -> Value {
        // Try local values first
        if let Some(value) = dcx.local_values.get(&var_id) {
            return value.clone();
        }

        self.global_values[&var_id].clone()
    }

    fn eval_do(
        &mut self,
        dcx: &mut DefCtx,
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
        dcx: &mut DefCtx,
        b: &mut Option<Builder>,
        hir_let: &hir::Let<ty::Poly>,
    ) -> Result<Value> {
        let value = self.eval_expr(dcx, b, &hir_let.value_expr)?;
        Self::destruc_value(b, &mut dcx.local_values, &hir_let.destruc, value);

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
        dcx: &mut DefCtx,
        b: &mut Option<Builder>,
        span: Span,
        closure: &value::Closure,
        arg_list_value: Value,
    ) -> Result<Value> {
        let fun_expr = &closure.fun_expr;

        Self::destruc_list(
            b,
            span,
            &mut dcx.local_values,
            &fun_expr.params,
            arg_list_value,
        );

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
        dcx: &mut DefCtx,
        b: &mut Option<Builder>,
        span: Span,
        test_mono: &ty::Mono,
        arg_list_value: Value,
    ) -> Result<Value> {
        use crate::mir::value::mono_for_value;
        use crate::ty::pred::InterpretedPred;

        let mut arg_list_iter = arg_list_value.into_list_iter();
        let subject_value = arg_list_iter.next_unchecked(b, span);

        let subject_mono = mono_for_value(self.runtime_task.heap().interner(), &subject_value);

        match ty::pred::interpret_ty_refs(&dcx.tvars, &subject_mono, test_mono) {
            InterpretedPred::Static(value) => {
                Ok(Value::Const(boxed::Bool::singleton_ref(value).as_any_ref()))
            }
            InterpretedPred::Dynamic(_, _) => unimplemented!("Runtime type check"),
        }
    }

    pub fn rust_fun_to_boxed(
        &mut self,
        rust_fun_value: &Value,
        rust_fun: &hir::rfi::Fun,
    ) -> Gc<boxed::FunThunk> {
        use std::ptr;

        let closure = ptr::null();
        let entry = self.thunk_for_rust_fun(rust_fun);
        let new_boxed = boxed::FunThunk::new(self, (closure, entry));

        self.boxed_fun_values
            .insert(new_boxed, rust_fun_value.clone());
        new_boxed
    }

    fn thunk_for_rust_fun(&mut self, rust_fun: &hir::rfi::Fun) -> boxed::ThunkEntry {
        use crate::mir::rust_fun::jit_thunk_for_rust_fun;

        // Create a dynamic thunk to this Rust function if it doesn't exist
        let thunk_gen = &mut self.thunk_gen;
        let thunk_jit = &mut self.thunk_jit;

        *(self
            .rust_fun_thunks
            .entry(rust_fun.entry_point())
            .or_insert_with(|| jit_thunk_for_rust_fun(thunk_gen, thunk_jit, rust_fun)))
    }

    fn call_native_fun<F>(span: Span, block: F) -> Result<Value>
    where
        F: FnOnce() -> Gc<boxed::Any>,
    {
        // By convention convert string panics in to our `ErrorKind::Panic`
        panic::catch_unwind(panic::AssertUnwindSafe(block))
            .map(Value::Const)
            .map_err(|err| {
                let message = if let Some(message) = err.downcast_ref::<String>() {
                    message.clone()
                } else {
                    "Unexpected panic type".to_owned()
                };

                Error::new(span, ErrorKind::Panic(message))
            })
    }

    fn eval_rust_fun_app(
        &mut self,
        b: &mut Option<Builder>,
        span: Span,
        rust_fun: &hir::rfi::Fun,
        arg_list_value: Value,
    ) -> Result<Value> {
        use crate::mir::intrinsic;
        use crate::mir::rust_fun::build_rust_fun_app;
        use crate::mir::value::to_boxed::value_to_boxed;
        use std::ptr;

        // TODO: Fix for polymorphism once it's supported
        let can_const_eval =
            b.is_none() || rust_fun.arret_fun_type().purity() == &purity::Purity::Pure.into_poly();

        if let Some(intrinsic_name) = rust_fun.intrinsic_name() {
            // Attempt specialised evaluation
            if let Some(value) =
                intrinsic::try_eval(self, b, span, intrinsic_name, &arg_list_value)?
            {
                return Ok(value);
            }
        }

        if can_const_eval {
            let boxed_arg_list = value_to_boxed(self, &arg_list_value);

            if let Some(boxed_arg_list) = boxed_arg_list {
                let thunk = self.thunk_for_rust_fun(rust_fun);

                // By convention convert string panics in to our `ErrorKind::Panic`
                let runtime_task = &mut self.runtime_task;
                return Self::call_native_fun(span, || {
                    thunk(runtime_task, ptr::null(), boxed_arg_list)
                });
            }
        }

        if let Some(b) = b {
            Ok(build_rust_fun_app(b, span, rust_fun, arg_list_value))
        } else {
            panic!("Need builder for non-const function application");
        }
    }

    fn eval_fun_thunk_app(
        &mut self,
        dcx: &mut DefCtx,
        b: &mut Option<Builder>,
        span: Span,
        fun_thunk: Gc<boxed::FunThunk>,
        arg_list_value: Value,
    ) -> Result<Value> {
        use crate::mir::value::to_boxed::value_to_boxed;

        if let Some(actual_value) = self.boxed_fun_values.get(&fun_thunk) {
            return self.eval_value_app(dcx, b, span, &actual_value.clone(), arg_list_value);
        }

        if b.is_some() {
            unimplemented!("attempt to apply unknown fun thunk during compile phase");
        }

        let boxed_arg_list =
            value_to_boxed(self, &arg_list_value).expect("uninhabited value during apply");

        Self::call_native_fun(span, || {
            fun_thunk.apply(&mut self.runtime_task, boxed_arg_list)
        })
    }

    fn eval_value_app(
        &mut self,
        dcx: &mut DefCtx,
        b: &mut Option<Builder>,
        span: Span,
        fun_value: &Value,
        arg_list_value: Value,
    ) -> Result<Value> {
        match fun_value {
            Value::Closure(closure) => {
                self.eval_closure_app(dcx, b, span, &closure, arg_list_value)
            }
            Value::RustFun(rust_fun) => {
                self.eval_rust_fun_app(b, span, rust_fun.as_ref(), arg_list_value)
            }
            Value::TyPred(test_poly) => {
                self.eval_ty_pred_app(dcx, b, span, test_poly.as_ref(), arg_list_value)
            }
            Value::Const(boxed_fun) => match boxed_fun.as_subtype() {
                boxed::AnySubtype::FunThunk(fun_thunk) => self.eval_fun_thunk_app(
                    dcx,
                    b,
                    span,
                    unsafe { Gc::new(fun_thunk) },
                    arg_list_value,
                ),
                other => unimplemented!("applying boxed function value type: {:?}", other),
            },
            other => {
                unimplemented!("applying function value type: {:?}", other);
            }
        }
    }

    fn eval_app(
        &mut self,
        dcx: &mut DefCtx,
        b: &mut Option<Builder>,
        span: Span,
        app: &hir::App<ty::Poly>,
    ) -> Result<Value> {
        let fun_value = self.eval_expr(dcx, b, &app.fun_expr)?;

        let fixed_values = app
            .fixed_arg_exprs
            .iter()
            .map(|arg| self.eval_expr(dcx, b, arg))
            .collect::<Result<Vec<Value>>>()?;

        let rest_value = match &app.rest_arg_expr {
            Some(rest_arg) => Some(Box::new(self.eval_expr(dcx, b, rest_arg)?)),
            None => None,
        };

        let arg_list_value = Value::List(fixed_values.into_boxed_slice(), rest_value);
        self.eval_value_app(dcx, b, span, &fun_value, arg_list_value)
    }

    fn eval_cond(
        &mut self,
        dcx: &mut DefCtx,
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

    fn eval_fun(&mut self, dcx: &mut DefCtx, fun_expr: Rc<hir::Fun<ty::Poly>>) -> Value {
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

    fn build_closure(&mut self, dcx: &mut DefCtx, closure: &value::Closure) -> Result<ops::Fun> {
        use runtime::abitype;

        let mut b = Some(Builder::new());

        let value::Closure { captures, fun_expr } = closure;
        let params = &fun_expr.params;

        if !captures.is_empty() {
            unimplemented!("building Arret functions with captures");
        }

        if !params.fixed().is_empty() || params.rest().is_some() {
            unimplemented!("building Arret functions with parameters");
        }

        if fun_expr.ret_ty != ty::Ty::unit().into_poly() {
            unimplemented!("building Arret functions with return");
        }

        self.eval_expr(dcx, &mut b, &fun_expr.body_expr)?;
        let mut ops = b.unwrap().into_ops();
        ops.push(ops::Op::new(EMPTY_SPAN, ops::OpKind::RetVoid));

        let main_abi = ops::FunABI {
            takes_task: true,
            takes_closure: false,
            params: Box::new([]),
            ret: abitype::RetABIType::Void,
        };

        Ok(ops::Fun {
            source_name: None,
            abi: main_abi,
            params: vec![],
            ops,
        })
    }

    pub fn consume_def(&mut self, def: hir::Def<ty::Poly>) -> Result<()> {
        let hir::Def {
            destruc,
            value_expr,
            ..
        } = def;

        let mut dcx = DefCtx::new();

        // Don't pass a builder; we should never generate ops based on a def
        let value = self.consume_expr(&mut dcx, &mut None, value_expr)?;

        Self::destruc_value(&mut None, &mut self.global_values, &destruc, value);
        Ok(())
    }

    /// Collect any boxed values that are no longer reachable
    pub fn collect_garbage(&mut self) {
        use runtime::boxed::collect;
        use std::mem;

        let old_heap = mem::replace(self.runtime_task.heap_mut(), boxed::Heap::with_capacity(0));
        let mut strong_pass = collect::StrongPass::new(old_heap);

        // Move all of our global values to the new heap
        for value_ref in self.global_values.values_mut() {
            value::visit_value_root(&mut strong_pass, value_ref);
        }

        for value_ref in self.boxed_fun_values.values_mut() {
            // TODO: This can cause a circular reference with the weak pass below
            value::visit_value_root(&mut strong_pass, value_ref);
        }

        // Any function values that are still live need to be updated
        let weak_pass = strong_pass.into_weak_pass();

        let old_boxed_fun_values = mem::replace(&mut self.boxed_fun_values, HashMap::new());
        self.boxed_fun_values = old_boxed_fun_values
            .into_iter()
            .filter_map(|(fun_thunk, value)| {
                weak_pass
                    .new_heap_ref_for(fun_thunk)
                    .map(|new_fun_thunk| (new_fun_thunk, value))
            })
            .collect();

        *self.runtime_task.heap_mut() = weak_pass.into_new_heap();
    }

    pub fn eval_expr(
        &mut self,
        dcx: &mut DefCtx,
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
            hir::Expr::TyPred(_, test_poly) => {
                let test_mono =
                    ty::subst::monomorphise(&dcx.pvar_purities, &dcx.tvar_types, test_poly);
                Ok(Value::TyPred(Rc::new(test_mono)))
            }
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
        dcx: &mut DefCtx,
        b: &mut Option<Builder>,
        expr: Expr,
    ) -> Result<Value> {
        match expr {
            hir::Expr::Fun(_, fun_expr) => Ok(self.eval_fun(dcx, fun_expr.into())),
            hir::Expr::RustFun(_, rust_fun) => Ok(Value::RustFun(rust_fun.into())),
            other => self.eval_expr(dcx, b, &other),
        }
    }

    /// Evaluates the main function of a program
    ///
    /// This is intended for use by run-pass tests to avoid creating a temporary binary
    pub fn eval_main_fun(&mut self, main_var_id: hir::VarId) -> Result<()> {
        use syntax::span::EMPTY_SPAN;

        let mut dcx = DefCtx::new();
        let main_value = self.eval_ref(&dcx, main_var_id);
        let empty_list_value = Value::List(Box::new([]), None);

        self.eval_value_app(
            &mut dcx,
            &mut None,
            EMPTY_SPAN,
            &main_value,
            empty_list_value,
        )?;

        Ok(())
    }

    /// Builds the main function of the program
    pub fn build_program(&mut self, main_var_id: hir::VarId) -> Result<BuiltProgram> {
        let mut dcx = DefCtx::new();

        let main_value = self.eval_ref(&dcx, main_var_id);

        let main_closure = if let Value::Closure(main_closure) = main_value {
            main_closure
        } else {
            unimplemented!("Non-closure main!");
        };

        Ok(BuiltProgram {
            main: self.build_closure(&mut dcx, &main_closure)?,
            funs: vec![],
        })
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
