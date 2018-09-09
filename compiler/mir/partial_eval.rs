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
use crate::mir::error::{Error, ErrorKind, Result};
use crate::mir::value;
use crate::mir::{Expr, Value};
use crate::ty;

pub struct PartialEvalCtx {
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

impl PartialEvalCtx {
    pub fn new() -> PartialEvalCtx {
        PartialEvalCtx {
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

    fn eval_do(&mut self, dcx: &mut DefCtx<'_>, exprs: &[Expr]) -> Result<Value> {
        let initial_value = Value::List(Box::new([]), None);

        // TODO: This needs to handle Never values once we can create them
        exprs
            .iter()
            .try_fold(initial_value, |_, expr| self.eval_expr(dcx, expr))
    }

    fn eval_let(&mut self, dcx: &mut DefCtx<'_>, hir_let: &hir::Let<ty::Poly>) -> Result<Value> {
        let value = self.eval_expr(dcx, &hir_let.value_expr)?;
        Self::destruc_value(&mut dcx.local_values, &hir_let.destruc, value);

        self.eval_expr(dcx, &hir_let.body_expr)
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
        closure: &value::Closure,
        fixed_args: &[Expr],
        rest_arg: Option<&Expr>,
    ) -> Result<Value> {
        let fun_expr = &closure.fun_expr;

        let fixed_values = fixed_args
            .iter()
            .map(|arg| self.eval_expr(dcx, arg))
            .collect::<Result<Vec<Value>>>()?;

        let rest_value = match rest_arg {
            Some(rest_arg) => Some(Box::new(self.eval_expr(dcx, rest_arg)?)),
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

        self.eval_expr(dcx, &fun_expr.body_expr)
    }

    fn eval_ty_pred_app(
        &mut self,
        dcx: &mut DefCtx<'_>,
        test_poly: &ty::Poly,
        fixed_args: &[Expr],
        rest_arg: Option<&Expr>,
    ) -> Result<Value> {
        use crate::mir::value::poly_for_value;
        use crate::ty::pred::InterpretedPred;

        let value = if !fixed_args.is_empty() {
            self.eval_expr(dcx, &fixed_args[0])?
        } else if let Some(rest_arg) = rest_arg {
            let rest_value = self.eval_expr(dcx, rest_arg)?;
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

    fn eval_rust_fun_app(
        &mut self,
        dcx: &mut DefCtx<'_>,
        span: Span,
        rust_fun: &hir::rfi::Fun,
        fixed_args: &[Expr],
        rest_arg: Option<&Expr>,
    ) -> Result<Value> {
        use crate::codegen::portal::jit_portal_for_rust_fun;
        use crate::mir::intrinsic;

        if let Some(intrinsic_name) = rust_fun.intrinsic_name() {
            // Attempt specialised evaluation
            if let Some(value) =
                intrinsic::try_eval(self, dcx, span, intrinsic_name, fixed_args, rest_arg)?
            {
                return Ok(value);
            }
        }

        let fixed_values = fixed_args
            .iter()
            .map(|arg| self.eval_expr(dcx, arg))
            .collect::<Result<Vec<Value>>>()?;

        let rest_value = match rest_arg {
            Some(rest_arg) => Some(Box::new(self.eval_expr(dcx, rest_arg)?)),
            None => None,
        };

        let arg_list_value = Value::List(fixed_values.into_boxed_slice(), rest_value);
        let boxed_arg_list = self.value_to_boxed(&arg_list_value);

        // Create a dynamic portal to this Rust function if it doesn't exist
        let portal_gen = &mut self.portal_gen;
        let portal_jit = &mut self.portal_jit;
        let portal = self
            .rust_fun_portals
            .entry(rust_fun.entry_point())
            .or_insert_with(|| jit_portal_for_rust_fun(portal_gen, portal_jit, rust_fun));

        let runtime_task = &mut self.runtime_task;

        // By convention convert string panics in to our `ErrorKind::Panic`
        panic::catch_unwind(panic::AssertUnwindSafe(|| {
            Value::Const(portal(runtime_task, boxed_arg_list))
        })).map_err(|err| {
            let message = if let Some(message) = err.downcast_ref::<String>() {
                message.clone()
            } else {
                "Unexpected panic type".to_owned()
            };

            Error::new(span, ErrorKind::Panic(message))
        })
    }

    fn eval_app(
        &mut self,
        dcx: &mut DefCtx<'_>,
        span: Span,
        app: &hir::App<ty::Poly>,
    ) -> Result<Value> {
        let fun_value = self.eval_expr(dcx, &app.fun_expr)?;

        match fun_value {
            Value::Closure(closure) => self.eval_closure_app(
                dcx,
                &closure,
                app.fixed_arg_exprs.as_slice(),
                app.rest_arg_expr.as_ref(),
            ),
            Value::RustFun(rust_fun) => self.eval_rust_fun_app(
                dcx,
                span,
                rust_fun.as_ref(),
                app.fixed_arg_exprs.as_slice(),
                app.rest_arg_expr.as_ref(),
            ),
            Value::TyPred(test_poly) => self.eval_ty_pred_app(
                dcx,
                &test_poly,
                app.fixed_arg_exprs.as_slice(),
                app.rest_arg_expr.as_ref(),
            ),
            _ => {
                unimplemented!("Unimplemented function value type");
            }
        }
    }

    fn eval_cond(&mut self, dcx: &mut DefCtx<'_>, cond: &hir::Cond<ty::Poly>) -> Result<Value> {
        let test_value = self.eval_expr(dcx, &cond.test_expr)?;

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
            self.eval_expr(dcx, &cond.true_expr)
        } else {
            self.eval_expr(dcx, &cond.false_expr)
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

        let value = self.consume_expr(&mut dcx, value_expr)?;
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

    pub fn eval_expr(&mut self, dcx: &mut DefCtx<'_>, expr: &Expr) -> Result<Value> {
        match expr {
            hir::Expr::Lit(literal) => Ok(self.eval_lit(literal)),
            hir::Expr::Do(exprs) => self.eval_do(dcx, &exprs),
            hir::Expr::Fun(_, fun_expr) => {
                Ok(self.eval_fun(dcx, Rc::new(fun_expr.as_ref().clone())))
            }
            hir::Expr::RustFun(_, rust_fun) => {
                Ok(Value::RustFun(Rc::new(rust_fun.as_ref().clone())))
            }
            hir::Expr::TyPred(_, test_poly) => Ok(Value::TyPred(Rc::new(test_poly.clone()))),
            hir::Expr::Ref(_, var_id) => Ok(self.eval_ref(dcx, *var_id)),
            hir::Expr::Let(_, hir_let) => self.eval_let(dcx, hir_let),
            hir::Expr::App(span, app) => self.eval_app(dcx, *span, app),
            hir::Expr::MacroExpand(span, expr) => self
                .eval_expr(dcx, expr)
                .map_err(|err| err.with_macro_invocation_span(*span)),
            hir::Expr::Cond(_, cond) => self.eval_cond(dcx, cond),
        }
    }

    pub fn consume_expr(&mut self, dcx: &mut DefCtx<'_>, expr: Expr) -> Result<Value> {
        match expr {
            hir::Expr::Fun(_, fun_expr) => Ok(self.eval_fun(dcx, fun_expr.into())),
            hir::Expr::RustFun(_, rust_fun) => Ok(Value::RustFun(rust_fun.into())),
            hir::Expr::TyPred(_, test_poly) => Ok(Value::TyPred(Rc::new(test_poly))),
            other => self.eval_expr(dcx, &other),
        }
    }

    pub fn value_to_boxed(&mut self, value: &Value) -> Gc<boxed::Any> {
        match value {
            Value::Const(boxed) => *boxed,
            Value::List(fixed, rest) => {
                let fixed_boxes: Vec<Gc<boxed::Any>> = fixed
                    .iter()
                    .map(|value| self.value_to_boxed(value))
                    .collect();

                let rest_box = match rest {
                    Some(rest) => {
                        let rest_boxed = self.value_to_boxed(rest);
                        if let Some(top_list) = rest_boxed.downcast_ref::<boxed::TopList>() {
                            top_list.as_list()
                        } else {
                            panic!("Attempted to build list with non-list tail");
                        }
                    }
                    None => boxed::List::<boxed::Any>::empty(),
                };

                let list = boxed::List::<boxed::Any>::new_with_tail(
                    &mut self.runtime_task,
                    fixed_boxes.into_iter(),
                    rest_box,
                );

                list.as_any_ref()
            }
            Value::TyPred(ref test_poly) => {
                unimplemented!("Boxing of type predicates: {:?}", test_poly)
            }
            Value::Closure(ref closure) => {
                unimplemented!("Boxing of Arret closures: {:?}", closure.fun_expr)
            }
            Value::RustFun(ref rust_fun) => unimplemented!("Boxing of Rust funs: {:?}", rust_fun),
        }
    }
}

impl Default for PartialEvalCtx {
    fn default() -> PartialEvalCtx {
        PartialEvalCtx::new()
    }
}

impl AsHeap for PartialEvalCtx {
    fn as_heap(&self) -> &boxed::Heap {
        self.runtime_task.heap()
    }

    fn as_heap_mut(&mut self) -> &mut boxed::Heap {
        self.runtime_task.heap_mut()
    }
}
