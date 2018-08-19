use std::collections::HashMap;
use std::rc::Rc;

use runtime::boxed;
use runtime::boxed::prelude::*;
use runtime::boxed::refs::Gc;

use runtime_syntax::reader;
use syntax::datum::Datum;

use crate::hir;
use crate::mir::{Expr, Value};
use crate::ty;

pub struct PartialEvalCtx {
    heap: boxed::Heap,
    global_values: HashMap<hir::VarId, Value>,
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
            heap: boxed::Heap::new(),
            global_values: HashMap::new(),
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

    fn eval_do<'a>(&'a mut self, dcx: &mut DefCtx<'_>, exprs: &[Expr]) -> Value {
        let initial_value = Value::List(Box::new([]), None);

        // TODO: This needs to handle Never values once we can create them
        exprs
            .iter()
            .fold(initial_value, |_, expr| self.eval_expr(dcx, expr))
    }

    fn eval_let(&mut self, dcx: &mut DefCtx<'_>, hir_let: &hir::Let<ty::Poly>) -> Value {
        let value = self.eval_expr(dcx, &hir_let.value_expr);
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

    fn eval_fun_app(
        &mut self,
        dcx: &mut DefCtx<'_>,
        fun_expr: &hir::Fun<ty::Poly>,
        fixed_args: &[Expr],
        rest_arg: Option<&Expr>,
    ) -> Value {
        let fixed_values: Vec<Value> = fixed_args
            .iter()
            .map(|arg| self.eval_expr(dcx, arg))
            .collect();
        let rest_value = rest_arg.map(|rest_arg| Rc::new(self.eval_expr(dcx, rest_arg)));

        let arg_list_value = Value::List(fixed_values.into_boxed_slice(), rest_value);
        Self::destruc_list(&mut dcx.local_values, &fun_expr.params, &arg_list_value);

        self.eval_expr(dcx, &fun_expr.body_expr)
    }

    fn eval_ty_pred_app(
        &mut self,
        dcx: &mut DefCtx<'_>,
        test_poly: &ty::Poly,
        fixed_args: &[Expr],
        _rest_arg: Option<&Expr>,
    ) -> Value {
        use crate::mir::value::poly_for_value;
        use crate::ty::pred::InterpretedPred;

        let value = self.eval_expr(dcx, &fixed_args[0]);
        let subject_poly = poly_for_value(self.heap.interner(), &value);

        match ty::pred::interpret_poly_pred(dcx.tvars, &subject_poly, test_poly) {
            InterpretedPred::Static(value) => {
                Value::Const(boxed::Bool::singleton_ref(value).as_any_ref())
            }
            InterpretedPred::Dynamic(_, _) => unimplemented!("Runtime type check"),
        }
    }

    fn eval_rust_fun_app(
        &mut self,
        dcx: &mut DefCtx<'_>,
        rust_fun: &hir::rfi::Fun,
        fixed_args: &[Expr],
        rest_arg: Option<&Expr>,
    ) -> Value {
        use crate::mir::intrinsic;

        if let Some(intrinsic_name) = rust_fun.intrinsic_name() {
            // Attempt specialised evaluation
            if let Some(value) =
                intrinsic::try_eval(self, dcx, intrinsic_name, fixed_args, rest_arg)
            {
                return value;
            }
        }

        unimplemented!("Applying Rust functions not implemented")
    }

    fn eval_app(&mut self, dcx: &mut DefCtx<'_>, app: &hir::App<ty::Poly>) -> Value {
        let fun_value = self.eval_expr(dcx, &app.fun_expr);

        match fun_value {
            Value::Fun(fun_expr) => self.eval_fun_app(
                dcx,
                fun_expr.as_ref(),
                app.fixed_arg_exprs.as_slice(),
                app.rest_arg_expr.as_ref(),
            ),
            Value::RustFun(rust_fun) => self.eval_rust_fun_app(
                dcx,
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

    fn eval_cond(&mut self, dcx: &mut DefCtx<'_>, cond: &hir::Cond<ty::Poly>) -> Value {
        let test_value = self.eval_expr(dcx, &cond.test_expr);

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

    pub fn consume_def(&mut self, tvars: &[ty::TVar], def: hir::Def<ty::Poly>) {
        let hir::Def {
            destruc,
            value_expr,
            ..
        } = def;

        let mut dcx = DefCtx::new(tvars);

        let value = self.consume_expr(&mut dcx, value_expr);
        Self::destruc_value(&mut self.global_values, &destruc, value);
    }

    /// Collect any boxed values that are no longer reachable
    pub fn collect_garbage(&mut self) {
        use std::mem;
        let old_heap = mem::replace(&mut self.heap, boxed::Heap::with_capacity(0));

        // Move all of our global values to the new heap
        let roots_iter = self
            .global_values
            .values_mut()
            .filter_map(|value| match value {
                Value::Const(ref mut any_ref) => Some(any_ref),
                _ => None,
            });

        self.heap = old_heap.collect_roots(roots_iter);
    }

    pub fn eval_expr<'a>(&'a mut self, dcx: &mut DefCtx<'_>, expr: &Expr) -> Value {
        match expr {
            hir::Expr::Lit(literal) => self.eval_lit(literal),
            hir::Expr::Do(exprs) => self.eval_do(dcx, &exprs),
            hir::Expr::Fun(_, fun) => Value::Fun(Rc::new(fun.as_ref().clone())),
            hir::Expr::RustFun(_, rust_fun) => Value::RustFun(Rc::new(rust_fun.as_ref().clone())),
            hir::Expr::TyPred(_, test_poly) => Value::TyPred(Rc::new(test_poly.clone())),
            hir::Expr::Ref(_, var_id) => self.eval_ref(dcx, *var_id),
            hir::Expr::Let(_, hir_let) => self.eval_let(dcx, hir_let),
            hir::Expr::App(_, app) => self.eval_app(dcx, app),
            hir::Expr::MacroExpand(_, expr) => self.eval_expr(dcx, expr),
            hir::Expr::Cond(_, cond) => self.eval_cond(dcx, cond),
        }
    }

    pub fn consume_expr<'a>(&'a mut self, dcx: &mut DefCtx<'_>, expr: Expr) -> Value {
        match expr {
            hir::Expr::Fun(_, fun) => Value::Fun(fun.into()),
            hir::Expr::RustFun(_, rust_fun) => Value::RustFun(rust_fun.into()),
            hir::Expr::TyPred(_, test_poly) => Value::TyPred(Rc::new(test_poly)),
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
                    &mut self.heap,
                    fixed_boxes.into_iter(),
                    rest_box,
                );

                list.as_any_ref()
            }
            Value::TyPred(ref test_poly) => {
                unimplemented!("Boxing of type predicates implemented: {:?}", test_poly)
            }
            Value::Fun(ref fun) => {
                unimplemented!("Boxing of Arret funs not implemented: {:?}", fun)
            }
            Value::RustFun(ref rust_fun) => {
                unimplemented!("Boxing of Rust funs not implemented: {:?}", rust_fun)
            }
        }
    }
}

impl AsHeap for PartialEvalCtx {
    fn as_heap(&self) -> &boxed::Heap {
        &self.heap
    }

    fn as_heap_mut(&mut self) -> &mut boxed::Heap {
        &mut self.heap
    }
}
