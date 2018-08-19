use std::collections::HashMap;

use runtime::boxed;
use runtime::boxed::refs::Gc;
use runtime::boxed::AsHeap;
use runtime_syntax::reader;
use syntax::datum::Datum;

use crate::hir;
use crate::mir::{Expr, Value};
use crate::ty;

pub struct PartialEvalCtx {
    heap: boxed::Heap,
    var_values: HashMap<hir::VarId, Value>,
}

pub struct DefCtx<'tvar> {
    tvars: &'tvar [ty::TVar],
    var_values: HashMap<hir::VarId, Value>,
}

impl PartialEvalCtx {
    pub fn new() -> PartialEvalCtx {
        PartialEvalCtx {
            heap: boxed::Heap::new(),
            var_values: HashMap::new(),
        }
    }

    fn destruc_scalar(&mut self, scalar: &hir::destruc::Scalar<ty::Poly>, value: Value) {
        if let Some(var_id) = scalar.var_id() {
            self.var_values.insert(*var_id, value);
        }
    }

    fn destruc_list(&mut self, list: &hir::destruc::List<ty::Poly>, value: &Value) {
        let mut iter = value.list_iter();

        for fixed_destruc in list.fixed() {
            self.destruc_value(fixed_destruc, iter.next_unchecked().clone())
        }

        if let Some(rest_destruc) = list.rest() {
            self.destruc_scalar(rest_destruc, iter.rest())
        }
    }

    fn destruc_value(&mut self, destruc: &hir::destruc::Destruc<ty::Poly>, value: Value) {
        use crate::hir::destruc::Destruc;

        match destruc {
            Destruc::Scalar(_, scalar) => self.destruc_scalar(scalar, value),
            Destruc::List(_, list) => self.destruc_list(list, &value),
        }
    }

    fn eval_destruc(&mut self, destruc: &hir::destruc::Destruc<ty::Poly>, expr: &Expr) {
        let value = self.eval_expr(expr);
        self.destruc_value(destruc, value)
    }

    fn eval_ref(&mut self, var_id: hir::VarId) -> Value {
        self.var_values[&var_id].clone()
    }

    fn eval_do<'a>(&'a mut self, exprs: &[Expr]) -> Value {
        let initial_value = Value::List(Box::new([]), None);

        // TODO: This needs to handle Never values once we can create them
        exprs
            .iter()
            .fold(initial_value, |_, expr| self.eval_expr(expr))
    }

    fn eval_let(&mut self, hir_let: &hir::Let<ty::Poly>) -> Value {
        self.eval_destruc(&hir_let.destruc, &hir_let.value_expr);
        self.eval_expr(&hir_let.body_expr)
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
        fun_expr: &hir::Fun<ty::Poly>,
        fixed_args: &[Expr],
        rest_arg: Option<&Expr>,
    ) -> Value {
        let fixed_values: Vec<Value> = fixed_args.iter().map(|arg| self.eval_expr(arg)).collect();
        let rest_value = rest_arg.map(|rest_arg| Box::new(self.eval_expr(rest_arg)));

        let arg_list_value = Value::List(fixed_values.into_boxed_slice(), rest_value);
        self.destruc_list(&fun_expr.params, &arg_list_value);

        self.eval_expr(&fun_expr.body_expr)
    }

    fn eval_rust_fun_app(
        &mut self,
        rust_fun: &hir::rfi::Fun,
        fixed_args: &[Expr],
        rest_arg: Option<&Expr>,
    ) -> Value {
        use crate::mir::intrinsic;

        if let Some(intrinsic_name) = rust_fun.intrinsic_name() {
            // Attempt specialised evaluation
            if let Some(value) = intrinsic::try_eval(self, intrinsic_name, fixed_args, rest_arg) {
                return value;
            }
        }

        unimplemented!("Applying Rust functions not implemented")
    }

    fn eval_app(&mut self, app: &hir::App<ty::Poly>) -> Value {
        let fun_value = self.eval_expr(&app.fun_expr);

        match fun_value {
            Value::Fun(fun_expr) => self.eval_fun_app(
                fun_expr.as_ref(),
                app.fixed_arg_exprs.as_slice(),
                app.rest_arg_expr.as_ref(),
            ),
            Value::RustFun(rust_fun) => self.eval_rust_fun_app(
                rust_fun.as_ref(),
                app.fixed_arg_exprs.as_slice(),
                app.rest_arg_expr.as_ref(),
            ),
            _ => {
                unimplemented!("Unimplemented function value type");
            }
        }
    }

    fn eval_cond(&mut self, cond: &hir::Cond<ty::Poly>) -> Value {
        let test_value = self.eval_expr(&cond.test_expr);

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
            self.eval_expr(&cond.true_expr)
        } else {
            self.eval_expr(&cond.false_expr)
        }
    }

    pub fn eval_def(&mut self, def: hir::Def<ty::Poly>) {
        let hir::Def {
            destruc,
            value_expr,
            ..
        } = def;

        self.eval_destruc(&destruc, &value_expr);
    }

    pub fn eval_expr<'a>(&'a mut self, expr: &Expr) -> Value {
        match expr {
            hir::Expr::Lit(literal) => self.eval_lit(literal),
            hir::Expr::Do(exprs) => self.eval_do(&exprs),
            hir::Expr::Fun(_, fun) => Value::Fun(fun.clone()),
            hir::Expr::RustFun(_, rust_fun) => Value::RustFun(rust_fun.clone()),
            hir::Expr::TyPred(_, test_poly) => Value::TyPred(test_poly.clone()),
            hir::Expr::Ref(_, var_id) => self.eval_ref(*var_id),
            hir::Expr::Let(_, hir_let) => self.eval_let(hir_let),
            hir::Expr::App(_, app) => self.eval_app(app),
            hir::Expr::MacroExpand(_, expr) => self.eval_expr(expr),
            hir::Expr::Cond(_, cond) => self.eval_cond(cond),
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
