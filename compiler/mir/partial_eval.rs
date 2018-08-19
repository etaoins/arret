use std::borrow::Cow;
use std::collections::HashMap;

use runtime::boxed;
use runtime::boxed::refs::Gc;
use runtime::boxed::AsHeap;
use runtime_syntax::reader;

use crate::hir;
use crate::mir::Value;
use crate::ty;

pub struct PartialEvalCtx {
    heap: boxed::Heap,
    var_values: HashMap<hir::VarId, Value>,
}

impl PartialEvalCtx {
    pub fn new() -> PartialEvalCtx {
        PartialEvalCtx {
            heap: boxed::Heap::new(),
            var_values: HashMap::new(),
        }
    }

    fn eval_destruc(
        &mut self,
        destruc: hir::destruc::Destruc<ty::Poly>,
        expr: &hir::Expr<ty::Poly>,
    ) {
        use crate::hir::destruc::Destruc;
        let value = self.eval_expr(expr).into_owned();

        match destruc {
            Destruc::Scalar(_, scalar) => {
                if let Some(var_id) = scalar.var_id() {
                    self.var_values.insert(*var_id, value);
                }
            }
            Destruc::List(_, list) => unimplemented!("Unimplemented list destruc: {:?}", list),
        }
    }

    fn eval_ref(&mut self, var_id: hir::VarId) -> Cow<'_, Value> {
        Cow::Borrowed(&self.var_values[&var_id])
    }

    pub fn eval_def(&mut self, def: hir::Def<ty::Poly>) {
        let hir::Def {
            destruc,
            value_expr,
            ..
        } = def;

        self.eval_destruc(destruc, &value_expr);
    }

    pub fn eval_expr<'a>(&'a mut self, expr: &hir::Expr<ty::Poly>) -> Cow<'a, Value> {
        match expr {
            hir::Expr::Lit(literal) => {
                let boxed = reader::box_syntax_datum(self, &literal);
                Cow::Owned(Value::Const(boxed))
            }
            hir::Expr::Fun(_, fun) => Cow::Owned(Value::Fun(fun.clone())),
            hir::Expr::RustFun(_, rust_fun) => Cow::Owned(Value::RustFun(rust_fun.clone())),
            hir::Expr::TyPred(_, test_poly) => Cow::Owned(Value::TyPred(test_poly.clone())),
            hir::Expr::Ref(_, var_id) => self.eval_ref(*var_id),
            other => {
                unimplemented!("Unimplemented expression type: {:?}", other);
            }
        }
    }

    pub fn value_to_boxed(&mut self, value: &Value) -> Gc<boxed::Any> {
        match *value {
            Value::Const(boxed) => boxed,
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
