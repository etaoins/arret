use runtime::boxed;
use runtime::boxed::refs::Gc;
use runtime_syntax::reader;

use crate::hir;
use crate::ty;

pub enum Value {
    Const(Gc<boxed::Any>),
}

pub struct PartialEvalCtx {
    heap: boxed::Heap,
}

impl PartialEvalCtx {
    pub fn new() -> PartialEvalCtx {
        PartialEvalCtx {
            heap: boxed::Heap::new(),
        }
    }

    pub fn eval_expr(&mut self, expr: &hir::Expr<ty::Poly>) -> Value {
        match expr {
            hir::Expr::Lit(literal) => {
                let boxed = reader::box_syntax_datum(self, literal);
                Value::Const(boxed)
            }
            other => {
                unimplemented!("Unimplemented expression type: {:?}", other);
            }
        }
    }

    pub fn value_to_boxed(&mut self, value: &Value) -> Gc<boxed::Any> {
        match *value {
            Value::Const(boxed) => boxed,
        }
    }
}

impl boxed::AsHeap for PartialEvalCtx {
    fn as_heap(&self) -> &boxed::Heap {
        &self.heap
    }

    fn as_heap_mut(&mut self) -> &mut boxed::Heap {
        &mut self.heap
    }
}
