use runtime::boxed;
use runtime::boxed::refs::Gc;

use crate::hir;
use crate::ty;

#[derive(Clone)]
pub enum Value {
    Const(Gc<boxed::Any>),
    List(Box<[Value]>, Option<Box<Value>>),
    Fun(Box<hir::Fun<ty::Poly>>),
    RustFun(Box<hir::rfi::Fun>),
    TyPred(ty::Poly),
}

impl Value {
    pub fn list_iter(&self) -> ListIterator<'_> {
        match self {
            Value::List(fixed, rest) => ListIterator {
                fixed,
                rest: rest.as_ref().map(|rest| rest.as_ref()),
            },
            other => ListIterator {
                fixed: &[],
                rest: Some(other),
            },
        }
    }
}

pub struct ListIterator<'list> {
    fixed: &'list [Value],
    rest: Option<&'list Value>,
}

impl<'list> ListIterator<'list> {
    pub fn next_unchecked(&mut self) -> &'list Value {
        if self.fixed.is_empty() {
            unimplemented!("Need to pull element off rest")
        } else {
            let next = self.fixed.first().unwrap();
            self.fixed = &self.fixed[1..];
            next
        }
    }

    pub fn rest(&self) -> Value {
        Value::List(
            self.fixed.to_vec().into_boxed_slice(),
            self.rest.cloned().map(Box::new),
        )
    }
}
