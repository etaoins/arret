pub mod build_reg;
pub mod from_reg;
pub mod list;
pub mod plan_phi;
pub mod synthetic_fun;
pub mod to_const;
pub mod types;

use std::rc::Rc;

use syntax::span::Span;

use runtime::abitype;
use runtime::boxed;
use runtime::boxed::refs::Gc;

use crate::hir;
use crate::mir::closure::Closure;
use crate::mir::ops::RegId;
use crate::ty;

#[derive(Clone, Debug)]
pub struct ArretFun {
    pub span: Span,
    pub source_name: Option<String>,
    pub closure: Closure,
    pub fun_expr: Rc<hir::Fun<hir::Inferred>>,
}

#[derive(Clone, Debug)]
pub struct RegValue {
    pub reg: RegId,
    pub abi_type: abitype::ABIType,
}

#[derive(Clone, Debug)]
pub enum Value {
    Const(Gc<boxed::Any>),
    // This uses Box<[]> because we can't convert from a Vec<> to Rc<[]> without reallocating
    List(Box<[Value]>, Option<Box<Value>>),
    ArretFun(ArretFun),
    RustFun(Rc<hir::rfi::Fun>),
    TyPred(ty::pred::TestTy),
    EqPred,
    Reg(Rc<RegValue>),
    Divergent,
}

impl Value {
    pub fn list_iter(&self) -> list::ListIterator {
        use std::ops::Deref;
        match self {
            Value::List(fixed, Some(rest)) => {
                list::ListIterator::new(fixed.clone().into_vec(), Some(rest.deref().clone()))
            }
            Value::List(fixed, None) => list::ListIterator::new(fixed.clone().into_vec(), None),
            other => list::ListIterator::new(vec![], Some(other.clone())),
        }
    }

    pub fn into_list_iter(self) -> list::ListIterator {
        match self {
            Value::List(fixed, Some(rest)) => {
                list::ListIterator::new(fixed.into_vec(), Some(*rest))
            }
            Value::List(fixed, None) => list::ListIterator::new(fixed.into_vec(), None),
            other => list::ListIterator::new(vec![], Some(other)),
        }
    }

    pub fn is_divergent(&self) -> bool {
        match self {
            Value::Divergent => true,
            _ => false,
        }
    }
}

pub fn visit_value_root(strong_pass: &mut boxed::collect::StrongPass, value: &mut Value) {
    match value {
        Value::Const(ref mut any_ref) => strong_pass.visit_box(any_ref),
        Value::List(ref mut fixed, ref mut rest) => {
            for any_ref in fixed.iter_mut() {
                visit_value_root(strong_pass, any_ref);
            }
            for any_ref in rest {
                visit_value_root(strong_pass, any_ref);
            }
        }
        Value::ArretFun(ref mut arret_fun) => {
            for (_, value) in &mut arret_fun.closure.const_values {
                visit_value_root(strong_pass, value);
            }
            for (_, value) in &mut arret_fun.closure.free_values {
                visit_value_root(strong_pass, value);
            }
        }
        _ => {}
    }
}
