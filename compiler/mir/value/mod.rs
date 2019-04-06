mod arret_fun;
pub mod build_reg;
pub mod from_reg;
pub mod list;
pub mod plan_phi;
pub mod synthetic_fun;
pub mod to_const;
pub mod types;

use std::rc::Rc;

use runtime::abitype;
use runtime::boxed;
use runtime::boxed::refs::Gc;

use crate::mir::builder::BuiltReg;
use crate::mir::tagset::TypeTagSet;
use crate::rfi;
use crate::ty;

pub use arret_fun::{ArretFun, ArretFunId};

#[derive(Clone, Debug)]
pub struct RegValue {
    pub reg: BuiltReg,
    pub abi_type: abitype::ABIType,
    pub possible_type_tags: TypeTagSet,
}

impl RegValue {
    pub fn new(reg: BuiltReg, abi_type: abitype::ABIType) -> RegValue {
        RegValue {
            reg,
            possible_type_tags: (&abi_type).into(),
            abi_type,
        }
    }
}

#[derive(Clone, Debug)]
pub enum Value {
    Const(Gc<boxed::Any>),
    // This uses `Box<[]>` because we can't convert from a `Vec<>` to `Rc<[]>` without reallocating
    List(Box<[Value]>, Option<Box<Value>>),
    ArretFun(ArretFun),
    RustFun(Rc<rfi::Fun>),
    TyPred(ty::pred::TestTy),
    EqPred,
    Reg(Rc<RegValue>),
}

impl Value {
    pub fn unsized_list_iter(&self) -> list::UnsizedListIterator {
        self.clone().into_unsized_list_iter()
    }

    pub fn into_unsized_list_iter(self) -> list::UnsizedListIterator {
        list::UnsizedListIterator::new(self)
    }

    pub fn try_sized_list_iter(&self) -> Option<list::SizedListIterator> {
        list::SizedListIterator::try_new(self)
    }
}

impl<T: boxed::Boxed> From<Gc<T>> for Value {
    fn from(boxed_ref: Gc<T>) -> Self {
        Value::Const(boxed_ref.as_any_ref())
    }
}

impl From<RegValue> for Value {
    fn from(reg_value: RegValue) -> Self {
        Value::Reg(Rc::new(reg_value))
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
            for (_, value) in arret_fun.closure_mut().const_values.iter_mut() {
                visit_value_root(strong_pass, value);
            }
            for (_, value) in arret_fun.closure_mut().free_values.iter_mut() {
                visit_value_root(strong_pass, value);
            }
        }
        _ => {}
    }
}
