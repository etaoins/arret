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
use crate::mir::builder::BuiltReg;
use crate::mir::closure::Closure;
use crate::mir::tagset::TypeTagSet;
use crate::ty;
use crate::ty::ty_args::TyArgs;

new_global_id_type!(ArretFunId);

#[derive(Clone, Debug)]
pub struct ArretFun {
    pub id: ArretFunId,
    pub span: Span,
    pub source_name: Option<String>,
    pub env_ty_args: TyArgs<ty::Mono>,
    pub closure: Closure,
    pub fun_expr: Rc<hir::Fun<hir::Inferred>>,
}

impl ArretFun {
    /// Indicates if this `ArretFun` is used in multiple places
    ///
    /// This is a heuristic; if a `Fun` is bound to a variable this will return true regardless
    /// of the number of usages.
    pub fn has_multiple_usages(&self) -> bool {
        // This is a hack but has the benefit of not requiring a separate analysis pass
        Rc::strong_count(&self.fun_expr) > 1
    }
}

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
    RustFun(Rc<hir::rfi::Fun>),
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
