use std::rc::Rc;

use arret_runtime::boxed;
use arret_runtime::boxed::prelude::*;
use arret_runtime::boxed::refs::Gc;

use crate::mir::tagset::TypeTagSet;
use crate::mir::value::{RegValue, Value};
use crate::ty;
use crate::ty::Ty;

pub fn mono_to_const(
    heap: &mut impl boxed::AsHeap,
    mono: &ty::Ref<ty::Mono>,
) -> Option<Gc<boxed::Any>> {
    match mono.as_ty() {
        Ty::LitBool(value) => Some(boxed::Bool::singleton_ref(*value).as_any_ref()),
        Ty::LitSym(value) => Some(boxed::Sym::new(heap, value.as_ref()).as_any_ref()),
        Ty::List(list) if !list.has_rest() => {
            let fixed_consts = list
                .fixed()
                .iter()
                .map(|fixed| mono_to_const(heap, fixed))
                .collect::<Option<Vec<Gc<boxed::Any>>>>()?;

            Some(boxed::List::new(heap, fixed_consts.into_iter()).as_any_ref())
        }
        _ => None,
    }
}

/// Returns a TypeTagSet containing the possible type tags for a given value
pub fn possible_type_tags_for_value(value: &Value) -> TypeTagSet {
    match value {
        Value::Const(any_ref) => any_ref.header().type_tag().into(),
        Value::ArretFun(_) | Value::RustFun(_) | Value::TyPred(_) | Value::EqPred => {
            boxed::TypeTag::FunThunk.into()
        }
        Value::List(fixed, rest) => {
            if !fixed.is_empty() {
                // Non-empty list
                boxed::TypeTag::Pair.into()
            } else if let Some(tail) = rest {
                possible_type_tags_for_value(tail)
            } else {
                // Empty list
                boxed::TypeTag::Nil.into()
            }
        }
        Value::Reg(reg_value) => reg_value.possible_type_tags,
    }
}

/// Annotates an existing value with Arret type information
///
/// For the majority of values this is a no-op. For this reason this function takes a builder for
/// the Arret type that is only invoked if the type information can be used.
pub fn value_with_arret_ty<F>(
    heap: &mut impl boxed::AsHeap,
    value: Value,
    build_arret_ty: F,
) -> Value
where
    F: FnOnce() -> ty::Ref<ty::Mono>,
{
    if let Value::Reg(reg_value) = value {
        // This could be useful; request the type
        let arret_ty = build_arret_ty();
        if let Some(any_ref) = mono_to_const(heap, &arret_ty) {
            return any_ref.into();
        }

        let old_type_tags = reg_value.possible_type_tags;
        let new_type_tags = old_type_tags & TypeTagSet::from(&arret_ty);

        // Avoid allocating a new Rc if this is a no-op
        let new_reg_value = if new_type_tags != old_type_tags {
            Rc::new(RegValue {
                reg: reg_value.reg,
                abi_type: reg_value.abi_type.clone(),
                possible_type_tags: new_type_tags,
            })
        } else {
            reg_value
        };

        Value::Reg(new_reg_value)
    } else {
        value
    }
}
