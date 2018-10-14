use runtime::abitype;

use crate::ty;

pub fn compact_abi_type_for_mono(mono: &ty::Mono) -> abitype::ABIType {
    match mono.as_ty() {
        ty::Ty::Int => abitype::ABIType::Int,
        ty::Ty::Bool => abitype::ABIType::Bool,
        ty::Ty::Char => abitype::ABIType::Char,
        ty::Ty::Float => abitype::ABIType::Float,
        _ => abitype::BoxedABIType::Any.into(),
    }
}
