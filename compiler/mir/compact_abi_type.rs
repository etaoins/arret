use runtime::abitype;

use crate::ty;

pub fn compact_abi_type_for_poly(poly: &ty::Poly) -> abitype::ABIType {
    match poly {
        ty::Poly::Fixed(ty::Ty::Int) => abitype::ABIType::Int,
        ty::Poly::Fixed(ty::Ty::Bool) => abitype::ABIType::Bool,
        ty::Poly::Fixed(ty::Ty::Char) => abitype::ABIType::Char,
        ty::Poly::Fixed(ty::Ty::Float) => abitype::ABIType::Float,
        _ => abitype::BoxedABIType::Any.into(),
    }
}
