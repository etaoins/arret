use std::rc::Rc;

use runtime::abitype;
use runtime::boxed;
use runtime::boxed::prelude::*;
use runtime::boxed::refs::Gc;

use crate::mir::ops;
use crate::mir::value;
use crate::mir::value::Value;
use crate::ty;

fn poly_ty_to_const(heap: &mut impl boxed::AsHeap, poly_ty: &ty::Poly) -> Option<Gc<boxed::Any>> {
    match poly_ty {
        ty::Poly::Fixed(ty::Ty::LitBool(value)) => {
            Some(boxed::Bool::singleton_ref(*value).as_any_ref())
        }
        ty::Poly::Fixed(ty::Ty::LitSym(value)) => {
            Some(boxed::Sym::new(heap, value.as_ref()).as_any_ref())
        }
        ty::Poly::Fixed(ty::Ty::List(list)) if list.rest().is_none() => {
            let fixed_consts = list
                .fixed()
                .iter()
                .map(|fixed| poly_ty_to_const(heap, fixed))
                .collect::<Option<Vec<Gc<boxed::Any>>>>()?;

            Some(boxed::List::new(heap, fixed_consts.into_iter()).as_any_ref())
        }
        _ => None,
    }
}

/// Creates a Value from a register of the given ABI and Arret type
///
/// Supported literal types will be converted to `Value::Const`. Everything else will become a
/// `Value::Reg`.
pub fn reg_to_value(
    heap: &mut impl boxed::AsHeap,
    reg: ops::RegId,
    abi_type: &abitype::ABIType,
    arret_ty: &ty::Poly,
) -> Value {
    poly_ty_to_const(heap, arret_ty)
        .map(Value::Const)
        .unwrap_or_else(|| {
            let reg_value = value::RegValue {
                reg,
                abi_type: abi_type.clone(),
            };

            Value::Reg(Rc::new(reg_value))
        })
}
