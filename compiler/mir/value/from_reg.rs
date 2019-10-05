use arret_runtime::abitype;
use arret_runtime::boxed;
use arret_runtime::boxed::prelude::*;

use crate::mir::builder::BuiltReg;
use crate::mir::tagset::TypeTagSet;
use crate::mir::value;
use crate::mir::value::types::TypeHint;
use crate::mir::value::Value;
use crate::ty;
use crate::ty::Ty;

fn reg_to_value_with_constraints<M>(
    heap: &mut impl boxed::AsHeap,
    reg: BuiltReg,
    abi_type: &abitype::ABIType,
    arret_ty: &ty::Ref<M>,
    constrain_possible_type_tags: TypeTagSet,
    fallback_type_hint: &TypeHint,
) -> Value
where
    M: ty::PM,
{
    use crate::mir::value::types::type_hint_for_ty_ref;

    let type_hint_from_ty_ref = type_hint_for_ty_ref(arret_ty);

    let type_hint = if type_hint_from_ty_ref == TypeHint::None {
        // Hopefully our fallback type hint has more information
        fallback_type_hint.clone()
    } else {
        type_hint_from_ty_ref
    };

    if let Ty::LitSym(value) = arret_ty.resolve_to_ty() {
        // Unlike other literal types we can't encode a literal sym in a `RegValue` without losing
        // information. This means we will need to rebuild the sym every time it's referenced but
        // this should be a net win.
        boxed::Sym::new(heap, value.as_ref()).as_any_ref().into()
    } else {
        value::RegValue {
            reg,
            abi_type: abi_type.clone(),
            possible_type_tags: TypeTagSet::from(abi_type)
                & TypeTagSet::from(arret_ty)
                & constrain_possible_type_tags,
            type_hint,
        }
        .into()
    }
}

/// Creates a Value from a register of the given ABI and Arret type
///
/// Supported literal types will be converted to `Value::Const`. Everything else will become a
/// `Value::Reg`.
pub fn reg_to_value<M>(
    heap: &mut impl boxed::AsHeap,
    reg: BuiltReg,
    abi_type: &abitype::ABIType,
    arret_ty: &ty::Ref<M>,
) -> Value
where
    M: ty::PM,
{
    reg_to_value_with_constraints(
        heap,
        reg,
        abi_type,
        arret_ty,
        TypeTagSet::all(),
        &TypeHint::None,
    )
}

pub fn refine_reg_value_with_arret_ty<M>(
    heap: &mut impl boxed::AsHeap,
    reg_value: &value::RegValue,
    arret_ty: &ty::Ref<M>,
) -> Value
where
    M: ty::PM,
{
    reg_to_value_with_constraints(
        heap,
        reg_value.reg,
        &reg_value.abi_type,
        arret_ty,
        reg_value.possible_type_tags,
        &reg_value.type_hint,
    )
}
