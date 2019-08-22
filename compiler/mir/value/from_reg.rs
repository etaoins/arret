use arret_runtime::abitype;
use arret_runtime::boxed;
use arret_runtime::boxed::prelude::*;

use crate::mir::builder::BuiltReg;
use crate::mir::tagset::TypeTagSet;
use crate::mir::value;
use crate::mir::value::Value;
use crate::ty;
use crate::ty::record;
use crate::ty::Ty;

fn reg_to_value_with_constraints<M>(
    heap: &mut impl boxed::AsHeap,
    reg: BuiltReg,
    abi_type: &abitype::ABIType,
    arret_ty: &ty::Ref<M>,
    constrain_possible_type_tags: TypeTagSet,
    constrain_known_record_cons: &Option<record::ConsId>,
) -> Value
where
    M: ty::PM,
{
    let known_record_cons = arret_ty
        .find_member(|poly_ty| match poly_ty {
            Ty::Record(instance) => Some(instance.cons()),
            Ty::RecordClass(cons) => Some(cons),
            _ => None,
        })
        .or_else(|| constrain_known_record_cons.as_ref())
        .cloned();

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
            known_record_cons,
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
    reg_to_value_with_constraints(heap, reg, abi_type, arret_ty, TypeTagSet::all(), &None)
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
        &reg_value.known_record_cons,
    )
}
