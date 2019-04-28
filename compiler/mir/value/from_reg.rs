use arret_runtime::abitype;
use arret_runtime::boxed;

use crate::mir::builder::BuiltReg;
use crate::mir::tagset::TypeTagSet;
use crate::mir::value;
use crate::mir::value::Value;
use crate::ty;

/// Creates a Value from a register of the given ABI and Arret type
///
/// Supported literal types will be converted to `Value::Const`. Everything else will become a
/// `Value::Reg`.
pub fn reg_to_value(
    heap: &mut impl boxed::AsHeap,
    reg: BuiltReg,
    abi_type: &abitype::ABIType,
    arret_ty: &ty::Ref<ty::Mono>,
) -> Value {
    use crate::mir::value::types::mono_to_const;

    mono_to_const(heap, arret_ty)
        .map(Value::Const)
        .unwrap_or_else(|| {
            value::RegValue {
                reg,
                abi_type: abi_type.clone(),
                possible_type_tags: TypeTagSet::from(abi_type) & TypeTagSet::from(arret_ty),
            }
            .into()
        })
}
