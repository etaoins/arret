use arret_syntax::span::Span;

use arret_runtime::abitype;
use arret_runtime::boxed;

use crate::mir::builder::Builder;
use crate::mir::eval_hir::EvalHirCtx;
use crate::mir::value::Value;

// TODO: MIR shouldn't know about this codegen restriction; this is a temporary hack.
pub const MAX_DIRECT_ACCESS_LEN: usize = boxed::Vector::<boxed::Any>::MAX_INLINE_LEN;

fn vector_member_type(vector_value: &Value) -> &abitype::BoxedABIType {
    if let Value::Reg(reg_value) = vector_value {
        if let abitype::ABIType::Boxed(abitype::BoxedABIType::Vector(member_boxed_abi_type)) =
            &reg_value.abi_type
        {
            return *member_boxed_abi_type;
        }
    }

    &abitype::BoxedABIType::Any
}

/// Loads a vector member from a vector of known length
///
/// [`vector_length`] must be less than [`MAX_DIRECT_ACCESS_LENGTH`]
pub fn load_vector_member(
    ehx: &mut EvalHirCtx,
    b: &mut Builder,
    span: Span,
    vector_len: usize,
    vector_value: &Value,
    member_index: usize,
) -> Value {
    use crate::mir::ops::*;
    use crate::mir::tagset::TypeTagSet;
    use crate::mir::value::build_reg::value_to_reg;
    use crate::mir::value::types::TypeHint;
    use crate::mir::value::RegValue;

    let member_possible_type_tags: TypeTagSet = vector_member_type(vector_value).into();

    let vector_reg = value_to_reg(
        ehx,
        b,
        span,
        &vector_value,
        &abitype::BoxedABIType::Vector(&abitype::BoxedABIType::Any).into(),
    );

    let member_reg = b.push_reg(
        span,
        OpKind::LoadBoxedVectorMember,
        LoadBoxedVectorMemberOp {
            vector_reg: vector_reg.into(),
            known_vector_len: vector_len as usize,
            member_index: member_index as usize,
        },
    );

    (RegValue {
        reg: member_reg,
        possible_type_tags: member_possible_type_tags,
        abi_type: abitype::BoxedABIType::Any.into(),
        type_hint: TypeHint::None,
    })
    .into()
}
