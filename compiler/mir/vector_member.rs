use arret_syntax::span::Span;

use arret_runtime::abitype;
use arret_runtime::boxed;

use crate::mir::builder::Builder;
use crate::mir::eval_hir::EvalHirCtx;
use crate::mir::value;
use crate::mir::value::Value;

// TODO: MIR shouldn't know about this codegen restriction; this is a temporary hack.
pub const MAX_DIRECT_ACCESS_LENGTH: usize = boxed::Vector::<boxed::Any>::MAX_INLINE_LENGTH;

/// Loads a vector member from a vector of known length
///
/// [`vector_length`] must be less than [`MAX_DIRECT_ACCESS_LENGTH`]
pub fn load_vector_member(
    ehx: &mut EvalHirCtx,
    b: &mut Builder,
    span: Span,
    vector_length: usize,
    vector_value: &Value,
    member_index: usize,
) -> Value {
    use crate::mir::ops::*;
    use crate::mir::value::build_reg::value_to_reg;

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
            known_vector_length: vector_length as usize,
            member_index: member_index as usize,
        },
    );

    value::RegValue::new(member_reg, abitype::BoxedABIType::Any.into()).into()
}
