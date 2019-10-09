use arret_syntax::span::Span;

use arret_runtime::boxed;

use crate::mir::builder::Builder;
use crate::mir::error::Result;
use crate::mir::eval_hir::EvalHirCtx;
use crate::mir::intrinsic::num_utils::try_value_to_i64;
use crate::mir::value::types::known_vector_length_for_value;
use crate::mir::Value;

pub fn vector_length(
    ehx: &mut EvalHirCtx,
    b: &mut Builder,
    span: Span,
    arg_list_value: &Value,
) -> Result<Option<Value>> {
    let mut iter = arg_list_value.unsized_list_iter();
    let vector_value = iter.next_unchecked(b, span);

    if let Some(known_length) = known_vector_length_for_value(&vector_value) {
        return Ok(Some(boxed::Int::new(ehx, known_length as i64).into()));
    }

    Ok(None)
}

pub fn vector_ref(
    ehx: &mut EvalHirCtx,
    b: &mut Builder,
    span: Span,
    arg_list_value: &Value,
) -> Result<Option<Value>> {
    use crate::mir::ops::*;
    use crate::mir::value;
    use crate::mir::value::build_reg::value_to_reg;
    use arret_runtime::abitype;

    let mut iter = arg_list_value.unsized_list_iter();
    let vector_value = iter.next_unchecked(b, span);
    let index_value = iter.next_unchecked(b, span);

    let known_length = if let Some(known_length) = known_vector_length_for_value(&vector_value) {
        known_length
    } else {
        return Ok(None);
    };

    let index = if let Some(index) = try_value_to_i64(index_value) {
        index
    } else {
        return Ok(None);
    };

    // TODO: MIR shouldn't know about this codegen restriction; this is a temporary hack.
    if index > boxed::Vector::<boxed::Any>::MAX_INLINE_LENGTH as i64 {
        return Ok(None);
    }

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
            known_vector_length: known_length as usize,
            member_index: index as usize,
        },
    );

    Ok(Some(
        value::RegValue::new(member_reg, abitype::BoxedABIType::Any.into()).into(),
    ))
}
