use arret_syntax::span::Span;

use arret_runtime::boxed;

use crate::mir::builder::Builder;
use crate::mir::error::Result;
use crate::mir::eval_hir::EvalHirCtx;
use crate::mir::intrinsic::num_utils::try_value_to_i64;
use crate::mir::intrinsic::BuildOutcome;
use crate::mir::value::types::known_vector_len_for_value;
use crate::mir::Value;

pub fn vector_length(
    ehx: &mut EvalHirCtx,
    b: &mut Builder,
    span: Span,
    arg_list_value: &Value,
) -> Result<BuildOutcome> {
    use crate::mir::ops::*;
    use crate::mir::value;
    use crate::mir::value::build_reg::value_to_reg;
    use arret_runtime::abitype;

    let mut iter = arg_list_value.unsized_list_iter();
    let vector_value = iter.next_unchecked(b, span);

    if let Some(known_len) = known_vector_len_for_value(&vector_value) {
        return Ok(BuildOutcome::ReturnValue(
            boxed::Int::new(ehx, known_len as i64).into(),
        ));
    }

    let vector_reg = value_to_reg(
        ehx,
        b,
        span,
        &vector_value,
        &abitype::BoxedABIType::Vector(&abitype::BoxedABIType::Any).into(),
    )
    .into();

    let vector_len_reg = b.push_reg(span, OpKind::LoadBoxedVectorLen, vector_reg);
    Ok(BuildOutcome::ReturnValue(
        value::RegValue::new(vector_len_reg, abitype::ABIType::Int).into(),
    ))
}

pub fn vector_ref(
    ehx: &mut EvalHirCtx,
    b: &mut Builder,
    span: Span,
    arg_list_value: &Value,
) -> Result<BuildOutcome> {
    use crate::mir::vector_member;

    let mut iter = arg_list_value.unsized_list_iter();
    let vector_value = iter.next_unchecked(b, span);
    let index_value = iter.next_unchecked(b, span);

    let known_len = if let Some(known_len) = known_vector_len_for_value(&vector_value) {
        known_len
    } else {
        return Ok(BuildOutcome::None);
    };

    let index = if let Some(index) = try_value_to_i64(index_value) {
        index as usize
    } else {
        return Ok(BuildOutcome::None);
    };

    Ok(BuildOutcome::ReturnValue(
        vector_member::load_vector_member(ehx, b, span, known_len, &vector_value, index),
    ))
}
