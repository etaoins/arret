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
    use crate::mir::vector_member;

    let mut iter = arg_list_value.unsized_list_iter();
    let vector_value = iter.next_unchecked(b, span);
    let index_value = iter.next_unchecked(b, span);

    let known_length = if let Some(known_length) = known_vector_length_for_value(&vector_value) {
        known_length
    } else {
        return Ok(None);
    };

    let index = if let Some(index) = try_value_to_i64(index_value) {
        index as usize
    } else {
        return Ok(None);
    };

    if index > vector_member::MAX_DIRECT_ACCESS_LENGTH {
        return Ok(None);
    }

    Ok(Some(vector_member::load_vector_member(
        ehx,
        b,
        span,
        known_length,
        &vector_value,
        index,
    )))
}
