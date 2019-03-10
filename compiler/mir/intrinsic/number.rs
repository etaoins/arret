use syntax::span::Span;

use runtime::boxed::TypeTag;

use crate::mir::builder::Builder;
use crate::mir::error::Result;
use crate::mir::eval_hir::EvalHirCtx;
use crate::mir::value::types::possible_type_tags_for_value;
use crate::mir::Value;

pub fn int(
    _ehx: &mut EvalHirCtx,
    b: &mut Option<Builder>,
    span: Span,
    arg_list_value: &Value,
) -> Result<Option<Value>> {
    let value = arg_list_value.list_iter().next_unchecked(b, span);

    Ok(
        if possible_type_tags_for_value(&value) == TypeTag::Int.into() {
            Some(value)
        } else {
            None
        },
    )
}

pub fn float(
    _ehx: &mut EvalHirCtx,
    b: &mut Option<Builder>,
    span: Span,
    arg_list_value: &Value,
) -> Result<Option<Value>> {
    let value = arg_list_value.list_iter().next_unchecked(b, span);

    Ok(
        if possible_type_tags_for_value(&value) == TypeTag::Float.into() {
            Some(value)
        } else {
            None
        },
    )
}
