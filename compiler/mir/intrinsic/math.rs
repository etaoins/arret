use syntax::span::Span;

use crate::mir::builder::Builder;
use crate::mir::error::Result;
use crate::mir::eval_hir::EvalHirCtx;
use crate::mir::value::list::list_value_length;
use crate::mir::Value;

pub fn add(
    _ehx: &mut EvalHirCtx,
    b: &mut Option<Builder>,
    span: Span,
    arg_list_value: &Value,
) -> Result<Option<Value>> {
    if list_value_length(&arg_list_value) == Some(1) {
        Ok(Some(arg_list_value.list_iter().next_unchecked(b, span)))
    } else {
        Ok(None)
    }
}

pub fn mul(
    _ehx: &mut EvalHirCtx,
    b: &mut Option<Builder>,
    span: Span,
    arg_list_value: &Value,
) -> Result<Option<Value>> {
    if list_value_length(&arg_list_value) == Some(1) {
        Ok(Some(arg_list_value.list_iter().next_unchecked(b, span)))
    } else {
        Ok(None)
    }
}
