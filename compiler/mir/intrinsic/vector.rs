use arret_syntax::span::Span;

use arret_runtime::boxed;

use crate::mir::builder::Builder;
use crate::mir::error::Result;
use crate::mir::eval_hir::EvalHirCtx;
use crate::mir::value::types::TypeHint;
use crate::mir::Value;

pub fn vector_length(
    ehx: &mut EvalHirCtx,
    b: &mut Builder,
    span: Span,
    arg_list_value: &Value,
) -> Result<Option<Value>> {
    let mut iter = arg_list_value.unsized_list_iter();
    let single_arg = iter.next_unchecked(b, span);

    if let Value::Reg(reg_value) = single_arg {
        if let TypeHint::KnownVectorLength(known_length) = reg_value.type_hint {
            return Ok(Some(boxed::Int::new(ehx, known_length as i64).into()));
        }
    }

    Ok(None)
}
