use arret_syntax::span::Span;

use crate::mir::builder::Builder;
use crate::mir::error::{Error, Result};
use crate::mir::eval_hir::EvalHirCtx;
use crate::mir::value::to_const::value_to_const;
use crate::mir::value::Value;

fn try_pretty_print_list_value(
    ehx: &mut EvalHirCtx,
    b: &mut Builder,
    span: Span,
    arg_list_value: &Value,
) -> Option<String> {
    let mut output: Vec<u8> = vec![];
    let mut list_iter = arg_list_value.try_sized_list_iter()?;

    while let Some(value) = list_iter.next(b, span) {
        let boxed = value_to_const(ehx, &value)?;
        arret_runtime_syntax::writer::pretty_print_boxed(&mut output, ehx, boxed);
    }

    Some(String::from_utf8(output).expect("pretty printed invalid UTF-8"))
}

pub fn panics(
    ehx: &mut EvalHirCtx,
    b: &mut Builder,
    span: Span,
    arg_list_value: &Value,
) -> Result<Option<Value>> {
    use crate::mir::ops::*;

    if let Some(message) = try_pretty_print_list_value(ehx, b, span, arg_list_value) {
        b.push(span, OpKind::Panic(message));
        Err(Error::Diverged)
    } else {
        Ok(None)
    }
}
