use arret_syntax::span::Span;

use crate::mir::builder::Builder;
use crate::mir::error::Result;
use crate::mir::eval_hir::EvalHirCtx;
use crate::mir::intrinsic::BuildOutcome;
use crate::mir::value::Value;

use crate::mir::intrinsic::partial_print::partial_pretty_print;

// `print!`, `println!` & `print-str`
pub fn print(
    ehx: &mut EvalHirCtx,
    b: &mut Builder,
    span: Span,
    arg_list_value: &Value,
) -> Result<BuildOutcome> {
    match partial_pretty_print(ehx, b, span, arg_list_value) {
        Some(partial_print) => Ok(BuildOutcome::SimplifiedArgs(
            partial_print.into_arg_list_value(ehx),
        )),
        None => Ok(BuildOutcome::None),
    }
}
