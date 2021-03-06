use arret_syntax::span::Span;

use crate::mir::builder::Builder;
use crate::mir::error::{Error, Result};
use crate::mir::eval_hir::EvalHirCtx;
use crate::mir::intrinsic::BuildOutcome;
use crate::mir::value::Value;

use crate::mir::intrinsic::partial_print::{partial_pretty_print, PartialPrint};

pub fn panics(
    ehx: &mut EvalHirCtx,
    b: &mut Builder,
    span: Span,
    arg_list_value: &Value,
) -> Result<BuildOutcome> {
    use crate::mir::ops::*;

    match partial_pretty_print(ehx, b, span, arg_list_value) {
        // Can simplify this to a single MIR opt
        Some(PartialPrint::Constant(message)) => {
            b.push(span, OpKind::Panic(message));
            Err(Error::Diverged)
        }
        // Still contains variables. Simplify the arguments.
        Some(partial_print) => Ok(BuildOutcome::SimplifiedArgs(
            partial_print.into_arg_list_value(ehx),
        )),
        // Declined to partial print
        None => Ok(BuildOutcome::None),
    }
}
