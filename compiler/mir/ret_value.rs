use runtime::abitype;
use syntax::span::Span;

use crate::mir::builder::Builder;
use crate::mir::eval_hir::EvalHirCtx;
use crate::mir::ops;
use crate::mir::value::Value;

/// Builds the ops to return a value from a function
///
/// This deals with uninhabited and void return values which require special handling.
pub fn build_ret_value(
    ehx: &mut EvalHirCtx,
    b: &mut Builder,
    span: Span,
    result_value: &Value,
    ret_abi: &abitype::RetABIType,
) {
    use crate::mir::value::build_reg::value_to_reg;

    if result_value.is_divergent() {
        return;
    }

    match ret_abi {
        abitype::RetABIType::Inhabited(abi_type) => {
            let ret_reg = value_to_reg(ehx, b, span, &result_value, abi_type);
            b.push(span, ops::OpKind::Ret(ret_reg.into()));
        }
        abitype::RetABIType::Never => {
            b.push(span, ops::OpKind::Unreachable);
        }
        abitype::RetABIType::Void => {
            b.push(span, ops::OpKind::RetVoid);
        }
    }
}
