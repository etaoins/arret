use arret_runtime::abitype;
use arret_syntax::span::Span;

use crate::mir::builder::{Builder, BuiltReg};
use crate::mir::error::{Error, Result};
use crate::mir::eval_hir::EvalHirCtx;
use crate::mir::ops;
use crate::mir::value;
use crate::mir::value::Value;

/// Builds the ops to return a value from a function
///
/// This deals with uninhabited and void return values which require special handling.
pub fn build_value_ret(
    ehx: &mut EvalHirCtx,
    b: &mut Builder,
    span: Span,
    result: Result<Value>,
    ret_abi: &abitype::RetAbiType,
) {
    use crate::mir::value::build_reg::value_to_reg;

    match result {
        Ok(result_value) => match ret_abi {
            abitype::RetAbiType::Inhabited(abi_type) => {
                let ret_reg = value_to_reg(ehx, b, span, &result_value, abi_type);
                b.push(span, ops::OpKind::Ret(ret_reg.into()));
            }
            abitype::RetAbiType::Never => {
                b.push(span, ops::OpKind::Unreachable);
            }
            abitype::RetAbiType::Void => {
                b.push(span, ops::OpKind::RetVoid);
            }
        },
        Err(Error::Diverged) => {}
        Err(other) => {
            panic!("unexpected error when returning value: {:?}", other);
        }
    }
}

pub fn ret_reg_to_value(ret_reg: BuiltReg, ret_abi: abitype::RetAbiType) -> Result<Value> {
    match ret_abi {
        abitype::RetAbiType::Inhabited(abi_type) => {
            Ok(value::RegValue::new(ret_reg, abi_type).into())
        }
        abitype::RetAbiType::Never => Err(Error::Diverged),
        abitype::RetAbiType::Void => Ok(Value::List(Box::new([]), None)),
    }
}
