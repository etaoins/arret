use arret_syntax::span::Span;

use arret_runtime::boxed;

use crate::mir::builder::Builder;
use crate::mir::error::Result;
use crate::mir::eval_hir::EvalHirCtx;
use crate::mir::value::list::{list_value_length, ListValueLength};
use crate::mir::Value;

pub fn length(
    ehx: &mut EvalHirCtx,
    b: &mut Option<Builder>,
    span: Span,
    arg_list_value: &Value,
) -> Result<Option<Value>> {
    let mut iter = arg_list_value.unsized_list_iter();
    let single_arg = iter.next_unchecked(b, span);

    let list_length = list_value_length(&single_arg);

    if let ListValueLength::Exact(known_length) = list_length {
        return Ok(Some(boxed::Int::new(ehx, known_length as i64).into()));
    }

    if let Some(b) = b {
        use crate::mir::ops::*;
        use crate::mir::value;
        use crate::mir::value::build_reg::value_to_reg;
        use arret_runtime::abitype;

        let list_reg = value_to_reg(
            ehx,
            b,
            span,
            &single_arg,
            &abitype::TOP_LIST_BOXED_ABI_TYPE.into(),
        );

        let length_reg = b.push_reg(
            span,
            OpKind::LoadBoxedListLength,
            LoadBoxedListLengthOp {
                list_reg: list_reg.into(),
                min_length: list_length.lower_bound(),
            },
        );

        return Ok(Some(
            value::RegValue::new(length_reg, abitype::ABIType::Int).into(),
        ));
    }

    Ok(None)
}

pub fn cons(
    _ehx: &mut EvalHirCtx,
    b: &mut Option<Builder>,
    span: Span,
    arg_list_value: &Value,
) -> Result<Option<Value>> {
    let mut iter = arg_list_value.unsized_list_iter();

    let head = iter.next_unchecked(b, span);
    let rest = iter.next_unchecked(b, span);

    Ok(Some(Value::List(Box::new([head]), Some(Box::new(rest)))))
}
