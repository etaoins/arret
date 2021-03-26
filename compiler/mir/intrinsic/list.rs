use arret_syntax::span::Span;

use arret_runtime::boxed;

use crate::mir::builder::Builder;
use crate::mir::error::Result;
use crate::mir::eval_hir::EvalHirCtx;
use crate::mir::value::list::{list_value_len, ListValueLen};
use crate::mir::Value;

pub fn length(
    ehx: &mut EvalHirCtx,
    b: &mut Option<Builder>,
    span: Span,
    arg_list_value: &Value,
) -> Result<Option<Value>> {
    let mut iter = arg_list_value.unsized_list_iter();
    let single_arg = iter.next_unchecked(b, span);

    let list_len = list_value_len(&single_arg);

    if let ListValueLen::Exact(known_len) = list_len {
        return Ok(Some(boxed::Int::new(ehx, known_len as i64).into()));
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

        let list_len_reg = b.push_reg(
            span,
            OpKind::LoadBoxedListLen,
            LoadBoxedListLenOp {
                list_reg: list_reg.into(),
                min_list_len: list_len.lower_bound(),
            },
        );

        return Ok(Some(
            value::RegValue::new(list_len_reg, abitype::AbiType::Int).into(),
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

pub fn repeat(
    _ehx: &mut EvalHirCtx,
    b: &mut Option<Builder>,
    span: Span,
    arg_list_value: &Value,
) -> Result<Option<Value>> {
    // Avoid creating giant constants at compile time
    const MAX_REPEAT_EVAL_LEN: i64 = 64;

    use crate::mir::intrinsic::num_utils::try_value_to_i64;

    let mut iter = arg_list_value.unsized_list_iter();

    let count_value = iter.next_unchecked(b, span);
    let count = if let Some(count) = try_value_to_i64(count_value) {
        count
    } else {
        return Ok(None);
    };

    let value = iter.next_unchecked(b, span);

    if count <= 0 {
        return Ok(Some(Value::List(Box::new([]), None)));
    } else if count > MAX_REPEAT_EVAL_LEN {
        return Ok(None);
    }

    // This lets us build a list of a known length and MIR values
    Ok(Some(Value::List(
        std::iter::repeat(value).take(count as usize).collect(),
        None,
    )))
}
