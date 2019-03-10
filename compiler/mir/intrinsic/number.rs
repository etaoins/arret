use syntax::span::Span;

use runtime::boxed::TypeTag;

use crate::mir::builder::Builder;
use crate::mir::error::Result;
use crate::mir::eval_hir::EvalHirCtx;
use crate::mir::value::types::possible_type_tags_for_value;
use crate::mir::Value;

pub fn int(
    _ehx: &mut EvalHirCtx,
    b: &mut Builder,
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
    ehx: &mut EvalHirCtx,
    b: &mut Builder,
    span: Span,
    arg_list_value: &Value,
) -> Result<Option<Value>> {
    let value = arg_list_value.list_iter().next_unchecked(b, span);
    let possible_type_tags = possible_type_tags_for_value(&value);

    Ok(if possible_type_tags == TypeTag::Float.into() {
        Some(value)
    } else if possible_type_tags == TypeTag::Int.into() {
        use crate::mir::ops::*;
        use crate::mir::value;
        use crate::mir::value::build_reg::value_to_reg;
        use runtime::abitype;

        let int_reg = value_to_reg(ehx, b, span, &value, &abitype::ABIType::Int);
        let float_reg = b.push_reg(span, OpKind::Int64ToFloat, int_reg.into());

        Some(value::RegValue::new(float_reg, abitype::ABIType::Float).into())
    } else {
        None
    })
}
