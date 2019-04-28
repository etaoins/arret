use arret_syntax::span::Span;

use arret_runtime::abitype;
use arret_runtime::boxed;

use crate::mir::builder::{Builder, BuiltReg};
use crate::mir::error::Result;
use crate::mir::eval_hir::EvalHirCtx;

use crate::mir::value;
use crate::mir::value::build_reg::value_to_reg;
use crate::mir::value::types::possible_type_tags_for_value;
use crate::mir::value::Value;

/// Converts a value of type `Num` to a float reg
pub fn num_value_to_float_reg(
    ehx: &mut EvalHirCtx,
    outer_b: &mut Builder,
    span: Span,
    value: &Value,
) -> BuiltReg {
    use crate::mir::ops::*;

    let num_type_tags = [boxed::TypeTag::Int, boxed::TypeTag::Float]
        .iter()
        .collect();

    let possible_type_tags = possible_type_tags_for_value(value) & num_type_tags;

    if possible_type_tags == boxed::TypeTag::Float.into() {
        value_to_reg(ehx, outer_b, span, &value, &abitype::ABIType::Float)
    } else if possible_type_tags == boxed::TypeTag::Int.into() {
        let int64_reg = value_to_reg(ehx, outer_b, span, value, &abitype::ABIType::Int);
        outer_b.push_reg(span, OpKind::Int64ToFloat, int64_reg.into())
    } else {
        let boxed_any_reg = value_to_reg(
            ehx,
            outer_b,
            span,
            value,
            &abitype::BoxedABIType::Any.into(),
        )
        .into();

        let value_type_tag_reg = outer_b.push_reg(
            span,
            OpKind::LoadBoxedTypeTag,
            LoadBoxedTypeTagOp {
                subject_reg: boxed_any_reg,
                possible_type_tags: num_type_tags,
            },
        );

        let float_tag_reg = outer_b.push_reg(span, OpKind::ConstTypeTag, boxed::TypeTag::Float);

        let is_float_reg = outer_b.push_reg(
            span,
            OpKind::TypeTagEqual,
            BinaryOp {
                lhs_reg: value_type_tag_reg.into(),
                rhs_reg: float_tag_reg.into(),
            },
        );

        let mut is_float_b = Builder::new();
        let is_float_result_reg =
            value_to_reg(ehx, &mut is_float_b, span, &value, &abitype::ABIType::Float);

        let mut is_int_b = Builder::new();
        let int64_reg = value_to_reg(ehx, &mut is_int_b, span, value, &abitype::ABIType::Int);
        let is_int_result_reg = is_int_b.push_reg(span, OpKind::Int64ToFloat, int64_reg.into());

        let output_reg = RegId::alloc();
        outer_b.push(
            span,
            OpKind::Cond(CondOp {
                reg_phi: Some(RegPhi {
                    output_reg,
                    true_result_reg: is_float_result_reg.into(),
                    false_result_reg: is_int_result_reg.into(),
                }),
                test_reg: is_float_reg.into(),
                true_ops: is_float_b.into_ops(),
                false_ops: is_int_b.into_ops(),
            }),
        );

        BuiltReg::Local(output_reg)
    }
}

pub fn int(
    _ehx: &mut EvalHirCtx,
    b: &mut Builder,
    span: Span,
    arg_list_value: &Value,
) -> Result<Option<Value>> {
    let value = arg_list_value.unsized_list_iter().next_unchecked(b, span);

    Ok(
        if possible_type_tags_for_value(&value) == boxed::TypeTag::Int.into() {
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
    let value = arg_list_value.unsized_list_iter().next_unchecked(b, span);

    Ok(Some(
        value::RegValue::new(
            num_value_to_float_reg(ehx, b, span, &value),
            abitype::ABIType::Float,
        )
        .into(),
    ))
}
