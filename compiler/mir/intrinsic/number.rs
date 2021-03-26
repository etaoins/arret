use arret_syntax::span::Span;

use arret_runtime::abitype;
use arret_runtime::boxed;
use arret_runtime::boxed::prelude::*;

use crate::mir::builder::{Builder, BuiltReg};
use crate::mir::error::Result;
use crate::mir::eval_hir::EvalHirCtx;
use crate::mir::intrinsic::num_utils::{num_value_to_float_reg, NumOperand};
use crate::mir::intrinsic::BuildOutcome;
use crate::mir::ops::Comparison;

use crate::mir::value;
use crate::mir::value::list::SizedListIterator;
use crate::mir::value::types::possible_type_tags_for_value;
use crate::mir::value::Value;

fn build_operand_pair_compare(
    ehx: &mut EvalHirCtx,
    b: &mut Builder,
    span: Span,
    left_value: &Value,
    right_value: &Value,
    comparison: Comparison,
) -> Option<BuiltReg> {
    use crate::mir::ops::*;

    let left_num_operand = NumOperand::try_from_value(ehx, b, span, left_value)?;
    let right_num_operand = NumOperand::try_from_value(ehx, b, span, right_value)?;

    match (left_num_operand, right_num_operand) {
        (NumOperand::Int(left_int_reg), NumOperand::Int(right_int_reg)) => Some(b.push_reg(
            span,
            OpKind::IntCompare,
            CompareOp {
                comparison,
                lhs_reg: left_int_reg.into(),
                rhs_reg: right_int_reg.into(),
            },
        )),
        (NumOperand::Float(left_float_reg), NumOperand::Int(right_int_reg)) => {
            let right_float_reg = b.push_reg(span, OpKind::Int64ToFloat, right_int_reg.into());
            Some(b.push_reg(
                span,
                OpKind::FloatCompare,
                CompareOp {
                    comparison,
                    lhs_reg: left_float_reg.into(),
                    rhs_reg: right_float_reg.into(),
                },
            ))
        }
        (NumOperand::Int(left_int_reg), NumOperand::Float(right_float_reg)) => {
            let left_float_reg = b.push_reg(span, OpKind::Int64ToFloat, left_int_reg.into());
            Some(b.push_reg(
                span,
                OpKind::FloatCompare,
                CompareOp {
                    comparison,
                    lhs_reg: left_float_reg.into(),
                    rhs_reg: right_float_reg.into(),
                },
            ))
        }
        (NumOperand::Float(left_float_reg), NumOperand::Float(right_float_reg)) => {
            Some(b.push_reg(
                span,
                OpKind::FloatCompare,
                CompareOp {
                    comparison,
                    lhs_reg: left_float_reg.into(),
                    rhs_reg: right_float_reg.into(),
                },
            ))
        }
    }
}

fn build_operand_iter_compare(
    ehx: &mut EvalHirCtx,
    b: &mut Builder,
    span: Span,
    left_value: &Value,
    rest_iter: &mut SizedListIterator,
    comparison: Comparison,
) -> Option<BuiltReg> {
    use crate::mir::ops::*;

    let right_value = rest_iter.next(b, span).unwrap();
    let compare_result_reg =
        build_operand_pair_compare(ehx, b, span, left_value, &right_value, comparison)?;

    let combined_result_reg;
    if rest_iter.is_empty() {
        // We're terminal, this is simple
        combined_result_reg = compare_result_reg;
    } else {
        let mut rest_b = Builder::new();
        let rest_result_reg = build_operand_iter_compare(
            ehx,
            &mut rest_b,
            span,
            &right_value,
            rest_iter,
            comparison,
        )?;

        combined_result_reg = b.alloc_local();
        b.push(
            span,
            OpKind::Cond(CondOp {
                reg_phi: Some(RegPhi {
                    output_reg: combined_result_reg.into(),
                    true_result_reg: rest_result_reg.into(),
                    // This is known false
                    false_result_reg: compare_result_reg.into(),
                }),
                test_reg: compare_result_reg.into(),
                true_ops: rest_b.into_ops(),
                false_ops: Box::new([]),
            }),
        );
    };

    Some(combined_result_reg)
}

fn compare_operand_list(
    ehx: &mut EvalHirCtx,
    b: &mut Builder,
    span: Span,
    arg_list_value: &Value,
    comparison: Comparison,
) -> BuildOutcome {
    let mut list_iter = if let Some(list_iter) = arg_list_value.try_sized_list_iter() {
        list_iter
    } else {
        return BuildOutcome::None;
    };

    if list_iter.len() == 1 {
        return BuildOutcome::ReturnValue(boxed::TRUE_INSTANCE.as_any_ref().into());
    }

    let left_value = list_iter.next(b, span).unwrap();
    match build_operand_iter_compare(ehx, b, span, &left_value, &mut list_iter, comparison) {
        Some(result_reg) => BuildOutcome::ReturnValue(
            value::RegValue::new(result_reg, abitype::AbiType::Bool).into(),
        ),
        None => BuildOutcome::None,
    }
}

pub fn int(
    _ehx: &mut EvalHirCtx,
    b: &mut Builder,
    span: Span,
    arg_list_value: &Value,
) -> Result<BuildOutcome> {
    let value = arg_list_value.unsized_list_iter().next_unchecked(b, span);

    Ok(
        if possible_type_tags_for_value(&value) == boxed::TypeTag::Int.into() {
            BuildOutcome::ReturnValue(value)
        } else {
            BuildOutcome::None
        },
    )
}

pub fn float(
    ehx: &mut EvalHirCtx,
    b: &mut Builder,
    span: Span,
    arg_list_value: &Value,
) -> Result<BuildOutcome> {
    let value = arg_list_value.unsized_list_iter().next_unchecked(b, span);

    Ok(BuildOutcome::ReturnValue(
        value::RegValue::new(
            num_value_to_float_reg(ehx, b, span, &value),
            abitype::AbiType::Float,
        )
        .into(),
    ))
}

pub fn num_lt(
    ehx: &mut EvalHirCtx,
    b: &mut Builder,
    span: Span,
    arg_list_value: &Value,
) -> Result<BuildOutcome> {
    Ok(compare_operand_list(
        ehx,
        b,
        span,
        arg_list_value,
        Comparison::Lt,
    ))
}

pub fn num_le(
    ehx: &mut EvalHirCtx,
    b: &mut Builder,
    span: Span,
    arg_list_value: &Value,
) -> Result<BuildOutcome> {
    Ok(compare_operand_list(
        ehx,
        b,
        span,
        arg_list_value,
        Comparison::Le,
    ))
}

pub fn num_eq(
    ehx: &mut EvalHirCtx,
    b: &mut Builder,
    span: Span,
    arg_list_value: &Value,
) -> Result<BuildOutcome> {
    Ok(compare_operand_list(
        ehx,
        b,
        span,
        arg_list_value,
        Comparison::Eq,
    ))
}

pub fn num_gt(
    ehx: &mut EvalHirCtx,
    b: &mut Builder,
    span: Span,
    arg_list_value: &Value,
) -> Result<BuildOutcome> {
    Ok(compare_operand_list(
        ehx,
        b,
        span,
        arg_list_value,
        Comparison::Gt,
    ))
}

pub fn num_ge(
    ehx: &mut EvalHirCtx,
    b: &mut Builder,
    span: Span,
    arg_list_value: &Value,
) -> Result<BuildOutcome> {
    Ok(compare_operand_list(
        ehx,
        b,
        span,
        arg_list_value,
        Comparison::Ge,
    ))
}
