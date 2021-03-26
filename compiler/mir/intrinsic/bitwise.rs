use arret_syntax::span::Span;

use arret_runtime::abitype;

use crate::mir::builder::Builder;
use crate::mir::error::Result;
use crate::mir::eval_hir::EvalHirCtx;
use crate::mir::intrinsic::num_utils::try_value_to_i64;
use crate::mir::intrinsic::BuildOutcome;
use crate::mir::ops::{BinaryOp, OpKind, RegId, ShiftOp};
use crate::mir::value::build_reg::value_to_reg;

use crate::mir::value;
use crate::mir::value::Value;

fn fold_bitwise_operands<O>(
    ehx: &mut EvalHirCtx,
    b: &mut Builder,
    span: Span,
    arg_list_value: &Value,
    bitwise_op: O,
) -> BuildOutcome
where
    O: Fn(RegId, BinaryOp) -> OpKind + Copy,
{
    let mut list_iter = if let Some(list_iter) = arg_list_value.try_sized_list_iter() {
        list_iter
    } else {
        return BuildOutcome::None;
    };

    let initial_value = list_iter.next(b, span).unwrap();
    let mut acc_reg = value_to_reg(ehx, b, span, &initial_value, &abitype::AbiType::Int);

    while let Some(next_value) = list_iter.next(b, span) {
        let next_reg = value_to_reg(ehx, b, span, &next_value, &abitype::AbiType::Int);

        acc_reg = b.push_reg(
            span,
            bitwise_op,
            BinaryOp {
                lhs_reg: acc_reg.into(),
                rhs_reg: next_reg.into(),
            },
        );
    }

    BuildOutcome::ReturnValue(value::RegValue::new(acc_reg, abitype::AbiType::Int).into())
}

fn bit_shift_op<O>(
    ehx: &mut EvalHirCtx,
    b: &mut Builder,
    span: Span,
    arg_list_value: &Value,
    shift_op: O,
) -> BuildOutcome
where
    O: Fn(RegId, ShiftOp) -> OpKind + Copy,
{
    let mut iter = arg_list_value.unsized_list_iter();

    let int_value = iter.next_unchecked(b, span);
    let int_reg = value_to_reg(ehx, b, span, &int_value, &abitype::AbiType::Int);

    let bit_count_value = iter.next_unchecked(b, span);

    let bit_count = if let Some(bit_count) = try_value_to_i64(bit_count_value) {
        bit_count
    } else {
        return BuildOutcome::None;
    };

    if !(0..=64).contains(&bit_count) {
        return BuildOutcome::None;
    }

    let result_reg = b.push_reg(
        span,
        shift_op,
        ShiftOp {
            int_reg: int_reg.into(),
            bit_count: bit_count as u32,
        },
    );

    BuildOutcome::ReturnValue(value::RegValue::new(result_reg, abitype::AbiType::Int).into())
}

pub fn bit_and(
    ehx: &mut EvalHirCtx,
    b: &mut Builder,
    span: Span,
    arg_list_value: &Value,
) -> Result<BuildOutcome> {
    use crate::mir::ops::*;

    Ok(fold_bitwise_operands(
        ehx,
        b,
        span,
        arg_list_value,
        OpKind::Int64BitwiseAnd,
    ))
}

pub fn bit_or(
    ehx: &mut EvalHirCtx,
    b: &mut Builder,
    span: Span,
    arg_list_value: &Value,
) -> Result<BuildOutcome> {
    use crate::mir::ops::*;

    Ok(fold_bitwise_operands(
        ehx,
        b,
        span,
        arg_list_value,
        OpKind::Int64BitwiseOr,
    ))
}

pub fn bit_xor(
    ehx: &mut EvalHirCtx,
    b: &mut Builder,
    span: Span,
    arg_list_value: &Value,
) -> Result<BuildOutcome> {
    use crate::mir::ops::*;

    Ok(fold_bitwise_operands(
        ehx,
        b,
        span,
        arg_list_value,
        OpKind::Int64BitwiseXor,
    ))
}

pub fn bit_not(
    ehx: &mut EvalHirCtx,
    b: &mut Builder,
    span: Span,
    arg_list_value: &Value,
) -> Result<BuildOutcome> {
    use crate::mir::ops::*;

    let mut iter = arg_list_value.unsized_list_iter();

    let int_value = iter.next_unchecked(b, span);
    let int_reg = value_to_reg(ehx, b, span, &int_value, &abitype::AbiType::Int);

    let result_reg = b.push_reg(span, OpKind::Int64BitwiseNot, int_reg.into());

    Ok(BuildOutcome::ReturnValue(
        value::RegValue::new(result_reg, abitype::AbiType::Int).into(),
    ))
}

pub fn bit_shift_left(
    ehx: &mut EvalHirCtx,
    b: &mut Builder,
    span: Span,
    arg_list_value: &Value,
) -> Result<BuildOutcome> {
    Ok(bit_shift_op(
        ehx,
        b,
        span,
        arg_list_value,
        OpKind::Int64ShiftLeft,
    ))
}

pub fn bit_shift_right(
    ehx: &mut EvalHirCtx,
    b: &mut Builder,
    span: Span,
    arg_list_value: &Value,
) -> Result<BuildOutcome> {
    Ok(bit_shift_op(
        ehx,
        b,
        span,
        arg_list_value,
        OpKind::Int64ArithmeticShiftRight,
    ))
}

pub fn unsigned_bit_shift_right(
    ehx: &mut EvalHirCtx,
    b: &mut Builder,
    span: Span,
    arg_list_value: &Value,
) -> Result<BuildOutcome> {
    Ok(bit_shift_op(
        ehx,
        b,
        span,
        arg_list_value,
        OpKind::Int64LogicalShiftRight,
    ))
}
