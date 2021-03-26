//! Intrinsics for math operations on numbers
//!
//! This strives to match the behaviour of the stdlib in term of operation and conversion order.
//! For multi-operand math operations we behave as if we reduce our input pairwise from left to
//! right. If either pairwise operand is a `Float` then both operands are converted to `Float` and
//! the result is a `Float`. If both operands are `Int`s then we perform checked math to ensure
//! the value doesn't overflow its `Int` result.
//!
//! The input-dependent result type makes it difficult for us to build operations on values that
//! aren't definite `Int` or `Float` (i.e. `Num`). Once we encounter a known `Float` we can safely
//! convert every remaining operand to `Float` with at most a single branch per operand. However, if
//! we encounter an unknown `Num` along with another `Num` or `Int` we don't know the result type
//! of the pairwise operation. This can produce a combinatorial explosion of branches.
//!
//! For this reason this code will return `None` if it encounters a `Num` before a `Float`. This
//! will cause us to fallback to the stdlib at runtime.
//!
//! This also makes no attempt at simplification or strength reduction. The presumption is LLVM is
//! much better at this than we are.

use arret_syntax::span::Span;

use arret_runtime::abitype;

use crate::mir::builder::{Builder, BuiltReg};
use crate::mir::error::Result;
use crate::mir::eval_hir::EvalHirCtx;
use crate::mir::intrinsic::num_utils::{num_value_to_float_reg, try_value_to_i64, NumOperand};
use crate::mir::intrinsic::BuildOutcome;
use crate::mir::ops::{BinaryOp, OpKind, RegId};

use crate::mir::value;
use crate::mir::value::build_reg::value_to_reg;
use crate::mir::value::list::SizedListIterator;
use crate::mir::value::Value;

/// Folds a series of numerical operands as `Float`s
///
/// This is used once we know our result will be a `Float`
fn fold_float_operands<F>(
    ehx: &mut EvalHirCtx,
    b: &mut Builder,
    span: Span,
    mut acc_float_reg: BuiltReg,
    mut list_iter: SizedListIterator,
    float_op: F,
) -> Value
where
    F: Fn(RegId, BinaryOp) -> OpKind + Copy,
{
    while let Some(value) = list_iter.next(b, span) {
        let operand_reg = num_value_to_float_reg(ehx, b, span, &value);

        acc_float_reg = b.push_reg(
            span,
            float_op,
            BinaryOp {
                lhs_reg: acc_float_reg.into(),
                rhs_reg: operand_reg.into(),
            },
        );
    }

    value::RegValue::new(acc_float_reg, abitype::AbiType::Float).into()
}

/// Folds a series of numerical operands with the given reducers for `Int` and `Float`s
///
/// This is used when the precise type of the result is still unknown
fn fold_num_operands<I, F>(
    ehx: &mut EvalHirCtx,
    b: &mut Builder,
    span: Span,
    mut acc_int_reg: BuiltReg,
    mut list_iter: SizedListIterator,
    int64_op: I,
    float_op: F,
) -> BuildOutcome
where
    I: Fn(RegId, BinaryOp) -> OpKind + Copy,
    F: Fn(RegId, BinaryOp) -> OpKind + Copy,
{
    while let Some(value) = list_iter.next(b, span) {
        let operand = if let Some(operand) = NumOperand::try_from_value(ehx, b, span, &value) {
            operand
        } else {
            // Can't continue. Use the work we've done so far to simplify the
            // stdlib call.
            return BuildOutcome::SimplifiedArgs(Value::List(
                Box::new([
                    value::RegValue::new(acc_int_reg, abitype::AbiType::Int).into(),
                    value,
                ]),
                Some(Box::new(list_iter.into_rest())),
            ));
        };

        acc_int_reg = match operand {
            NumOperand::Int(operand_int_reg) => b.push_reg(
                span,
                int64_op,
                BinaryOp {
                    lhs_reg: acc_int_reg.into(),
                    rhs_reg: operand_int_reg.into(),
                },
            ),
            NumOperand::Float(operand_float_reg) => {
                let int_as_float_reg = b.push_reg(span, OpKind::Int64ToFloat, acc_int_reg.into());

                let result_reg = b.push_reg(
                    span,
                    float_op,
                    BinaryOp {
                        lhs_reg: int_as_float_reg.into(),
                        rhs_reg: operand_float_reg.into(),
                    },
                );

                return BuildOutcome::ReturnValue(fold_float_operands(
                    ehx, b, span, result_reg, list_iter, float_op,
                ));
            }
        }
    }

    BuildOutcome::ReturnValue(value::RegValue::new(acc_int_reg, abitype::AbiType::Int).into())
}

/// Reduces a series of numerical operands with the given reducer ops for `Int` and `Float`s
///
/// This does not assume the reducers are associative
fn reduce_operands<I, F>(
    ehx: &mut EvalHirCtx,
    b: &mut Builder,
    span: Span,
    mut list_iter: SizedListIterator,
    int64_op: I,
    float_op: F,
) -> BuildOutcome
where
    I: Fn(RegId, BinaryOp) -> OpKind + Copy,
    F: Fn(RegId, BinaryOp) -> OpKind + Copy,
{
    let initial_value = list_iter.next(b, span).unwrap();
    let initial_operand =
        if let Some(initial_operand) = NumOperand::try_from_value(ehx, b, span, &initial_value) {
            initial_operand
        } else {
            return BuildOutcome::None;
        };

    match initial_operand {
        NumOperand::Int(int_reg) => {
            fold_num_operands(ehx, b, span, int_reg, list_iter, int64_op, float_op)
        }
        NumOperand::Float(float_reg) => BuildOutcome::ReturnValue(fold_float_operands(
            ehx, b, span, float_reg, list_iter, float_op,
        )),
    }
}

/// Reduces a series of numerical operands with the given reducer ops for `Int` and `Float`s
///
/// This assumes the reducers are associative
fn reduce_assoc_operands<I, F>(
    ehx: &mut EvalHirCtx,
    b: &mut Builder,
    span: Span,
    arg_list_value: &Value,
    int64_op: I,
    float_op: F,
) -> BuildOutcome
where
    I: Fn(RegId, BinaryOp) -> OpKind + Copy,
    F: Fn(RegId, BinaryOp) -> OpKind + Copy,
{
    let mut list_iter = if let Some(list_iter) = arg_list_value.try_sized_list_iter() {
        list_iter
    } else {
        return BuildOutcome::None;
    };

    if list_iter.len() == 1 {
        // The associative math functions (`+` and `*`) act as the identity function with 1 arg.
        // We check here so even if the value doesn't have a definite type it's still returned.
        list_iter
            .next(b, span)
            .map_or(BuildOutcome::None, BuildOutcome::ReturnValue)
    } else {
        reduce_operands(ehx, b, span, list_iter, int64_op, float_op)
    }
}

pub fn add(
    ehx: &mut EvalHirCtx,
    b: &mut Builder,
    span: Span,
    arg_list_value: &Value,
) -> Result<BuildOutcome> {
    use crate::mir::ops::*;

    Ok(reduce_assoc_operands(
        ehx,
        b,
        span,
        arg_list_value,
        OpKind::Int64CheckedAdd,
        OpKind::FloatAdd,
    ))
}

pub fn mul(
    ehx: &mut EvalHirCtx,
    b: &mut Builder,
    span: Span,
    arg_list_value: &Value,
) -> Result<BuildOutcome> {
    use crate::mir::ops::*;

    Ok(reduce_assoc_operands(
        ehx,
        b,
        span,
        arg_list_value,
        OpKind::Int64CheckedMul,
        OpKind::FloatMul,
    ))
}

pub fn sub(
    ehx: &mut EvalHirCtx,
    b: &mut Builder,
    span: Span,
    arg_list_value: &Value,
) -> Result<BuildOutcome> {
    use crate::mir::ops::*;

    let list_iter = if let Some(list_iter) = arg_list_value.try_sized_list_iter() {
        list_iter
    } else {
        return Ok(BuildOutcome::None);
    };

    if list_iter.len() == 1 {
        // Rewrite `(- x)` to `(- 0 x)`
        let int_zero_reg = b.push_reg(span, OpKind::ConstInt64, 0);

        Ok(fold_num_operands(
            ehx,
            b,
            span,
            int_zero_reg,
            list_iter,
            OpKind::Int64CheckedSub,
            OpKind::FloatSub,
        ))
    } else {
        Ok(reduce_operands(
            ehx,
            b,
            span,
            list_iter,
            OpKind::Int64CheckedSub,
            OpKind::FloatSub,
        ))
    }
}

pub fn div(
    ehx: &mut EvalHirCtx,
    b: &mut Builder,
    span: Span,
    arg_list_value: &Value,
) -> Result<BuildOutcome> {
    use crate::mir::ops::*;

    let mut list_iter = if let Some(list_iter) = arg_list_value.try_sized_list_iter() {
        list_iter
    } else {
        return Ok(BuildOutcome::None);
    };

    let initial_value = list_iter.next(b, span).unwrap();
    let initial_reg = value_to_reg(ehx, b, span, &initial_value, &abitype::AbiType::Float);

    let result_reg = if list_iter.is_empty() {
        // Rewrite `(/ x)` to `(/ 1.0 x)`
        let const_one_reg = b.push_reg(span, OpKind::ConstFloat, 1.0f64);

        b.push_reg(
            span,
            OpKind::FloatDiv,
            BinaryOp {
                lhs_reg: const_one_reg.into(),
                rhs_reg: initial_reg.into(),
            },
        )
    } else {
        let mut acc = initial_reg;
        while let Some(value) = list_iter.next(b, span) {
            let value_reg = value_to_reg(ehx, b, span, &value, &abitype::AbiType::Float);

            acc = b.push_reg(
                span,
                OpKind::FloatDiv,
                BinaryOp {
                    lhs_reg: acc.into(),
                    rhs_reg: value_reg.into(),
                },
            );
        }

        acc
    };

    Ok(BuildOutcome::ReturnValue(
        value::RegValue::new(result_reg, abitype::AbiType::Float).into(),
    ))
}

fn int_division_op<CI, UI>(
    ehx: &mut EvalHirCtx,
    b: &mut Builder,
    span: Span,
    arg_list_value: &Value,
    checked_op_kind: CI,
    unchecked_op_kind: UI,
) -> BuildOutcome
where
    CI: FnOnce(RegId, BinaryOp) -> OpKind,
    UI: FnOnce(RegId, BinaryOp) -> OpKind,
{
    let mut iter = arg_list_value.unsized_list_iter();

    let numer_value = iter.next_unchecked(b, span);
    let numer_reg = value_to_reg(ehx, b, span, &numer_value, &abitype::AbiType::Int);

    let denom_value = iter.next_unchecked(b, span);
    let denom_reg = value_to_reg(ehx, b, span, &denom_value, &abitype::AbiType::Int);

    let needs_checked = match try_value_to_i64(denom_value) {
        None => {
            // Completely unknown, we need a check
            true
        }
        Some(0) | Some(-1) => {
            // Definite divide-by-zero or possible overflow
            true
        }
        Some(_) => {
            // Don't need a check
            false
        }
    };

    let div_binary_op = BinaryOp {
        lhs_reg: numer_reg.into(),
        rhs_reg: denom_reg.into(),
    };

    let result_reg = if needs_checked {
        b.push_reg(span, checked_op_kind, div_binary_op)
    } else {
        b.push_reg(span, unchecked_op_kind, div_binary_op)
    };

    BuildOutcome::ReturnValue(value::RegValue::new(result_reg, abitype::AbiType::Int).into())
}

pub fn quot(
    ehx: &mut EvalHirCtx,
    b: &mut Builder,
    span: Span,
    arg_list_value: &Value,
) -> Result<BuildOutcome> {
    Ok(int_division_op(
        ehx,
        b,
        span,
        arg_list_value,
        OpKind::Int64CheckedDiv,
        OpKind::Int64Div,
    ))
}

pub fn rem(
    ehx: &mut EvalHirCtx,
    b: &mut Builder,
    span: Span,
    arg_list_value: &Value,
) -> Result<BuildOutcome> {
    Ok(int_division_op(
        ehx,
        b,
        span,
        arg_list_value,
        OpKind::Int64CheckedRem,
        OpKind::Int64Rem,
    ))
}

pub fn sqrt(
    ehx: &mut EvalHirCtx,
    b: &mut Builder,
    span: Span,
    arg_list_value: &Value,
) -> Result<BuildOutcome> {
    let radicand_value = arg_list_value.unsized_list_iter().next_unchecked(b, span);

    let radicand_reg = value_to_reg(ehx, b, span, &radicand_value, &abitype::AbiType::Float);
    let result_reg = b.push_reg(span, OpKind::FloatSqrt, radicand_reg.into());

    Ok(BuildOutcome::ReturnValue(
        value::RegValue::new(result_reg, abitype::AbiType::Float).into(),
    ))
}
