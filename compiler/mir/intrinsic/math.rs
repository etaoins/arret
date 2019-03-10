use syntax::span::Span;

use runtime::abitype;
use runtime::boxed;

use crate::mir::builder::{Builder, BuiltReg};
use crate::mir::error::Result;
use crate::mir::eval_hir::EvalHirCtx;
use crate::mir::ops::{BinaryOp, OpKind, RegId};

use crate::mir::value;
use crate::mir::value::build_reg::value_to_reg;
use crate::mir::value::list::SizedListIterator;
use crate::mir::value::Value;

/// Represents a numerical operand of a known type
enum NumOperand {
    Int(BuiltReg),
    Float(BuiltReg),
}

impl NumOperand {
    /// Attempts to build numerical operand from a value
    ///
    /// If the value isn't a definite `Int` or `Float` this will return `None`
    fn try_from_value(
        ehx: &mut EvalHirCtx,
        b: &mut Builder,
        span: Span,
        value: Value,
    ) -> Option<NumOperand> {
        use crate::mir::value::types::possible_type_tags_for_value;

        let possible_type_tags = possible_type_tags_for_value(&value);

        if possible_type_tags == boxed::TypeTag::Int.into() {
            let int64_reg = value_to_reg(ehx, b, span, &value, &abitype::ABIType::Int);
            Some(NumOperand::Int(int64_reg))
        } else if possible_type_tags == boxed::TypeTag::Float.into() {
            let float_reg = value_to_reg(ehx, b, span, &value, &abitype::ABIType::Float);
            Some(NumOperand::Float(float_reg))
        } else {
            None
        }
    }

    /// Converts this operand in to a value
    fn into_value(self) -> Value {
        match self {
            NumOperand::Int(built_reg) => {
                value::RegValue::new(built_reg, abitype::ABIType::Int).into()
            }
            NumOperand::Float(built_reg) => {
                value::RegValue::new(built_reg, abitype::ABIType::Float).into()
            }
        }
    }
}

/// Folds a series of numerical operands with the given reducers for `Int` and `Float`s
fn fold_operands<I, F>(
    ehx: &mut EvalHirCtx,
    b: &mut Builder,
    span: Span,
    mut acc: NumOperand,
    mut list_iter: SizedListIterator,
    int64_op: I,
    float_op: F,
) -> Option<Value>
where
    I: Fn(RegId, BinaryOp) -> OpKind + Copy,
    F: Fn(RegId, BinaryOp) -> OpKind + Copy,
{
    while let Some(value) = list_iter.next(b, span) {
        let operand = NumOperand::try_from_value(ehx, b, span, value)?;

        acc = match (acc, operand) {
            (NumOperand::Int(lhs_reg), NumOperand::Int(rhs_reg)) => {
                let result_reg = b.push_reg(
                    span,
                    int64_op,
                    BinaryOp {
                        lhs_reg: lhs_reg.into(),
                        rhs_reg: rhs_reg.into(),
                    },
                );

                NumOperand::Int(result_reg)
            }
            (NumOperand::Float(lhs_reg), NumOperand::Float(rhs_reg)) => {
                let result_reg = b.push_reg(
                    span,
                    float_op,
                    BinaryOp {
                        lhs_reg: lhs_reg.into(),
                        rhs_reg: rhs_reg.into(),
                    },
                );

                NumOperand::Float(result_reg)
            }
            (NumOperand::Float(float_reg), NumOperand::Int(int_reg)) => {
                let int_as_float_reg = b.push_reg(span, OpKind::Int64ToFloat, int_reg.into());

                let result_reg = b.push_reg(
                    span,
                    float_op,
                    BinaryOp {
                        lhs_reg: float_reg.into(),
                        rhs_reg: int_as_float_reg.into(),
                    },
                );

                NumOperand::Float(result_reg)
            }
            (NumOperand::Int(int_reg), NumOperand::Float(float_reg)) => {
                let int_as_float_reg = b.push_reg(span, OpKind::Int64ToFloat, int_reg.into());

                let result_reg = b.push_reg(
                    span,
                    float_op,
                    BinaryOp {
                        lhs_reg: int_as_float_reg.into(),
                        rhs_reg: float_reg.into(),
                    },
                );

                NumOperand::Float(result_reg)
            }
        }
    }

    Some(acc.into_value())
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
) -> Option<Value>
where
    I: Fn(RegId, BinaryOp) -> OpKind + Copy,
    F: Fn(RegId, BinaryOp) -> OpKind + Copy,
{
    let initial_value = list_iter.next(b, span).unwrap();
    let acc = NumOperand::try_from_value(ehx, b, span, initial_value)?;

    fold_operands(ehx, b, span, acc, list_iter, int64_op, float_op)
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
) -> Option<Value>
where
    I: Fn(RegId, BinaryOp) -> OpKind + Copy,
    F: Fn(RegId, BinaryOp) -> OpKind + Copy,
{
    let mut list_iter = arg_list_value.try_sized_list_iter()?;

    if list_iter.len() == 1 {
        // The associative math functions (`+` and `*`) act as the identity function with 1 arg.
        // We check here so even if the value doesn't have a definite type it's still returned.
        return list_iter.next(b, span);
    }

    reduce_operands(ehx, b, span, list_iter, int64_op, float_op)
}

pub fn add(
    ehx: &mut EvalHirCtx,
    b: &mut Builder,
    span: Span,
    arg_list_value: &Value,
) -> Result<Option<Value>> {
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
) -> Result<Option<Value>> {
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
) -> Result<Option<Value>> {
    use crate::mir::ops::*;

    let list_iter = if let Some(list_iter) = arg_list_value.try_sized_list_iter() {
        list_iter
    } else {
        return Ok(None);
    };

    if list_iter.len() == 1 {
        // Rewrite `(- x)` to `(- 0 x)`
        let const_int_zero = NumOperand::Int(b.push_reg(span, OpKind::ConstInt64, 0));

        Ok(fold_operands(
            ehx,
            b,
            span,
            const_int_zero,
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
) -> Result<Option<Value>> {
    use crate::mir::ops::*;

    let mut list_iter = if let Some(list_iter) = arg_list_value.try_sized_list_iter() {
        list_iter
    } else {
        return Ok(None);
    };

    let initial_value = list_iter.next(b, span).unwrap();
    let initial_reg = value_to_reg(ehx, b, span, &initial_value, &abitype::ABIType::Float);

    let result_reg = if list_iter.len() == 0 {
        // Rewrite `(/ x)` to `(/ 1 x)`
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
            let value_reg = value_to_reg(ehx, b, span, &value, &abitype::ABIType::Float);

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

    Ok(Some(
        value::RegValue::new(result_reg, abitype::ABIType::Float).into(),
    ))
}
