use syntax::span::Span;

use runtime::abitype;
use runtime::boxed;
use runtime::boxed::prelude::*;

use crate::codegen::GenABI;
use crate::mir::builder::{Builder, BuiltReg};
use crate::mir::eval_hir::EvalHirCtx;
use crate::mir::ops::*;
use crate::mir::value::build_reg::value_to_reg;
use crate::mir::value::from_reg::reg_to_value;
use crate::mir::value::to_const::value_to_const;
use crate::mir::value::Value;
use crate::ty;

fn runtime_compare(
    ehx: &mut EvalHirCtx,
    b: &mut Builder,
    span: Span,
    left_value: &Value,
    right_value: &Value,
) -> BuiltReg {
    let left_reg = value_to_reg(ehx, b, span, left_value, &abitype::BoxedABIType::Any.into());

    let right_reg = value_to_reg(
        ehx,
        b,
        span,
        right_value,
        &abitype::BoxedABIType::Any.into(),
    );

    let abi = GenABI {
        takes_task: false,
        params: Box::new([
            abitype::BoxedABIType::Any.into(),
            abitype::BoxedABIType::Any.into(),
        ]),
        ret: abitype::ABIType::Bool.into(),
    };

    let callee = Callee::StaticSymbol(StaticSymbol {
        symbol: "arret_runtime_equals",
        impure: false,
        abi,
    });

    b.push_reg(
        span,
        OpKind::Call,
        CallOp {
            callee,
            impure: false,
            args: Box::new([left_reg.into(), right_reg.into()]),
        },
    )
}

fn build_native_compare<F>(
    ehx: &mut EvalHirCtx,
    b: &mut Builder,
    span: Span,
    left_value: &Value,
    right_value: &Value,
    abi_type: &abitype::ABIType,
    op_kind: F,
) -> BuiltReg
where
    F: FnOnce(RegId, BinaryOp) -> OpKind,
{
    let left_reg = value_to_reg(ehx, b, span, left_value, abi_type);
    let right_reg = value_to_reg(ehx, b, span, right_value, abi_type);

    b.push_reg(
        span,
        op_kind,
        BinaryOp {
            lhs_reg: left_reg.into(),
            rhs_reg: right_reg.into(),
        },
    )
}

/// Determines if two values are statically equal
pub fn values_statically_equal(
    ehx: &mut EvalHirCtx,
    left_value: &Value,
    right_value: &Value,
) -> Option<bool> {
    match (left_value, right_value) {
        (Value::Reg(left_reg), Value::Reg(right_reg)) => {
            if [left_reg, right_reg]
                .iter()
                .all(|reg| reg.possible_type_tags.contains(boxed::TypeTag::Float))
            {
                // Either side could be NaN
                return None;
            }

            if left_reg.reg.into_reg_id() == right_reg.reg.into_reg_id() {
                Some(true)
            } else {
                None
            }
        }
        (Value::RustFun(left_fun), Value::RustFun(right_fun)) => {
            Some(left_fun.symbol() == right_fun.symbol())
        }
        (Value::EqPred, Value::EqPred) => Some(true),
        (Value::TyPred(left_ty), Value::TyPred(right_ty)) => Some(left_ty == right_ty),
        _ => {
            if let Some(const_left) = value_to_const(ehx, left_value) {
                if let Some(const_right) = value_to_const(ehx, right_value) {
                    return Some(const_left == const_right);
                }
            }

            None
        }
    }
}

/// Evaluates if two values are equal
///
/// This attempts `values_statically_equal` before building a runtime comparison.
pub fn eval_equality(
    ehx: &mut EvalHirCtx,
    b: &mut Option<Builder>,
    span: Span,
    left_value: &Value,
    right_value: &Value,
) -> Value {
    use crate::mir::tagset::TypeTagSet;
    use crate::mir::value::types::possible_type_tags_for_value;

    if let Some(static_result) = values_statically_equal(ehx, left_value, right_value) {
        return Value::Const(boxed::Bool::singleton_ref(static_result).as_any_ref());
    }

    let b = if let Some(some_b) = b {
        some_b
    } else {
        panic!("runtime equality without builder")
    };

    let left_type_tags = possible_type_tags_for_value(left_value);
    let right_type_tags = possible_type_tags_for_value(right_value);
    let all_type_tags = left_type_tags | right_type_tags;
    let common_type_tags = left_type_tags & right_type_tags;

    let boxed_singleton_type_tags: TypeTagSet = [
        boxed::TypeTag::True,
        boxed::TypeTag::False,
        boxed::TypeTag::Nil,
    ]
    .iter()
    .collect();

    let result_reg = if common_type_tags.is_subset(boxed_singleton_type_tags) {
        // We an do a direct pointer comparison
        build_native_compare(
            ehx,
            b,
            span,
            left_value,
            right_value,
            &abitype::BoxedABIType::Any.into(),
            OpKind::BoxIdentical,
        )
    } else if all_type_tags == boxed::TypeTag::Int.into() {
        build_native_compare(
            ehx,
            b,
            span,
            left_value,
            right_value,
            &abitype::ABIType::Int,
            OpKind::IntEqual,
        )
    } else if all_type_tags == boxed::TypeTag::Float.into() {
        build_native_compare(
            ehx,
            b,
            span,
            left_value,
            right_value,
            &abitype::ABIType::Float,
            OpKind::FloatEqual,
        )
    } else {
        runtime_compare(ehx, b, span, left_value, right_value)
    };

    reg_to_value(
        ehx,
        result_reg,
        &abitype::ABIType::Bool,
        &ty::Ty::Bool.into(),
    )
}
