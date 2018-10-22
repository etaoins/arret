use syntax::span::Span;

use runtime::abitype;
use runtime::boxed;
use runtime::boxed::prelude::*;

use crate::codegen::GenABI;
use crate::mir::builder::Builder;
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
) -> RegId {
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
        takes_closure: false,
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

fn int_compare(
    ehx: &mut EvalHirCtx,
    b: &mut Builder,
    span: Span,
    left_value: &Value,
    right_value: &Value,
) -> RegId {
    let left_reg = value_to_reg(ehx, b, span, left_value, &abitype::ABIType::Int);
    let right_reg = value_to_reg(ehx, b, span, right_value, &abitype::ABIType::Int);

    b.push_reg(
        span,
        OpKind::IntEqual,
        BinaryOp {
            lhs_reg: left_reg.into(),
            rhs_reg: right_reg.into(),
        },
    )
}

/// Evaluates if two values are equal
///
/// If `left_value` and `right_value` are const then a const is returned. Otherwise, a comparison
/// is built.
pub fn eval_equality(
    ehx: &mut EvalHirCtx,
    b: &mut Option<Builder>,
    span: Span,
    left_value: &Value,
    right_value: &Value,
) -> Value {
    use crate::mir::value::types::possible_type_tags_for_value;

    if let Some(const_left) = value_to_const(ehx, left_value) {
        if let Some(const_right) = value_to_const(ehx, right_value) {
            let static_result = const_left == const_right;
            return Value::Const(boxed::Bool::singleton_ref(static_result).as_any_ref());
        }
    }

    let b = if let Some(some_b) = b {
        some_b
    } else {
        panic!("runtime equality without builder")
    };

    let common_type_tags =
        possible_type_tags_for_value(left_value) & possible_type_tags_for_value(right_value);

    let result_reg = if common_type_tags == boxed::TypeTag::Int.into() {
        int_compare(ehx, b, span, left_value, right_value)
    } else {
        runtime_compare(ehx, b, span, left_value, right_value)
    };

    reg_to_value(
        ehx,
        result_reg,
        &abitype::ABIType::Bool,
        &ty::Ty::Bool.into_mono(),
    )
}
