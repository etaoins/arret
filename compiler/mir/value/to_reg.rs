use syntax::span::Span;

use runtime::abitype;
use runtime::boxed;
use runtime::boxed::refs::Gc;

use crate::mir::builder::Builder;
use crate::mir::ops::RegId;
use crate::mir::value;
use crate::mir::value::Value;

fn const_to_reg(
    b: &mut Builder,
    span: Span,
    any_ref: Gc<boxed::Any>,
    abi_type: &abitype::ABIType,
) -> RegId {
    use crate::mir::ops::*;

    let subtype = any_ref.as_subtype();

    match (subtype, abi_type) {
        (boxed::AnySubtype::Int(int_ref), abitype::ABIType::Int) => {
            b.push_reg(span, OpKind::ConstInt, int_ref.value())
        }

        (
            boxed::AnySubtype::Int(int_ref),
            abitype::ABIType::Boxed(abitype::BoxedABIType::DirectTagged(boxed::TypeTag::Int)),
        ) => b.push_reg(span, OpKind::ConstBoxedInt, int_ref.value()),

        (
            boxed::AnySubtype::Str(str_ref),
            abitype::ABIType::Boxed(abitype::BoxedABIType::DirectTagged(boxed::TypeTag::Str)),
        ) => b.push_reg(span, OpKind::ConstBoxedStr, str_ref.as_str().into()),

        (boxed::AnySubtype::Str(str_ref), abitype::ABIType::Boxed(boxed_abi_type)) => {
            // TODO: Temporary hack - this needs to be handled in a general way
            let from_reg = b.push_reg(span, OpKind::ConstBoxedStr, str_ref.as_str().into());
            b.cast_boxed(span, from_reg, boxed_abi_type.clone())
        }

        (subtype, abi_type) => unimplemented!(
            "Unimplemented const {:?} to reg {:?} conversion",
            subtype,
            abi_type
        ),
    }
}

pub fn list_to_reg(
    b: &mut Builder,
    span: Span,
    fixed: &[Value],
    rest: Option<&Value>,
    boxed_abi_type: &abitype::BoxedABIType,
) -> RegId {
    use crate::mir::ops::*;
    use crate::mir::value::list::list_value_length;

    let tail_reg = if let Some(rest) = rest {
        value_to_reg(
            b,
            span,
            rest,
            &abitype::BoxedABIType::List(&abitype::BoxedABIType::Any).into(),
        )
    } else {
        b.push_reg(span, OpKind::ConstNil, ())
    };

    let list_reg = if fixed.is_empty() {
        tail_reg
    } else {
        let tail_length = rest
            .map(|rest| list_value_length(rest).expect("Cannot calculate tail length"))
            .unwrap_or(0);

        fixed
            .iter()
            .rfold((tail_length, tail_reg), |(tail_length, tail_reg), fixed| {
                let head_length = tail_length + 1;
                let fixed_reg = value_to_reg(b, span, fixed, &abitype::BoxedABIType::Any.into());

                let head_reg = b.push_reg(
                    span,
                    OpKind::ConstBoxedPair,
                    ConstBoxedPairOp {
                        car_reg: fixed_reg,
                        cdr_reg: tail_reg,
                        length: head_length,
                    },
                );

                (head_length, head_reg)
            }).1
    };

    b.cast_boxed(span, list_reg, boxed_abi_type.clone())
}

fn reg_to_reg(
    b: &mut Builder,
    span: Span,
    reg_value: &value::RegValue,
    abi_type: &abitype::ABIType,
) -> RegId {
    use crate::mir::ops::*;
    use runtime::boxed::TypeTag;

    match (&reg_value.abi_type, abi_type) {
        (from, to) if from == to => reg_value.reg,
        (abitype::ABIType::Boxed(_), abitype::ABIType::Boxed(to_boxed)) => {
            b.cast_boxed(span, reg_value.reg, to_boxed.clone())
        }
        (abitype::ABIType::Boxed(from_boxed), abitype::ABIType::Int) => {
            let boxed_int_reg =
                b.cast_boxed_cond(span, from_boxed, reg_value.reg, TypeTag::Int.into());
            b.push_reg(span, OpKind::LoadBoxedIntValue, boxed_int_reg)
        }
        (abitype::ABIType::Bool, abitype::ABIType::Boxed(to_boxed)) => b.push_cond(
            span,
            reg_value.reg,
            |b| {
                let const_true_reg = b.push_reg(span, OpKind::ConstTrue, ());
                b.cast_boxed(span, const_true_reg, to_boxed.clone())
            },
            |b| {
                let const_false_reg = b.push_reg(span, OpKind::ConstFalse, ());
                b.cast_boxed(span, const_false_reg, to_boxed.clone())
            },
        ),
        (from, to) => unimplemented!("reg {:?} to reg {:?} conversion", from, to),
    }
}

pub fn value_to_reg(
    b: &mut Builder,
    span: Span,
    value: &Value,
    abi_type: &abitype::ABIType,
) -> RegId {
    match value {
        Value::Reg(reg_value) => reg_to_reg(b, span, reg_value, abi_type),
        Value::Const(any_ref) => const_to_reg(b, span, *any_ref, abi_type),
        Value::List(fixed, rest) => {
            if let abitype::ABIType::Boxed(boxed_abi_type) = abi_type {
                list_to_reg(
                    b,
                    span,
                    fixed,
                    rest.as_ref().map(|rest| rest.as_ref()),
                    boxed_abi_type,
                )
            } else {
                panic!("Attempt to construct non-boxed list");
            }
        }
        _ => unimplemented!("value {:?} to reg {:?} conversion", value, abi_type),
    }
}
