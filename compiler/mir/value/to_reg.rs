use syntax::span::Span;

use runtime::abitype;
use runtime::boxed;
use runtime::boxed::refs::Gc;

use crate::mir::builder::Builder;
use crate::mir::ops::RegId;
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
            b.push_reg(
                span,
                OpKind::CastBoxed,
                CastBoxedOp {
                    from_reg,
                    to_type: boxed_abi_type.clone(),
                },
            )
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

    let tail_length;
    let tail_reg;
    if let Some(rest) = rest {
        tail_length = list_value_length(rest).expect("Cannot calculate tail length");
        tail_reg = value_to_reg(
            b,
            span,
            rest,
            &abitype::BoxedABIType::List(&abitype::BoxedABIType::Any).into(),
        );
    } else {
        tail_length = 0;
        tail_reg = b.push_reg(span, OpKind::ConstNil, ());
    }

    let (_, list_reg) =
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
            });

    b.push_reg(
        span,
        OpKind::CastBoxed,
        CastBoxedOp {
            from_reg: list_reg,
            to_type: boxed_abi_type.clone(),
        },
    )
}

pub fn value_to_reg(
    b: &mut Builder,
    span: Span,
    value: &Value,
    abi_type: &abitype::ABIType,
) -> RegId {
    match value {
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
        _ => unimplemented!("Unimplemented value to reg conversion"),
    }
}
