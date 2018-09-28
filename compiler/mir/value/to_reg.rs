use syntax::span::Span;

use runtime::abitype;
use runtime::boxed;
use runtime::boxed::refs::Gc;

use crate::mir::builder::Builder;
use crate::mir::eval_hir::EvalHirCtx;
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
        (boxed::AnySubtype::Int(int_ref), abitype::ABIType::Boxed(to_abi_type)) => {
            let from_abi_type = boxed::TypeTag::Int.into();
            let from_reg = b.push_reg(span, OpKind::ConstBoxedInt, int_ref.value());

            b.cast_boxed_cond(span, &from_abi_type, from_reg, to_abi_type.clone())
        }
        (boxed::AnySubtype::Str(str_ref), abitype::ABIType::Boxed(to_abi_type)) => {
            let from_abi_type = boxed::TypeTag::Str.into();
            let from_reg = b.push_reg(span, OpKind::ConstBoxedStr, str_ref.as_str().into());

            b.cast_boxed_cond(span, &from_abi_type, from_reg, to_abi_type.clone())
        }
        (subtype, abi_type) => unimplemented!(
            "Unimplemented const {:?} to reg {:?} conversion",
            subtype,
            abi_type
        ),
    }
}

fn list_to_reg(
    ehx: &mut EvalHirCtx,
    b: &mut Builder,
    span: Span,
    fixed: &[Value],
    rest: Option<&Value>,
    boxed_abi_type: &abitype::BoxedABIType,
) -> RegId {
    use crate::mir::ops::*;
    use crate::mir::value::list::list_value_length;
    use runtime::abitype::TOP_LIST_BOXED_ABI_TYPE;

    let tail_reg = if let Some(rest) = rest {
        value_to_reg(
            ehx,
            b,
            span,
            rest,
            &abitype::ABIType::Boxed(TOP_LIST_BOXED_ABI_TYPE),
        )
    } else {
        let nil_reg = b.push_reg(span, OpKind::ConstNil, ());
        b.const_cast_boxed(span, nil_reg, TOP_LIST_BOXED_ABI_TYPE)
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
                let fixed_reg =
                    value_to_reg(ehx, b, span, fixed, &abitype::BoxedABIType::Any.into());

                let pair_head_reg = b.push_reg(
                    span,
                    OpKind::ConstBoxedPair,
                    ConstBoxedPairOp {
                        car_reg: fixed_reg,
                        cdr_reg: tail_reg,
                        length: head_length,
                    },
                );

                let head_reg =
                    b.const_cast_boxed(span, pair_head_reg, TOP_LIST_BOXED_ABI_TYPE.clone());
                (head_length, head_reg)
            })
            .1
    };

    b.cast_boxed(span, list_reg, boxed_abi_type.clone())
}

pub fn reg_to_reg(
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

fn thunk_reg_to_reg(
    b: &mut Builder,
    span: Span,
    boxed_thunk_reg: RegId,
    abi_type: &abitype::ABIType,
) -> RegId {
    use runtime::boxed::TypeTag;

    let boxed_abi_type = if let abitype::ABIType::Boxed(boxed_abi_type) = abi_type {
        boxed_abi_type
    } else {
        panic!("attempt to create unboxed function");
    };

    b.cast_boxed_cond(
        span,
        &TypeTag::FunThunk.into(),
        boxed_thunk_reg,
        boxed_abi_type.clone(),
    )
}

pub fn value_to_reg(
    ehx: &mut EvalHirCtx,
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
                    ehx,
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
        Value::ArretFun(ref arret_fun) => {
            let thunk_reg = ehx.arret_fun_to_thunk_reg(b, span, arret_fun);
            thunk_reg_to_reg(b, span, thunk_reg, abi_type)
        }
        Value::RustFun(ref rust_fun) => {
            let thunk_reg = ehx.rust_fun_to_thunk_reg(b, span, rust_fun);
            thunk_reg_to_reg(b, span, thunk_reg, abi_type)
        }
        _ => unimplemented!("value {:?} to reg {:?} conversion", value, abi_type),
    }
}
