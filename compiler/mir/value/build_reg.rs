use syntax::span::Span;

use runtime::abitype;
use runtime::boxed;
use runtime::boxed::refs::Gc;

use crate::hir::rfi;
use crate::mir::builder::Builder;
use crate::mir::builder::BuiltReg;
use crate::mir::eval_hir::EvalHirCtx;
use crate::mir::value;
use crate::mir::value::Value;

enum RestLength {
    Known(usize),
    Loaded(BuiltReg),
}

fn const_to_reg(
    ehx: &mut EvalHirCtx,
    b: &mut Builder,
    span: Span,
    any_ref: Gc<boxed::Any>,
    abi_type: &abitype::ABIType,
) -> BuiltReg {
    use crate::mir::ops::*;
    use runtime::boxed::AsHeap;

    let subtype = any_ref.as_subtype();

    match (subtype, abi_type) {
        (boxed::AnySubtype::Int(int_ref), abitype::ABIType::Int) => {
            b.push_reg(span, OpKind::ConstInt64, int_ref.value())
        }
        (boxed::AnySubtype::Float(float_ref), abitype::ABIType::Float) => {
            b.push_reg(span, OpKind::ConstFloat, float_ref.value())
        }
        (boxed::AnySubtype::True(_), abitype::ABIType::Bool) => {
            b.push_reg(span, OpKind::ConstBool, true)
        }
        (boxed::AnySubtype::False(_), abitype::ABIType::Bool) => {
            b.push_reg(span, OpKind::ConstBool, false)
        }
        (boxed::AnySubtype::Int(int_ref), abitype::ABIType::Boxed(to_abi_type)) => {
            let from_abi_type = boxed::TypeTag::Int.into();
            let from_reg = b.push_reg(span, OpKind::ConstBoxedInt, int_ref.value());

            b.cast_boxed_cond(span, &from_abi_type, from_reg, to_abi_type.clone())
        }
        (boxed::AnySubtype::Float(float_ref), abitype::ABIType::Boxed(to_abi_type)) => {
            let from_abi_type = boxed::TypeTag::Float.into();
            let from_reg = b.push_reg(span, OpKind::ConstBoxedFloat, float_ref.value());

            b.cast_boxed_cond(span, &from_abi_type, from_reg, to_abi_type.clone())
        }
        (boxed::AnySubtype::Str(str_ref), abitype::ABIType::Boxed(to_abi_type)) => {
            let from_abi_type = boxed::TypeTag::Str.into();
            let from_reg = b.push_reg(span, OpKind::ConstBoxedStr, str_ref.as_str().into());

            b.cast_boxed_cond(span, &from_abi_type, from_reg, to_abi_type.clone())
        }
        (boxed::AnySubtype::Sym(sym_ref), abitype::ABIType::Boxed(to_abi_type)) => {
            let from_abi_type = boxed::TypeTag::Sym.into();
            let from_reg = b.push_reg(
                span,
                OpKind::ConstBoxedSym,
                sym_ref.name(ehx.as_heap().interner()).into(),
            );

            b.cast_boxed_cond(span, &from_abi_type, from_reg, to_abi_type.clone())
        }
        (boxed::AnySubtype::False(_), abitype::ABIType::Boxed(to_abi_type)) => {
            let from_abi_type = boxed::TypeTag::False.into();
            let from_reg = b.push_reg(span, OpKind::ConstBoxedFalse, ());

            b.cast_boxed_cond(span, &from_abi_type, from_reg, to_abi_type.clone())
        }
        (boxed::AnySubtype::True(_), abitype::ABIType::Boxed(to_abi_type)) => {
            let from_abi_type = boxed::TypeTag::True.into();
            let from_reg = b.push_reg(span, OpKind::ConstBoxedTrue, ());

            b.cast_boxed_cond(span, &from_abi_type, from_reg, to_abi_type.clone())
        }
        (boxed::AnySubtype::Nil(_), abitype::ABIType::Boxed(to_abi_type)) => {
            let from_abi_type = boxed::TypeTag::Nil.into();
            let from_reg = b.push_reg(span, OpKind::ConstBoxedNil, ());

            b.cast_boxed_cond(span, &from_abi_type, from_reg, to_abi_type.clone())
        }
        (boxed::AnySubtype::TopPair(top_pair), abitype::ABIType::Boxed(to_abi_type)) => {
            let pair_ref = top_pair.as_pair();

            let head_reg = const_to_reg(
                ehx,
                b,
                span,
                pair_ref.head(),
                &abitype::BoxedABIType::Any.into(),
            );
            let rest_reg = const_to_reg(
                ehx,
                b,
                span,
                pair_ref.rest().as_any_ref(),
                &abitype::TOP_LIST_BOXED_ABI_TYPE.into(),
            );
            let length_reg = b.push_reg(span, OpKind::ConstUsize, pair_ref.len());

            let from_reg = b.push_reg(
                span,
                OpKind::ConstBoxedPair,
                BoxPairOp {
                    head_reg: head_reg.into(),
                    rest_reg: rest_reg.into(),
                    length_reg: length_reg.into(),
                },
            );

            b.cast_boxed_cond(
                span,
                &boxed::TypeTag::TopPair.into(),
                from_reg,
                to_abi_type.clone(),
            )
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
) -> BuiltReg {
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
        let nil_reg = b.push_reg(span, OpKind::ConstBoxedNil, ());
        b.cast_boxed(span, nil_reg, TOP_LIST_BOXED_ABI_TYPE)
    };

    let list_reg = if fixed.is_empty() {
        tail_reg
    } else {
        let rest_length = match rest {
            Some(rest) => match list_value_length(rest) {
                Some(known) => RestLength::Known(known),
                None => {
                    let length_reg = b.push_reg(span, OpKind::LoadBoxedListLength, tail_reg.into());
                    RestLength::Loaded(length_reg)
                }
            },
            None => RestLength::Known(0),
        };

        fixed
            .iter()
            .rev()
            .enumerate()
            .fold(tail_reg, |tail_reg, (i, fixed)| {
                let length_reg = match rest_length {
                    RestLength::Known(known) => b.push_reg(span, OpKind::ConstUsize, known + i + 1),
                    RestLength::Loaded(rest_length_reg) => {
                        let index_reg = b.push_reg(span, OpKind::ConstUsize, i + 1);
                        b.push_reg(
                            span,
                            OpKind::Add,
                            BinaryOp {
                                lhs_reg: rest_length_reg.into(),
                                rhs_reg: index_reg.into(),
                            },
                        )
                    }
                };

                let fixed_reg =
                    value_to_reg(ehx, b, span, fixed, &abitype::BoxedABIType::Any.into());

                let box_pair_op = BoxPairOp {
                    head_reg: fixed_reg.into(),
                    rest_reg: tail_reg.into(),
                    length_reg: length_reg.into(),
                };

                let pair_head_reg = if fixed_reg.is_const() && tail_reg.is_const() {
                    b.push_reg(span, OpKind::ConstBoxedPair, box_pair_op)
                } else {
                    b.push_reg(span, OpKind::AllocBoxedPair, box_pair_op)
                };

                b.cast_boxed(span, pair_head_reg, TOP_LIST_BOXED_ABI_TYPE.clone())
            })
    };

    b.cast_boxed(span, list_reg, boxed_abi_type.clone())
}

pub fn reg_to_boxed_reg(
    b: &mut Builder,
    span: Span,
    reg_value: &value::RegValue,
    to_boxed: &abitype::BoxedABIType,
) -> BuiltReg {
    use crate::mir::ops::*;
    use runtime::boxed::TypeTag;

    match &reg_value.abi_type {
        abitype::ABIType::Boxed(from_boxed) => {
            b.cast_boxed_cond(span, from_boxed, reg_value.reg, to_boxed.clone())
        }
        abitype::ABIType::Int => {
            let boxed_int_reg = b.push_reg(span, OpKind::AllocBoxedInt, reg_value.reg.into());
            b.cast_boxed_cond(span, &TypeTag::Int.into(), boxed_int_reg, to_boxed.clone())
        }
        abitype::ABIType::Float => {
            let boxed_float_reg = b.push_reg(span, OpKind::AllocBoxedFloat, reg_value.reg.into());
            b.cast_boxed_cond(
                span,
                &TypeTag::Float.into(),
                boxed_float_reg,
                to_boxed.clone(),
            )
        }
        abitype::ABIType::Bool => b.push_cond(
            span,
            reg_value.reg.into(),
            |b| {
                let const_true_reg = b.push_reg(span, OpKind::ConstBoxedTrue, ());
                b.cast_boxed_cond(
                    span,
                    &TypeTag::True.into(),
                    const_true_reg,
                    to_boxed.clone(),
                )
                .into()
            },
            |b| {
                let const_false_reg = b.push_reg(span, OpKind::ConstBoxedFalse, ());
                b.cast_boxed_cond(
                    span,
                    &TypeTag::False.into(),
                    const_false_reg,
                    to_boxed.clone(),
                )
                .into()
            },
        ),
        from => unimplemented!("reg {:?} to boxed reg {:?} conversion", from, to_boxed),
    }
}

fn boxed_to_bool(
    b: &mut Builder,
    span: Span,
    from_boxed: &abitype::BoxedABIType,
    boxed_reg: BuiltReg,
) -> BuiltReg {
    use crate::mir::ops::*;
    use runtime::boxed::TypeTag;

    let boxed_any_reg = b.cast_boxed_cond(span, from_boxed, boxed_reg, abitype::BoxedABIType::Any);

    let boxed_type_tag_reg = b.push_reg(
        span,
        OpKind::LoadBoxedTypeTag,
        LoadBoxedTypeTagOp {
            subject_reg: boxed_any_reg.into(),
            possible_type_tags: [TypeTag::True, TypeTag::False].iter().collect(),
        },
    );

    let true_type_tag_reg = b.push_reg(span, OpKind::ConstTypeTag, TypeTag::True);

    b.push_reg(
        span,
        OpKind::IntEqual,
        BinaryOp {
            lhs_reg: boxed_type_tag_reg.into(),
            rhs_reg: true_type_tag_reg.into(),
        },
    )
}

fn reg_to_reg(
    ehx: &mut EvalHirCtx,
    b: &mut Builder,
    span: Span,
    reg_value: &value::RegValue,
    abi_type: &abitype::ABIType,
) -> BuiltReg {
    use crate::mir::ops::*;
    use runtime::boxed::TypeTag;

    match (&reg_value.abi_type, abi_type) {
        (from, to) if from == to => reg_value.reg,
        (_, abitype::ABIType::Boxed(to_boxed)) => reg_to_boxed_reg(b, span, reg_value, to_boxed),
        (abitype::ABIType::Boxed(from_boxed), abitype::ABIType::Int) => {
            let boxed_int_reg =
                b.cast_boxed_cond(span, from_boxed, reg_value.reg, TypeTag::Int.into());
            b.push_reg(span, OpKind::LoadBoxedIntValue, boxed_int_reg.into())
        }
        (abitype::ABIType::Boxed(from_boxed), abitype::ABIType::Float) => {
            let boxed_float_reg =
                b.cast_boxed_cond(span, from_boxed, reg_value.reg, TypeTag::Float.into());
            b.push_reg(span, OpKind::LoadBoxedFloatValue, boxed_float_reg.into())
        }
        (abitype::ABIType::Boxed(from_boxed), abitype::ABIType::Bool) => {
            boxed_to_bool(b, span, from_boxed, reg_value.reg)
        }
        (abitype::ABIType::Boxed(from_boxed), abitype::ABIType::Callback(entry_point_abi)) => {
            ehx.thunk_reg_to_callback_reg(b, span, from_boxed, reg_value.reg, entry_point_abi)
        }
        (from, to) => unimplemented!("reg {:?} to reg {:?} conversion", from, to),
    }
}

fn thunk_reg_to_reg(
    b: &mut Builder,
    span: Span,
    boxed_thunk_reg: BuiltReg,
    boxed_abi_type: &abitype::BoxedABIType,
) -> BuiltReg {
    use runtime::boxed::TypeTag;

    b.cast_boxed_cond(
        span,
        &TypeTag::FunThunk.into(),
        boxed_thunk_reg,
        boxed_abi_type.clone(),
    )
}

fn arret_fun_to_reg(
    ehx: &mut EvalHirCtx,
    b: &mut Builder,
    span: Span,
    arret_fun: &value::ArretFun,
    abi_type: &abitype::ABIType,
) -> BuiltReg {
    match abi_type {
        abitype::ABIType::Boxed(boxed_abi_type) => {
            let thunk_reg = ehx.arret_fun_to_thunk_reg(b, span, arret_fun);
            thunk_reg_to_reg(b, span, thunk_reg, boxed_abi_type)
        }
        abitype::ABIType::Callback(entry_point_abi) => {
            ehx.arret_fun_to_callback_reg(b, span, arret_fun, entry_point_abi)
        }
        other => {
            panic!("Attempt to convert Arret fun to {:?}", other);
        }
    }
}

fn rust_fun_to_reg(
    ehx: &mut EvalHirCtx,
    b: &mut Builder,
    span: Span,
    rust_fun: &rfi::Fun,
    abi_type: &abitype::ABIType,
) -> BuiltReg {
    match abi_type {
        abitype::ABIType::Boxed(boxed_abi_type) => {
            let thunk_reg = ehx.rust_fun_to_thunk_reg(b, span, rust_fun);
            thunk_reg_to_reg(b, span, thunk_reg, boxed_abi_type)
        }
        abitype::ABIType::Callback(entry_point_abi) => {
            ehx.rust_fun_to_callback_reg(b, span, rust_fun, entry_point_abi)
        }
        other => {
            panic!("Attempt to convert Rust fun to {:?}", other);
        }
    }
}

pub fn value_to_reg(
    ehx: &mut EvalHirCtx,
    b: &mut Builder,
    span: Span,
    value: &Value,
    abi_type: &abitype::ABIType,
) -> BuiltReg {
    match value {
        Value::Reg(reg_value) => reg_to_reg(ehx, b, span, reg_value, abi_type),
        Value::Const(any_ref) => const_to_reg(ehx, b, span, *any_ref, abi_type),
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
        Value::ArretFun(ref arret_fun) => arret_fun_to_reg(ehx, b, span, arret_fun, abi_type),
        Value::TyPred(test_ty) => {
            use crate::mir::value::synthetic_fun::ty_pred_arret_fun;
            arret_fun_to_reg(ehx, b, span, &ty_pred_arret_fun(*test_ty), abi_type)
        }
        Value::EqPred => {
            use crate::mir::value::synthetic_fun::eq_pred_arret_fun;
            arret_fun_to_reg(ehx, b, span, &eq_pred_arret_fun(), abi_type)
        }
        Value::RustFun(ref rust_fun) => rust_fun_to_reg(ehx, b, span, rust_fun, abi_type),
    }
}
