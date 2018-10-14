use syntax::span::Span;

use runtime::abitype;
use runtime::boxed;
use runtime::boxed::refs::Gc;

use crate::mir::builder::Builder;
use crate::mir::eval_hir::EvalHirCtx;
use crate::mir::ops::RegId;
use crate::mir::value;
use crate::mir::value::Value;

#[derive(Clone, Copy)]
pub enum BuiltReg {
    Const(RegId),
    Local(RegId),
}

enum RestLength {
    Known(usize),
    Loaded(RegId),
}

impl BuiltReg {
    pub fn into_reg_id(self) -> RegId {
        match self {
            BuiltReg::Const(reg_id) | BuiltReg::Local(reg_id) => reg_id,
        }
    }

    pub fn is_const(self) -> bool {
        match self {
            BuiltReg::Const(_) => true,
            BuiltReg::Local(_) => false,
        }
    }

    pub fn cast_to_boxed(
        self,
        b: &mut Builder,
        span: Span,
        boxed_abi_type: abitype::BoxedABIType,
    ) -> BuiltReg {
        match self {
            BuiltReg::Const(reg_id) => {
                BuiltReg::Const(b.const_cast_boxed(span, reg_id, boxed_abi_type))
            }
            BuiltReg::Local(reg_id) => BuiltReg::Local(b.cast_boxed(span, reg_id, boxed_abi_type)),
        }
    }
}

impl From<BuiltReg> for RegId {
    fn from(built_reg: BuiltReg) -> RegId {
        built_reg.into_reg_id()
    }
}

fn const_to_reg(
    b: &mut Builder,
    span: Span,
    any_ref: Gc<boxed::Any>,
    abi_type: &abitype::ABIType,
) -> BuiltReg {
    use crate::mir::ops::*;

    let subtype = any_ref.as_subtype();

    BuiltReg::Const(match (subtype, abi_type) {
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
        (boxed::AnySubtype::False(_), abitype::ABIType::Boxed(to_abi_type)) => {
            let from_abi_type = boxed::TypeTag::False.into();
            let from_reg = b.push_reg(span, OpKind::ConstFalse, ());

            b.cast_boxed_cond(span, &from_abi_type, from_reg, to_abi_type.clone())
        }
        (boxed::AnySubtype::True(_), abitype::ABIType::Boxed(to_abi_type)) => {
            let from_abi_type = boxed::TypeTag::True.into();
            let from_reg = b.push_reg(span, OpKind::ConstTrue, ());

            b.cast_boxed_cond(span, &from_abi_type, from_reg, to_abi_type.clone())
        }
        (boxed::AnySubtype::Nil(_), abitype::ABIType::Boxed(to_abi_type)) => {
            let from_abi_type = boxed::TypeTag::Nil.into();
            let from_reg = b.push_reg(span, OpKind::ConstNil, ());

            b.cast_boxed_cond(span, &from_abi_type, from_reg, to_abi_type.clone())
        }
        (boxed::AnySubtype::TopPair(top_pair), abitype::ABIType::Boxed(to_abi_type)) => {
            let pair_ref = top_pair.as_pair();

            let head_reg =
                const_to_reg(b, span, pair_ref.head(), &abitype::BoxedABIType::Any.into());
            let rest_reg = const_to_reg(
                b,
                span,
                pair_ref.rest().as_any_ref(),
                &abitype::TOP_LIST_BOXED_ABI_TYPE.into(),
            );
            let length_reg = b.push_reg(span, OpKind::ConstInt, pair_ref.len() as i64);

            let from_reg = b.push_reg(
                span,
                OpKind::ConstBoxedPair,
                BoxPairOp {
                    head_reg: head_reg.into(),
                    rest_reg: rest_reg.into(),
                    length_reg,
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
    })
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
        let nil_reg = b.push_reg(span, OpKind::ConstNil, ());
        BuiltReg::Const(b.const_cast_boxed(span, nil_reg, TOP_LIST_BOXED_ABI_TYPE))
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
                    RestLength::Known(known) => {
                        b.push_reg(span, OpKind::ConstInt, (known + i + 1) as i64)
                    }
                    RestLength::Loaded(rest_length_reg) => {
                        let index_reg = b.push_reg(span, OpKind::ConstInt, (i + 1) as i64);
                        b.push_reg(
                            span,
                            OpKind::Add,
                            BinaryOp {
                                lhs_reg: rest_length_reg,
                                rhs_reg: index_reg,
                            },
                        )
                    }
                };

                let fixed_reg =
                    value_to_reg(ehx, b, span, fixed, &abitype::BoxedABIType::Any.into());

                let box_pair_op = BoxPairOp {
                    head_reg: fixed_reg.into(),
                    rest_reg: tail_reg.into(),
                    length_reg,
                };

                let pair_head_reg = if fixed_reg.is_const() && tail_reg.is_const() {
                    BuiltReg::Const(b.push_reg(span, OpKind::ConstBoxedPair, box_pair_op))
                } else {
                    BuiltReg::Local(b.push_reg(span, OpKind::AllocBoxedPair, box_pair_op))
                };

                pair_head_reg.cast_to_boxed(b, span, TOP_LIST_BOXED_ABI_TYPE.clone())
            })
    };

    list_reg.cast_to_boxed(b, span, boxed_abi_type.clone())
}

pub fn reg_to_reg(
    b: &mut Builder,
    span: Span,
    reg_value: &value::RegValue,
    abi_type: &abitype::ABIType,
) -> BuiltReg {
    use crate::mir::ops::*;
    use runtime::boxed::TypeTag;

    BuiltReg::Local(match (&reg_value.abi_type, abi_type) {
        (from, to) if from == to => reg_value.reg,
        (abitype::ABIType::Boxed(_), abitype::ABIType::Boxed(to_boxed)) => {
            b.cast_boxed(span, reg_value.reg, to_boxed.clone())
        }
        (abitype::ABIType::Boxed(from_boxed), abitype::ABIType::Int) => {
            let boxed_int_reg =
                b.cast_boxed_cond(span, from_boxed, reg_value.reg, TypeTag::Int.into());
            b.push_reg(span, OpKind::LoadBoxedIntValue, boxed_int_reg)
        }
        (abitype::ABIType::Int, abitype::ABIType::Boxed(to_boxed)) => {
            let boxed_int_reg = b.push_reg(span, OpKind::AllocInt, reg_value.reg);
            b.cast_boxed_cond(span, &TypeTag::Int.into(), boxed_int_reg, to_boxed.clone())
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
    })
}

fn thunk_reg_to_reg(
    b: &mut Builder,
    span: Span,
    boxed_thunk_reg: RegId,
    abi_type: &abitype::ABIType,
) -> BuiltReg {
    use runtime::boxed::TypeTag;

    let boxed_abi_type = if let abitype::ABIType::Boxed(boxed_abi_type) = abi_type {
        boxed_abi_type
    } else {
        panic!("attempt to create unboxed function");
    };

    BuiltReg::Local(b.cast_boxed_cond(
        span,
        &TypeTag::FunThunk.into(),
        boxed_thunk_reg,
        boxed_abi_type.clone(),
    ))
}

pub fn value_to_reg(
    ehx: &mut EvalHirCtx,
    b: &mut Builder,
    span: Span,
    value: &Value,
    abi_type: &abitype::ABIType,
) -> BuiltReg {
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
