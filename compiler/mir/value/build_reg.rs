use arret_syntax::span::Span;

use arret_runtime::abitype;
use arret_runtime::boxed;
use arret_runtime::boxed::refs::Gc;

use crate::mir::builder::Builder;
use crate::mir::builder::BuiltReg;
use crate::mir::eval_hir::EvalHirCtx;
use crate::mir::value;
use crate::mir::value::Value;
use crate::rfi;
use crate::ty::record;

enum RestLen {
    Known(usize),
    Loaded(BuiltReg),
}

fn const_to_reg(
    ehx: &mut EvalHirCtx,
    b: &mut Builder,
    span: Span,
    any_ref: Gc<boxed::Any>,
    abi_type: &abitype::AbiType,
) -> BuiltReg {
    use crate::mir::ops::*;
    use arret_runtime::boxed::prelude::*;

    let subtype = any_ref.as_subtype();

    match (subtype, abi_type) {
        (boxed::AnySubtype::Int(int_ref), abitype::AbiType::Int) => {
            b.push_reg(span, OpKind::ConstInt64, int_ref.value())
        }
        (boxed::AnySubtype::Float(float_ref), abitype::AbiType::Float) => {
            b.push_reg(span, OpKind::ConstFloat, float_ref.value())
        }
        (boxed::AnySubtype::Char(char_ref), abitype::AbiType::Char) => {
            b.push_reg(span, OpKind::ConstChar, char_ref.value())
        }
        (boxed::AnySubtype::True(_), abitype::AbiType::Bool) => {
            b.push_reg(span, OpKind::ConstBool, true)
        }
        (boxed::AnySubtype::False(_), abitype::AbiType::Bool) => {
            b.push_reg(span, OpKind::ConstBool, false)
        }
        (boxed::AnySubtype::Sym(sym_ref), abitype::AbiType::InternedSym) => {
            b.push_reg(span, OpKind::ConstInternedSym, sym_ref.name(ehx).into())
        }
        (boxed::AnySubtype::Int(int_ref), abitype::AbiType::Boxed(to_abi_type)) => {
            let from_abi_type = boxed::TypeTag::Int.into();
            let from_reg = b.push_reg(span, OpKind::ConstBoxedInt, int_ref.value());

            b.cast_boxed_cond(span, &from_abi_type, from_reg, to_abi_type.clone())
        }
        (boxed::AnySubtype::Float(float_ref), abitype::AbiType::Boxed(to_abi_type)) => {
            let from_abi_type = boxed::TypeTag::Float.into();
            let from_reg = b.push_reg(span, OpKind::ConstBoxedFloat, float_ref.value());

            b.cast_boxed_cond(span, &from_abi_type, from_reg, to_abi_type.clone())
        }
        (boxed::AnySubtype::Char(char_ref), abitype::AbiType::Boxed(to_abi_type)) => {
            let from_abi_type = boxed::TypeTag::Char.into();
            let from_reg = b.push_reg(span, OpKind::ConstBoxedChar, char_ref.value());

            b.cast_boxed_cond(span, &from_abi_type, from_reg, to_abi_type.clone())
        }
        (boxed::AnySubtype::Str(str_ref), abitype::AbiType::Boxed(to_abi_type)) => {
            let from_abi_type = boxed::TypeTag::Str.into();
            let from_reg = b.push_reg(span, OpKind::ConstBoxedStr, str_ref.as_str().into());

            b.cast_boxed_cond(span, &from_abi_type, from_reg, to_abi_type.clone())
        }
        (boxed::AnySubtype::Sym(sym_ref), abitype::AbiType::Boxed(to_abi_type)) => {
            let from_abi_type = boxed::TypeTag::Sym.into();
            let from_reg = b.push_reg(span, OpKind::ConstBoxedSym, sym_ref.name(ehx).into());

            b.cast_boxed_cond(span, &from_abi_type, from_reg, to_abi_type.clone())
        }
        (boxed::AnySubtype::False(_), abitype::AbiType::Boxed(to_abi_type)) => {
            let from_abi_type = boxed::TypeTag::False.into();
            let from_reg = b.push_reg(span, OpKind::ConstBoxedFalse, ());

            b.cast_boxed_cond(span, &from_abi_type, from_reg, to_abi_type.clone())
        }
        (boxed::AnySubtype::True(_), abitype::AbiType::Boxed(to_abi_type)) => {
            let from_abi_type = boxed::TypeTag::True.into();
            let from_reg = b.push_reg(span, OpKind::ConstBoxedTrue, ());

            b.cast_boxed_cond(span, &from_abi_type, from_reg, to_abi_type.clone())
        }
        (boxed::AnySubtype::Nil(_), abitype::AbiType::Boxed(to_abi_type)) => {
            let from_abi_type = boxed::TypeTag::Nil.into();
            let from_reg = b.push_reg(span, OpKind::ConstBoxedNil, ());

            b.cast_boxed_cond(span, &from_abi_type, from_reg, to_abi_type.clone())
        }
        (boxed::AnySubtype::Pair(pair_ref), abitype::AbiType::Boxed(to_abi_type)) => {
            let head_reg = const_to_reg(
                ehx,
                b,
                span,
                pair_ref.head(),
                &abitype::BoxedAbiType::Any.into(),
            );
            let rest_reg = const_to_reg(
                ehx,
                b,
                span,
                pair_ref.rest().as_any_ref(),
                &abitype::TOP_LIST_BOXED_ABI_TYPE.into(),
            );
            let list_len_reg = b.push_reg(span, OpKind::ConstInt64, pair_ref.len() as i64);

            let from_reg = b.push_reg(
                span,
                OpKind::ConstBoxedPair,
                BoxPairOp {
                    head_reg: head_reg.into(),
                    rest_reg: rest_reg.into(),
                    list_len_reg: list_len_reg.into(),
                },
            );

            b.cast_boxed_cond(
                span,
                &boxed::TypeTag::Pair.into(),
                from_reg,
                to_abi_type.clone(),
            )
        }
        (boxed::AnySubtype::Record(record_ref), abitype::AbiType::Boxed(to_abi_type)) => {
            let record_cons = ehx
                .cons_for_jit_record_class_id(record_ref.class_id())
                .expect("unable to lookup record cons for JIT record class ID");

            let record_struct = ehx
                .record_class_for_cons
                .get(record_cons)
                .expect("unable to lookup record class for cons")
                .record_struct
                .clone();

            let field_values: Vec<_> = record_ref.field_values(ehx.as_heap()).collect();

            let field_regs = field_values
                .into_iter()
                .zip(record_struct.field_abi_types.iter())
                .map(|(field_value, abi_type)| {
                    let built_reg =
                        record_field_value_to_const_reg(ehx, b, span, &field_value, abi_type);

                    built_reg.into()
                })
                .collect();

            let box_record_op = BoxRecordOp {
                record_struct,
                field_regs,
            };

            let from_abi_type = boxed::TypeTag::Record.into();
            let from_reg = b.push_reg(span, OpKind::ConstBoxedRecord, box_record_op);

            b.cast_boxed_cond(span, &from_abi_type, from_reg, to_abi_type.clone())
        }
        (boxed::AnySubtype::Vector(vector_ref), abitype::AbiType::Boxed(to_abi_type)) => {
            let element_regs = vector_ref
                .iter()
                .map(|element_ref| {
                    const_to_reg(
                        ehx,
                        b,
                        span,
                        element_ref,
                        &abitype::BoxedAbiType::Any.into(),
                    )
                    .into()
                })
                .collect();

            let from_reg = b.push_reg(span, OpKind::ConstBoxedVector, element_regs);

            b.cast_boxed_cond(
                span,
                &boxed::TypeTag::Vector.into(),
                from_reg,
                to_abi_type.clone(),
            )
        }
        (boxed::AnySubtype::Set(set_ref), abitype::AbiType::Boxed(to_abi_type)) => {
            let element_regs = set_ref
                .iter()
                .map(|element_ref| {
                    const_to_reg(
                        ehx,
                        b,
                        span,
                        element_ref,
                        &abitype::BoxedAbiType::Any.into(),
                    )
                    .into()
                })
                .collect();

            let from_reg = b.push_reg(span, OpKind::ConstBoxedSet, element_regs);

            b.cast_boxed_cond(
                span,
                &boxed::TypeTag::Set.into(),
                from_reg,
                to_abi_type.clone(),
            )
        }
        (boxed::AnySubtype::Map(map_ref), abitype::AbiType::Boxed(to_abi_type)) => {
            let entry_regs = map_ref
                .iter()
                .map(|(key_ref, value_ref)| {
                    let key_reg =
                        const_to_reg(ehx, b, span, key_ref, &abitype::BoxedAbiType::Any.into())
                            .into();

                    let value_reg =
                        const_to_reg(ehx, b, span, value_ref, &abitype::BoxedAbiType::Any.into())
                            .into();

                    (key_reg, value_reg)
                })
                .collect();

            let from_reg = b.push_reg(span, OpKind::ConstBoxedMap, entry_regs);

            b.cast_boxed_cond(
                span,
                &boxed::TypeTag::Map.into(),
                from_reg,
                to_abi_type.clone(),
            )
        }
        (boxed::AnySubtype::FunThunk(fun_thunk_ref), abi_type) => {
            let fun_value = ehx
                .jit_boxed_to_fun_value(unsafe { Gc::new(fun_thunk_ref as *const _) })
                .expect("attempt to convert unknown fun thunk to reg")
                .clone();

            value_to_reg(ehx, b, span, &fun_value, abi_type)
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
    boxed_abi_type: &abitype::BoxedAbiType,
) -> BuiltReg {
    use crate::mir::ops::*;
    use crate::mir::value::list::{list_value_len, ListValueLen};
    use arret_runtime::abitype::TOP_LIST_BOXED_ABI_TYPE;

    let tail_reg = if let Some(rest) = rest {
        value_to_reg(
            ehx,
            b,
            span,
            rest,
            &abitype::AbiType::Boxed(TOP_LIST_BOXED_ABI_TYPE),
        )
    } else {
        let nil_reg = b.push_reg(span, OpKind::ConstBoxedNil, ());
        b.cast_boxed(span, nil_reg, TOP_LIST_BOXED_ABI_TYPE)
    };

    let list_reg = if fixed.is_empty() {
        tail_reg
    } else {
        let rest_len = match rest {
            Some(rest) => match list_value_len(rest) {
                ListValueLen::Exact(known) => RestLen::Known(known),
                ListValueLen::Min(min_list_len) => {
                    let len_reg = b.push_reg(
                        span,
                        OpKind::LoadBoxedListLen,
                        LoadBoxedListLenOp {
                            list_reg: tail_reg.into(),
                            min_list_len,
                        },
                    );
                    RestLen::Loaded(len_reg)
                }
            },
            None => RestLen::Known(0),
        };

        fixed
            .iter()
            .rev()
            .enumerate()
            .fold(tail_reg, |tail_reg, (i, fixed)| {
                let list_len_reg = match rest_len {
                    RestLen::Known(known) => {
                        b.push_reg(span, OpKind::ConstInt64, (known + i + 1) as i64)
                    }
                    RestLen::Loaded(rest_len_reg) => {
                        let index_reg = b.push_reg(span, OpKind::ConstInt64, (i + 1) as i64);
                        b.push_reg(
                            span,
                            OpKind::Int64Add,
                            BinaryOp {
                                lhs_reg: rest_len_reg.into(),
                                rhs_reg: index_reg.into(),
                            },
                        )
                    }
                };

                let fixed_reg =
                    value_to_reg(ehx, b, span, fixed, &abitype::BoxedAbiType::Any.into());

                let box_pair_op = BoxPairOp {
                    head_reg: fixed_reg.into(),
                    rest_reg: tail_reg.into(),
                    list_len_reg: list_len_reg.into(),
                };

                let pair_head_reg = if fixed_reg.is_const() && tail_reg.is_const() {
                    b.push_reg(span, OpKind::ConstBoxedPair, box_pair_op)
                } else {
                    b.push_reg(span, OpKind::AllocBoxedPair, box_pair_op)
                };

                b.cast_boxed(span, pair_head_reg, TOP_LIST_BOXED_ABI_TYPE.clone())
            })
    };

    b.cast_boxed_cond(
        span,
        &TOP_LIST_BOXED_ABI_TYPE,
        list_reg,
        boxed_abi_type.clone(),
    )
}

fn record_to_reg(
    ehx: &mut EvalHirCtx,
    b: &mut Builder,
    span: Span,
    record_cons: &record::ConsId,
    fields: &[Value],
    boxed_abi_type: &abitype::BoxedAbiType,
) -> BuiltReg {
    use crate::mir::ops::*;

    let record_struct = ehx
        .evaled_record_class_for_cons(record_cons)
        .record_struct
        .clone();

    let mut has_non_const_fields = false;
    let field_regs = fields
        .iter()
        .zip(record_struct.field_abi_types.iter())
        .map(|(field, abi_type)| {
            let built_reg = value_to_reg(ehx, b, span, field, abi_type);
            has_non_const_fields = has_non_const_fields || !built_reg.is_const();

            built_reg.into()
        })
        .collect();

    let box_record_op = BoxRecordOp {
        record_struct,
        field_regs,
    };

    let record_reg = if has_non_const_fields {
        b.push_reg(span, OpKind::AllocBoxedRecord, box_record_op)
    } else {
        b.push_reg(span, OpKind::ConstBoxedRecord, box_record_op)
    };

    b.cast_boxed(span, record_reg, boxed_abi_type.clone())
}

fn record_field_value_to_const_reg(
    ehx: &mut EvalHirCtx,
    b: &mut Builder,
    span: Span,
    field_value: &boxed::FieldValue,
    abi_type: &abitype::AbiType,
) -> BuiltReg {
    use crate::mir::ops::*;
    use arret_runtime::boxed::prelude::*;
    use boxed::FieldValue;

    // This depends on the fact we're encoding the exact record layout we're reading from. We only
    // need `abi_type` to find the specific pointer type for boxed values.
    match field_value {
        FieldValue::Int(v) => b.push_reg(span, OpKind::ConstInt64, *v),
        FieldValue::Float(v) => b.push_reg(span, OpKind::ConstFloat, *v),
        FieldValue::Bool(v) => b.push_reg(span, OpKind::ConstBool, *v),
        FieldValue::Char(v) => b.push_reg(span, OpKind::ConstChar, *v),
        FieldValue::InternedSym(interned) => {
            let name = ehx.as_heap().type_info().interner().unintern(interned);
            b.push_reg(span, OpKind::ConstInternedSym, name.into())
        }
        FieldValue::Boxed(any_ref) => const_to_reg(ehx, b, span, *any_ref, abi_type),
    }
}

pub fn reg_to_boxed_reg(
    b: &mut Builder,
    span: Span,
    reg_value: &value::RegValue,
    to_boxed: &abitype::BoxedAbiType,
) -> BuiltReg {
    use crate::mir::ops::*;
    use arret_runtime::boxed::TypeTag;

    match &reg_value.abi_type {
        abitype::AbiType::Boxed(from_boxed) => {
            b.cast_boxed_cond(span, from_boxed, reg_value.reg, to_boxed.clone())
        }
        abitype::AbiType::Int => {
            let boxed_int_reg = b.push_reg(span, OpKind::AllocBoxedInt, reg_value.reg.into());
            b.cast_boxed_cond(span, &TypeTag::Int.into(), boxed_int_reg, to_boxed.clone())
        }
        abitype::AbiType::Char => {
            let boxed_char_reg = b.push_reg(span, OpKind::AllocBoxedChar, reg_value.reg.into());
            b.cast_boxed_cond(
                span,
                &TypeTag::Char.into(),
                boxed_char_reg,
                to_boxed.clone(),
            )
        }
        abitype::AbiType::InternedSym => {
            let boxed_sym_reg = b.push_reg(span, OpKind::AllocBoxedSym, reg_value.reg.into());
            b.cast_boxed_cond(span, &TypeTag::Sym.into(), boxed_sym_reg, to_boxed.clone())
        }
        abitype::AbiType::Float => {
            let boxed_float_reg = b.push_reg(span, OpKind::AllocBoxedFloat, reg_value.reg.into());
            b.cast_boxed_cond(
                span,
                &TypeTag::Float.into(),
                boxed_float_reg,
                to_boxed.clone(),
            )
        }
        abitype::AbiType::Bool => b.push_cond(
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
        // Callbacks are ephemeral unboxed types. They cannot be returned from functions and should
        // never need to be boxed.
        abitype::AbiType::Callback(_) => {
            unimplemented!("callback to boxed reg {:?} conversion", to_boxed)
        }
    }
}

fn boxed_to_bool(
    b: &mut Builder,
    span: Span,
    from_boxed: &abitype::BoxedAbiType,
    reg_value: &value::RegValue,
) -> BuiltReg {
    use crate::mir::ops::*;
    use arret_runtime::boxed::TypeTag;

    let possible_type_tags =
        reg_value.possible_type_tags & [TypeTag::True, TypeTag::False].iter().collect();

    if possible_type_tags == TypeTag::True.into() {
        b.push_reg(span, OpKind::ConstBool, true)
    } else if possible_type_tags == TypeTag::False.into() {
        b.push_reg(span, OpKind::ConstBool, false)
    } else {
        let boxed_any_reg =
            b.cast_boxed_cond(span, from_boxed, reg_value.reg, abitype::BoxedAbiType::Any);

        let boxed_type_tag_reg = b.push_reg(
            span,
            OpKind::LoadBoxedTypeTag,
            LoadBoxedTypeTagOp {
                subject_reg: boxed_any_reg.into(),
                possible_type_tags,
            },
        );

        let true_type_tag_reg = b.push_reg(span, OpKind::ConstTypeTag, TypeTag::True);

        b.push_reg(
            span,
            OpKind::TypeTagEqual,
            BinaryOp {
                lhs_reg: boxed_type_tag_reg.into(),
                rhs_reg: true_type_tag_reg.into(),
            },
        )
    }
}

fn reg_to_reg(
    ehx: &mut EvalHirCtx,
    b: &mut Builder,
    span: Span,
    reg_value: &value::RegValue,
    abi_type: &abitype::AbiType,
) -> BuiltReg {
    use crate::mir::ops::*;
    use arret_runtime::boxed::TypeTag;

    match (&reg_value.abi_type, abi_type) {
        (from, to) if from == to => reg_value.reg,
        (_, abitype::AbiType::Boxed(to_boxed)) => reg_to_boxed_reg(b, span, reg_value, to_boxed),
        (abitype::AbiType::Boxed(from_boxed), abitype::AbiType::Int) => {
            let boxed_int_reg =
                b.cast_boxed_cond(span, from_boxed, reg_value.reg, TypeTag::Int.into());
            b.push_reg(span, OpKind::LoadBoxedIntValue, boxed_int_reg.into())
        }
        (abitype::AbiType::Boxed(from_boxed), abitype::AbiType::Float) => {
            let boxed_float_reg =
                b.cast_boxed_cond(span, from_boxed, reg_value.reg, TypeTag::Float.into());
            b.push_reg(span, OpKind::LoadBoxedFloatValue, boxed_float_reg.into())
        }
        (abitype::AbiType::Boxed(from_boxed), abitype::AbiType::Char) => {
            let boxed_char_reg =
                b.cast_boxed_cond(span, from_boxed, reg_value.reg, TypeTag::Char.into());
            b.push_reg(span, OpKind::LoadBoxedCharValue, boxed_char_reg.into())
        }
        (abitype::AbiType::Boxed(from_boxed), abitype::AbiType::Bool) => {
            boxed_to_bool(b, span, from_boxed, reg_value)
        }
        (abitype::AbiType::Boxed(from_boxed), abitype::AbiType::InternedSym) => {
            let boxed_sym_reg =
                b.cast_boxed_cond(span, from_boxed, reg_value.reg, TypeTag::Sym.into());
            b.push_reg(span, OpKind::LoadBoxedSymInterned, boxed_sym_reg.into())
        }
        (abitype::AbiType::Boxed(from_boxed), abitype::AbiType::Callback(entry_point_abi)) => {
            ehx.thunk_reg_to_callback_reg(b, span, from_boxed, reg_value.reg, entry_point_abi)
        }
        (from, to) => unimplemented!("reg {:?} to reg {:?} conversion", from, to),
    }
}

fn thunk_reg_to_reg(
    b: &mut Builder,
    span: Span,
    boxed_thunk_reg: BuiltReg,
    boxed_abi_type: &abitype::BoxedAbiType,
) -> BuiltReg {
    use arret_runtime::boxed::TypeTag;

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
    abi_type: &abitype::AbiType,
) -> BuiltReg {
    match abi_type {
        abitype::AbiType::Boxed(boxed_abi_type) => {
            let thunk_reg = ehx.arret_fun_to_thunk_reg(b, span, arret_fun);
            thunk_reg_to_reg(b, span, thunk_reg, boxed_abi_type)
        }
        abitype::AbiType::Callback(entry_point_abi) => {
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
    abi_type: &abitype::AbiType,
) -> BuiltReg {
    match abi_type {
        abitype::AbiType::Boxed(boxed_abi_type) => {
            let thunk_reg = ehx.rust_fun_to_thunk_reg(b, span, rust_fun);
            thunk_reg_to_reg(b, span, thunk_reg, boxed_abi_type)
        }
        abitype::AbiType::Callback(entry_point_abi) => {
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
    abi_type: &abitype::AbiType,
) -> BuiltReg {
    match value {
        Value::Reg(reg_value) => reg_to_reg(ehx, b, span, reg_value, abi_type),
        Value::Const(any_ref) => const_to_reg(ehx, b, span, *any_ref, abi_type),
        Value::List(fixed, rest) => {
            if let abitype::AbiType::Boxed(boxed_abi_type) = abi_type {
                list_to_reg(
                    ehx,
                    b,
                    span,
                    fixed,
                    rest.as_ref().map(AsRef::as_ref),
                    boxed_abi_type,
                )
            } else {
                panic!("Attempt to construct non-boxed list");
            }
        }
        Value::Record(record_cons, fields) => {
            if let abitype::AbiType::Boxed(boxed_abi_type) = abi_type {
                record_to_reg(ehx, b, span, record_cons, fields, boxed_abi_type)
            } else {
                panic!("Attempt to construct non-boxed record");
            }
        }
        Value::ArretFun(ref arret_fun) => arret_fun_to_reg(ehx, b, span, arret_fun, abi_type),
        Value::TyPred(test_ty) => {
            let ty_pred_arret_fun = ehx
                .synthetic_funs()
                .ty_pred_arret_fun(test_ty.clone())
                .clone();

            arret_fun_to_reg(ehx, b, span, &ty_pred_arret_fun, abi_type)
        }
        Value::EqPred => {
            let eq_pred_arret_fun = ehx.synthetic_funs().eq_pred_arret_fun().clone();
            arret_fun_to_reg(ehx, b, span, &eq_pred_arret_fun, abi_type)
        }
        Value::RecordCons(cons) => {
            let record_cons_arret_fun = ehx.synthetic_funs().record_cons_arret_fun(cons).clone();
            arret_fun_to_reg(ehx, b, span, &record_cons_arret_fun, abi_type)
        }
        Value::FieldAccessor(cons, field_index) => {
            let field_accessor_arret_fun = ehx
                .synthetic_funs()
                .field_accessor_arret_fun(cons, *field_index)
                .clone();
            arret_fun_to_reg(ehx, b, span, &field_accessor_arret_fun, abi_type)
        }
        Value::RustFun(ref rust_fun) => rust_fun_to_reg(ehx, b, span, rust_fun, abi_type),
    }
}
