use arret_syntax::span::Span;

use arret_runtime::abitype;
use arret_runtime::boxed;
use arret_runtime::boxed::prelude::*;

use crate::mir::builder::{Builder, BuiltReg};
use crate::mir::eval_hir::EvalHirCtx;
use crate::mir::ops::*;
use crate::mir::tagset::TypeTagSet;
use crate::mir::value::build_reg::value_to_reg;
use crate::mir::value::from_reg::reg_to_value;
use crate::mir::value::Value;
use crate::ty;
use crate::ty::record;
use crate::ty::Ty;

/// Returns a set of type tags that would satisfy the type predicate
fn type_tags_for_test_ty(test_ty: &ty::pred::TestTy) -> TypeTagSet {
    use crate::ty::pred::TestTy;

    match test_ty {
        TestTy::Str => boxed::TypeTag::Str.into(),
        TestTy::Sym => boxed::TypeTag::Sym.into(),
        TestTy::Int => boxed::TypeTag::Int.into(),
        TestTy::Float => boxed::TypeTag::Float.into(),
        TestTy::Char => boxed::TypeTag::Char.into(),
        TestTy::Nil => boxed::TypeTag::Nil.into(),
        TestTy::Fun => boxed::TypeTag::FunThunk.into(),
        TestTy::Bool => [boxed::TypeTag::True, boxed::TypeTag::False]
            .iter()
            .collect(),
        TestTy::Num => [boxed::TypeTag::Int, boxed::TypeTag::Float]
            .iter()
            .collect(),
        TestTy::List => [boxed::TypeTag::Pair, boxed::TypeTag::Nil].iter().collect(),
        TestTy::Vector => boxed::TypeTag::Vector.into(),
        TestTy::Map => {
            unimplemented!("maps");
        }
        TestTy::Set => {
            unimplemented!("sets");
        }
        TestTy::TopRecord => boxed::TypeTag::Record.into(),
        TestTy::RecordClass(_) => {
            unimplemented!("record classes");
        }
    }
}

fn build_load_type_tag(
    ehx: &mut EvalHirCtx,
    b: &mut Builder,
    span: Span,
    value: &Value,
    possible_type_tags: TypeTagSet,
) -> BuiltReg {
    let subject_reg = value_to_reg(ehx, b, span, value, &abitype::BoxedABIType::Any.into()).into();

    b.push_reg(
        span,
        OpKind::LoadBoxedTypeTag,
        LoadBoxedTypeTagOp {
            subject_reg,
            possible_type_tags,
        },
    )
}

fn build_is_type_tag(
    b: &mut Builder,
    span: Span,
    subject_type_tag_reg: BuiltReg,
    test_tag: boxed::TypeTag,
) -> BuiltReg {
    let test_tag_reg = b.push_reg(span, OpKind::ConstTypeTag, test_tag);

    b.push_reg(
        span,
        OpKind::TypeTagEqual,
        BinaryOp {
            lhs_reg: subject_type_tag_reg.into(),
            rhs_reg: test_tag_reg.into(),
        },
    )
}

fn eval_tagged_ty_pred(
    ehx: &mut EvalHirCtx,
    b: &mut Option<Builder>,
    span: Span,
    subject_value: &Value,
    qualifying_type_tags: TypeTagSet,
) -> Value {
    use crate::mir::value::types::possible_type_tags_for_value;
    let possible_type_tags = possible_type_tags_for_value(subject_value);

    if possible_type_tags.is_subset(qualifying_type_tags) {
        // Statically true
        return boxed::TRUE_INSTANCE.as_any_ref().into();
    } else if qualifying_type_tags.is_disjoint(possible_type_tags) {
        // Statically false
        return boxed::FALSE_INSTANCE.as_any_ref().into();
    }

    let b = if let Some(some_b) = b {
        some_b
    } else {
        panic!(
            "runtime tagged type predicate without builder: {:?} is in type tag set {:?}",
            subject_value, qualifying_type_tags
        );
    };

    let subject_type_tag_reg = build_load_type_tag(ehx, b, span, subject_value, possible_type_tags);
    let result_reg = (qualifying_type_tags & possible_type_tags)
        .into_iter()
        .fold(None, |tail_result_reg: Option<BuiltReg>, test_tag| {
            let is_test_tag = build_is_type_tag(b, span, subject_type_tag_reg, test_tag);

            if let Some(tail_result_reg) = tail_result_reg {
                // Logically or this with our tail result
                let or_result_reg = b.alloc_local();

                let cond_op_kind = OpKind::Cond(CondOp {
                    reg_phi: Some(RegPhi {
                        output_reg: or_result_reg.into(),
                        true_result_reg: is_test_tag.into(),
                        false_result_reg: tail_result_reg.into(),
                    }),
                    test_reg: is_test_tag.into(),
                    true_ops: Box::new([]),
                    false_ops: Box::new([]),
                });
                b.push(span, cond_op_kind);

                Some(or_result_reg)
            } else {
                // We are the first result
                Some(is_test_tag)
            }
        })
        .unwrap();

    reg_to_value(ehx, result_reg, &abitype::ABIType::Bool, &Ty::Bool.into())
}

fn eval_record_ty_pred(
    ehx: &mut EvalHirCtx,
    b: &mut Option<Builder>,
    span: Span,
    subject_value: &Value,
    test_cons: &record::ConsId,
) -> Value {
    match subject_value {
        Value::Const(any_ref) => {
            let test_evaled_record_class = ehx.evaled_record_class_for_cons(test_cons);

            // Check if our JIT record class matches
            let is_record_class = match any_ref.as_subtype() {
                boxed::AnySubtype::Record(record_ref) => {
                    record_ref.class_id() == test_evaled_record_class.jit_record_class_id
                }
                _ => false,
            };

            boxed::Bool::singleton_ref(is_record_class).into()
        }
        Value::Record(subject_cons, _) => {
            boxed::Bool::singleton_ref(test_cons == subject_cons).into()
        }
        Value::Reg(reg_value) => {
            let possible_type_tags = reg_value.possible_type_tags;

            if !possible_type_tags.contains(boxed::TypeTag::Record.into()) {
                // Cannot be a record
                return boxed::FALSE_INSTANCE.as_any_ref().into();
            }

            let b = if let Some(some_b) = b {
                some_b
            } else {
                panic!("runtime record type predicate without builder");
            };

            // If this isn't guaranteed to be a record we need to test its type tag first
            let is_record_reg = if possible_type_tags == boxed::TypeTag::Record.into() {
                None
            } else {
                let subject_type_tag_reg =
                    build_load_type_tag(ehx, b, span, subject_value, possible_type_tags);

                Some(build_is_type_tag(
                    b,
                    span,
                    subject_type_tag_reg,
                    boxed::TypeTag::Record,
                ))
            };

            // Create a builder for testing the record class
            let mut is_record_class_b = Builder::new();

            let record_reg = value_to_reg(
                ehx,
                &mut is_record_class_b,
                span,
                subject_value,
                &abitype::BoxedABIType::UniqueTagged(boxed::TypeTag::Record).into(),
            )
            .into();

            // Load our subject record class ID
            let subject_record_class_id_reg =
                is_record_class_b.push_reg(span, OpKind::LoadBoxedRecordClassId, record_reg);

            // Create our test record class ID
            let test_record_class_id_reg = is_record_class_b.push_reg(
                span,
                OpKind::ConstRecordClassId,
                ehx.evaled_record_class_for_cons(test_cons)
                    .record_struct
                    .clone(),
            );

            // Compare them for equality
            let is_record_class_reg = is_record_class_b.push_reg(
                span,
                OpKind::RecordClassIdEqual,
                BinaryOp {
                    lhs_reg: subject_record_class_id_reg.into(),
                    rhs_reg: test_record_class_id_reg.into(),
                },
            );

            let result_reg = match is_record_reg {
                Some(is_record_reg) => {
                    // Need to merge the type tag test with the record class test
                    let and_result_reg = b.alloc_local();

                    let cond_op_kind = OpKind::Cond(CondOp {
                        reg_phi: Some(RegPhi {
                            output_reg: and_result_reg.into(),
                            true_result_reg: is_record_class_reg.into(),
                            false_result_reg: is_record_reg.into(),
                        }),
                        test_reg: is_record_reg.into(),
                        true_ops: is_record_class_b.into_ops(),
                        false_ops: Box::new([]),
                    });

                    b.push(span, cond_op_kind);
                    and_result_reg
                }
                None => {
                    // Nothing to merge
                    b.append(is_record_class_b.into_ops().into_vec());
                    is_record_class_reg
                }
            };

            reg_to_value(ehx, result_reg, &abitype::ABIType::Bool, &Ty::Bool.into())
        }
        _ => boxed::FALSE_INSTANCE.as_any_ref().into(),
    }
}

pub fn eval_ty_pred(
    ehx: &mut EvalHirCtx,
    b: &mut Option<Builder>,
    span: Span,
    subject_value: &Value,
    test_ty: &ty::pred::TestTy,
) -> Value {
    match test_ty {
        ty::pred::TestTy::RecordClass(record_cons) => {
            eval_record_ty_pred(ehx, b, span, subject_value, record_cons)
        }
        tagged_ty => {
            let qualifying_type_tags = type_tags_for_test_ty(tagged_ty);
            eval_tagged_ty_pred(ehx, b, span, subject_value, qualifying_type_tags)
        }
    }
}
