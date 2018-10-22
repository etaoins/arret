use syntax::span::Span;

use runtime::abitype;
use runtime::boxed;
use runtime::boxed::prelude::*;

use crate::mir::builder::Builder;
use crate::mir::eval_hir::EvalHirCtx;
use crate::mir::ops::*;
use crate::mir::tagset::TypeTagSet;
use crate::mir::value::build_reg::value_to_reg;
use crate::mir::value::from_reg::reg_to_value;
use crate::mir::value::Value;
use crate::ty;

/// Returns a set of type tags that would satisfy the type predicate
fn type_tags_for_test_ty(test_ty: ty::pred::TestTy) -> TypeTagSet {
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
        TestTy::List => [boxed::TypeTag::TopPair, boxed::TypeTag::Nil]
            .iter()
            .collect(),
        TestTy::Vector => boxed::TypeTag::TopVector.into(),
        TestTy::Map => {
            unimplemented!("maps");
        }
        TestTy::Set => {
            unimplemented!("sets");
        }
    }
}

pub fn eval_ty_pred(
    ehx: &mut EvalHirCtx,
    b: &mut Option<Builder>,
    span: Span,
    subject_value: &Value,
    test_ty: ty::pred::TestTy,
) -> Value {
    use crate::mir::value::types::possible_type_tags_for_value;

    let qualifying_type_tags = type_tags_for_test_ty(test_ty);
    let possible_type_tags = possible_type_tags_for_value(subject_value);

    if possible_type_tags.is_subset(qualifying_type_tags) {
        // Statically true
        return Value::Const(boxed::TRUE_INSTANCE.as_any_ref());
    } else if qualifying_type_tags.is_disjoint(possible_type_tags) {
        // Statically false
        return Value::Const(boxed::FALSE_INSTANCE.as_any_ref());
    }

    let b = if let Some(some_b) = b {
        some_b
    } else {
        panic!(
            "runtime type predicate without builder: {:?} is {:?}",
            subject_value, test_ty
        );
    };

    let subject_reg = value_to_reg(
        ehx,
        b,
        span,
        subject_value,
        &abitype::BoxedABIType::Any.into(),
    );

    let subject_type_tag_reg = b.push_reg(span, OpKind::LoadBoxedTypeTag, subject_reg.into());

    let result_reg = (qualifying_type_tags & possible_type_tags)
        .into_iter()
        .fold(None, |tail_result_reg, test_tag| {
            let test_tag_reg = b.push_reg(span, OpKind::ConstTypeTag, test_tag);

            let is_test_tag = b.push_reg(
                span,
                OpKind::IntEqual,
                BinaryOp {
                    lhs_reg: subject_type_tag_reg,
                    rhs_reg: test_tag_reg,
                },
            );

            if let Some(tail_result_reg) = tail_result_reg {
                // Logically or this with our tail result
                let or_result_reg = b.alloc_reg();

                let cond_op_kind = OpKind::Cond(CondOp {
                    reg_phi: Some(RegPhi {
                        output_reg: or_result_reg,
                        true_result_reg: is_test_tag,
                        false_result_reg: tail_result_reg,
                    }),
                    test_reg: is_test_tag,
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

    reg_to_value(
        ehx,
        result_reg,
        &abitype::ABIType::Bool,
        &ty::Ty::Bool.into_mono(),
    )
}
