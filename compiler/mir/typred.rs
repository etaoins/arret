use syntax::span::Span;

use runtime::abitype;
use runtime::boxed;
use runtime::boxed::prelude::*;

use crate::mir::builder::Builder;
use crate::mir::eval_hir::EvalHirCtx;
use crate::mir::ops::*;
use crate::mir::value::build_reg::value_to_reg;
use crate::mir::value::from_reg::reg_to_value;
use crate::mir::value::Value;
use crate::ty;

fn type_tags_for_test_ty(test_ty: ty::pred::TestTy) -> &'static [boxed::TypeTag] {
    use crate::ty::pred::TestTy;

    match test_ty {
        TestTy::Str => &[boxed::TypeTag::Str],
        TestTy::Sym => &[boxed::TypeTag::Sym],
        TestTy::Int => &[boxed::TypeTag::Int],
        TestTy::Float => &[boxed::TypeTag::Float],
        TestTy::Char => &[boxed::TypeTag::Char],
        TestTy::Nil => &[boxed::TypeTag::Nil],
        TestTy::Fun => &[boxed::TypeTag::FunThunk],
        TestTy::Bool => &[boxed::TypeTag::True, boxed::TypeTag::False],
        TestTy::List => &[boxed::TypeTag::TopPair, boxed::TypeTag::Nil],
        TestTy::Vector => &[boxed::TypeTag::TopVector],
        TestTy::Map => {
            unimplemented!("maps");
        }
        TestTy::Set => {
            unimplemented!("sets");
        }
    }
}

fn possible_type_tags_for_boxed_abi_type(
    boxed_abi_type: &abitype::BoxedABIType,
) -> Vec<boxed::TypeTag> {
    use runtime::abitype::BoxedABIType;

    match boxed_abi_type {
        BoxedABIType::Any => boxed::ALL_TYPE_TAGS.to_vec(),
        BoxedABIType::DirectTagged(type_tag) => vec![*type_tag],
        BoxedABIType::List(_) => vec![boxed::TypeTag::TopPair, boxed::TypeTag::Nil],
        BoxedABIType::Pair(_) => vec![boxed::TypeTag::TopPair],
        BoxedABIType::Vector(_) => vec![boxed::TypeTag::TopVector],
        BoxedABIType::Union(_, type_tags) => type_tags.to_vec(),
    }
}

fn possible_type_tags_for_abi_type(abi_type: &abitype::ABIType) -> Vec<boxed::TypeTag> {
    use runtime::abitype::ABIType;

    match abi_type {
        ABIType::Int => vec![boxed::TypeTag::Int],
        ABIType::Float => vec![boxed::TypeTag::Float],
        ABIType::Char => vec![boxed::TypeTag::Char],
        ABIType::Bool => vec![boxed::TypeTag::True, boxed::TypeTag::False],
        ABIType::Boxed(boxed_abi_type) => possible_type_tags_for_boxed_abi_type(boxed_abi_type),
    }
}

fn possible_type_tags_for_value(value: &Value) -> Vec<boxed::TypeTag> {
    match value {
        Value::Const(any_ref) => vec![any_ref.header().type_tag()],
        Value::ArretFun(_) | Value::RustFun(_) | Value::TyPred(_) | Value::EqPred => {
            vec![boxed::TypeTag::FunThunk]
        }
        Value::List(fixed, rest) => {
            if fixed.is_empty() && rest.is_none() {
                vec![boxed::TypeTag::Nil]
            } else {
                vec![boxed::TypeTag::Nil, boxed::TypeTag::TopPair]
            }
        }
        Value::Reg(reg_value) => possible_type_tags_for_abi_type(&reg_value.abi_type),
        Value::Divergent => vec![],
    }
}

fn logical_or_result_regs(b: &mut Builder, span: Span, mut result_regs: Vec<RegId>) -> RegId {
    let head_result_reg = result_regs.pop().unwrap();

    if result_regs.is_empty() {
        return head_result_reg;
    }

    let tail_result_reg = logical_or_result_regs(b, span, result_regs);

    let or_result_reg = b.alloc_reg();
    let cond_op_kind = OpKind::Cond(CondOp {
        reg_phi: Some(RegPhi {
            output_reg: or_result_reg,
            true_result_reg: head_result_reg,
            false_result_reg: tail_result_reg,
        }),
        test_reg: head_result_reg,
        true_ops: Box::new([]),
        false_ops: Box::new([]),
    });
    b.push(span, cond_op_kind);

    or_result_reg
}

pub fn eval_ty_pred(
    ehx: &mut EvalHirCtx,
    b: &mut Option<Builder>,
    span: Span,
    subject_value: &Value,
    test_ty: ty::pred::TestTy,
) -> Value {
    let required_type_tags = type_tags_for_test_ty(test_ty);
    let possible_type_tags = possible_type_tags_for_value(subject_value);

    if possible_type_tags
        .iter()
        .all(|type_tag| required_type_tags.contains(type_tag))
    {
        // Statically true
        return Value::Const(boxed::TRUE_INSTANCE.as_any_ref());
    } else if !possible_type_tags
        .iter()
        .any(|type_tag| required_type_tags.contains(type_tag))
    {
        // Statically false
        return Value::Const(boxed::FALSE_INSTANCE.as_any_ref());
    }

    let testing_tags: Vec<boxed::TypeTag> = required_type_tags
        .iter()
        .filter(|type_tag| possible_type_tags.contains(type_tag))
        .cloned()
        .collect();

    let some_b = if let Some(some_b) = b {
        some_b
    } else {
        panic!(
            "runtime type predicate without builder: {:?} is {:?}",
            subject_value, test_ty
        );
    };

    let subject_reg = value_to_reg(
        ehx,
        some_b,
        span,
        subject_value,
        &abitype::BoxedABIType::Any.into(),
    );

    let subject_type_tag_reg = some_b.push_reg(span, OpKind::LoadBoxedTypeTag, subject_reg.into());

    let result_regs = testing_tags
        .into_iter()
        .map(|test_tag| {
            let test_tag_reg = some_b.push_reg(span, OpKind::ConstTypeTag, test_tag);

            some_b.push_reg(
                span,
                OpKind::IntEqual,
                BinaryOp {
                    lhs_reg: subject_type_tag_reg,
                    rhs_reg: test_tag_reg,
                },
            )
        })
        .collect();

    let result_reg = logical_or_result_regs(some_b, span, result_regs);

    reg_to_value(
        ehx,
        result_reg,
        &abitype::ABIType::Bool,
        &ty::Ty::Bool.into_mono(),
    )
}
