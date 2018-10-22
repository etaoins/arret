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

fn possible_type_tags_for_boxed_abi_type(boxed_abi_type: &abitype::BoxedABIType) -> TypeTagSet {
    use runtime::abitype::BoxedABIType;

    match boxed_abi_type {
        BoxedABIType::Any => TypeTagSet::all(),
        BoxedABIType::DirectTagged(type_tag) => (*type_tag).into(),
        BoxedABIType::List(_) => [boxed::TypeTag::TopPair, boxed::TypeTag::Nil]
            .iter()
            .collect(),
        BoxedABIType::Pair(_) => boxed::TypeTag::TopPair.into(),
        BoxedABIType::Vector(_) => boxed::TypeTag::TopVector.into(),
        BoxedABIType::Union(_, type_tags) => type_tags.iter().collect(),
    }
}

fn possible_type_tags_for_abi_type(abi_type: &abitype::ABIType) -> TypeTagSet {
    use runtime::abitype::ABIType;

    match abi_type {
        ABIType::Int => boxed::TypeTag::Int.into(),
        ABIType::Float => boxed::TypeTag::Float.into(),
        ABIType::Char => boxed::TypeTag::Char.into(),
        ABIType::Bool => [boxed::TypeTag::True, boxed::TypeTag::False]
            .iter()
            .collect(),
        ABIType::Boxed(boxed_abi_type) => possible_type_tags_for_boxed_abi_type(boxed_abi_type),
    }
}

fn possible_type_tags_for_value(value: &Value) -> TypeTagSet {
    match value {
        Value::Const(any_ref) => any_ref.header().type_tag().into(),
        Value::ArretFun(_) | Value::RustFun(_) | Value::TyPred(_) | Value::EqPred => {
            boxed::TypeTag::FunThunk.into()
        }
        Value::List(fixed, rest) => {
            if fixed.is_empty() && rest.is_none() {
                boxed::TypeTag::Nil.into()
            } else {
                [boxed::TypeTag::Nil, boxed::TypeTag::TopPair]
                    .iter()
                    .collect()
            }
        }
        Value::Reg(reg_value) => possible_type_tags_for_abi_type(&reg_value.abi_type),
        Value::Divergent => TypeTagSet::new(),
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
    let qualifying_type_tags = type_tags_for_test_ty(test_ty);
    let possible_type_tags = possible_type_tags_for_value(subject_value);

    if possible_type_tags.is_subset(qualifying_type_tags) {
        // Statically true
        return Value::Const(boxed::TRUE_INSTANCE.as_any_ref());
    } else if qualifying_type_tags.is_disjoint(possible_type_tags) {
        // Statically false
        return Value::Const(boxed::FALSE_INSTANCE.as_any_ref());
    }

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

    let testing_tags = qualifying_type_tags & possible_type_tags;
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
