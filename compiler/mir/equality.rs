use arret_syntax::span::Span;

use arret_runtime::abitype;
use arret_runtime::boxed;
use arret_runtime::boxed::prelude::*;

use crate::codegen::GenABI;
use crate::mir::builder::{Builder, BuiltReg, TryToBuilder};
use crate::mir::costing::{cost_for_op_category, cost_for_ops};
use crate::mir::eval_hir::EvalHirCtx;
use crate::mir::ops::*;
use crate::mir::tagset::TypeTagSet;
use crate::mir::value;
use crate::mir::value::build_reg::value_to_reg;
use crate::mir::value::to_const::value_to_const;
use crate::mir::value::Value;
use crate::ty::record;

pub enum EqualityResult {
    Static(bool),
    Dynamic(Value),
}

impl EqualityResult {
    fn from_bool_reg(reg: BuiltReg) -> EqualityResult {
        EqualityResult::Dynamic(value::RegValue::new(reg, abitype::ABIType::Bool).into())
    }
}

impl From<EqualityResult> for Value {
    fn from(er: EqualityResult) -> Value {
        match er {
            EqualityResult::Static(true) => boxed::TRUE_INSTANCE.as_any_ref().into(),
            EqualityResult::Static(false) => boxed::FALSE_INSTANCE.as_any_ref().into(),
            EqualityResult::Dynamic(value) => value,
        }
    }
}

fn runtime_compare(
    ehx: &mut EvalHirCtx,
    b: &mut Builder,
    span: Span,
    left_value: &Value,
    right_value: &Value,
) -> BuiltReg {
    let left_reg = value_to_reg(ehx, b, span, left_value, &abitype::BoxedABIType::Any.into());

    let right_reg = value_to_reg(
        ehx,
        b,
        span,
        right_value,
        &abitype::BoxedABIType::Any.into(),
    );

    let abi = GenABI {
        takes_task: true,
        params: Box::new([
            abitype::BoxedABIType::Any.into(),
            abitype::BoxedABIType::Any.into(),
        ]),
        ret: abitype::ABIType::Bool.into(),
    };

    let callee = Callee::StaticSymbol(StaticSymbol {
        symbol: "arret_runtime_equals",
        impure: false,
        abi,
    });

    b.push_reg(
        span,
        OpKind::Call,
        CallOp {
            callee,
            impure: false,
            args: Box::new([left_reg.into(), right_reg.into()]),
        },
    )
}

fn build_native_compare<F>(
    ehx: &mut EvalHirCtx,
    b: &mut Builder,
    span: Span,
    left_value: &Value,
    right_value: &Value,
    abi_type: &abitype::ABIType,
    op_kind: F,
) -> BuiltReg
where
    F: FnOnce(RegId, BinaryOp) -> OpKind,
{
    let left_reg = value_to_reg(ehx, b, span, left_value, abi_type);
    let right_reg = value_to_reg(ehx, b, span, right_value, abi_type);

    b.push_reg(
        span,
        op_kind,
        BinaryOp {
            lhs_reg: left_reg.into(),
            rhs_reg: right_reg.into(),
        },
    )
}

fn build_record_equality(
    ehx: &mut EvalHirCtx,
    parent_b: &mut Builder,
    span: Span,
    record_cons: &record::ConsId,
    left_value: &Value,
    right_value: &Value,
) -> EqualityResult {
    use crate::mir::record_field::load_record_field;

    // Try a fieldwise comparison
    let field_count = record_cons.fields().len();
    let mut fieldwise_b = Builder::new();
    let mut fieldwise_regs = Vec::<BuiltReg>::with_capacity(field_count);

    for field_index in 0..field_count {
        let left_field = load_record_field(
            ehx,
            &mut fieldwise_b,
            span,
            record_cons,
            left_value,
            field_index,
        );

        let right_field = load_record_field(
            ehx,
            &mut fieldwise_b,
            span,
            record_cons,
            right_value,
            field_index,
        );

        match eval_equality(ehx, &mut fieldwise_b, span, &left_field, &right_field) {
            EqualityResult::Static(false) => {
                // The whole comparison is false; we don't need to build anything
                return EqualityResult::Static(false);
            }
            EqualityResult::Static(true) => {
                // We can ignore this comparison
            }
            EqualityResult::Dynamic(value) => {
                let fieldwise_reg =
                    value_to_reg(ehx, &mut fieldwise_b, span, &value, &abitype::ABIType::Bool);
                fieldwise_regs.push(fieldwise_reg);
            }
        }
    }

    let mut fieldwise_reg_iter = fieldwise_regs.into_iter();
    let first_fieldwise_reg = if let Some(fieldwise_reg) = fieldwise_reg_iter.next() {
        fieldwise_reg
    } else {
        // This is statically true
        return EqualityResult::Static(true);
    };

    let combined_fieldwise_reg =
        fieldwise_reg_iter.fold(first_fieldwise_reg, |acc_reg, fieldwise_reg| {
            let phi_result_reg = fieldwise_b.alloc_local();
            fieldwise_b.push(
                span,
                OpKind::Cond(CondOp {
                    reg_phi: Some(RegPhi {
                        output_reg: phi_result_reg.into(),
                        true_result_reg: acc_reg.into(),
                        false_result_reg: fieldwise_reg.into(),
                    }),
                    test_reg: fieldwise_reg.into(),
                    true_ops: Box::new([]),
                    false_ops: Box::new([]),
                }),
            );

            phi_result_reg
        });

    // Try a runtime compare
    let mut runtime_b = Builder::new();
    let runtime_reg = runtime_compare(ehx, &mut runtime_b, span, left_value, right_value);

    // Build ops for both options and cost them
    let fieldwise_ops = fieldwise_b.into_ops();
    let fieldwise_cost = cost_for_ops(fieldwise_ops.iter());

    let runtime_ops = runtime_b.into_ops();
    // Favour fieldwise comparisons. Runtime comparisons of records are more expensive than other
    // types but this wouldn't be captured by `cost_for_ops`. Account for at least the cost of
    // loading the class map.
    let runtime_cost = cost_for_ops(runtime_ops.iter()) + cost_for_op_category(OpCategory::MemLoad);

    if runtime_cost < fieldwise_cost {
        parent_b.append(runtime_ops.into_vec().into_iter());
        EqualityResult::from_bool_reg(runtime_reg)
    } else {
        parent_b.append(fieldwise_ops.into_vec().into_iter());
        EqualityResult::from_bool_reg(combined_fieldwise_reg)
    }
}

/// Builds a comparison between two values known to be boolean
fn build_bool_equality(
    ehx: &mut EvalHirCtx,
    b: &mut Builder,
    span: Span,
    left_value: &Value,
    right_value: &Value,
) -> EqualityResult {
    enum ValueClass {
        ConstTrue,
        Boxed,
        Other,
    }

    fn classify_value(value: &Value) -> ValueClass {
        match value {
            Value::Const(any_ref) if any_ref.header().type_tag() == boxed::TypeTag::True => {
                ValueClass::ConstTrue
            }
            Value::Reg(reg_value) => {
                if let abitype::ABIType::Boxed(_) = &reg_value.abi_type {
                    ValueClass::Boxed
                } else {
                    ValueClass::Other
                }
            }
            _ => ValueClass::Other,
        }
    }

    let left_class = classify_value(left_value);
    let right_class = classify_value(right_value);

    let result_reg = match (left_class, right_class) {
        // Comparing a boolean to constant true can be simplified to a no-op
        (ValueClass::ConstTrue, _) => {
            return EqualityResult::Dynamic(right_value.clone());
        }
        (_, ValueClass::ConstTrue) => {
            return EqualityResult::Dynamic(left_value.clone());
        }
        (ValueClass::Boxed, ValueClass::Boxed) => {
            // If both values are boxed we can just compare the pointers
            build_native_compare(
                ehx,
                b,
                span,
                left_value,
                right_value,
                &abitype::BoxedABIType::Any.into(),
                OpKind::BoxIdentical,
            )
        }
        _ => {
            // Fall back to a native comparison of the unboxed values
            build_native_compare(
                ehx,
                b,
                span,
                left_value,
                right_value,
                &abitype::ABIType::Bool,
                OpKind::BoolEqual,
            )
        }
    };

    EqualityResult::from_bool_reg(result_reg)
}

/// Determines if two values are statically equal
pub fn values_statically_equal(
    ehx: &mut EvalHirCtx,
    left_value: &Value,
    right_value: &Value,
) -> Option<bool> {
    match (left_value, right_value) {
        (Value::Reg(left_reg), Value::Reg(right_reg)) => {
            if [left_reg, right_reg]
                .iter()
                .any(|reg| reg.possible_type_tags == boxed::TypeTag::FunThunk.into())
            {
                // Functions are equal to nothing, including themselves
                return Some(false);
            }

            if left_reg.reg.into_reg_id() != right_reg.reg.into_reg_id() {
                // We can't determine if these are statically equal
                return None;
            }

            for partial_equal_type_tag in TypeTagSet::all().into_iter().filter(|type_tag| {
                match type_tag {
                    // Functions never compare equal
                    boxed::TypeTag::FunThunk => true,
                    // NaN != NaN
                    boxed::TypeTag::Float => true,
                    // Can contain partial equal values
                    boxed::TypeTag::Pair
                    | boxed::TypeTag::Record
                    | boxed::TypeTag::Set
                    | boxed::TypeTag::Map
                    | boxed::TypeTag::Vector => true,
                    // The rest can be compared. Add them explicitly so we will be forced to
                    // classify new types
                    boxed::TypeTag::Int
                    | boxed::TypeTag::Char
                    | boxed::TypeTag::Str
                    | boxed::TypeTag::Sym
                    | boxed::TypeTag::True
                    | boxed::TypeTag::False
                    | boxed::TypeTag::Nil => false,
                }
            }) {
                if [left_reg, right_reg]
                    .iter()
                    .all(|reg| reg.possible_type_tags.contains(partial_equal_type_tag))
                {
                    return None;
                }
            }

            Some(true)
        }
        // Functions never compare equal
        (Value::ArretFun(_), _)
        | (_, Value::ArretFun(_))
        | (Value::RustFun(_), _)
        | (_, Value::RustFun(_))
        | (Value::TyPred(_), _)
        | (_, Value::TyPred(_))
        | (Value::EqPred, _)
        | (_, Value::EqPred) => Some(false),
        _ => {
            if let Some(const_left) = value_to_const(ehx, left_value) {
                if let Some(const_right) = value_to_const(ehx, right_value) {
                    return Some(const_left.eq_in_heap(ehx.as_heap(), &const_right));
                }
            }

            None
        }
    }
}

/// Evaluates if two values are equal
///
/// This attempts `values_statically_equal` before building a runtime comparison.
pub fn eval_equality(
    ehx: &mut EvalHirCtx,
    b: &mut impl TryToBuilder,
    span: Span,
    left_value: &Value,
    right_value: &Value,
) -> EqualityResult {
    use crate::mir::value::types::{known_record_cons_for_value, possible_type_tags_for_value};

    if let Some(static_result) = values_statically_equal(ehx, left_value, right_value) {
        return EqualityResult::Static(static_result);
    }

    let b = if let Some(some_b) = b.try_to_builder() {
        some_b
    } else {
        panic!("runtime equality without builder")
    };

    let left_type_tags = possible_type_tags_for_value(left_value);
    let right_type_tags = possible_type_tags_for_value(right_value);
    let all_type_tags = left_type_tags | right_type_tags;
    let common_type_tags = left_type_tags & right_type_tags;

    if common_type_tags.is_empty() {
        // No types in common
        return EqualityResult::Static(false);
    }

    if [left_type_tags, right_type_tags].contains(&boxed::TypeTag::FunThunk.into()) {
        // Functions always compare false
        return EqualityResult::Static(false);
    }

    if all_type_tags == abitype::ABIType::Bool.into() {
        // Build a specialised comparison for `Bool`
        return build_bool_equality(ehx, b, span, left_value, right_value);
    }

    let boxed_singleton_type_tags: TypeTagSet = [
        boxed::TypeTag::True,
        boxed::TypeTag::False,
        boxed::TypeTag::Nil,
    ]
    .iter()
    .collect();

    let result_reg = if common_type_tags.is_subset(boxed_singleton_type_tags) {
        // We an do a direct pointer comparison
        build_native_compare(
            ehx,
            b,
            span,
            left_value,
            right_value,
            &abitype::BoxedABIType::Any.into(),
            OpKind::BoxIdentical,
        )
    } else if all_type_tags == boxed::TypeTag::Int.into() {
        build_native_compare(
            ehx,
            b,
            span,
            left_value,
            right_value,
            &abitype::ABIType::Int,
            |reg_id, BinaryOp { lhs_reg, rhs_reg }| {
                OpKind::IntCompare(
                    reg_id,
                    CompareOp {
                        comparison: Comparison::Eq,
                        lhs_reg,
                        rhs_reg,
                    },
                )
            },
        )
    } else if all_type_tags == boxed::TypeTag::Char.into() {
        build_native_compare(
            ehx,
            b,
            span,
            left_value,
            right_value,
            &abitype::ABIType::Char,
            OpKind::CharEqual,
        )
    } else if all_type_tags == boxed::TypeTag::Sym.into() {
        build_native_compare(
            ehx,
            b,
            span,
            left_value,
            right_value,
            &abitype::ABIType::InternedSym,
            OpKind::InternedSymEqual,
        )
    } else if all_type_tags == boxed::TypeTag::Float.into() {
        build_native_compare(
            ehx,
            b,
            span,
            left_value,
            right_value,
            &abitype::ABIType::Float,
            |reg_id, BinaryOp { lhs_reg, rhs_reg }| {
                OpKind::FloatCompare(
                    reg_id,
                    CompareOp {
                        comparison: Comparison::Eq,
                        lhs_reg,
                        rhs_reg,
                    },
                )
            },
        )
    } else if all_type_tags == boxed::TypeTag::Record.into() {
        let known_left_cons = known_record_cons_for_value(ehx, left_value);
        let known_right_cons = known_record_cons_for_value(ehx, right_value);

        match (known_left_cons, known_right_cons) {
            (Some(left_cons), Some(right_cons)) => {
                if left_cons == right_cons {
                    let common_cons = left_cons.clone();

                    return build_record_equality(
                        ehx,
                        b,
                        span,
                        &common_cons,
                        left_value,
                        right_value,
                    );
                } else {
                    return EqualityResult::Static(false);
                }
            }
            _ => runtime_compare(ehx, b, span, left_value, right_value),
        }
    } else {
        runtime_compare(ehx, b, span, left_value, right_value)
    };

    EqualityResult::from_bool_reg(result_reg)
}
