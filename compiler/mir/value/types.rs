use arret_runtime::boxed;
use arret_runtime::boxed::prelude::*;

use crate::mir::eval_hir::EvalHirCtx;
use crate::mir::tagset::TypeTagSet;
use crate::mir::value::Value;
use crate::ty;
use crate::ty::record;

/// Returns a TypeTagSet containing the possible type tags for a given value
pub fn possible_type_tags_for_value(value: &Value) -> TypeTagSet {
    match value {
        Value::Const(any_ref) => any_ref.header().type_tag().into(),
        Value::ArretFun(_)
        | Value::RustFun(_)
        | Value::TyPred(_)
        | Value::EqPred
        | Value::RecordCons(_)
        | Value::FieldAccessor(_, _) => boxed::TypeTag::FunThunk.into(),
        Value::List(fixed, rest) => {
            if !fixed.is_empty() {
                // Non-empty list
                boxed::TypeTag::Pair.into()
            } else if let Some(tail) = rest {
                possible_type_tags_for_value(tail)
            } else {
                // Empty list
                boxed::TypeTag::Nil.into()
            }
        }
        Value::Record(_, _) => boxed::TypeTag::Record.into(),
        Value::Reg(reg_value) => reg_value.possible_type_tags,
    }
}

/// Returns the optional known record cons for a value
///
/// This does not imply that the value is definitely a record. For example, `(U SomeRecord false)`
/// will have a known record class of `SomeRecord` although it may be `false`.
pub fn known_record_cons_for_value<'a>(
    ehx: &'a EvalHirCtx,
    value: &'a Value,
) -> Option<&'a record::ConsId> {
    match value {
        Value::Const(any_ref) => any_ref.downcast_ref::<boxed::Record>().map(|record_ref| {
            ehx.cons_for_jit_record_class_id(record_ref.class_id())
                .expect("unable to lookup record cons for JIT record class ID")
        }),
        Value::Record(cons, _) => Some(&cons),
        Value::Reg(reg_value) => reg_value.known_record_cons.as_ref(),
        _ => None,
    }
}

/// Annotates an existing value with Arret type information
///
/// For the majority of values this is a no-op. For this reason this function takes a builder for
/// the Arret type that is only invoked if the type information can be used.
pub fn value_with_arret_ty<F>(
    heap: &mut impl boxed::AsHeap,
    value: Value,
    build_arret_ty: F,
) -> Value
where
    F: FnOnce() -> ty::Ref<ty::Mono>,
{
    if let Value::Reg(reg_value) = value {
        use crate::mir::value::from_reg::refine_reg_value_with_arret_ty;

        // This could be useful; request the type
        let arret_ty = build_arret_ty();
        refine_reg_value_with_arret_ty(heap, &reg_value, &arret_ty)
    } else {
        value
    }
}
