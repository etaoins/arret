use std::rc::Rc;

use arret_runtime::boxed;
use arret_runtime::boxed::prelude::*;
use arret_runtime::boxed::refs::Gc;

use crate::mir::eval_hir::EvalHirCtx;
use crate::mir::tagset::TypeTagSet;
use crate::mir::value::{RegValue, Value};
use crate::ty;
use crate::ty::record;
use crate::ty::Ty;

pub fn ty_ref_to_const<M>(
    heap: &mut impl boxed::AsHeap,
    ty_ref: &ty::Ref<M>,
) -> Option<Gc<boxed::Any>>
where
    M: ty::PM,
{
    match ty_ref.resolve_to_ty() {
        Ty::LitBool(value) => Some(boxed::Bool::singleton_ref(*value).as_any_ref()),
        Ty::LitSym(value) => Some(boxed::Sym::new(heap, value.as_ref()).as_any_ref()),
        Ty::List(list) if !list.has_rest() => {
            let fixed_consts = list
                .fixed()
                .iter()
                .map(|fixed| ty_ref_to_const(heap, fixed))
                .collect::<Option<Vec<Gc<boxed::Any>>>>()?;

            Some(boxed::List::new(heap, fixed_consts.into_iter()).as_any_ref())
        }
        _ => None,
    }
}

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
        // This could be useful; request the type
        let arret_ty = build_arret_ty();
        if let Some(any_ref) = ty_ref_to_const(heap, &arret_ty) {
            return any_ref.into();
        }

        let old_type_tags = reg_value.possible_type_tags;
        let new_type_tags = old_type_tags & TypeTagSet::from(&arret_ty);

        let known_record_cons = arret_ty
            .find_member(|poly_ty| match poly_ty {
                Ty::Record(instance) => Some(instance.cons()),
                Ty::RecordClass(cons) => Some(cons),
                _ => None,
            })
            .cloned();

        // Avoid allocating a new Rc if this is a no-op
        let new_reg_value = if new_type_tags != old_type_tags {
            Rc::new(RegValue {
                reg: reg_value.reg,
                abi_type: reg_value.abi_type.clone(),
                possible_type_tags: new_type_tags,
                known_record_cons,
            })
        } else {
            reg_value
        };

        Value::Reg(new_reg_value)
    } else {
        value
    }
}
