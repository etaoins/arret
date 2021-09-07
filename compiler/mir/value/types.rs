use arret_runtime::boxed;
use arret_runtime::boxed::prelude::*;

use crate::mir::eval_hir::EvalHirCtx;
use crate::mir::tagset::TypeTagSet;
use crate::mir::value::Value;
use crate::ty;
use crate::ty::record;
use crate::ty::Ty;

/// Compact hint for `RegValue`'s type that can't be captured in its type tags
///
/// To allow type hints to apply to unions, each hint is predicated on the value having the
/// appropriate type. For example, `KnownRecordCons` does not imply that the value is a record, its
/// type tag must be checked first.
///
/// It's possible for multiple `TypeHint`s to be applicable to the same type. However, this is
/// unlikely so only a single type hint will be stored. The choice of type hint in these cases is
/// arbitrary.
#[derive(Debug, Clone, PartialEq)]
pub enum TypeHint {
    /// Record of a known class
    KnownRecordCons(record::ConsId),

    /// List of a known length
    KnownListLen(usize),

    /// Vector of a known length
    KnownVectorLen(usize),

    /// No type hint
    None,
}

#[derive(PartialEq, Debug)]
enum FoundRecordConses<'a> {
    Multi,
    Single(&'a record::ConsId),
    None,
}

/// Looks for the possible record conses of a type reference
fn find_record_conses_for_ty_ref<M>(ty_ref: &ty::Ref<M>) -> FoundRecordConses<'_>
where
    M: ty::Pm,
{
    match ty_ref.try_to_fixed() {
        Some(Ty::Union(members)) => members
            .iter()
            .map(|member| find_record_conses_for_ty_ref(member))
            .fold(FoundRecordConses::None, |member1, member2| {
                match (member1, member2) {
                    (FoundRecordConses::Multi, _) | (_, FoundRecordConses::Multi) => {
                        FoundRecordConses::Multi
                    }
                    (FoundRecordConses::None, FoundRecordConses::Single(single))
                    | (FoundRecordConses::Single(single), FoundRecordConses::None) => {
                        FoundRecordConses::Single(single)
                    }
                    (FoundRecordConses::Single(single1), FoundRecordConses::Single(single2)) => {
                        if single1 == single2 {
                            FoundRecordConses::Single(single1)
                        } else {
                            FoundRecordConses::Multi
                        }
                    }
                    (FoundRecordConses::None, FoundRecordConses::None) => FoundRecordConses::None,
                }
            }),

        Some(Ty::Record(instance)) => FoundRecordConses::Single(instance.cons()),
        Some(Ty::RecordClass(cons)) => FoundRecordConses::Single(cons),
        // These could be anything
        None | Some(Ty::Any) | Some(Ty::TopRecord) => FoundRecordConses::Multi,
        Some(_) => FoundRecordConses::None,
    }
}

pub fn type_hint_for_ty_ref<M>(ty_ref: &ty::Ref<M>) -> TypeHint
where
    M: ty::Pm,
{
    if let FoundRecordConses::Single(known_record_cons) = find_record_conses_for_ty_ref(ty_ref) {
        return TypeHint::KnownRecordCons(known_record_cons.clone());
    }

    if let Some(Ty::List(list)) = ty_ref.try_to_fixed() {
        let std::ops::Range { start, end } = list.size_range();

        if start == end {
            return TypeHint::KnownListLen(start);
        }
    }

    if let Some(Ty::Vector(members)) = ty_ref.try_to_fixed() {
        return TypeHint::KnownVectorLen(members.len());
    }

    TypeHint::None
}

pub fn known_record_cons_for_value<'a>(
    ehx: &'a EvalHirCtx,
    value: &'a Value,
) -> Option<&'a record::ConsId> {
    match value {
        Value::Const(any_ref) => any_ref.downcast_ref::<boxed::Record>().map(|record_ref| {
            ehx.cons_for_jit_record_class_id(record_ref.class_id())
                .expect("unable to lookup record cons for JIT record class ID")
        }),
        Value::Record(cons, _) => Some(cons),
        Value::Reg(reg_value) => {
            if let TypeHint::KnownRecordCons(ref cons) = reg_value.type_hint {
                Some(cons)
            } else {
                None
            }
        }
        _ => None,
    }
}

pub fn known_vector_len_for_value(value: &Value) -> Option<usize> {
    match value {
        Value::Const(any_ref) => any_ref
            .downcast_ref::<boxed::Vector>()
            .map(|vector_ref| vector_ref.len()),
        Value::Reg(reg_value) => {
            if let TypeHint::KnownVectorLen(known_len) = reg_value.type_hint {
                Some(known_len)
            } else {
                None
            }
        }
        _ => None,
    }
}

pub fn type_hint_for_value(ehx: &EvalHirCtx, value: &Value) -> TypeHint {
    if let Some(cons) = known_record_cons_for_value(ehx, value) {
        return TypeHint::KnownRecordCons(cons.clone());
    }

    match value {
        Value::Const(any_ref) => any_ref
            .downcast_ref::<boxed::Vector>()
            .map(|vector_ref| TypeHint::KnownVectorLen(vector_ref.len()))
            .unwrap_or(TypeHint::None),
        Value::Reg(reg_value) => reg_value.type_hint.clone(),
        _ => TypeHint::None,
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

#[cfg(test)]
mod test {
    use super::*;

    use crate::hir::tvar_bounded_by;
    use crate::source::EMPTY_SPAN;
    use crate::ty::ty_args::TyArgs;

    #[test]
    fn test_find_record_conses_for_ty_ref() {
        let cons1 = record::Cons::new(
            EMPTY_SPAN,
            "cons1".into(),
            "cons1?".into(),
            None,
            Box::new([]),
        );

        let cons2 = record::Cons::new(
            EMPTY_SPAN,
            "cons2".into(),
            "cons2?".into(),
            None,
            Box::new([]),
        );

        let class1_poly: ty::Ref<ty::Poly> = cons1.clone().into();
        let class2_poly: ty::Ref<ty::Poly> = cons2.clone().into();

        let instance1_poly: ty::Ref<ty::Poly> =
            record::Instance::new(cons1.clone(), TyArgs::empty()).into();
        let instance2_poly: ty::Ref<ty::Poly> =
            record::Instance::new(cons2.clone(), TyArgs::empty()).into();

        // Unit type can't contain a record type
        assert_eq!(
            FoundRecordConses::None,
            find_record_conses_for_ty_ref::<ty::Poly>(&Ty::unit().into())
        );

        // `Any` could contain any record cons
        assert_eq!(
            FoundRecordConses::Multi,
            find_record_conses_for_ty_ref::<ty::Poly>(&Ty::Any.into())
        );

        // `TopRecord` could contain any record cons
        assert_eq!(
            FoundRecordConses::Multi,
            find_record_conses_for_ty_ref::<ty::Poly>(&Ty::TopRecord.into())
        );

        // TVar could contain any record cons
        assert_eq!(
            FoundRecordConses::Multi,
            find_record_conses_for_ty_ref(&tvar_bounded_by(Ty::Any.into()))
        );

        // Class type can have the record cons
        assert_eq!(
            FoundRecordConses::Single(&cons1),
            find_record_conses_for_ty_ref(&class1_poly)
        );

        // Instance type can have the record cons
        assert_eq!(
            FoundRecordConses::Single(&cons2),
            find_record_conses_for_ty_ref(&instance2_poly)
        );

        // Union of class and instance of the same class has the record cons
        assert_eq!(
            FoundRecordConses::Single(&cons1),
            find_record_conses_for_ty_ref(
                &Ty::Union(Box::new([class1_poly, instance1_poly.clone()])).into()
            )
        );

        // Bool + record could only have the record cons
        assert_eq!(
            FoundRecordConses::Single(&cons2),
            find_record_conses_for_ty_ref(
                &Ty::Union(Box::new([Ty::Bool.into(), instance2_poly.clone()])).into()
            )
        );

        // Multiple record types
        assert_eq!(
            FoundRecordConses::Multi,
            find_record_conses_for_ty_ref(
                &Ty::Union(Box::new([class2_poly, instance1_poly])).into()
            )
        );

        // TVar inside a union could be any record type
        assert_eq!(
            FoundRecordConses::Multi,
            find_record_conses_for_ty_ref(
                &Ty::Union(Box::new([tvar_bounded_by(Ty::Any.into()), instance2_poly])).into()
            )
        );
    }
}
