use arret_runtime::abitype;
use arret_runtime::boxed;
use arret_runtime::boxed::prelude::*;

use crate::mir::builder::BuiltReg;
use crate::mir::tagset::TypeTagSet;
use crate::mir::value;
use crate::mir::value::Value;
use crate::ty;
use crate::ty::record;
use crate::ty::Ty;

#[derive(PartialEq, Debug)]
enum FoundRecordConses<'a> {
    Multi,
    Single(&'a record::ConsId),
    None,
}

/// Looks for the possible record conses of a type reference
fn find_record_conses_for_ty_ref<M>(ty_ref: &ty::Ref<M>) -> FoundRecordConses<'_>
where
    M: ty::PM,
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

fn reg_to_value_with_constraints<M>(
    heap: &mut impl boxed::AsHeap,
    reg: BuiltReg,
    abi_type: &abitype::ABIType,
    arret_ty: &ty::Ref<M>,
    constrain_possible_type_tags: TypeTagSet,
    constrain_known_record_cons: &Option<record::ConsId>,
) -> Value
where
    M: ty::PM,
{
    // Make sure we have exactly one possible record cons
    let known_record_cons =
        if let FoundRecordConses::Single(record_cons) = find_record_conses_for_ty_ref(arret_ty) {
            Some(record_cons.clone())
        } else {
            constrain_known_record_cons.clone()
        };

    if let Ty::LitSym(value) = arret_ty.resolve_to_ty() {
        // Unlike other literal types we can't encode a literal sym in a `RegValue` without losing
        // information. This means we will need to rebuild the sym every time it's referenced but
        // this should be a net win.
        boxed::Sym::new(heap, value.as_ref()).as_any_ref().into()
    } else {
        value::RegValue {
            reg,
            abi_type: abi_type.clone(),
            possible_type_tags: TypeTagSet::from(abi_type)
                & TypeTagSet::from(arret_ty)
                & constrain_possible_type_tags,
            known_record_cons,
        }
        .into()
    }
}

/// Creates a Value from a register of the given ABI and Arret type
///
/// Supported literal types will be converted to `Value::Const`. Everything else will become a
/// `Value::Reg`.
pub fn reg_to_value<M>(
    heap: &mut impl boxed::AsHeap,
    reg: BuiltReg,
    abi_type: &abitype::ABIType,
    arret_ty: &ty::Ref<M>,
) -> Value
where
    M: ty::PM,
{
    reg_to_value_with_constraints(heap, reg, abi_type, arret_ty, TypeTagSet::all(), &None)
}

pub fn refine_reg_value_with_arret_ty<M>(
    heap: &mut impl boxed::AsHeap,
    reg_value: &value::RegValue,
    arret_ty: &ty::Ref<M>,
) -> Value
where
    M: ty::PM,
{
    reg_to_value_with_constraints(
        heap,
        reg_value.reg,
        &reg_value.abi_type,
        arret_ty,
        reg_value.possible_type_tags,
        &reg_value.known_record_cons,
    )
}

#[cfg(test)]
mod test {
    use super::*;

    use crate::hir::tvar_bounded_by;
    use crate::ty::ty_args::TyArgs;
    use arret_syntax::span::EMPTY_SPAN;

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
                &Ty::Union(Box::new([class1_poly.clone(), instance1_poly.clone()])).into()
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
                &Ty::Union(Box::new([class2_poly.clone(), instance1_poly.clone()])).into()
            )
        );

        // TVar inside a union could be any record type
        assert_eq!(
            FoundRecordConses::Multi,
            find_record_conses_for_ty_ref(
                &Ty::Union(Box::new([
                    tvar_bounded_by(Ty::Any.into()),
                    instance2_poly.clone()
                ]))
                .into()
            )
        );
    }
}
