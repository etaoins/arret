use crate::ty;
use crate::ty::Ty;

fn subtract_ref_iters<'a, I, M>(minuend_iter: I, subtrahend_ref: &ty::Ref<M>) -> ty::Ref<M>
where
    I: Iterator<Item = &'a ty::Ref<M>>,
    M: ty::Pm + 'a,
{
    ty::unify::unify_ty_ref_iter(
        minuend_iter.map(|minuend_ref| subtract_ty_refs(minuend_ref, subtrahend_ref)),
    )
}

fn subtract_tys<M: ty::Pm>(
    minuend_ty: &Ty<M>,
    subtrahend_ref: &ty::Ref<M>,
    subtrahend_ty: &Ty<M>,
) -> ty::Ref<M> {
    match (minuend_ty, subtrahend_ty) {
        (Ty::Bool, _) => subtract_ref_iters(
            [
                Ty::LitBool(false).into(),
                Ty::LitBool(true).into(),
            ]
                .iter(),
            subtrahend_ref,
        ),
        (Ty::Num, _) => subtract_ref_iters(
            [
                Ty::Int.into(),
                Ty::Float.into(),
            ]
                .iter(),
            subtrahend_ref,
        ),
        (Ty::Union(members), _) => subtract_ref_iters(members.iter(), subtrahend_ref),
        (Ty::List(minuend_list), Ty::List(subtrahend_list))
            // Make sure this is even useful or else we can recurse splitting list types
            // indefinitely
            if !subtrahend_list.has_rest() && minuend_list.fixed().len() == subtrahend_list.fixed().len() =>
        {
            // This is required for `(nil?)` to work correctly
            let minued_rest = minuend_list.rest();
            if !minued_rest.is_never() {
                // This is the list type if we have no rest elements
                let terminated_list =
                    ty::List::new_tuple(minuend_list.fixed().to_vec().into_boxed_slice());

                // This is the list type if we have at least one rest element
                let mut continued_fixed = minuend_list.fixed().to_vec();
                continued_fixed.push(minued_rest.clone());
                let continued_list =
                    ty::List::new(continued_fixed.into_boxed_slice(), minued_rest.clone());

                subtract_ref_iters(
                    [
                        terminated_list.into(),
                        continued_list.into(),
                    ].iter(),
                    subtrahend_ref,
                )
            } else {
                minuend_ty.clone().into()
            }
        },
        _ => minuend_ty.clone().into(),
    }
}

pub fn subtract_ty_refs<M: ty::Pm>(
    minuend_ref: &ty::Ref<M>,
    subtrahend_ref: &ty::Ref<M>,
) -> ty::Ref<M> {
    use crate::ty::intersect;

    if ty::is_a::ty_ref_is_a(minuend_ref, subtrahend_ref) {
        // No type remains
        Ty::Union(Box::new([])).into()
    } else {
        match (minuend_ref, subtrahend_ref) {
            (ty::Ref::Fixed(minuend_ty), ty::Ref::Fixed(subtrahend_ty)) => {
                // We can subtract directly
                subtract_tys(minuend_ty, subtrahend_ref, subtrahend_ty)
            }
            (ty::Ref::Var(_, _), ty::Ref::Fixed(subtrahend_ty)) => {
                // We can refine the bound using an intersection type
                let minuend_bound_ty = minuend_ref.resolve_to_ty();
                let refined_bound_poly =
                    subtract_tys(minuend_bound_ty, subtrahend_ref, subtrahend_ty);

                intersect::intersect_ty_refs(minuend_ref, &refined_bound_poly)
                    .unwrap_or_else(|_| minuend_ref.clone())
            }
            _ => minuend_ref.clone(),
        }
    }
}

#[cfg(test)]
mod test {
    use super::*;

    use std::collections::HashMap;

    use crate::hir::{poly_for_str, tvar_bounded_by};
    use crate::source::EMPTY_SPAN;
    use crate::ty::record;
    use crate::ty::ty_args::TyArgs;
    use crate::ty::var_usage::Variance;

    fn assert_subtraction(expected_str: &str, minuend_str: &str, subrahend_str: &str) {
        let expected_poly = poly_for_str(expected_str);
        let minuend_poly = poly_for_str(minuend_str);
        let subtrahend_poly = poly_for_str(subrahend_str);

        let actual_poly = subtract_ty_refs(&minuend_poly, &subtrahend_poly);
        assert_eq!(expected_poly, actual_poly);
    }

    #[test]
    fn trivial_subtraction() {
        assert_subtraction("Int", "Int", "Float");
        assert_subtraction("(RawU)", "Int", "Int");
    }

    #[test]
    fn bool_subtraction() {
        assert_subtraction("true", "Bool", "false");
        assert_subtraction("false", "Bool", "true");
    }

    #[test]
    fn num_subtraction() {
        assert_subtraction("Float", "Num", "Int");
        assert_subtraction("Int", "Num", "Float");
    }

    #[test]
    fn union_subtraction() {
        assert_subtraction("Sym", "(RawU Sym Str)", "Str");
        assert_subtraction(
            "(RawU 'foo 'bar)",
            "(RawU 'foo 'bar 'baz 'foobar)",
            "(RawU 'baz 'foobar)",
        );
    }

    #[test]
    fn list_subtraction() {
        assert_subtraction("(List Int & Int)", "(List & Int)", "()");
    }

    #[test]
    fn poly_substraction() {
        let ptype1_unbounded = tvar_bounded_by(Ty::Any.into());
        let ptype2_sym = tvar_bounded_by(Ty::Sym.into());
        let ptype3_num = tvar_bounded_by(Ty::Num.into());

        let any_float = poly_for_str("Float");
        let any_int = poly_for_str("Int");
        let foo_sym = poly_for_str("'foo");

        // PType1 - 'foo = PType1
        assert_eq!(
            ptype1_unbounded,
            subtract_ty_refs(&ptype1_unbounded, &foo_sym)
        );

        // [PType2 Sym] - 'foo = PType1
        assert_eq!(ptype2_sym, subtract_ty_refs(&ptype2_sym, &foo_sym));

        // [PType3 Num] - Float = (∩ PType3 Int)
        let ptype3_int_intersect: ty::Ref<ty::Poly> =
            Ty::Intersect(Box::new([ptype3_num.clone(), any_int])).into();

        assert_eq!(
            ptype3_int_intersect,
            subtract_ty_refs(&ptype3_num, &any_float)
        );
    }

    #[test]
    fn poly_record_type() {
        let tvar = ty::TVar::new(EMPTY_SPAN, "tvar".into(), Ty::Any.into());

        // Polymorphic record constructor and top type
        let poly_record_cons = record::Cons::new(
            EMPTY_SPAN,
            "record_cons".into(),
            "record_cons?".into(),
            Some(Box::new([record::PolyParam::TVar(
                Variance::Covariant,
                tvar.clone(),
            )])),
            Box::new([record::Field::new(
                EMPTY_SPAN,
                "num".into(),
                tvar.clone().into(),
            )]),
        );

        let record_class_ref: ty::Ref<ty::Poly> = poly_record_cons.clone().into();

        // Instance parameterised with an `Int`
        let mut int_tvars = HashMap::new();
        int_tvars.insert(tvar, Ty::Int.into());
        let int_ty_args = TyArgs::new(HashMap::new(), int_tvars);

        let int_instance_ref: ty::Ref<ty::Poly> =
            record::Instance::new(poly_record_cons, int_ty_args).into();

        // Record class minus an instance is the record class
        assert_eq!(
            record_class_ref,
            subtract_ty_refs(&record_class_ref, &int_instance_ref)
        );

        // Instance minus the record class is nothing
        let never_ref: ty::Ref<ty::Poly> = Ty::never().into();
        assert_eq!(
            never_ref,
            subtract_ty_refs(&int_instance_ref, &record_class_ref)
        );
    }
}
