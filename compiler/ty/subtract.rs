use crate::ty;

fn subtract_ref_iters<'a, I, M>(minuend_iter: I, subtrahend_ref: &ty::Ref<M>) -> ty::Ref<M>
where
    I: Iterator<Item = &'a ty::Ref<M>>,
    M: ty::PM + 'a,
{
    ty::unify::unify_ty_ref_iter(
        minuend_iter.map(|minuend_ref| subtract_ty_refs(minuend_ref, subtrahend_ref)),
    )
}

fn subtract_tys<M: ty::PM>(
    minuend_ty: &ty::Ty<M>,
    subtrahend_ref: &ty::Ref<M>,
    subtrahend_ty: &ty::Ty<M>,
) -> ty::Ref<M> {
    match (minuend_ty, subtrahend_ty) {
        (ty::Ty::Bool, _) => subtract_ref_iters(
            [
                ty::Ty::LitBool(false).into(),
                ty::Ty::LitBool(true).into(),
            ]
                .iter(),
            subtrahend_ref,
        ),
        (ty::Ty::Num, _) => subtract_ref_iters(
            [
                ty::Ty::Int.into(),
                ty::Ty::Float.into(),
            ]
                .iter(),
            subtrahend_ref,
        ),
        (ty::Ty::Union(members), _) => subtract_ref_iters(members.iter(), subtrahend_ref),
        (ty::Ty::List(minuend_list), ty::Ty::List(subtrahend_list))
            // Make sure this is even useful or else we can recurse splitting list types
            // indefinitely
            if subtrahend_list.rest().is_none() && minuend_list.fixed().len() == subtrahend_list.fixed().len() =>
        {
            // This is required for `(nil?)` to work correctly
            if let Some(rest) = minuend_list.rest() {
                // This is the list type if we have no rest elements
                let terminated_list =
                    ty::List::new(minuend_list.fixed().to_vec().into_boxed_slice(), None);

                // This is the list type if we have at least one rest element
                let mut continued_fixed = minuend_list.fixed().to_vec();
                continued_fixed.push(rest.clone());
                let continued_list =
                    ty::List::new(continued_fixed.into_boxed_slice(), Some(rest.clone()));

                subtract_ref_iters(
                    [
                        ty::Ty::List(terminated_list).into(),
                        ty::Ty::List(continued_list).into(),
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

pub fn subtract_ty_refs<M: ty::PM>(
    minuend_ref: &ty::Ref<M>,
    subtrahend_ref: &ty::Ref<M>,
) -> ty::Ref<M> {
    use crate::ty::intersect;

    match ty::is_a::ty_ref_is_a(minuend_ref, subtrahend_ref) {
        ty::is_a::Result::Yes => {
            // No type remains
            ty::Ty::Union(Box::new([])).into()
        }
        ty::is_a::Result::May => match (minuend_ref, subtrahend_ref) {
            (ty::Ref::Fixed(minuend_ty), ty::Ref::Fixed(subtrahend_ty)) => {
                // We can substract directly
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
        },
        ty::is_a::Result::No => {
            // Nothing to subtract
            minuend_ref.clone()
        }
    }
}

#[cfg(test)]
mod test {
    use super::*;
    use crate::hir::{poly_for_str, tvar_bounded_by};

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
        assert_subtraction("(List Int Int ...)", "(Listof Int)", "()");
    }

    #[test]
    fn poly_substraction() {
        let ptype1_unbounded = tvar_bounded_by(ty::Ty::Any.into());
        let ptype2_sym = tvar_bounded_by(ty::Ty::Sym.into());
        let ptype3_num = tvar_bounded_by(ty::Ty::Num.into());

        let any_float = poly_for_str("Float");
        let any_int = poly_for_str("Int");
        let foo_sym = poly_for_str("'foo");

        // PType1 - 'foo = PType1
        assert_eq!(
            ptype1_unbounded.clone(),
            subtract_ty_refs(&ptype1_unbounded, &foo_sym)
        );

        // [PType2 Sym] - 'foo = PType1
        assert_eq!(ptype2_sym.clone(), subtract_ty_refs(&ptype2_sym, &foo_sym));

        // [PType3 Num] - Float = (∩ PType3 Int)
        let ptype3_int_intersect: ty::Ref<ty::Poly> =
            ty::Ty::Intersect(Box::new([ptype3_num.clone(), any_int])).into();

        assert_eq!(
            ptype3_int_intersect,
            subtract_ty_refs(&ptype3_num, &any_float)
        );
    }
}
