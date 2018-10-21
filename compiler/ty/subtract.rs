use crate::ty;
use crate::ty::is_a::Isable;
use crate::ty::unify::Unifiable;

fn subtract_ref_iters<'a, I, S>(tvars: &ty::TVars, minuend_iter: I, subtrahend_ref: &S) -> S
where
    I: Iterator<Item = &'a S>,
    S: Isable + Unifiable + 'a,
{
    ty::unify::unify_ty_ref_iter(
        tvars,
        minuend_iter.map(|minuend_ref| subtract_ty_refs(tvars, minuend_ref, subtrahend_ref)),
    )
}

fn subtract_tys<S>(
    tvars: &ty::TVars,
    minuend_ty: &ty::Ty<S>,
    subtrahend_ref: &S,
    subtrahend_ty: &ty::Ty<S>,
) -> S
where
    S: Isable + Unifiable,
{
    match (minuend_ty, subtrahend_ty) {
        (ty::Ty::Bool, _) => subtract_ref_iters(
            tvars,
            [
                ty::Ty::LitBool(false).into_ty_ref(),
                ty::Ty::LitBool(true).into_ty_ref(),
            ]
                .iter(),
            subtrahend_ref,
        ),
        (ty::Ty::Union(members), _) => subtract_ref_iters(tvars, members.iter(), subtrahend_ref),
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
                    tvars,
                    [
                        ty::Ty::List(terminated_list).into_ty_ref(),
                        ty::Ty::List(continued_list).into_ty_ref(),
                    ].iter(),
                    subtrahend_ref,
                )
            } else {
                minuend_ty.clone().into_ty_ref()
            }
        },
        _ => minuend_ty.clone().into_ty_ref(),
    }
}

pub fn subtract_ty_refs<S>(tvars: &ty::TVars, minuend_ref: &S, subtrahend_ref: &S) -> S
where
    S: Isable + Unifiable,
{
    match ty::is_a::ty_ref_is_a(tvars, minuend_ref, subtrahend_ref) {
        ty::is_a::Result::Yes => {
            // No type remains
            ty::Ty::Union(Box::new([])).into_ty_ref()
        }
        ty::is_a::Result::May => {
            if let Some(minuend_ty) = minuend_ref.try_to_fixed() {
                if let Some(subtrahend_ty) = subtrahend_ref.try_to_fixed() {
                    return subtract_tys(tvars, minuend_ty, subtrahend_ref, subtrahend_ty);
                }
            }

            minuend_ref.clone()
        }
        ty::is_a::Result::No => {
            // Nothing to subtract
            minuend_ref.clone()
        }
    }
}

#[cfg(test)]
mod test {
    use super::*;
    use crate::hir::poly_for_str;

    fn assert_subtraction(expected_str: &str, minuend_str: &str, subrahend_str: &str) {
        let expected_poly = poly_for_str(expected_str);
        let minuend_poly = poly_for_str(minuend_str);
        let subtrahend_poly = poly_for_str(subrahend_str);

        let actual_poly = subtract_ty_refs(&ty::TVars::new(), &minuend_poly, &subtrahend_poly);
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
}
