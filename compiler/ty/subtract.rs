use crate::ty;
use crate::ty::intersect::Intersectable;
use crate::ty::is_a::Isable;
use crate::ty::unify::Unifiable;

pub trait Subtractable: ty::TyRef + Isable + Unifiable + Intersectable {
    fn subtract_ty_refs(tvars: &ty::TVars, minuend_ref: &Self, subtrahend_ref: &Self) -> Self;
}

impl Subtractable for ty::Mono {
    fn subtract_ty_refs(
        tvars: &ty::TVars,
        minuend_mono: &ty::Mono,
        subtrahend_mono: &ty::Mono,
    ) -> ty::Mono {
        match ty::is_a::ty_ref_is_a(tvars, minuend_mono, subtrahend_mono) {
            ty::is_a::Result::Yes => {
                // No type remains
                ty::Ty::Union(Box::new([])).into_mono()
            }
            ty::is_a::Result::May => subtract_tys(
                tvars,
                minuend_mono.as_ty(),
                subtrahend_mono,
                subtrahend_mono.as_ty(),
            ),
            ty::is_a::Result::No => {
                // Nothing to subtract
                minuend_mono.clone()
            }
        }
    }
}

impl Subtractable for ty::Poly {
    fn subtract_ty_refs(
        tvars: &ty::TVars,
        minuend_poly: &ty::Poly,
        subtrahend_poly: &ty::Poly,
    ) -> ty::Poly {
        use crate::ty::intersect;
        use crate::ty::resolve;

        match ty::is_a::ty_ref_is_a(tvars, minuend_poly, subtrahend_poly) {
            ty::is_a::Result::Yes => {
                // No type remains
                ty::Ty::Union(Box::new([])).into_ty_ref()
            }
            ty::is_a::Result::May => match (minuend_poly, subtrahend_poly) {
                (ty::Poly::Fixed(minuend_ty), ty::Poly::Fixed(subtrahend_ty)) => {
                    // We can substract directly
                    subtract_tys(tvars, minuend_ty, subtrahend_poly, subtrahend_ty)
                }
                (ty::Poly::Var(_), ty::Poly::Fixed(subtrahend_ty)) => {
                    // We can refine the bound using an intersection type
                    let minuend_bound_ty = resolve::resolve_poly_ty(tvars, minuend_poly).as_ty();
                    let refined_bound_poly =
                        subtract_tys(tvars, minuend_bound_ty, subtrahend_poly, subtrahend_ty);

                    intersect::intersect_ty_refs(tvars, minuend_poly, &refined_bound_poly)
                        .unwrap_or_else(|_| minuend_poly.clone())
                }
                _ => minuend_poly.clone(),
            },
            ty::is_a::Result::No => {
                // Nothing to subtract
                minuend_poly.clone()
            }
        }
    }
}

fn subtract_ref_iters<'a, I, S>(tvars: &ty::TVars, minuend_iter: I, subtrahend_ref: &S) -> S
where
    I: Iterator<Item = &'a S>,
    S: Subtractable + 'a,
{
    ty::unify::unify_ty_ref_iter(
        tvars,
        minuend_iter.map(|minuend_ref| subtract_ty_refs(tvars, minuend_ref, subtrahend_ref)),
    )
}

fn subtract_tys<S: Subtractable>(
    tvars: &ty::TVars,
    minuend_ty: &ty::Ty<S>,
    subtrahend_ref: &S,
    subtrahend_ty: &ty::Ty<S>,
) -> S {
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
        (ty::Ty::Num, _) => subtract_ref_iters(
            tvars,
            [
                ty::Ty::Int.into_ty_ref(),
                ty::Ty::Float.into_ty_ref(),
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
    S: Subtractable,
{
    S::subtract_ty_refs(tvars, minuend_ref, subtrahend_ref)
}

#[cfg(test)]
mod test {
    use super::*;
    use crate::hir::{poly_for_str, tvar_bounded_by};

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
        let mut tvars = ty::TVars::new();

        let ptype1_unbounded = tvar_bounded_by(&mut tvars, ty::Ty::Any.into_poly());
        let ptype2_sym = tvar_bounded_by(&mut tvars, ty::Ty::Sym.into_poly());
        let ptype3_num = tvar_bounded_by(&mut tvars, ty::Ty::Num.into_poly());

        let any_float = poly_for_str("Float");
        let any_int = poly_for_str("Int");
        let foo_sym = poly_for_str("'foo");

        // PType1 - 'foo = PType1
        assert_eq!(
            ptype1_unbounded.clone(),
            subtract_ty_refs(&tvars, &ptype1_unbounded, &foo_sym)
        );

        // [PType2 Sym] - 'foo = PType1
        assert_eq!(
            ptype2_sym.clone(),
            subtract_ty_refs(&tvars, &ptype2_sym, &foo_sym)
        );

        // [PType3 Num] - Float = (∩ PType3 Int)
        assert_eq!(
            ty::Ty::Intersect(Box::new([ptype3_num.clone(), any_int])).into_poly(),
            subtract_ty_refs(&tvars, &ptype3_num, &any_float)
        );
    }
}
