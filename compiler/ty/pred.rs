use crate::ty;

#[derive(Debug, PartialEq)]
pub enum InterpretedPred<S: ty::TyRef> {
    /// Statically known if the subject type satisfies the testing type
    Static(bool),

    /// Some values of the subject type satisfy the testing type
    ///
    /// The returned types are intended for use with occurrence typing. They are the new types for
    /// the subject if the predicate returns true and false respectively.
    Dynamic(S, S),
}

pub trait Interpretable: ty::TyRef {
    fn interpret_ty_refs(
        tvars: &[ty::TVar],
        subject_ref: &Self,
        test_ref: &Self,
    ) -> InterpretedPred<Self>;
}

impl Interpretable for ty::Mono {
    fn interpret_ty_refs(
        tvars: &[ty::TVar],
        subject_ref: &ty::Mono,
        test_ref: &ty::Mono,
    ) -> InterpretedPred<ty::Mono> {
        use crate::ty::is_a;

        match is_a::ty_ref_is_a(tvars, subject_ref, test_ref) {
            is_a::Result::Yes => InterpretedPred::Static(true),
            is_a::Result::May => interpret_non_subty(
                tvars,
                subject_ref,
                subject_ref.as_ty(),
                test_ref,
                test_ref.as_ty(),
            ),
            is_a::Result::No => InterpretedPred::Static(false),
        }
    }
}

impl Interpretable for ty::Poly {
    fn interpret_ty_refs(
        tvars: &[ty::TVar],
        subject_ref: &ty::Poly,
        test_ref: &ty::Poly,
    ) -> InterpretedPred<ty::Poly> {
        use crate::ty::is_a;
        use crate::ty::resolve;

        match is_a::ty_ref_is_a(tvars, subject_ref, test_ref) {
            is_a::Result::Yes => InterpretedPred::Static(true),
            is_a::Result::May => {
                let subject_resolved = resolve::resolve_poly_ty(tvars, subject_ref);
                if let resolve::Result::Fixed(subject_ty) = subject_resolved {
                    let test_resolved = resolve::resolve_poly_ty(tvars, test_ref);
                    if let resolve::Result::Fixed(test_ty) = test_resolved {
                        return interpret_non_subty(
                            tvars,
                            subject_ref,
                            subject_ty,
                            test_ref,
                            test_ty,
                        );
                    }
                }
                InterpretedPred::Dynamic(test_ref.clone(), subject_ref.clone())
            }
            is_a::Result::No => InterpretedPred::Static(false),
        }
    }
}

fn interpret_ref_iter<'a, S, I>(
    tvars: &[ty::TVar],
    subject_refs: I,
    test_ref: &'a S,
) -> InterpretedPred<S>
where
    S: Interpretable,
    I: Iterator<Item = &'a S>,
{
    let mut true_members: Vec<S> = vec![];
    let mut false_members: Vec<S> = vec![];

    for subject_ref in subject_refs {
        match interpret_ty_refs(tvars, subject_ref, test_ref) {
            InterpretedPred::Static(true) => {
                true_members.push(subject_ref.clone());
            }
            InterpretedPred::Dynamic(true_member, false_member) => {
                true_members.push(true_member);
                false_members.push(false_member);
            }
            InterpretedPred::Static(false) => {
                false_members.push(subject_ref.clone());
            }
        }
    }

    InterpretedPred::Dynamic(S::from_vec(true_members), S::from_vec(false_members))
}

/// Performs abstract interpretation of a type predicate where the subject and testing type are
/// not direct subtypes
///
/// This logic needs to deal with typical union types and "virtual" ones such as `Bool` and `Any`.
/// The virtual unions need to be broken up in to their subtypes for processing.
fn interpret_non_subty<S: Interpretable>(
    tvars: &[ty::TVar],
    subject_ref: &S,
    subject_ty: &ty::Ty<S>,
    test_ref: &S,
    test_ty: &ty::Ty<S>,
) -> InterpretedPred<S> {
    match (subject_ty, test_ty) {
            (ty::Ty::Bool, _) => interpret_ref_iter(tvars,
                [
                    ty::Ty::LitBool(true).into_ty_ref(),
                    ty::Ty::LitBool(false).into_ty_ref(),
                ].iter(),
                test_ref,
            ),
            (ty::Ty::List(list), ty::Ty::List(test_list))
                // Make sure this is even useful or else we can recurse splitting list types
                // indefinitely
                if test_list.rest().is_none() && list.fixed().len() == test_list.fixed().len() =>
            {
                // Allow checking for a rest list. This is required for `(nil?)` to work correctly.
                if let Some(rest) = list.rest() {
                    // This is the list type if we have no rest elements
                    let terminated_list =
                        ty::List::new(list.fixed().to_vec().into_boxed_slice(), None);

                    // This is the list type if we have at least one rest element
                    let mut continued_fixed = list.fixed().to_vec();
                    continued_fixed.push(rest.clone());
                    let continued_list =
                        ty::List::new(continued_fixed.into_boxed_slice(), Some(rest.clone()));
                    interpret_ref_iter(
                        tvars,
                        [
                            ty::Ty::List(terminated_list).into_ty_ref(),
                            ty::Ty::List(continued_list).into_ty_ref(),
                        ].iter(),
                        test_ref,
                    )
                } else {
                    InterpretedPred::Dynamic(test_ref.clone(), subject_ref.clone())
                }
            }
            (ty::Ty::Union(members), _) => interpret_ref_iter(tvars, members.iter(), test_ref),
            _ => InterpretedPred::Dynamic(test_ref.clone(), subject_ref.clone()),
        }
}

/// Performs abstract interpretation of applying a type predicate for `test` type on a `subject`
/// value
pub fn interpret_ty_refs<S: Interpretable>(
    tvars: &[ty::TVar],
    subject_ref: &S,
    test_ref: &S,
) -> InterpretedPred<S> {
    S::interpret_ty_refs(tvars, subject_ref, test_ref)
}

#[cfg(test)]
mod test {
    use super::*;

    fn poly_for_str(datum_str: &str) -> ty::Poly {
        use crate::hir;
        hir::poly_for_str(datum_str)
    }

    fn assert_result(result: &InterpretedPred<ty::Poly>, subject_str: &str, test_str: &str) {
        let subject_poly = poly_for_str(subject_str);
        let test_poly = poly_for_str(test_str);

        assert_eq!(*result, interpret_ty_refs(&[], &subject_poly, &test_poly));
    }

    fn assert_static_false(subject_str: &str, test_str: &str) {
        assert_result(&InterpretedPred::Static(false), subject_str, test_str);
    }

    fn assert_static_true(subject_str: &str, test_str: &str) {
        assert_result(&InterpretedPred::Static(true), subject_str, test_str);
    }

    fn assert_dynamic((true_str, false_str): (&str, &str), subject_str: &str, test_str: &str) {
        let expected = InterpretedPred::Dynamic(poly_for_str(true_str), poly_for_str(false_str));
        assert_result(&expected, subject_str, test_str)
    }

    #[test]
    fn static_results() {
        assert_static_true("true", "Bool");
        assert_static_false("true", "Char")
    }

    #[test]
    fn set_types() {
        assert_dynamic(("(Setof Int)", "(Setof Any)"), "(Setof Any)", "(Setof Int)");
    }

    #[test]
    fn fun_types() {
        assert_dynamic(
            ("(Any -> Int)", "(Any -> Any)"),
            "(Any -> Any)",
            "(Any -> Int)",
        );
    }

    #[test]
    fn any_type() {
        assert_dynamic(("(Listof Int)", "Any"), "Any", "(Listof Int)");
    }

    #[test]
    fn listof_type() {
        assert_dynamic(("()", "(List Int Int ...)"), "(Listof Int)", "()");
        assert_dynamic(
            ("(Listof Int)", "(Listof Any)"),
            "(Listof Any)",
            "(Listof Int)",
        );

        assert_dynamic(
            ("(List Float)", "(List Float Float Float ...)"),
            "(List Float Float ...)",
            "(List Float)",
        );
    }

    #[test]
    fn simple_unions() {
        assert_dynamic(("Sym", "Str"), "(RawU Sym Str)", "(RawU Sym Int)");

        assert_dynamic(("'foo", "Sym"), "Sym", "'foo");
        assert_dynamic(("(RawU 'foo 'bar)", "Sym"), "Sym", "(RawU 'foo 'bar)");
        assert_dynamic(("'bar", "'foo"), "(RawU 'foo 'bar)", "(RawU 'bar 'baz)");

        assert_dynamic(
            ("(List Any)", "(List Any Any)"),
            "(RawU (List Any) (List Any Any))",
            "(List Any)",
        );
    }

    #[test]
    fn bool_type() {
        assert_dynamic(("true", "false"), "Bool", "true");
    }
}
