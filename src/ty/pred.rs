use ty;

#[derive(Debug, PartialEq)]
pub enum InterpretedPred<S>
where
    S: ty::TyRef,
{
    /// Statically known if the subject type satisfies the testing type
    Static(bool),

    /// Some values of the subject type satisfy the testing type
    ///
    /// The returned types are intended for use with occurrence typing. They are the new types for
    /// the subject if the predicate returns true and false respectively.
    Dynamic(S, S),
}

trait InterpretPredCtx<S>
where
    S: ty::TyRef,
{
    fn interpret_refs(&self, &S, &S) -> InterpretedPred<S>;

    fn interpret_ref_iter<'a, I>(&self, subject_refs: I, test_ref: &'a S) -> InterpretedPred<S>
    where
        I: Iterator<Item = &'a S>,
    {
        let mut true_members: Vec<S> = vec![];
        let mut false_members: Vec<S> = vec![];

        for subject_ref in subject_refs {
            match self.interpret_refs(subject_ref, test_ref) {
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
    /// This logic needs to deal with typical union types and "virtual" ones such as `Bool` and
    /// `Any`. The virtual unions need to be broken up in to their subtypes for processing.
    fn interpret_non_subty(
        &self,
        subject_ref: &S,
        subject_ty: &ty::Ty<S>,
        test_ref: &S,
        test_ty: &ty::Ty<S>,
    ) -> InterpretedPred<S> {
        match (subject_ty, test_ty) {
            (ty::Ty::Bool, _) => self.interpret_ref_iter(
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
                    self.interpret_ref_iter(
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
            (ty::Ty::Union(members), _) => self.interpret_ref_iter(members.iter(), test_ref),
            _ => InterpretedPred::Dynamic(test_ref.clone(), subject_ref.clone()),
        }
    }
}

struct InterpretPolyPredCtx<'a> {
    tvars: &'a [ty::TVar],
}

impl<'a> InterpretPredCtx<ty::Poly> for InterpretPolyPredCtx<'a> {
    fn interpret_refs(
        &self,
        subject_ref: &ty::Poly,
        test_ref: &ty::Poly,
    ) -> InterpretedPred<ty::Poly> {
        use ty::is_a;
        use ty::resolve;

        match is_a::poly_is_a(self.tvars, subject_ref, test_ref) {
            is_a::Result::Yes => InterpretedPred::Static(true),
            is_a::Result::May => {
                let subject_resolved = resolve::resolve_poly_ty(self.tvars, subject_ref);
                if let resolve::Result::Fixed(subject_ty) = subject_resolved {
                    let test_resolved = resolve::resolve_poly_ty(self.tvars, test_ref);
                    if let resolve::Result::Fixed(test_ty) = test_resolved {
                        return self.interpret_non_subty(subject_ref, subject_ty, test_ref, test_ty);
                    }
                }
                InterpretedPred::Dynamic(test_ref.clone(), subject_ref.clone())
            }
            is_a::Result::No => InterpretedPred::Static(false),
        }
    }
}

/// Performs abstract interpretation of applying a type predicate for `test` type on a `subject`
/// value
pub fn interpret_poly_pred<'a>(
    tvars: &'a [ty::TVar],
    subject: &ty::Poly,
    test: &ty::Poly,
) -> InterpretedPred<ty::Poly> {
    let ctx = InterpretPolyPredCtx { tvars };
    ctx.interpret_refs(subject, test)
}

#[cfg(test)]
mod test {
    use super::*;

    fn poly_for_str(datum_str: &str) -> ty::Poly {
        use hir;
        hir::poly_for_str(datum_str).unwrap()
    }

    fn assert_result(result: &InterpretedPred<ty::Poly>, subject_str: &str, test_str: &str) {
        let subject_poly = poly_for_str(subject_str);
        let test_poly = poly_for_str(test_str);

        assert_eq!(*result, interpret_poly_pred(&[], &subject_poly, &test_poly));
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
        assert_dynamic(
            ("Symbol", "String"),
            "(RawU Symbol String)",
            "(RawU Symbol Int)",
        );

        assert_dynamic(("'foo", "Symbol"), "Symbol", "'foo");
        assert_dynamic(("(RawU 'foo 'bar)", "Symbol"), "Symbol", "(RawU 'foo 'bar)");
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
