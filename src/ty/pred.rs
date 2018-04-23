use std::result;

use ty;

#[derive(Debug, PartialEq)]
pub enum InterpretedPred<S>
where
    S: ty::TyRef,
{
    /// All values of the subject type satisfy the testing type
    StaticTrue,

    /// Some values of the subject type satisfy the testing type
    ///
    /// The returned types are intended for use with occurrence typing. They are the new types for
    /// the subject if the predicate returns true and false respectively.
    Dynamic(S, S),

    /// No values of the subject type satisfy the testing type
    StaticFalse,
}

#[derive(Debug, PartialEq)]
pub enum Error<S>
where
    S: ty::TyRef,
{
    /// Due to type erasure the type cannot be tested at runtime
    ///
    /// The subject and testing type are returned as they may nested in the passed types.
    TypeErased(S, S),
}

type Result<S> = result::Result<InterpretedPred<S>, Error<S>>;

trait InterpretPredCtx<S>
where
    S: ty::TyRef,
{
    fn interpret_refs(&self, &S, &S) -> Result<S>;

    fn interpret_ref_iter<'a, I>(&self, subject_refs: I, test_ref: &'a S) -> Result<S>
    where
        I: Iterator<Item = &'a S>,
    {
        let mut true_members: Vec<S> = vec![];
        let mut false_members: Vec<S> = vec![];

        for subject_ref in subject_refs {
            match self.interpret_refs(subject_ref, test_ref)? {
                InterpretedPred::StaticTrue => {
                    true_members.push(subject_ref.clone());
                }
                InterpretedPred::Dynamic(true_member, false_member) => {
                    true_members.push(true_member);
                    false_members.push(false_member);
                }
                InterpretedPred::StaticFalse => {
                    false_members.push(subject_ref.clone());
                }
            }
        }

        Ok(InterpretedPred::Dynamic(
            S::from_vec(true_members),
            S::from_vec(false_members),
        ))
    }

    /// Returns all of the direct subtypes of Any
    ///
    /// TODO: This can be expensive. `static_lazy` might make sense here.
    fn any_union_members() -> [S; 12] {
        use ty::Ty;

        [
            S::from_ty(Ty::Bool),
            S::from_ty(Ty::Char),
            S::from_ty(Ty::Float),
            S::from_ty(Ty::new_fun(
                true,
                S::from_ty(Ty::Union(vec![])),
                S::from_ty(Ty::Any),
            )),
            S::from_ty(Ty::Map(
                Box::new(S::from_ty(Ty::Any)),
                Box::new(S::from_ty(Ty::Any)),
            )),
            S::from_ty(Ty::Int),
            S::from_ty(Ty::Set(Box::new(S::from_ty(Ty::Any)))),
            S::from_ty(Ty::Str),
            S::from_ty(Ty::Sym),
            S::from_ty(Ty::Vecof(Box::new(S::from_ty(Ty::Any)))),
            S::from_ty(Ty::Cons(
                Box::new(S::from_ty(Ty::Any)),
                Box::new(S::from_ty(Ty::Any)),
            )),
            S::from_ty(Ty::Nil),
        ]
    }

    /// Performs abstract interpretation of a type predicate where the subject and testing type are
    /// not direct subtypes
    ///
    /// This logic needs to deal with typical union types and "virtual" ones such as `Bool`, `Any`
    /// and `Listof`. The virtual unions need to be broken up in to their subtypes for processing.
    fn interpret_non_subty(
        &self,
        subject_ref: &S,
        subject_ty: &ty::Ty<S>,
        test_ref: &S,
    ) -> Result<S> {
        match *subject_ty {
            ty::Ty::Any => self.interpret_ref_iter(Self::any_union_members().iter(), test_ref),
            ty::Ty::Bool => self.interpret_ref_iter(
                [
                    S::from_ty(ty::Ty::LitBool(true)),
                    S::from_ty(ty::Ty::LitBool(false)),
                ].iter(),
                test_ref,
            ),
            ty::Ty::Listof(ref member) => self.interpret_ref_iter(
                [
                    S::from_ty(ty::Ty::Nil),
                    S::from_ty(ty::Ty::Cons(member.clone(), Box::new(subject_ref.clone()))),
                ].iter(),
                test_ref,
            ),
            ty::Ty::Union(ref members) => self.interpret_ref_iter(members.iter(), test_ref),
            _ => Err(Error::TypeErased(subject_ref.clone(), test_ref.clone())),
        }
    }
}

struct InterpretPolyPredCtx<'a> {
    pvars: &'a [ty::PVar],
}

impl<'a> InterpretPredCtx<ty::Poly> for InterpretPolyPredCtx<'a> {
    fn interpret_refs(&self, subject_ref: &ty::Poly, test_ref: &ty::Poly) -> Result<ty::Poly> {
        use ty::is_a;
        use ty::resolve;

        match is_a::poly_is_a(self.pvars, subject_ref, test_ref) {
            is_a::Result::Yes => Ok(InterpretedPred::StaticTrue),
            is_a::Result::May => {
                let subject_resolved = resolve::resolve_poly_ty(self.pvars, subject_ref);
                if let resolve::Result::Fixed(subject_ty) = subject_resolved {
                    self.interpret_non_subty(subject_ref, subject_ty, test_ref)
                } else {
                    Err(Error::TypeErased(subject_ref.clone(), test_ref.clone()))
                }
            }
            is_a::Result::No => Ok(InterpretedPred::StaticFalse),
        }
    }
}

/// Performs abstract interpretation of applying a type predicate for `test` type on a `subject`
/// value
fn interpret_poly_pred<'a>(
    pvars: &'a [ty::PVar],
    subject: &ty::Poly,
    test: &ty::Poly,
) -> Result<ty::Poly> {
    let ctx = InterpretPolyPredCtx { pvars };
    ctx.interpret_refs(subject, test)
}

#[cfg(test)]
mod test {
    use super::*;

    fn poly_for_str(datum_str: &str) -> ty::Poly {
        use hir;
        hir::poly_for_str(datum_str).unwrap()
    }

    fn assert_result(result: Result<ty::Poly>, subject_str: &str, test_str: &str) {
        let subject_poly = poly_for_str(subject_str);
        let test_poly = poly_for_str(test_str);

        assert_eq!(result, interpret_poly_pred(&[], &subject_poly, &test_poly));
    }

    fn assert_static_false(subject_str: &str, test_str: &str) {
        assert_result(Ok(InterpretedPred::StaticFalse), subject_str, test_str);
    }

    fn assert_static_true(subject_str: &str, test_str: &str) {
        assert_result(Ok(InterpretedPred::StaticTrue), subject_str, test_str);
    }

    fn assert_dynamic((true_str, false_str): (&str, &str), subject_str: &str, test_str: &str) {
        let expected = Ok(InterpretedPred::Dynamic(
            poly_for_str(true_str),
            poly_for_str(false_str),
        ));
        assert_result(expected, subject_str, test_str)
    }

    fn assert_erased(subject_str: &str, test_str: &str) {
        let subject_poly = poly_for_str(subject_str);
        let test_poly = poly_for_str(test_str);

        interpret_poly_pred(&[], &subject_poly, &test_poly).unwrap_err();
    }

    #[test]
    fn static_results() {
        assert_static_true("true", "Bool");
        assert_static_false("true", "Char")
    }

    #[test]
    fn erased_types() {
        assert_erased("(Setof Any)", "(Setof Int)");
        assert_erased("(Any -> Any)", "(Any -> Int)");
    }

    #[test]
    fn any_type() {
        assert_dynamic(
            (
                "Symbol",
                "(RawU Bool Char Float (Fn! (RawU) Any) (Map Any Any) Int (Setof Any) String (Vectorof Any) (Cons Any Any) ())",
            ),
            "Any",
            "Symbol",
        );

        // TODO: Until we disallow arbitrary (Cons) types this is erased. If we only allow (Cons) with a list tail this can be allowed.
        assert_erased("Any", "(Listof Any)");
        assert_erased("Any", "(Listof Int)")
    }

    #[test]
    fn simple_unions() {
        assert_dynamic(
            ("Symbol", "String"),
            "(RawU Symbol String)",
            "(RawU Symbol Int)",
        );

        assert_dynamic(
            ("(List Any)", "(List Any Any)"),
            "(RawU (List Any) (List Any Any))",
            "(List Any)",
        );
    }

    #[test]
    fn list_types() {
        assert_dynamic(("()", "(Cons Int (Listof Int))"), "(Listof Int)", "()");
        assert_dynamic(
            ("(Cons Int (Listof Int))", "()"),
            "(Listof Int)",
            "(Cons Any Any)",
        );
    }

    #[test]
    fn bool_type() {
        assert_dynamic(("true", "false"), "Bool", "true");
    }
}
