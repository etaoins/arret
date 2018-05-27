use std::result;

use ty;
use ty::purity::{PRef, Purity};

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
    fn any_union_members() -> [S; 11] {
        use ty::Ty;

        [
            Ty::Bool.into_ref(),
            Ty::Char.into_ref(),
            Ty::Float.into_ref(),
            ty::TopFun::new(S::PRef::from_purity(Purity::Impure), Ty::Any.into_ref()).into_ref(),
            Ty::Map(Box::new(Ty::Any.into_ref()), Box::new(Ty::Any.into_ref())).into_ref(),
            Ty::Int.into_ref(),
            Ty::Set(Box::new(Ty::Any.into_ref())).into_ref(),
            Ty::Str.into_ref(),
            Ty::Sym.into_ref(),
            Ty::Vecof(Box::new(Ty::Any.into_ref())).into_ref(),
            Ty::List(ty::List::new(vec![], Some(Ty::Any.into_ref()))).into_ref(),
        ]
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
    ) -> Result<S> {
        match subject_ty {
            ty::Ty::Any => self.interpret_ref_iter(Self::any_union_members().iter(), test_ref),
            ty::Ty::Bool => self.interpret_ref_iter(
                [
                    ty::Ty::LitBool(true).into_ref(),
                    ty::Ty::LitBool(false).into_ref(),
                ].iter(),
                test_ref,
            ),
            ty::Ty::List(list) => {
                // Allow checking for a rest list. This is required for `(nil?)` to work correctly.
                if let Some(rest) = list.rest() {
                    // This is the list type if we have no rest elements
                    let terminated_list = ty::List::new(list.fixed().clone(), None);

                    // This is the list type if we have at least one rest element
                    let mut continued_fixed = list.fixed().clone();
                    continued_fixed.push(rest.clone());
                    let continued_list = ty::List::new(continued_fixed, Some(rest.clone()));

                    Ok(self.interpret_ref_iter(
                        [
                            ty::Ty::List(terminated_list).into_ref(),
                            ty::Ty::List(continued_list).into_ref(),
                        ].iter(),
                        test_ref,
                    )?)
                } else {
                    Err(Error::TypeErased(subject_ref.clone(), test_ref.clone()))
                }
            }
            ty::Ty::Union(members) => self.interpret_ref_iter(members.iter(), test_ref),
            _ => Err(Error::TypeErased(subject_ref.clone(), test_ref.clone())),
        }
    }
}

struct InterpretPolyPredCtx<'a> {
    tvars: &'a [ty::TVar],
}

impl<'a> InterpretPredCtx<ty::Poly> for InterpretPolyPredCtx<'a> {
    fn interpret_refs(&self, subject_ref: &ty::Poly, test_ref: &ty::Poly) -> Result<ty::Poly> {
        use ty::is_a;
        use ty::resolve;

        match is_a::poly_is_a(self.tvars, subject_ref, test_ref) {
            is_a::Result::Yes => Ok(InterpretedPred::StaticTrue),
            is_a::Result::May => {
                let subject_resolved = resolve::resolve_poly_ty(self.tvars, subject_ref);
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
pub fn interpret_poly_pred<'a>(
    tvars: &'a [ty::TVar],
    subject: &ty::Poly,
    test: &ty::Poly,
) -> Result<ty::Poly> {
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

    fn assert_result(result: &Result<ty::Poly>, subject_str: &str, test_str: &str) {
        let subject_poly = poly_for_str(subject_str);
        let test_poly = poly_for_str(test_str);

        assert_eq!(*result, interpret_poly_pred(&[], &subject_poly, &test_poly));
    }

    fn assert_static_false(subject_str: &str, test_str: &str) {
        assert_result(&Ok(InterpretedPred::StaticFalse), subject_str, test_str);
    }

    fn assert_static_true(subject_str: &str, test_str: &str) {
        assert_result(&Ok(InterpretedPred::StaticTrue), subject_str, test_str);
    }

    fn assert_dynamic((true_str, false_str): (&str, &str), subject_str: &str, test_str: &str) {
        let expected = Ok(InterpretedPred::Dynamic(
            poly_for_str(true_str),
            poly_for_str(false_str),
        ));
        assert_result(&expected, subject_str, test_str)
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
                "(RawU Bool Char Float (... ->! Any) (Map Any Any) Int (Setof Any) String (Vectorof Any) (Listof Any))",
            ),
            "Any",
            "Symbol",
        );

        assert_erased("Any", "(Listof Int)")
    }

    #[test]
    fn listof_type() {
        assert_dynamic(("()", "(List Int Int ...)"), "(Listof Int)", "()");
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
