use crate::ty;
use crate::ty::is_a::Isable;
use crate::ty::purity::Purity;

#[derive(PartialEq, Eq, Debug, Clone, Copy, Hash)]
pub enum TestTy {
    Sym,
    Str,
    Bool,
    Int,
    Float,
    Char,
    List,
    Vector,
    Set,
    Map,
    Fun,
    Nil,
}

impl TestTy {
    pub fn to_ty<S: ty::TyRef>(self) -> ty::Ty<S> {
        match self {
            TestTy::Sym => ty::Ty::Sym,
            TestTy::Str => ty::Ty::Str,
            TestTy::Bool => ty::Ty::Bool,
            TestTy::Int => ty::Ty::Int,
            TestTy::Float => ty::Ty::Float,
            TestTy::Char => ty::Ty::Char,
            TestTy::List => {
                ty::Ty::List(ty::List::new(Box::new([]), Some(ty::Ty::Any.into_ty_ref())))
            }
            TestTy::Vector => ty::Ty::Vectorof(Box::new(ty::Ty::Any.into_ty_ref())),
            TestTy::Set => ty::Ty::Set(Box::new(ty::Ty::Any.into_ty_ref())),
            TestTy::Map => ty::Ty::Map(Box::new(ty::Map::new(
                ty::Ty::Any.into_ty_ref(),
                ty::Ty::Any.into_ty_ref(),
            ))),
            TestTy::Fun => {
                ty::TopFun::new(Purity::Impure.into_poly(), ty::Ty::Any.into_poly()).into_ty()
            }
            TestTy::Nil => ty::Ty::List(ty::List::empty()),
        }
    }
}

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

fn interpret_ref_iter<'a, S, I>(
    tvars: &ty::TVars,
    subject_refs: I,
    test_ty: TestTy,
) -> InterpretedPred<S>
where
    S: Isable,
    I: Iterator<Item = &'a S>,
    S: 'a,
{
    let mut true_members: Vec<S> = vec![];
    let mut false_members: Vec<S> = vec![];

    for subject_ref in subject_refs {
        match interpret_ty_ref(tvars, subject_ref, test_ty) {
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
fn interpret_non_subty<S: Isable>(
    tvars: &ty::TVars,
    subject_ref: &S,
    subject_ty: &ty::Ty<S>,
    test_ty: TestTy,
) -> InterpretedPred<S> {
    match (subject_ty, test_ty) {
        (ty::Ty::List(list), TestTy::Nil) if list.fixed().is_empty() => {
            if let Some(rest) = list.rest() {
                let empty_list = ty::List::empty();
                let non_empty_list = ty::List::new(Box::new([rest.clone()]), Some(rest.clone()));

                interpret_ref_iter(
                    tvars,
                    [
                        ty::Ty::List(empty_list).into_ty_ref(),
                        ty::Ty::List(non_empty_list).into_ty_ref(),
                    ]
                        .iter(),
                    test_ty,
                )
            } else {
                InterpretedPred::Dynamic(test_ty.to_ty().into_ty_ref(), subject_ref.clone())
            }
        }
        (ty::Ty::Union(members), _) => interpret_ref_iter(tvars, members.iter(), test_ty),
        _ => InterpretedPred::Dynamic(test_ty.to_ty().into_ty_ref(), subject_ref.clone()),
    }
}

/// Performs abstract interpretation of applying a type predicate for `test` type on a `subject`
/// value
pub fn interpret_ty_ref<S: Isable>(
    tvars: &ty::TVars,
    subject_ref: &S,
    test_ty: TestTy,
) -> InterpretedPred<S> {
    use crate::ty::is_a;

    match is_a::ty_ref_is_a(tvars, subject_ref, &test_ty.to_ty().into_ty_ref()) {
        is_a::Result::Yes => InterpretedPred::Static(true),
        is_a::Result::May => {
            if let Some(subject_ty) = subject_ref.try_to_fixed() {
                interpret_non_subty(tvars, subject_ref, subject_ty, test_ty)
            } else {
                InterpretedPred::Dynamic(test_ty.to_ty().into_ty_ref(), subject_ref.clone())
            }
        }
        is_a::Result::No => InterpretedPred::Static(false),
    }
}

#[cfg(test)]
mod test {
    use super::*;

    fn poly_for_str(datum_str: &str) -> ty::Poly {
        use crate::hir;
        hir::poly_for_str(datum_str)
    }

    fn assert_result(result: &InterpretedPred<ty::Poly>, subject_str: &str, test_ty: TestTy) {
        let subject_poly = poly_for_str(subject_str);

        assert_eq!(
            *result,
            interpret_ty_ref(&ty::TVars::new(), &subject_poly, test_ty)
        );
    }

    fn assert_static_false(subject_str: &str, test_ty: TestTy) {
        assert_result(&InterpretedPred::Static(false), subject_str, test_ty);
    }

    fn assert_static_true(subject_str: &str, test_ty: TestTy) {
        assert_result(&InterpretedPred::Static(true), subject_str, test_ty);
    }

    fn assert_dynamic((true_str, false_str): (&str, &str), subject_str: &str, test_ty: TestTy) {
        let expected = InterpretedPred::Dynamic(poly_for_str(true_str), poly_for_str(false_str));
        assert_result(&expected, subject_str, test_ty)
    }

    #[test]
    fn static_results() {
        assert_static_true("true", TestTy::Bool);
        assert_static_false("true", TestTy::Char)
    }

    #[test]
    fn any_type() {
        assert_dynamic(("(Listof Any)", "Any"), "Any", TestTy::List);
    }

    #[test]
    fn simple_unions() {
        assert_static_true("(RawU 'foo 'bar)", TestTy::Sym);
        assert_dynamic(("Sym", "Str"), "(RawU Sym Str)", TestTy::Sym);
        assert_dynamic(("()", "(List Any Any ...)"), "(Listof Any)", TestTy::Nil);
    }
}
