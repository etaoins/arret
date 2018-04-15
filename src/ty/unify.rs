use std::result;
use std::iter;

use ty;

#[derive(Debug, PartialEq)]
pub enum UnifiedTy<S>
where
    S: ty::TyRef,
{
    /// The types are disjoint but can be distinguished at runtime
    ///
    /// An example would be Str and Sym. These can be safely placed in a Union to create a new
    /// type.
    Disjoint,

    /// The types can be unified in to a single non-union type
    ///
    /// A trivial example would be Sym and 'foo because of their subtype relationship. More complex
    /// per-type logic exists that is aware of the runtime's type checking capabilities.
    Merged(S),
}

#[derive(Debug, PartialEq)]
pub enum Error<S>
where
    S: ty::TyRef,
{
    /// The types cannot be distinguished at runtime due to type erasure
    ///
    /// An example would be (Setof Str) and (Setof Sym) and most function types. This is also
    /// commonly encountered when one of the types is a polymorphic variable as it's difficult to
    /// guarantee all possible subtypes of the variable's bound can be distinguished. As a result
    /// the union of two unbounded polymorphic types will always conflict.
    ///
    /// The conflicting types are returned as they may have been nested in to the passed types.
    Erased(S, S),

    /// The types can be distinguished at runtime but do not produce a "natural" type when unified.
    ///
    /// Natural types are defined as types that are likely intended by the developer when we infer
    /// types on their behalf. For example `(Listof Int)` or `(Bool)` are natural types while
    /// `(Listof (U Int Symbol))` and `(U Bool (-> Int String))` would be considered unnatural.
    /// This is a purely subjective judgement that's intended to produce type errors when we would
    /// infer a potentially unintended type. Developers can specifically opt-in to unnatural types
    /// by providing type annotations.
    Unnatural(S, S),
}

pub type Result<T, S> = result::Result<T, Error<S>>;

trait UnifyCtx<S>
where
    S: ty::TyRef,
{
    fn allow_unnatural(&self) -> bool;
    fn unify_ref(&self, &S, &S) -> Result<UnifiedTy<S>, S>;

    fn unify_into_ty_ref(&self, ty_ref1: &S, ty_ref2: &S) -> Result<S, S> {
        match self.unify_ref(ty_ref1, ty_ref2)? {
            UnifiedTy::Merged(ty_ref) => Ok(ty_ref),
            UnifiedTy::Disjoint => Ok(S::from_ty(ty::Ty::Union(vec![
                ty_ref1.clone(),
                ty_ref2.clone(),
            ]))),
        }
    }

    /// Unifies two iterators in to a new type
    ///
    /// It is assumed `existing_members` refers to the members of an already unified union. This is
    /// important as they are not re-unified to avoid triggering an unnatural type error for an
    /// existing unnatural union.
    ///
    /// `existing_union` is used for error reporting when generating an unnatural union. If it's
    /// None then unnatural unions will be allowed.
    fn unify_ref_iters<'a, I, J>(
        &self,
        existing_union: Option<&S>,
        existing_members: I,
        new_members: J,
    ) -> Result<S, S>
    where
        S: 'a,
        I: Iterator<Item = &'a S>,
        J: Iterator<Item = &'a S>,
    {
        // Start with the members of one of iterators
        // It's important we don't attempt to re-unify them in case it's an unnatural union and
        // allow_unnatural=false. We only want to avoid *creating* unnatural types.
        let mut output_members: Vec<S> = existing_members.cloned().collect();

        'outer: for new_member in new_members {
            let mut found_natural_disjoint = false;

            for output_member in &mut output_members {
                match self.unify_ref(output_member, new_member) {
                    Ok(UnifiedTy::Merged(merged_member)) => {
                        // Found a member to merge with!
                        *output_member = merged_member;
                        continue 'outer;
                    }
                    Ok(UnifiedTy::Disjoint) => {
                        // We are disjoint. If we're disjoint with all members we'll be added to the
                        // union at the bottom of 'outer
                        found_natural_disjoint = true;
                    }
                    Err(Error::Unnatural(_, _)) => {
                        // We can ignore an unnatural disjoint as long as we're naturally disjoint with
                        // at least one member. This allow us to unify with an existing unnatural
                        // union.
                    }
                    Err(erased @ Error::Erased(_, _)) => {
                        // We can't be distinguished from another union member at runtime
                        return Err(erased);
                    }
                }
            }

            if !found_natural_disjoint && !self.allow_unnatural() {
                if let Some(existing_union) = existing_union {
                    return Err(Error::Unnatural(existing_union.clone(), new_member.clone()));
                }
            }

            output_members.push(new_member.clone());
        }

        if output_members.len() == 1 {
            Ok(output_members.pop().unwrap())
        } else {
            Ok(S::from_ty(ty::Ty::Union(output_members)))
        }
    }

    /// Unifies two types under the assumption that they are not subtypes
    fn non_subty_unify(
        &self,
        ref1: &S,
        ty1: &ty::Ty<S>,
        ref2: &S,
        ty2: &ty::Ty<S>,
    ) -> Result<UnifiedTy<S>, S> {
        match (ty1, ty2) {
            (&ty::Ty::LitBool(true), &ty::Ty::LitBool(false))
            | (&ty::Ty::LitBool(false), &ty::Ty::LitBool(true)) => {
                Ok(UnifiedTy::Merged(S::from_ty(ty::Ty::Bool)))
            }
            (&ty::Ty::LitSym(_), &ty::Ty::LitSym(_)) => {
                // Always consider literal symbol unions to be natural. They are frequently used as
                // ad-hoc enums and having to annotate them would add friction. This is the only
                // natural type of union.
                Ok(UnifiedTy::Disjoint)
            }
            (&ty::Ty::Set(ref ty_ref1), &ty::Ty::Set(ref ty_ref2)) => {
                let unified_ty_ref = self.unify_into_ty_ref(&ty_ref1, &ty_ref2)?;

                Ok(UnifiedTy::Merged(S::from_ty(ty::Ty::Set(Box::new(
                    unified_ty_ref,
                )))))
            }
            (
                &ty::Ty::Hash(ref key_ref1, ref val_ref1),
                &ty::Ty::Hash(ref key_ref2, ref val_ref2),
            ) => {
                let unified_key_ref = self.unify_into_ty_ref(&key_ref1, &key_ref2)?;
                let unified_val_ref = self.unify_into_ty_ref(&val_ref1, &val_ref2)?;

                Ok(UnifiedTy::Merged(S::from_ty(ty::Ty::Hash(
                    Box::new(unified_key_ref),
                    Box::new(unified_val_ref),
                ))))
            }
            (&ty::Ty::Vec(_, _), &ty::Ty::Vec(_, _)) => {
                // TODO: All sorts of logic here
                Err(Error::Erased(ref1.clone(), ref2.clone()))
            }
            (&ty::Ty::List(_, _), &ty::Ty::List(_, _)) => {
                // TODO: All sorts of logic here
                Err(Error::Erased(ref1.clone(), ref2.clone()))
            }
            (&ty::Ty::Fun(_), &ty::Ty::Fun(_)) => {
                // TODO: We can't intersect the parameter lists due to lack of intersect logic
                Err(Error::Erased(ref1.clone(), ref2.clone()))
            }
            (&ty::Ty::Union(ref members1), &ty::Ty::Union(ref members2)) => {
                let new_union = self.unify_ref_iters(None, members1.iter(), members2.iter())?;
                Ok(UnifiedTy::Merged(new_union))
            }
            (&ty::Ty::Union(ref members1), _) => {
                let new_union =
                    self.unify_ref_iters(Some(ref1), members1.iter(), iter::once(ref2))?;
                Ok(UnifiedTy::Merged(new_union))
            }
            (_, &ty::Ty::Union(ref members2)) => {
                let new_union =
                    self.unify_ref_iters(Some(ref2), members2.iter(), iter::once(ref1))?;
                Ok(UnifiedTy::Merged(new_union))
            }
            _ => {
                if self.allow_unnatural() {
                    Ok(UnifiedTy::Disjoint)
                } else {
                    Err(Error::Unnatural(ref1.clone(), ref2.clone()))
                }
            }
        }
    }
}

struct PolyUnifyCtx<'a> {
    pvars: &'a [ty::PVar],
    allow_unnatural: bool,
}

impl<'a> UnifyCtx<ty::Poly> for PolyUnifyCtx<'a> {
    fn allow_unnatural(&self) -> bool {
        self.allow_unnatural
    }

    fn unify_ref(
        &self,
        poly1: &ty::Poly,
        poly2: &ty::Poly,
    ) -> Result<UnifiedTy<ty::Poly>, ty::Poly> {
        if ty::is_a::poly_is_a(self.pvars, poly1, poly2).to_bool() {
            return Ok(UnifiedTy::Merged(poly2.clone()));
        } else if ty::is_a::poly_is_a(self.pvars, poly2, poly1).to_bool() {
            return Ok(UnifiedTy::Merged(poly1.clone()));
        }

        // Determine if we're dealing with fixed types or polymorphic bounds
        let resolved1 = ty::resolve::resolve_poly_ty(self.pvars, poly1);
        let resolved2 = ty::resolve::resolve_poly_ty(self.pvars, poly2);

        match (&resolved1, &resolved2) {
            (&ty::resolve::Result::Fixed(ty1), &ty::resolve::Result::Fixed(ty2)) => {
                // We can invoke full unification logic if we have fixed types
                self.non_subty_unify(poly1, ty1, poly2, ty2)
            }
            _ => {
                // TODO: We are probably too eager to conflict here. If one type is a literal
                // we should be able to unify. Lists of different lengths should be allowed as
                // they're disjoint and testable at runtime - however two function types can be
                // disjoint and *not* testable so they should still conflict.
                Err(Error::Erased(poly1.clone(), poly2.clone()))
            }
        }
    }
}

pub fn poly_unify<'a>(
    pvars: &'a [ty::PVar],
    poly1: &'a ty::Poly,
    poly2: &'a ty::Poly,
    allow_unnatural: bool,
) -> Result<UnifiedTy<ty::Poly>, ty::Poly> {
    let ctx = PolyUnifyCtx {
        pvars,
        allow_unnatural,
    };

    ctx.unify_ref(poly1, poly2)
}

pub fn poly_unify_iter<'a, I>(pvars: &'a [ty::PVar], members: I) -> Result<ty::Poly, ty::Poly>
where
    I: Iterator<Item = &'a ty::Poly>,
{
    let ctx = PolyUnifyCtx {
        pvars,
        allow_unnatural: true,
    };

    ctx.unify_ref_iters(None, iter::empty(), members)
}

#[cfg(test)]
mod test {
    use super::*;

    fn poly_for_str(datum_str: &str) -> ty::Poly {
        use hir;
        hir::poly_for_str(datum_str).unwrap()
    }

    fn assert_unnatural_disjoint(ty_str1: &str, ty_str2: &str) {
        let poly1 = poly_for_str(ty_str1);
        let poly2 = poly_for_str(ty_str2);

        assert_eq!(
            UnifiedTy::Disjoint,
            poly_unify(&[], &poly1, &poly2, true).unwrap()
        );

        let unnatural_result = poly_unify(&[], &poly1, &poly2, false).unwrap_err();
        if let Error::Unnatural(_, _) = unnatural_result {
        } else {
            panic!("Union unexpectedly natural!");
        }
    }

    fn assert_natural_disjoint(ty_str1: &str, ty_str2: &str) {
        let poly1 = poly_for_str(ty_str1);
        let poly2 = poly_for_str(ty_str2);

        for allow_unnatural in &[true, false] {
            assert_eq!(
                UnifiedTy::Disjoint,
                poly_unify(&[], &poly1, &poly2, *allow_unnatural).unwrap()
            );
        }
    }

    fn assert_unnatural_merged(expected_str: &str, ty_str1: &str, ty_str2: &str) {
        let expected = poly_for_str(expected_str);
        let poly1 = poly_for_str(ty_str1);
        let poly2 = poly_for_str(ty_str2);

        assert_eq!(
            UnifiedTy::Merged(expected.clone()),
            poly_unify(&[], &poly1, &poly2, true).unwrap()
        );

        let unnatural_result = poly_unify(&[], &poly1, &poly2, false).unwrap_err();
        if let Error::Unnatural(_, _) = unnatural_result {
        } else {
            panic!("Union unexpectedly natural!");
        }
    }

    fn assert_natural_merged(expected_str: &str, ty_str1: &str, ty_str2: &str) {
        let expected = poly_for_str(expected_str);
        let poly1 = poly_for_str(ty_str1);
        let poly2 = poly_for_str(ty_str2);

        for allow_unnatural in &[true, false] {
            assert_eq!(
                UnifiedTy::Merged(expected.clone()),
                poly_unify(&[], &poly1, &poly2, *allow_unnatural).unwrap()
            );
        }
    }

    fn assert_erased(ty_str1: &str, ty_str2: &str) {
        let poly1 = poly_for_str(ty_str1);
        let poly2 = poly_for_str(ty_str2);

        for allow_unnatural in &[true, false] {
            let erased_result = poly_unify(&[], &poly1, &poly2, *allow_unnatural).unwrap_err();
            if let Error::Erased(_, _) = erased_result {
            } else {
                panic!("Union unexpectedly unerased!");
            }
        }
    }

    fn assert_merged_iter(expected_str: &str, ty_strs: &[&str]) {
        let expected = poly_for_str(expected_str);
        let polys: Vec<ty::Poly> = ty_strs.iter().map(|&s| poly_for_str(s)).collect();

        assert_eq!(expected, poly_unify_iter(&[], polys.iter()).unwrap());
    }

    fn assert_erased_iter(ty_strs: &[&str]) {
        let polys: Vec<ty::Poly> = ty_strs.iter().map(|&s| poly_for_str(s)).collect();

        let erased_result = poly_unify_iter(&[], polys.iter()).unwrap_err();
        if let Error::Erased(_, _) = erased_result {
        } else {
            panic!("Union unexpectedly unerased!");
        }
    }

    #[test]
    fn disjoint_types() {
        assert_unnatural_disjoint("String", "Symbol");
    }

    #[test]
    fn two_sym_types() {
        assert_natural_disjoint("'foo", "'bar");
    }

    #[test]
    fn literal_sym_and_any_sym() {
        assert_natural_merged("Symbol", "Symbol", "'foo");
    }

    #[test]
    fn two_bool_types() {
        assert_natural_merged("Bool", "true", "false");
    }

    #[test]
    fn fun_types() {
        assert_erased("(-> Int)", "(-> Float)");
    }

    #[test]
    fn set_types() {
        assert_natural_merged("(Setof Bool)", "(Setof true)", "(Setof false)");
        assert_unnatural_merged(
            "(Setof (RawU String Symbol))",
            "(Setof String)",
            "(Setof Symbol)",
        );
    }

    #[test]
    fn hash_types() {
        assert_natural_merged(
            "(Hash Bool (RawU 'bar 'foo))",
            "(Hash true 'bar)",
            "(Hash false 'foo)",
        );
    }

    #[test]
    fn union_types() {
        assert_natural_merged("(RawU 'foo 'bar 'baz)", "(RawU 'foo 'bar)", "'baz");
        assert_natural_merged("(RawU 'foo 'bar 'baz)", "'baz", "(RawU 'foo 'bar)");
        assert_natural_merged(
            "(RawU Bool (-> Int))",
            "(RawU true (-> Int))",
            "(RawU false (-> Int))",
        );
        assert_natural_merged(
            "(RawU Char Int String Symbol)",
            "(RawU Char Int)",
            "(RawU String Symbol)",
        );
        assert_erased("(RawU true (-> Float))", "(RawU true (-> Int))");
        assert_unnatural_merged("(RawU 'foo 'bar Bool)", "(RawU 'foo 'bar)", "Bool");
        assert_natural_merged("Symbol", "(RawU 'foo 'bar)", "Symbol");
        assert_natural_merged("Symbol", "(RawU)", "Symbol");
        assert_natural_merged("(RawU)", "(RawU)", "(RawU)");

        assert_natural_merged(
            "(RawU Char Int String Symbol)",
            "(RawU Char Int)",
            "(RawU String Symbol)",
        );
    }

    #[test]
    fn unify_iter() {
        assert_merged_iter("(RawU)", &[]);
        assert_merged_iter("Symbol", &["Symbol"]);
        assert_merged_iter("Bool", &["true", "false"]);
        assert_merged_iter(
            "(Setof (RawU String Symbol Int))",
            &["(Setof String)", "(Setof Symbol)", "(Setof Int)"],
        );

        assert_erased_iter(&["(-> String)", "(-> Symbol)"]);
        assert_erased_iter(&["(-> String)", "(RawU)", "(-> Symbol)"]);
    }
}
