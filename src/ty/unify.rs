use std::result::Result;
use std::iter;
use std::borrow::Borrow;

use ty;
use ty::seq_ty_iter::{ListTyIterator, RevVecTyIterator, SeqTyIterator};

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

enum UnifiedSeq<S>
where
    S: ty::TyRef,
{
    Disjoint,
    Merged(Vec<S>, Option<S>),
}

trait UnifyCtx<S, E>
where
    S: ty::TyRef,
{
    fn unify_ref(&self, &S, &S) -> Result<UnifiedTy<S>, E>;
    fn intersect_ref(&self, &S, &S) -> ty::intersect::IntersectedTy<S>;

    fn unify_seq<'a, I>(&self, mut iter1: I, mut iter2: I) -> Result<UnifiedSeq<S>, E>
    where
        S: 'a,
        I: SeqTyIterator<'a, S>,
    {
        let range1 = iter1.size_range();
        let range2 = iter2.size_range();

        if range2.start > range1.end || range2.end < range1.start {
            // The sizes are completely disjoint; we can perform a simple length check at runtime.
            // It's important we bail out here so we don't hit a type erasure error below.
            return Ok(UnifiedSeq::Disjoint);
        }

        let mut merged_fixed: Vec<S> = vec![];
        while iter1.fixed_len() > 0 && iter2.fixed_len() > 0 {
            let next1 = iter1.next().unwrap();
            let next2 = iter2.next().unwrap();

            let merged_next = match self.unify_ref(next1, next2)? {
                UnifiedTy::Merged(ty_ref) => ty_ref,
                UnifiedTy::Disjoint => {
                    // Within our fixed types we're willing to build a runtime type check of the
                    // member. This means we can consider the seq disjoint.
                    return Ok(UnifiedSeq::Disjoint);
                }
            };

            merged_fixed.push(merged_next);
        }

        let mut rest_members: Vec<S> = vec![];
        // TODO: We can almost get away with checking `rest_members.is_empty()`. However, that
        // breaks if one of the list has a rest of `(U)`. It might actually make sense to represent
        // rest lists as `(U)` instead of None but that probably complicates as much as it
        // simplifies.
        let mut have_rest = false;

        macro_rules! consume_iter_rest {
            ( $iter:expr ) => {
                while $iter.fixed_len() > 0 {
                    have_rest = true;
                    self.unify_ref_into_vec(&mut rest_members, $iter.next().unwrap())?;
                }
                if $iter.is_infinite() {
                    have_rest = true;
                    self.unify_ref_into_vec(&mut rest_members, $iter.next().unwrap())?;
                }
            }
        }

        consume_iter_rest!(iter1);
        consume_iter_rest!(iter2);

        let merged_rest = if have_rest {
            if rest_members.len() == 1 {
                Some(rest_members.pop().unwrap())
            } else {
                Some(S::from_ty(ty::Ty::Union(rest_members)))
            }
        } else {
            None
        };

        Ok(UnifiedSeq::Merged(merged_fixed, merged_rest))
    }

    fn unify_into_ty_ref(&self, ty_ref1: &S, ty_ref2: &S) -> Result<S, E> {
        match self.unify_ref(ty_ref1, ty_ref2)? {
            UnifiedTy::Merged(ty_ref) => Ok(ty_ref),
            UnifiedTy::Disjoint => Ok(S::from_ty(ty::Ty::Union(vec![
                ty_ref1.clone(),
                ty_ref2.clone(),
            ]))),
        }
    }

    /// Unifies a member in to an existing vector of members
    ///
    /// It is assumed `output_members` refers to members of an already unified union.
    fn unify_ref_into_vec(&self, output_members: &mut Vec<S>, new_member: &S) -> Result<(), E> {
        for output_member in output_members.iter_mut() {
            match self.unify_ref(output_member, new_member)? {
                UnifiedTy::Merged(merged_member) => {
                    // Found a member to merge with!
                    *output_member = merged_member;
                    return Ok(());
                }
                UnifiedTy::Disjoint => {
                    // If we're disjoint with all output types we'll push this type once we
                    // exit the loop
                }
            }
        }

        output_members.push(new_member.clone());
        Ok(())
    }

    /// Unifies two iterators in to a new type
    ///
    /// It is assumed `existing_members` refers to the members of an already unified union. This is
    /// important as they are not re-unified as a performance optimisation.
    fn unify_ref_iters<'a, I, J, K>(&self, existing_members: I, new_members: J) -> Result<S, E>
    where
        S: 'a,
        I: Iterator<Item = &'a S>,
        J: IntoIterator<Item = K>,
        K: Borrow<S>,
    {
        // Start with the members of existing union
        let mut output_members: Vec<S> = existing_members.cloned().collect();

        for new_member in new_members {
            self.unify_ref_into_vec(&mut output_members, new_member.borrow())?;
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
    ) -> Result<UnifiedTy<S>, E> {
        match (ty1, ty2) {
            (&ty::Ty::LitBool(true), &ty::Ty::LitBool(false))
            | (&ty::Ty::LitBool(false), &ty::Ty::LitBool(true)) => {
                Ok(UnifiedTy::Merged(S::from_ty(ty::Ty::Bool)))
            }
            (&ty::Ty::Set(ref ty_ref1), &ty::Ty::Set(ref ty_ref2)) => {
                let unified_ty_ref = self.unify_into_ty_ref(&ty_ref1, &ty_ref2)?;

                Ok(UnifiedTy::Merged(S::from_ty(ty::Ty::Set(Box::new(
                    unified_ty_ref,
                )))))
            }
            (
                &ty::Ty::Map(ref key_ref1, ref val_ref1),
                &ty::Ty::Map(ref key_ref2, ref val_ref2),
            ) => {
                let unified_key_ref = self.unify_into_ty_ref(&key_ref1, &key_ref2)?;
                let unified_val_ref = self.unify_into_ty_ref(&val_ref1, &val_ref2)?;

                Ok(UnifiedTy::Merged(S::from_ty(ty::Ty::Map(
                    Box::new(unified_key_ref),
                    Box::new(unified_val_ref),
                ))))
            }
            (&ty::Ty::Vec(ref begin1, ref fixed1), &ty::Ty::Vec(ref begin2, ref fixed2)) => {
                self.unify_seq(
                    RevVecTyIterator::new(begin1, fixed1),
                    RevVecTyIterator::new(begin2, fixed2),
                ).map(|result| match result {
                    UnifiedSeq::Disjoint => UnifiedTy::Disjoint,
                    UnifiedSeq::Merged(mut fixed, begin) => {
                        // We iterator over our types in reverse so we need to flip them back here
                        fixed.reverse();
                        UnifiedTy::Merged(S::from_ty(ty::Ty::Vec(begin.map(Box::new), fixed)))
                    }
                })
            }
            (&ty::Ty::List(ref fixed1, ref rest1), &ty::Ty::List(ref fixed2, ref rest2)) => {
                self.unify_seq(
                    ListTyIterator::new(fixed1, rest1),
                    ListTyIterator::new(fixed2, rest2),
                ).map(|result| match result {
                    UnifiedSeq::Disjoint => UnifiedTy::Disjoint,
                    UnifiedSeq::Merged(fixed, rest) => {
                        UnifiedTy::Merged(S::from_ty(ty::Ty::List(fixed, rest.map(Box::new))))
                    }
                })
            }
            (&ty::Ty::Fun(ref fun1), &ty::Ty::Fun(ref fun2)) => {
                let unified_impure = fun1.impure() || fun2.impure();
                let unified_params = self.intersect_ref(fun1.params(), fun2.params())
                    .into_ty_ref();
                let unified_ret = self.unify_into_ty_ref(fun1.ret(), fun2.ret())?;

                Ok(UnifiedTy::Merged(S::from_ty(ty::Ty::new_fun(
                    unified_impure,
                    unified_params,
                    unified_ret,
                ))))
            }
            (&ty::Ty::Union(ref members1), &ty::Ty::Union(ref members2)) => {
                let new_union = self.unify_ref_iters(members1.iter(), members2.iter())?;
                Ok(UnifiedTy::Merged(new_union))
            }
            (&ty::Ty::Union(ref members1), _) => {
                let new_union = self.unify_ref_iters(members1.iter(), iter::once(ref2))?;
                Ok(UnifiedTy::Merged(new_union))
            }
            (_, &ty::Ty::Union(ref members2)) => {
                let new_union = self.unify_ref_iters(members2.iter(), iter::once(ref1))?;
                Ok(UnifiedTy::Merged(new_union))
            }
            _ => Ok(UnifiedTy::Disjoint),
        }
    }
}

#[derive(Debug, PartialEq)]
pub enum PolyError {
    /// A polymorphic type does not have strict enough bounds to be unified with another type
    ///
    /// Our unification logic is careful to merge types that cannot be distinguished by our runtime
    /// type system. This is to preserve the invariant that any pattern match or type predicate
    /// that returns a definite result for each union member can be performed at runtime. For
    /// example:
    ///
    /// ```
    /// ; These are allowed in the same union as we can perform length checks at runtime.
    /// (U (List Int) (List Int Int))
    /// ; These are merged as we won't check the types of optional list members
    /// (U (List Symbol ...) (List String ...)) => (List (U Symbol String) ...)
    /// ```
    ///
    /// This runs in to issues with polymorphic types as it's hard to prove all of their possible
    /// subtypes don't need to be merged with another union member. This is best effort logic to
    /// allow this in some cases (e.g. bounds with no subtypes) but in the general case it will
    /// return a `PolyMember` error.
    ///
    /// The conflicting types are returned as they may have been nested in to the passed types.
    PolyMember(ty::Poly, ty::Poly),
}

struct PolyUnifyCtx<'a> {
    pvars: &'a [ty::PVar],
}

impl<'a> UnifyCtx<ty::Poly, PolyError> for PolyUnifyCtx<'a> {
    fn unify_ref(
        &self,
        poly1: &ty::Poly,
        poly2: &ty::Poly,
    ) -> Result<UnifiedTy<ty::Poly>, PolyError> {
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
            _ => Err(PolyError::PolyMember(poly1.clone(), poly2.clone())),
        }
    }

    fn intersect_ref(
        &self,
        poly1: &ty::Poly,
        poly2: &ty::Poly,
    ) -> ty::intersect::IntersectedTy<ty::Poly> {
        ty::intersect::poly_intersect(self.pvars, poly1, poly2)
    }
}

pub fn poly_unify<'a>(
    pvars: &'a [ty::PVar],
    poly1: &'a ty::Poly,
    poly2: &'a ty::Poly,
) -> Result<UnifiedTy<ty::Poly>, PolyError> {
    let ctx = PolyUnifyCtx { pvars };
    ctx.unify_ref(poly1, poly2)
}

pub fn poly_unify_iter<I, J>(pvars: &[ty::PVar], members: I) -> Result<ty::Poly, PolyError>
where
    I: IntoIterator<Item = J>,
    J: Borrow<ty::Poly>,
{
    let ctx = PolyUnifyCtx { pvars };
    ctx.unify_ref_iters(iter::empty(), members.into_iter())
}

// TODO: Replace with bang type once it's stable
#[derive(Debug, PartialEq)]
pub enum MonoError {}

struct MonoUnifyCtx {}

impl<'a> UnifyCtx<ty::Mono, MonoError> for MonoUnifyCtx {
    fn unify_ref(
        &self,
        mono1: &ty::Mono,
        mono2: &ty::Mono,
    ) -> Result<UnifiedTy<ty::Mono>, MonoError> {
        if ty::is_a::mono_is_a(mono1, mono2).to_bool() {
            return Ok(UnifiedTy::Merged(mono2.clone()));
        } else if ty::is_a::mono_is_a(mono2, mono1).to_bool() {
            return Ok(UnifiedTy::Merged(mono1.clone()));
        }

        self.non_subty_unify(mono1, mono1.as_ty(), mono2, mono2.as_ty())
    }

    fn intersect_ref(
        &self,
        mono1: &ty::Mono,
        mono2: &ty::Mono,
    ) -> ty::intersect::IntersectedTy<ty::Mono> {
        ty::intersect::mono_intersect(mono1, mono2)
    }
}

pub fn mono_unify<'a>(
    mono1: &'a ty::Mono,
    mono2: &'a ty::Mono,
) -> Result<UnifiedTy<ty::Mono>, MonoError> {
    let ctx = MonoUnifyCtx {};
    ctx.unify_ref(mono1, mono2)
}

pub fn mono_unify_iter<I, J>(members: I) -> Result<ty::Mono, MonoError>
where
    I: IntoIterator<Item = J>,
    J: Borrow<ty::Mono>,
{
    let ctx = MonoUnifyCtx {};
    ctx.unify_ref_iters(iter::empty(), members.into_iter())
}

#[cfg(test)]
mod test {
    use super::*;

    fn poly_for_str(datum_str: &str) -> ty::Poly {
        use hir;
        hir::poly_for_str(datum_str).unwrap()
    }

    fn assert_disjoint(ty_str1: &str, ty_str2: &str) {
        let poly1 = poly_for_str(ty_str1);
        let poly2 = poly_for_str(ty_str2);

        assert_eq!(
            UnifiedTy::Disjoint,
            poly_unify(&[], &poly1, &poly2).unwrap()
        );
    }

    fn assert_merged(expected_str: &str, ty_str1: &str, ty_str2: &str) {
        let expected = poly_for_str(expected_str);
        let poly1 = poly_for_str(ty_str1);
        let poly2 = poly_for_str(ty_str2);

        // This is the basic invariant we're testing - each of our input types satsifies the merged
        // type
        assert_eq!(
            ty::is_a::Result::Yes,
            ty::is_a::poly_is_a(&[], &poly1, &expected)
        );
        assert_eq!(
            ty::is_a::Result::Yes,
            ty::is_a::poly_is_a(&[], &poly2, &expected)
        );

        assert_eq!(
            UnifiedTy::Merged(expected),
            poly_unify(&[], &poly1, &poly2).unwrap()
        );
    }

    fn assert_merged_iter(expected_str: &str, ty_strs: &[&str]) {
        let expected = poly_for_str(expected_str);
        let polys = ty_strs.iter().map(|&s| poly_for_str(s));

        assert_eq!(expected, poly_unify_iter(&[], polys).unwrap());
    }

    #[test]
    fn disjoint_types() {
        assert_disjoint("String", "Symbol");
    }

    #[test]
    fn two_sym_types() {
        assert_disjoint("'foo", "'bar");
    }

    #[test]
    fn literal_sym_and_any_sym() {
        assert_merged("Symbol", "Symbol", "'foo");
    }

    #[test]
    fn two_bool_types() {
        assert_merged("Bool", "true", "false");
    }

    #[test]
    fn fun_types() {
        // Parameters are contravariant and Float/Int are disjoint
        assert_merged(
            "((RawU) -> (RawU Int Float))",
            "(Float -> Int)",
            "(Int -> Float)",
        );

        assert_merged(
            "(-> true (RawU Int Float))",
            "(-> Bool Int)",
            "(-> true Float)",
        );
        assert_merged("(->! Int)", "(-> Int)", "(->! Int)");
        assert_merged("(->! Bool)", "(-> true)", "(->! false)");
    }

    #[test]
    fn set_types() {
        assert_merged("(Setof Bool)", "(Setof true)", "(Setof false)");
        assert_merged(
            "(Setof (RawU String Symbol))",
            "(Setof String)",
            "(Setof Symbol)",
        );
    }

    #[test]
    fn map_types() {
        assert_merged(
            "(Map Bool (RawU 'bar 'foo))",
            "(Map true 'bar)",
            "(Map false 'foo)",
        );
    }

    #[test]
    fn union_types() {
        assert_merged("(RawU 'foo 'bar 'baz)", "(RawU 'foo 'bar)", "'baz");
        assert_merged("(RawU 'foo 'bar 'baz)", "'baz", "(RawU 'foo 'bar)");
        assert_merged(
            "(RawU Bool (-> Int))",
            "(RawU true (-> Int))",
            "(RawU false (-> Int))",
        );
        assert_merged(
            "(RawU Char Int String Symbol)",
            "(RawU Char Int)",
            "(RawU String Symbol)",
        );
        assert_merged(
            "(RawU true ((RawU) -> (RawU Float Int)))",
            "(RawU true (Int -> Float))",
            "(RawU true (Float -> Int))",
        );
        assert_merged("(RawU 'foo 'bar Bool)", "(RawU 'foo 'bar)", "Bool");
        assert_merged("Symbol", "(RawU 'foo 'bar)", "Symbol");
        assert_merged("Symbol", "(RawU)", "Symbol");
        assert_merged("(RawU)", "(RawU)", "(RawU)");

        assert_merged(
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

        assert_merged_iter(
            "((RawU) -> (RawU Symbol String))",
            &["(String -> Symbol)", "(RawU)", "(Symbol -> String)"],
        );
    }

    #[test]
    fn list_types() {
        assert_merged("(Listof Any)", "(List Any)", "(Listof Any)");
        assert_disjoint("(List Any)", "(List Any Any)");
        assert_disjoint("(List Symbol)", "(List String)");
        assert_disjoint("(List String)", "(List String String String ...)");
        assert_merged(
            "(List Int (RawU Symbol Float String) ...)",
            "(List Int Symbol ...)",
            "(List Int Float String ...)",
        );
    }

    #[test]
    fn vec_types() {
        assert_merged("(Vectorof Bool)", "(Vector true)", "(Vectorof false)");
        assert_disjoint(
            "(Vector 'foo ... Int Symbol)",
            "(Vector 'bar ... Int String)",
        );
        assert_merged(
            "(Vector (RawU 'foo 'bar) ... Int Symbol)",
            "(Vector 'foo ... Int Symbol)",
            "(Vector 'bar ... Int Symbol)",
        );
    }
}
