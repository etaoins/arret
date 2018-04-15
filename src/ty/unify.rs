use std::result;
use std::iter;

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
}

enum UnifiedSeq<S>
where
    S: ty::TyRef,
{
    Disjoint,
    Merged(Vec<S>, Option<S>),
}

pub type Result<T, S> = result::Result<T, Error<S>>;

trait UnifyCtx<S>
where
    S: ty::TyRef,
{
    fn unify_ref(&self, &S, &S) -> Result<UnifiedTy<S>, S>;

    fn unify_seq<'a, I>(&self, mut iter1: I, mut iter2: I) -> Result<UnifiedSeq<S>, S>
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

            let merged_next = match self.unify_ref(&next1, &next2)? {
                UnifiedTy::Merged(ty_ref) => ty_ref,
                UnifiedTy::Disjoint => {
                    // Within our fixed types we're willing to build a runtime type check of the
                    // member. This means we can consider the seq disjoint.
                    return Ok(UnifiedSeq::Disjoint);
                }
            };

            merged_fixed.push(merged_next);
        }

        // TODO: Building a temporary here sucks
        let mut rest_types: Vec<&S> = vec![];
        while iter1.fixed_len() > 0 {
            rest_types.push(iter1.next().unwrap());
        }
        if iter1.is_infinite() {
            rest_types.push(iter1.next().unwrap());
        }
        while iter2.fixed_len() > 0 {
            rest_types.push(iter2.next().unwrap());
        }
        if iter2.is_infinite() {
            rest_types.push(iter2.next().unwrap());
        }

        let merged_rest = if rest_types.is_empty() {
            None
        } else {
            Some(self.unify_ref_iters(iter::empty(), rest_types.into_iter())?)
        };

        Ok(UnifiedSeq::Merged(merged_fixed, merged_rest))
    }

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
    /// important as they are not re-unified as a performance optimisation.
    fn unify_ref_iters<'a, I, J>(&self, existing_members: I, new_members: J) -> Result<S, S>
    where
        S: 'a,
        I: Iterator<Item = &'a S>,
        J: Iterator<Item = &'a S>,
    {
        // Start with the members of existing union
        let mut output_members: Vec<S> = existing_members.cloned().collect();

        'outer: for new_member in new_members {
            for output_member in &mut output_members {
                match self.unify_ref(output_member, new_member) {
                    Ok(UnifiedTy::Merged(merged_member)) => {
                        // Found a member to merge with!
                        *output_member = merged_member;
                        continue 'outer;
                    }
                    Ok(UnifiedTy::Disjoint) => {
                        // If we're disjoint with all output types we'll push this type once we
                        // exit the loop
                    }
                    Err(erased @ Error::Erased(_, _)) => {
                        // We can't be distinguished from another union member at runtime
                        return Err(erased);
                    }
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
                if fun1.params() == fun2.params() {
                    let unified_impure = fun1.impure() || fun2.impure();
                    let unified_ret = self.unify_into_ty_ref(fun1.ret(), fun2.ret())?;

                    Ok(UnifiedTy::Merged(S::from_ty(ty::Ty::new_fun(
                        unified_impure,
                        fun1.params().clone(),
                        unified_ret,
                    ))))
                } else {
                    // TODO: We can't intersect the parameter lists due to lack of intersect logic
                    Err(Error::Erased(ref1.clone(), ref2.clone()))
                }
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

struct PolyUnifyCtx<'a> {
    pvars: &'a [ty::PVar],
}

impl<'a> UnifyCtx<ty::Poly> for PolyUnifyCtx<'a> {
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
) -> Result<UnifiedTy<ty::Poly>, ty::Poly> {
    let ctx = PolyUnifyCtx { pvars };
    ctx.unify_ref(poly1, poly2)
}

pub fn poly_unify_iter<'a, I>(pvars: &'a [ty::PVar], members: I) -> Result<ty::Poly, ty::Poly>
where
    I: Iterator<Item = &'a ty::Poly>,
{
    let ctx = PolyUnifyCtx { pvars };
    ctx.unify_ref_iters(iter::empty(), members)
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

        assert_eq!(
            UnifiedTy::Merged(expected.clone()),
            poly_unify(&[], &poly1, &poly2).unwrap()
        );
    }

    fn assert_erased(ty_str1: &str, ty_str2: &str) {
        let poly1 = poly_for_str(ty_str1);
        let poly2 = poly_for_str(ty_str2);

        poly_unify(&[], &poly1, &poly2).unwrap_err();
    }

    fn assert_merged_iter(expected_str: &str, ty_strs: &[&str]) {
        let expected = poly_for_str(expected_str);
        let polys: Vec<ty::Poly> = ty_strs.iter().map(|&s| poly_for_str(s)).collect();

        assert_eq!(expected, poly_unify_iter(&[], polys.iter()).unwrap());
    }

    fn assert_erased_iter(ty_strs: &[&str]) {
        let polys: Vec<ty::Poly> = ty_strs.iter().map(|&s| poly_for_str(s)).collect();
        poly_unify_iter(&[], polys.iter()).unwrap_err();
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
        assert_erased("(-> Float Int)", "(-> Int Float)");
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
    fn hash_types() {
        assert_merged(
            "(Hash Bool (RawU 'bar 'foo))",
            "(Hash true 'bar)",
            "(Hash false 'foo)",
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
        assert_erased("(RawU true (-> Int Float))", "(RawU true (-> Float Int))");
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

        assert_erased_iter(&["(-> String Symbol)", "(-> Symbol String)"]);
        assert_erased_iter(&["(-> String Symbol)", "(RawU)", "(-> Symbol String)"]);
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
