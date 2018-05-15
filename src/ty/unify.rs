use std::cmp;
use std::iter;
use std::result::Result;

use ty;
use ty::params_iter::ParamsIterator;
use ty::purity::PVarIds;
use ty::purity::Purity;
use ty::TVarIds;

#[derive(Debug, PartialEq)]
enum UnifiedTy<S>
where
    S: ty::TyRef,
{
    /// The types can be discerned at runtime
    ///
    /// An example would be Str and Sym. These can be safely placed in a Union to create a new
    /// type.
    Discerned,

    /// The types can be unified in to a single non-union type
    ///
    /// A trivial example would be Sym and 'foo because of their subtype relationship. More complex
    /// per-type logic exists, especially surrounding sequences.
    Merged(S),
}

trait UnifyCtx<S, E>
where
    S: ty::TyRef,
{
    fn unify_ty_refs(&self, &S, &S) -> Result<UnifiedTy<S>, E>;
    fn intersect_ty_refs(&self, &S, &S) -> Result<S, ty::intersect::Error>;
    fn unify_purity_refs(&self, &S::PRef, &S::PRef) -> Result<S::PRef, E>;

    fn unify_to_ty_ref(&self, ty_ref1: &S, ty_ref2: &S) -> Result<S, E> {
        match self.unify_ty_refs(ty_ref1, ty_ref2)? {
            UnifiedTy::Merged(ty_ref) => Ok(ty_ref),
            UnifiedTy::Discerned => {
                Ok(ty::Ty::Union(vec![ty_ref1.clone(), ty_ref2.clone()]).into_ref())
            }
        }
    }

    /// Unifies a uniform list with with a Cons
    fn unify_list_cons_refs(
        &self,
        list_ref: &S,
        car_ref: &S,
        cdr_ref: &S,
    ) -> Result<UnifiedTy<S>, E> {
        match self.unify_ty_refs(list_ref, cdr_ref)? {
            UnifiedTy::Discerned => Ok(UnifiedTy::Discerned),
            UnifiedTy::Merged(merged_tail) => self.unify_ty_refs(
                &merged_tail,
                &ty::Ty::Listof(Box::new(car_ref.clone())).into_ref(),
            ),
        }
    }

    /// Unifies a member in to an existing vector of members
    ///
    /// It is assumed `output_members` refers to members of an already unified union.
    fn unify_ref_into_vec(&self, output_members: &mut Vec<S>, new_member: S) -> Result<(), E> {
        for i in 0..output_members.len() {
            match self.unify_ty_refs(&output_members[i], &new_member)? {
                UnifiedTy::Merged(merged_member) => {
                    // Our merged type may now unify with one of the already processed members of
                    // the union. Remove the member we merged with and recurse using the merged
                    // member.
                    output_members.swap_remove(i);
                    return self.unify_ref_into_vec(output_members, merged_member);
                }
                UnifiedTy::Discerned => {}
            }
        }

        output_members.push(new_member);
        Ok(())
    }

    /// Unifies an iterators in to a new type
    ///
    /// `existing_members` are assumed to be the members of an existing union. If there is no
    /// existing union this must be empty. This is an optimisation to avoid processing the already
    /// unified members.
    fn unify_ref_iter<I>(&self, existing_members: Vec<S>, new_members: I) -> Result<S, E>
    where
        I: Iterator<Item = S>,
    {
        let mut output_members = existing_members;

        for new_member in new_members {
            self.unify_ref_into_vec(&mut output_members, new_member)?;
        }

        Ok(S::from_vec(output_members))
    }

    fn unify_top_fun(
        &self,
        top_fun1: &ty::TopFun<S>,
        top_fun2: &ty::TopFun<S>,
    ) -> Result<UnifiedTy<S>, E> {
        let unified_purity = self.unify_purity_refs(top_fun1.purity(), top_fun2.purity())?;
        let unified_ret = self.unify_to_ty_ref(top_fun1.ret(), top_fun2.ret())?;

        Ok(UnifiedTy::Merged(
            ty::TopFun::new(unified_purity, unified_ret).into_ref(),
        ))
    }

    fn unify_params(
        &self,
        params1: &ty::Params<S>,
        params2: &ty::Params<S>,
    ) -> Result<ty::Params<S>, ty::intersect::Error> {
        if params1.has_disjoint_arity(&params2) {
            return Err(ty::intersect::Error::Disjoint);
        }

        let mut iter1 = ParamsIterator::new(params1);
        let mut iter2 = ParamsIterator::new(params2);

        let mut merged_fixed: Vec<S> =
            Vec::with_capacity(cmp::max(iter1.fixed_len(), iter2.fixed_len()));

        while iter1.fixed_len() > 0 || iter2.fixed_len() > 0 {
            let next1 = iter1.next().unwrap();
            let next2 = iter2.next().unwrap();

            let merged_next = self.intersect_ty_refs(next1, next2)?;
            merged_fixed.push(merged_next);
        }

        let merged_rest = match (params1.rest(), params2.rest()) {
            (Some(rest1), Some(rest2)) => Some(self.intersect_ty_refs(rest1, rest2)?),
            _ => None,
        };

        Ok(ty::Params::new(merged_fixed, merged_rest))
    }

    fn unify_fun(&self, fun1: &ty::Fun<S>, fun2: &ty::Fun<S>) -> Result<UnifiedTy<S>, E> {
        let unified_purity = self.unify_purity_refs(fun1.purity(), fun2.purity())?;

        if fun1.is_polymorphic() || fun2.is_polymorphic() {
            // TODO: We could do better here by finding our upper bound and unifying them
            // Preserving the polymorphicness would be very complex
            Ok(UnifiedTy::Merged(
                ty::TopFun::new(unified_purity, ty::Ty::Any.into_ref()).into_ref(),
            ))
        } else {
            let unified_ret = self.unify_to_ty_ref(fun1.ret(), fun2.ret())?;

            match self.unify_params(fun1.params(), fun2.params()) {
                Ok(unified_params) => Ok(UnifiedTy::Merged(
                    ty::Fun::new(
                        S::PVarIds::empty(),
                        S::TVarIds::empty(),
                        ty::TopFun::new(unified_purity, unified_ret),
                        unified_params,
                    ).into_ref(),
                )),
                Err(ty::intersect::Error::Disjoint) => Ok(UnifiedTy::Merged(
                    ty::TopFun::new(unified_purity, unified_ret).into_ref(),
                )),
            }
        }
    }

    /// Unifies two types under the assumption that they are not subtypes
    fn unify_ty(
        &self,
        ref1: &S,
        ty1: &ty::Ty<S>,
        ref2: &S,
        ty2: &ty::Ty<S>,
    ) -> Result<UnifiedTy<S>, E> {
        if ty1 == ty2 {
            return Ok(UnifiedTy::Merged(ref1.clone()));
        }
        Ok(match (ty1, ty2) {
            // Handle supertype relationships
            (_, ty::Ty::Any) | (ty::Ty::Any, _) => UnifiedTy::Merged(ty::Ty::Any.into_ref()),
            (ty::Ty::LitSym(_), ty::Ty::Sym) | (ty::Ty::Sym, ty::Ty::LitSym(_)) => {
                UnifiedTy::Merged(ty::Ty::Sym.into_ref())
            }
            (ty::Ty::LitBool(_), ty::Ty::Bool) | (ty::Ty::Bool, ty::Ty::LitBool(_)) => {
                UnifiedTy::Merged(ty::Ty::Bool.into_ref())
            }

            // Simplify (U true false) => Bool
            (ty::Ty::LitBool(true), ty::Ty::LitBool(false))
            | (ty::Ty::LitBool(false), ty::Ty::LitBool(true)) => {
                UnifiedTy::Merged(ty::Ty::Bool.into_ref())
            }

            // Set type
            (ty::Ty::Set(ty_ref1), ty::Ty::Set(ty_ref2)) => {
                let unified_ty_ref = self.unify_to_ty_ref(&ty_ref1, &ty_ref2)?;

                UnifiedTy::Merged(ty::Ty::Set(Box::new(unified_ty_ref)).into_ref())
            }

            // Map type
            (ty::Ty::Map(key_ref1, val_ref1), ty::Ty::Map(key_ref2, val_ref2)) => {
                let unified_key_ref = self.unify_to_ty_ref(&key_ref1, &key_ref2)?;
                let unified_val_ref = self.unify_to_ty_ref(&val_ref1, &val_ref2)?;

                UnifiedTy::Merged(
                    ty::Ty::Map(Box::new(unified_key_ref), Box::new(unified_val_ref)).into_ref(),
                )
            }

            // Vector types
            (ty::Ty::Vec(members1), ty::Ty::Vec(members2)) => {
                if members1.len() != members2.len() {
                    // We can check vector lengths at runtime
                    UnifiedTy::Discerned
                } else {
                    let mut unified_members = Vec::with_capacity(members1.len());

                    for (member1, member2) in members1.iter().zip(members2.iter()) {
                        match self.unify_ty_refs(member1, member2)? {
                            UnifiedTy::Discerned => {
                                // We can check the types of fixed vector elements
                                return Ok(UnifiedTy::Discerned);
                            }
                            UnifiedTy::Merged(merged) => {
                                unified_members.push(merged);
                            }
                        }
                    }

                    UnifiedTy::Merged(ty::Ty::Vec(unified_members).into_ref())
                }
            }
            (ty::Ty::Vecof(member1), ty::Ty::Vecof(member2)) => UnifiedTy::Merged(
                ty::Ty::Vecof(Box::new(self.unify_to_ty_ref(member1, member2)?)).into_ref(),
            ),
            (ty::Ty::Vec(members1), ty::Ty::Vecof(member2))
            | (ty::Ty::Vecof(member2), ty::Ty::Vec(members1)) => {
                let unified_member =
                    self.unify_ref_iter(vec![member2.as_ref().clone()], members1.iter().cloned())?;

                UnifiedTy::Merged(ty::Ty::Vecof(Box::new(unified_member)).into_ref())
            }

            // Function types
            (ty::Ty::TopFun(top_fun1), ty::Ty::TopFun(top_fun2)) => {
                self.unify_top_fun(top_fun1, top_fun2)?
            }
            (ty::Ty::Fun(fun), ty::Ty::TopFun(top_fun))
            | (ty::Ty::TopFun(top_fun), ty::Ty::Fun(fun)) => {
                self.unify_top_fun(fun.top_fun(), top_fun)?
            }
            (ty::Ty::TyPred(_), ty::Ty::TopFun(top_fun))
            | (ty::Ty::TopFun(top_fun), ty::Ty::TyPred(_)) => {
                self.unify_top_fun(&ty::TopFun::new_for_ty_pred(), top_fun)?
            }

            (ty::Ty::Fun(fun1), ty::Ty::Fun(fun2)) => self.unify_fun(fun1, fun2)?,
            (ty::Ty::TyPred(_), ty::Ty::Fun(fun)) | (ty::Ty::Fun(fun), ty::Ty::TyPred(_)) => {
                self.unify_fun(&ty::Fun::new_for_ty_pred(), fun)?
            }

            (ty::Ty::TyPred(_), ty::Ty::TyPred(_)) => {
                UnifiedTy::Merged(ty::Ty::Fun(Box::new(ty::Fun::new_for_ty_pred())).into_ref())
            }

            // Union types
            (ty::Ty::Union(members1), ty::Ty::Union(members2)) => {
                let new_union = self.unify_ref_iter(members1.clone(), members2.iter().cloned())?;
                UnifiedTy::Merged(new_union)
            }
            (ty::Ty::Union(members1), _) => {
                let new_union = self.unify_ref_iter(members1.clone(), iter::once(ref2).cloned())?;
                UnifiedTy::Merged(new_union)
            }
            (_, ty::Ty::Union(members2)) => {
                let new_union = self.unify_ref_iter(members2.clone(), iter::once(ref1).cloned())?;
                UnifiedTy::Merged(new_union)
            }

            // List types
            (ty::Ty::Listof(member1), ty::Ty::Listof(member2)) => UnifiedTy::Merged(
                ty::Ty::Listof(Box::new(self.unify_to_ty_ref(member1, member2)?)).into_ref(),
            ),
            (ty::Ty::Cons(car1, cdr1), ty::Ty::Cons(car2, cdr2)) => {
                match self.unify_ty_refs(car1, car2)? {
                    UnifiedTy::Discerned => UnifiedTy::Discerned,
                    UnifiedTy::Merged(merged_car) => match self.unify_ty_refs(cdr1, cdr2)? {
                        UnifiedTy::Discerned => UnifiedTy::Discerned,
                        UnifiedTy::Merged(merged_cdr) => UnifiedTy::Merged(
                            ty::Ty::Cons(Box::new(merged_car), Box::new(merged_cdr)).into_ref(),
                        ),
                    },
                }
            }
            (ty::Ty::Nil, ty::Ty::Listof(member)) | (ty::Ty::Listof(member), ty::Ty::Nil) => {
                UnifiedTy::Merged(ty::Ty::Listof(member.clone()).into_ref())
            }
            (ty::Ty::Listof(_), ty::Ty::Cons(car, cdr)) => {
                self.unify_list_cons_refs(ref1, car, cdr)?
            }
            (ty::Ty::Cons(car, cdr), ty::Ty::Listof(_)) => {
                self.unify_list_cons_refs(ref2, car, cdr)?
            }
            _ => UnifiedTy::Discerned,
        })
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
    /// return a `PolyConflict` error.
    ///
    /// The conflicting types are returned as they may have been nested in to the passed types.
    TyConflict(ty::Poly, ty::Poly),

    PurityConflict(ty::purity::Poly, ty::purity::Poly),
}

struct PolyUnifyCtx<'a> {
    tvars: &'a [ty::TVar],
}

impl<'a> PolyUnifyCtx<'a> {
    /// Determines if a type and all of its subtypes are discernible at runtime
    ///
    /// This is to allow literal types to appear with type variables even if they may have a
    /// possible subtype relationship. Otherwise, (U ([A : Symbol] 'foo)) would not be allowed as A
    /// may need to merge with 'foo.
    fn poly_ty_is_discernible(&self, ty: &ty::Ty<ty::Poly>) -> bool {
        match ty {
            // `Any` has non-discernible subtypes
            ty::Ty::Any => false,

            // These are tagged directly
            ty::Ty::Bool
            | ty::Ty::Char
            | ty::Ty::Float
            | ty::Ty::Int
            | ty::Ty::LitBool(_)
            | ty::Ty::Str
            | ty::Ty::Sym
            | ty::Ty::Nil => true,

            // We can build type checks for literal symbols
            ty::Ty::LitSym(_) => true,

            // We will build type checks that need to look inside Cons, as long as the types
            // themselves are discernible
            ty::Ty::Cons(car, cdr) => {
                self.poly_is_discernible(car) && self.poly_is_discernible(cdr)
            }

            // If we can discern every union member we can discern the union itself
            ty::Ty::Union(members) => members.iter().all(|m| self.poly_is_discernible(m)),

            // Type erased types
            ty::Ty::TopFun(_)
            | ty::Ty::Fun(_)
            | ty::Ty::TyPred(_)
            | ty::Ty::Map(_, _)
            | ty::Ty::Set(_)
            | ty::Ty::Vec(_)
            | ty::Ty::Vecof(_)
            | ty::Ty::Listof(_) => false,
        }
    }

    fn poly_is_discernible(&self, poly: &ty::Poly) -> bool {
        let ty = ty::resolve::resolve_poly_ty(self.tvars, poly).as_ty();
        self.poly_ty_is_discernible(ty)
    }
}

impl<'a> UnifyCtx<ty::Poly, PolyError> for PolyUnifyCtx<'a> {
    fn unify_ty_refs(
        &self,
        poly1: &ty::Poly,
        poly2: &ty::Poly,
    ) -> Result<UnifiedTy<ty::Poly>, PolyError> {
        use ty::resolve;

        // Determine if we're dealing with fixed types or polymorphic bounds
        let resolved1 = resolve::resolve_poly_ty(self.tvars, poly1);
        let resolved2 = resolve::resolve_poly_ty(self.tvars, poly2);

        if let (resolve::Result::Fixed(ty1), resolve::Result::Fixed(ty2)) = (&resolved1, &resolved2)
        {
            // We can invoke full unification logic if we have fixed types
            self.unify_ty(poly1, ty1, poly2, ty2)
        } else {
            let ty1 = resolved1.as_ty();
            let ty2 = resolved2.as_ty();

            if self.poly_ty_is_discernible(ty1) && self.poly_ty_is_discernible(ty2) {
                // One of the members can be fully distinguished at runtime so we can let them be
                // members of the same union
                Ok(UnifiedTy::Discerned)
            } else {
                match self.unify_ty(poly1, ty1, poly2, ty2)? {
                    UnifiedTy::Merged(_) => {
                        // We need to merge for correctness but we don't know the specific subtypes
                        // we're merging.
                        Err(PolyError::TyConflict(poly1.clone(), poly2.clone()))
                    }
                    UnifiedTy::Discerned => Ok(UnifiedTy::Discerned),
                }
            }
        }
    }

    fn intersect_ty_refs(
        &self,
        poly1: &ty::Poly,
        poly2: &ty::Poly,
    ) -> Result<ty::Poly, ty::intersect::Error> {
        ty::intersect::poly_intersect(self.tvars, poly1, poly2)
    }

    fn unify_purity_refs(
        &self,
        purity1: &ty::purity::Poly,
        purity2: &ty::purity::Poly,
    ) -> Result<ty::purity::Poly, PolyError> {
        poly_unify_purity(purity1, purity2)
    }
}

pub fn poly_unify_to_poly<'a>(
    tvars: &'a [ty::TVar],
    poly1: &'a ty::Poly,
    poly2: &'a ty::Poly,
) -> Result<ty::Poly, PolyError> {
    let ctx = PolyUnifyCtx { tvars };
    ctx.unify_to_ty_ref(poly1, poly2)
}

pub fn poly_unify_purity(
    purity1: &ty::purity::Poly,
    purity2: &ty::purity::Poly,
) -> Result<ty::purity::Poly, PolyError> {
    if purity1 == purity2 {
        return Ok(purity1.clone());
    }

    match (purity1, purity1) {
        // Pure is the "empty type" so this is a no-op
        // TODO: Replacing `_` and `purity2` with `other` doesn't work
        (ty::purity::Poly::Fixed(Purity::Pure), _) => Ok(purity2.clone()),
        (_, ty::purity::Poly::Fixed(Purity::Pure)) => Ok(purity1.clone()),
        (ty::purity::Poly::Fixed(Purity::Impure), _)
        | (_, ty::purity::Poly::Fixed(Purity::Impure)) => {
            // Impure is the "top type" so this becomes impure
            Ok(Purity::Impure.into_poly())
        }
        _ => Err(PolyError::PurityConflict(purity1.clone(), purity2.clone())),
    }
}

pub fn poly_unify_iter<I>(tvars: &[ty::TVar], members: I) -> Result<ty::Poly, PolyError>
where
    I: Iterator<Item = ty::Poly>,
{
    let ctx = PolyUnifyCtx { tvars };
    ctx.unify_ref_iter(vec![], members)
}

// TODO: Replace with bang type once it's stable
#[derive(Debug, PartialEq)]
pub enum MonoError {}

struct MonoUnifyCtx {}

impl<'a> UnifyCtx<ty::Mono, MonoError> for MonoUnifyCtx {
    fn unify_ty_refs(
        &self,
        mono1: &ty::Mono,
        mono2: &ty::Mono,
    ) -> Result<UnifiedTy<ty::Mono>, MonoError> {
        self.unify_ty(mono1, mono1.as_ty(), mono2, mono2.as_ty())
    }

    fn intersect_ty_refs(
        &self,
        mono1: &ty::Mono,
        mono2: &ty::Mono,
    ) -> Result<ty::Mono, ty::intersect::Error> {
        ty::intersect::mono_intersect(mono1, mono2)
    }

    fn unify_purity_refs(&self, purity1: &Purity, purity2: &Purity) -> Result<Purity, MonoError> {
        Ok(if purity1 == purity2 {
            *purity1
        } else {
            Purity::Impure
        })
    }
}

pub fn mono_unify_to_mono<'a>(
    mono1: &'a ty::Mono,
    mono2: &'a ty::Mono,
) -> Result<ty::Mono, MonoError> {
    let ctx = MonoUnifyCtx {};
    ctx.unify_to_ty_ref(mono1, mono2)
}

pub fn mono_unify_iter<I>(members: I) -> Result<ty::Mono, MonoError>
where
    I: Iterator<Item = ty::Mono>,
{
    let ctx = MonoUnifyCtx {};
    ctx.unify_ref_iter(vec![], members)
}

#[cfg(test)]
mod test {
    use super::*;

    fn poly_for_str(datum_str: &str) -> ty::Poly {
        use hir;
        hir::poly_for_str(datum_str).unwrap()
    }

    fn poly_unify<'a>(
        tvars: &'a [ty::TVar],
        poly1: &'a ty::Poly,
        poly2: &'a ty::Poly,
    ) -> Result<UnifiedTy<ty::Poly>, PolyError> {
        let ctx = PolyUnifyCtx { tvars };
        ctx.unify_ty_refs(poly1, poly2)
    }

    fn assert_discerned(ty_str1: &str, ty_str2: &str) {
        let poly1 = poly_for_str(ty_str1);
        let poly2 = poly_for_str(ty_str2);

        assert_eq!(
            UnifiedTy::Discerned,
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

    fn bounded_polys(bound_str1: &str, bound_str2: &str) -> (Vec<ty::TVar>, ty::Poly, ty::Poly) {
        let bound1 = poly_for_str(bound_str1);
        let bound2 = poly_for_str(bound_str2);

        let tvars = vec![
            ty::TVar::new("poly1".to_owned(), bound1),
            ty::TVar::new("poly2".to_owned(), bound2),
        ];

        let poly1 = ty::Poly::Var(ty::TVarId::new(0));
        let poly2 = ty::Poly::Var(ty::TVarId::new(1));

        (tvars, poly1, poly2)
    }

    fn assert_poly_bound_merged(expected_str: &str, bound_str1: &str, bound_str2: &str) {
        let expected = poly_for_str(expected_str);
        let (tvars, poly1, poly2) = bounded_polys(bound_str1, bound_str2);

        let result = poly_unify(&tvars, &poly1, &poly2).unwrap();

        if let UnifiedTy::Merged(merged_poly) = result {
            let merged_ty = ty::resolve::resolve_poly_ty(&tvars, &merged_poly).as_ty();
            assert_eq!(expected, ty::Poly::Fixed(merged_ty.clone()));
        } else {
            panic!("Expected merged type; got {:?}", result);
        }
    }

    fn assert_poly_bound_discerned(bound_str1: &str, bound_str2: &str) {
        let (tvars, poly1, poly2) = bounded_polys(bound_str1, bound_str2);

        assert_eq!(
            Ok(UnifiedTy::Discerned),
            poly_unify(&tvars, &poly1, &poly2),
            "Poly bounds are not discernible"
        );
    }

    fn assert_poly_bound_conflict(bound_str1: &str, bound_str2: &str) {
        let (tvars, poly1, poly2) = bounded_polys(bound_str1, bound_str2);

        assert_eq!(
            Err(PolyError::TyConflict(poly1.clone(), poly2.clone())),
            poly_unify(&tvars, &poly1, &poly2),
        );
    }

    #[test]
    fn disjoint_types() {
        assert_discerned("String", "Symbol");
    }

    #[test]
    fn two_sym_types() {
        assert_discerned("'foo", "'bar");
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
    fn top_fun_types() {
        assert_merged("(... ->! Bool)", "(... ->! true)", "(... -> false)");
    }

    #[test]
    fn fun_types() {
        // Parameters are contravariant and Float/Int are disjoint
        assert_merged(
            "(... -> (RawU Int Float))",
            "(Float -> Int)",
            "(Int -> Float)",
        );

        assert_merged(
            "(true -> (RawU Int Float))",
            "(Bool -> Int)",
            "(true -> Float)",
        );
        assert_merged("(->! Int)", "(-> Int)", "(->! Int)");
        assert_merged("(->! Bool)", "(-> true)", "(->! false)");

        assert_merged("(... ->! Bool)", "(... -> true)", "(->! false)");
    }

    #[test]
    fn ty_pred_types() {
        assert_merged("(Type? String)", "(Type? String)", "(Type? String)");
        assert_merged("(Any -> Bool)", "(Type? String)", "(Type? Symbol)");
        assert_merged("(Int -> Any)", "(Int -> Any)", "(Type? Symbol)");
        assert_merged("(... ->! Bool)", "(... ->! Bool)", "(Type? Symbol)");
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
            "(RawU true (... -> (RawU Float Int)))",
            "(RawU true (Int -> Float))",
            "(RawU true (Float -> Int))",
        );
        assert_merged("(RawU 'foo 'bar Bool)", "(RawU 'foo 'bar)", "Bool");
        assert_merged("Symbol", "(RawU 'foo 'bar)", "Symbol");
        assert_merged("(RawU Int Symbol)", "(RawU 'foo 'bar Int)", "Symbol");
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
            "(... -> (RawU Symbol String))",
            &["(String -> Symbol)", "(RawU)", "(Symbol -> String)"],
        );
    }

    #[test]
    fn cons_types() {
        // These are implicitly tested more extensively in the list tests

        assert_discerned("(Cons Int Float)", "(Cons Float Int)");
        assert_merged(
            "(Cons (Setof (RawU Int Float)) Bool)",
            "(Cons (Setof Int) true)",
            "(Cons (Setof Float) false)",
        );
    }

    #[test]
    fn list_types() {
        assert_merged("(Listof Any)", "(List Any)", "(Listof Any)");
        assert_discerned("(List Any)", "(List Any Any)");
        assert_discerned("(List Symbol)", "(List String)");
        assert_discerned("(List String)", "(List String String String ...)");
        assert_merged(
            "(List Int (RawU Symbol String Float) ...)",
            "(List Int Symbol ...)",
            "(List Int Float String ...)",
        );
    }

    #[test]
    fn vec_types() {
        assert_merged("(Vectorof Bool)", "(Vector true)", "(Vectorof false)");
        assert_discerned("(Vector 'foo Int Symbol)", "(Vector 'bar Int String)");
    }

    #[test]
    fn poly_bounds() {
        // Literals can be treated as fixed types
        assert_poly_bound_merged("Bool", "true", "false");

        // These are discernible and can't be merged
        assert_poly_bound_discerned("Symbol", "String");

        // These are discernible because all symbol subtypes are discernible
        assert_poly_bound_discerned("Symbol", "Symbol");
        assert_poly_bound_discerned("Symbol", "'foo");

        // This does not have subtypes; it can be unified
        assert_poly_bound_merged(
            "(Any ... -> (RawU))",
            "(Any ... -> (RawU))",
            "(Any ... -> (RawU))",
        );

        // This does have subtypes; they can't be in the same union
        assert_poly_bound_conflict("(Int -> Float)", "(Int -> Float)");

        // Any is the most extreme case of this
        assert_poly_bound_conflict("Any", "(Int -> Float)");

        // These are not disjoint but have subtypes the can't be discerned at runtime as it would
        // require iterating over the list tail
        assert_poly_bound_conflict("(Listof Symbol)", "(Listof Symbol)");

        // These are almost disjoint except an empty list is both
        assert_poly_bound_conflict("(Listof Symbol)", "(Listof String)");

        // These types are completely disjoint but they can't be discerned at runtime
        assert_poly_bound_conflict("(Setof Symbol)", "(Setof String)");
    }

    #[test]
    fn polymorphic_funs() {
        let ptype1_unbounded = ty::Poly::Var(ty::TVarId::new(0));
        let ptype2_string = ty::Poly::Var(ty::TVarId::new(1));

        let tvars = [
            ty::TVar::new("PAny".to_owned(), poly_for_str("Any")),
            ty::TVar::new("PString".to_owned(), poly_for_str("String")),
        ];

        // (All A (A -> A))
        let pidentity_fun = ty::Fun::new(
            ty::purity::PVarIds::empty(),
            ty::TVarId::new(0)..ty::TVarId::new(1),
            ty::TopFun::new(Purity::Pure.into_poly(), ptype1_unbounded.clone()),
            ty::Params::new(vec![ptype1_unbounded.clone()], None),
        ).into_ref();

        // (All A (A A -> (Cons A A))
        let panys_to_cons = ty::Fun::new(
            ty::purity::PVarIds::empty(),
            ty::TVarId::new(0)..ty::TVarId::new(1),
            ty::TopFun::new(
                Purity::Pure.into_poly(),
                ty::Ty::Cons(
                    Box::new(ptype1_unbounded.clone()),
                    Box::new(ptype1_unbounded.clone()),
                ).into_poly(),
            ),
            ty::Params::new(
                vec![ptype1_unbounded.clone(), ptype1_unbounded.clone()],
                None,
            ),
        ).into_ref();

        // (All [A : String] (A ->! A))
        let pidentity_impure_string_fun = ty::Fun::new(
            ty::purity::PVarIds::empty(),
            ty::TVarId::new(1)..ty::TVarId::new(2),
            ty::TopFun::new(Purity::Impure.into_poly(), ptype2_string.clone()),
            ty::Params::new(vec![ptype2_string.clone()], None),
        ).into_ref();

        assert_eq!(
            UnifiedTy::Merged(pidentity_fun.clone()),
            poly_unify(&tvars, &pidentity_fun, &pidentity_fun).unwrap()
        );

        let top_pure_fun = poly_for_str("(... -> Any)");
        assert_eq!(
            UnifiedTy::Merged(top_pure_fun),
            poly_unify(&tvars, &pidentity_fun, &panys_to_cons).unwrap()
        );

        let top_impure_fun = poly_for_str("(... ->! Any)");
        assert_eq!(
            UnifiedTy::Merged(top_impure_fun),
            poly_unify(&tvars, &pidentity_fun, &pidentity_impure_string_fun).unwrap()
        );
    }

    #[test]
    fn purity_refs() {
        let purity_pure = Purity::Pure.into_poly();
        let purity_impure = Purity::Impure.into_poly();
        let purity_var1 = ty::purity::Poly::Var(ty::purity::PVarId::new(1));
        let purity_var2 = ty::purity::Poly::Var(ty::purity::PVarId::new(2));

        assert_eq!(
            purity_pure,
            poly_unify_purity(&purity_pure, &purity_pure).unwrap()
        );

        assert_eq!(
            purity_impure,
            poly_unify_purity(&purity_impure, &purity_impure).unwrap()
        );

        assert_eq!(
            purity_var1,
            poly_unify_purity(&purity_var1, &purity_var1).unwrap()
        );

        assert_eq!(
            purity_impure,
            poly_unify_purity(&purity_pure, &purity_impure).unwrap()
        );

        assert_eq!(
            purity_var1,
            poly_unify_purity(&purity_pure, &purity_var1).unwrap()
        );

        assert_eq!(
            purity_impure,
            poly_unify_purity(&purity_impure, &purity_var1).unwrap()
        );

        assert_eq!(
            PolyError::PurityConflict(purity_var1.clone(), purity_var2.clone()),
            poly_unify_purity(&purity_var1, &purity_var2).unwrap_err()
        );
    }
}
