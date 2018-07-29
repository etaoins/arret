//! Builds new types by unifying zero or more input types
//!
//! Every type can be distinguished from every other type at runtime. This would allow our most
//! naive implementation to simply detect any duplicate or subtypes and remove them.
//!
//! However, while every type can be tested at runtime some type checks are very expensive. A
//! pathological case would be testing if a long `(Listof Any)` is a `(Listof Int)`. We need to
//! allow these checks for completeness but they should be discouraged. To that end, any types
//! that would be expensive to distinguish at runtime are merged by this code. This ensures in the
//! general case it should be quick to test for individual members of a union.
use std::cmp;
use std::iter;

use ty;
use ty::purity::PVarIds;
use ty::purity::Purity;
use ty::TVarIds;

#[derive(Debug, PartialEq)]
enum UnifiedTy<S: ty::TyRef> {
    /// The types are distinct and have no clean simplification
    ///
    /// An example would be Str and Sym.
    Discerned,

    /// The types can be simplified in to a single non-union type
    ///
    /// A trivial example would be Sym and 'foo because of their subtype relationship. More complex
    /// per-type logic exists, especially surrounding sequences.
    Merged(S),
}

#[derive(Debug, PartialEq)]
pub enum UnifiedList<S: ty::TyRef> {
    Discerned,
    Merged(ty::List<S>),
}

trait UnifyCtx<S: ty::TyRef> {
    fn unify_ty_refs(&self, &S, &S) -> UnifiedTy<S>;
    fn unify_purity_refs(&self, &S::PRef, &S::PRef) -> S::PRef;
    fn intersect_list(
        &self,
        &ty::List<S>,
        &ty::List<S>,
    ) -> Result<ty::List<S>, ty::intersect::Error>;

    fn unify_to_ty_ref(&self, ty_ref1: &S, ty_ref2: &S) -> S {
        match self.unify_ty_refs(ty_ref1, ty_ref2) {
            UnifiedTy::Merged(ty_ref) => ty_ref,
            UnifiedTy::Discerned => {
                ty::Ty::Union(Box::new([ty_ref1.clone(), ty_ref2.clone()])).into_ty_ref()
            }
        }
    }

    fn try_list_to_exact_pair(list: &ty::List<S>) -> Option<&S> {
        if let Some(ref rest) = list.rest {
            if list.fixed.len() == 1 && &list.fixed[0] == rest.as_ref() {
                return Some(rest);
            }
        }

        None
    }

    fn unify_list(&self, list1: &ty::List<S>, list2: &ty::List<S>) -> UnifiedList<S> {
        if list1.is_empty() {
            if let Some(member) = Self::try_list_to_exact_pair(list2) {
                return UnifiedList::Merged(ty::List::new(Box::new([]), Some(member.clone())));
            }
        } else if list2.is_empty() {
            if let Some(member) = Self::try_list_to_exact_pair(list1) {
                return UnifiedList::Merged(ty::List::new(Box::new([]), Some(member.clone())));
            }
        }

        if list1.has_disjoint_arity(list2) {
            // We can quickly check list lengths at runtime
            return UnifiedList::Discerned;
        }

        let mut fixed_iter1 = list1.fixed().iter();
        let mut fixed_iter2 = list2.fixed().iter();

        let mut merged_fixed: Vec<S> =
            Vec::with_capacity(cmp::min(fixed_iter1.len(), fixed_iter2.len()));

        while fixed_iter1.len() > 0 && fixed_iter2.len() > 0 {
            let fixed1 = fixed_iter1.next().unwrap();
            let fixed2 = fixed_iter2.next().unwrap();

            merged_fixed.push(self.unify_to_ty_ref(fixed1, fixed2));
        }

        let merged_rest = if fixed_iter1.len() > 0
            || fixed_iter2.len() > 0
            || list1.rest().is_some()
            || list2.rest().is_some()
        {
            // Merge all remaining fixed and rest args together
            let rest_iter = fixed_iter1
                .chain(fixed_iter2.chain(list1.rest().into_iter().chain(list2.rest().into_iter())));

            Some(self.unify_ref_iter(vec![], rest_iter.cloned()))
        } else {
            None
        };

        UnifiedList::Merged(ty::List::new(merged_fixed.into_boxed_slice(), merged_rest))
    }

    /// Unifies a member in to an existing vector of members
    ///
    /// It is assumed `output_members` refers to members of an already unified union.
    fn unify_ref_into_vec(&self, output_members: &mut Vec<S>, new_member: S) {
        for i in 0..output_members.len() {
            match self.unify_ty_refs(&output_members[i], &new_member) {
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
    }

    /// Unifies an iterators in to a new type
    ///
    /// `existing_members` are assumed to be the members of an existing union. If there is no
    /// existing union this must be empty. This is an optimisation to avoid processing the already
    /// unified members.
    fn unify_ref_iter<I>(&self, existing_members: Vec<S>, new_members: I) -> S
    where
        I: Iterator<Item = S>,
    {
        let mut output_members = existing_members;

        for new_member in new_members {
            self.unify_ref_into_vec(&mut output_members, new_member);
        }

        S::from_vec(output_members)
    }

    fn unify_top_fun(&self, top_fun1: &ty::TopFun<S>, top_fun2: &ty::TopFun<S>) -> UnifiedTy<S> {
        let unified_purity = self.unify_purity_refs(top_fun1.purity(), top_fun2.purity());
        let unified_ret = self.unify_to_ty_ref(top_fun1.ret(), top_fun2.ret());

        UnifiedTy::Merged(ty::TopFun::new(unified_purity, unified_ret).into_ty_ref())
    }

    fn unify_fun(&self, fun1: &ty::Fun<S>, fun2: &ty::Fun<S>) -> UnifiedTy<S> {
        let unified_purity = self.unify_purity_refs(fun1.purity(), fun2.purity());

        if fun1.is_monomorphic() && fun2.is_monomorphic() {
            let unified_ret = self.unify_to_ty_ref(fun1.ret(), fun2.ret());

            match self.intersect_list(fun1.params(), fun2.params()) {
                Ok(unified_params) => UnifiedTy::Merged(
                    ty::Fun::new(
                        S::PVarIds::monomorphic(),
                        S::TVarIds::monomorphic(),
                        ty::TopFun::new(unified_purity, unified_ret),
                        unified_params,
                    ).into_ty_ref(),
                ),
                Err(ty::intersect::Error::Disjoint) => {
                    UnifiedTy::Merged(ty::TopFun::new(unified_purity, unified_ret).into_ty_ref())
                }
            }
        } else {
            // TODO: We could do better here by finding our upper bound and unifying them
            // Preserving the polymorphicness would be very complex
            UnifiedTy::Merged(
                ty::TopFun::new(unified_purity, ty::Ty::Any.into_ty_ref()).into_ty_ref(),
            )
        }
    }

    fn unify_ty(&self, ref1: &S, ty1: &ty::Ty<S>, ref2: &S, ty2: &ty::Ty<S>) -> UnifiedTy<S> {
        if ty1 == ty2 {
            return UnifiedTy::Merged(ref1.clone());
        }
        match (ty1, ty2) {
            // Handle supertype relationships
            (_, ty::Ty::Any) | (ty::Ty::Any, _) => UnifiedTy::Merged(ty::Ty::Any.into_ty_ref()),
            (ty::Ty::LitSym(_), ty::Ty::Sym) | (ty::Ty::Sym, ty::Ty::LitSym(_)) => {
                UnifiedTy::Merged(ty::Ty::Sym.into_ty_ref())
            }
            (ty::Ty::LitBool(_), ty::Ty::Bool) | (ty::Ty::Bool, ty::Ty::LitBool(_)) => {
                UnifiedTy::Merged(ty::Ty::Bool.into_ty_ref())
            }

            // Simplify (U true false) => Bool
            (ty::Ty::LitBool(true), ty::Ty::LitBool(false))
            | (ty::Ty::LitBool(false), ty::Ty::LitBool(true)) => {
                UnifiedTy::Merged(ty::Ty::Bool.into_ty_ref())
            }

            // Set type
            (ty::Ty::Set(ty_ref1), ty::Ty::Set(ty_ref2)) => {
                let unified_ty_ref = self.unify_to_ty_ref(&ty_ref1, &ty_ref2);

                UnifiedTy::Merged(ty::Ty::Set(Box::new(unified_ty_ref)).into_ty_ref())
            }

            // Map type
            (ty::Ty::Map(map1), ty::Ty::Map(map2)) => {
                let unified_key_ref = self.unify_to_ty_ref(map1.key(), map2.key());
                let unified_val_ref = self.unify_to_ty_ref(map1.value(), map2.value());

                UnifiedTy::Merged(
                    ty::Ty::Map(Box::new(ty::Map::new(unified_key_ref, unified_val_ref)))
                        .into_ty_ref(),
                )
            }

            // Vector types
            (ty::Ty::Vec(members1), ty::Ty::Vec(members2)) => {
                if members1.len() != members2.len() {
                    // We can quickly check vector lengths at runtime
                    UnifiedTy::Discerned
                } else {
                    let unified_members = members1
                        .iter()
                        .zip(members2.iter())
                        .map(|(member1, member2)| self.unify_to_ty_ref(member1, member2))
                        .collect::<Vec<S>>();

                    UnifiedTy::Merged(ty::Ty::Vec(unified_members.into_boxed_slice()).into_ty_ref())
                }
            }
            (ty::Ty::Vecof(member1), ty::Ty::Vecof(member2)) => UnifiedTy::Merged(
                ty::Ty::Vecof(Box::new(self.unify_to_ty_ref(member1, member2))).into_ty_ref(),
            ),
            (ty::Ty::Vec(members1), ty::Ty::Vecof(member2))
            | (ty::Ty::Vecof(member2), ty::Ty::Vec(members1)) => {
                let unified_member =
                    self.unify_ref_iter(vec![member2.as_ref().clone()], members1.iter().cloned());

                UnifiedTy::Merged(ty::Ty::Vecof(Box::new(unified_member)).into_ty_ref())
            }

            // Function types
            (ty::Ty::TopFun(top_fun1), ty::Ty::TopFun(top_fun2)) => {
                self.unify_top_fun(top_fun1, top_fun2)
            }
            (ty::Ty::Fun(fun), ty::Ty::TopFun(top_fun))
            | (ty::Ty::TopFun(top_fun), ty::Ty::Fun(fun)) => {
                self.unify_top_fun(fun.top_fun(), top_fun)
            }
            (ty::Ty::TyPred(_), ty::Ty::TopFun(top_fun))
            | (ty::Ty::TopFun(top_fun), ty::Ty::TyPred(_)) => {
                self.unify_top_fun(&ty::TopFun::new_for_ty_pred(), top_fun)
            }

            (ty::Ty::Fun(fun1), ty::Ty::Fun(fun2)) => self.unify_fun(fun1, fun2),
            (ty::Ty::TyPred(_), ty::Ty::Fun(fun)) | (ty::Ty::Fun(fun), ty::Ty::TyPred(_)) => {
                self.unify_fun(&ty::Fun::new_for_ty_pred(), fun)
            }

            (ty::Ty::TyPred(_), ty::Ty::TyPred(_)) => {
                UnifiedTy::Merged(ty::Ty::Fun(Box::new(ty::Fun::new_for_ty_pred())).into_ty_ref())
            }

            // Union types
            (ty::Ty::Union(members1), ty::Ty::Union(members2)) => {
                let new_union = self.unify_ref_iter(members1.to_vec(), members2.iter().cloned());
                UnifiedTy::Merged(new_union)
            }
            (ty::Ty::Union(members1), _) => {
                let new_union = self.unify_ref_iter(members1.to_vec(), iter::once(ref2).cloned());
                UnifiedTy::Merged(new_union)
            }
            (_, ty::Ty::Union(members2)) => {
                let new_union = self.unify_ref_iter(members2.to_vec(), iter::once(ref1).cloned());
                UnifiedTy::Merged(new_union)
            }

            // List types
            (ty::Ty::List(list1), ty::Ty::List(list2)) => match self.unify_list(list1, list2) {
                UnifiedList::Discerned => UnifiedTy::Discerned,
                UnifiedList::Merged(merged_list) => {
                    UnifiedTy::Merged(ty::Ty::List(merged_list).into_ty_ref())
                }
            },

            _ => UnifiedTy::Discerned,
        }
    }
}

struct PolyUnifyCtx<'tvars> {
    tvars: &'tvars [ty::TVar],
}

impl<'tvars> UnifyCtx<ty::Poly> for PolyUnifyCtx<'tvars> {
    fn unify_ty_refs(&self, poly1: &ty::Poly, poly2: &ty::Poly) -> UnifiedTy<ty::Poly> {
        use ty::resolve;

        // Determine if we're dealing with fixed types or polymorphic bounds
        let resolved1 = resolve::resolve_poly_ty(self.tvars, poly1);
        let resolved2 = resolve::resolve_poly_ty(self.tvars, poly2);

        if let (resolve::Result::Fixed(ty1), resolve::Result::Fixed(ty2)) = (&resolved1, &resolved2)
        {
            // We can invoke full simplfication logic if we have fixed types
            self.unify_ty(poly1, ty1, poly2, ty2)
        } else {
            // Leave these separate
            UnifiedTy::Discerned
        }
    }

    fn unify_purity_refs(
        &self,
        purity1: &ty::purity::Poly,
        purity2: &ty::purity::Poly,
    ) -> ty::purity::Poly {
        poly_unify_purity(purity1, purity2)
    }

    fn intersect_list(
        &self,
        list1: &ty::List<ty::Poly>,
        list2: &ty::List<ty::Poly>,
    ) -> Result<ty::List<ty::Poly>, ty::intersect::Error> {
        ty::intersect::poly_intersect_list(self.tvars, list1, list2)
    }
}

pub fn poly_unify_to_poly(tvars: &[ty::TVar], poly1: &ty::Poly, poly2: &ty::Poly) -> ty::Poly {
    let ctx = PolyUnifyCtx { tvars };
    ctx.unify_to_ty_ref(poly1, poly2)
}

pub fn poly_unify_purity(
    purity1: &ty::purity::Poly,
    purity2: &ty::purity::Poly,
) -> ty::purity::Poly {
    if purity1 == purity2 {
        return purity1.clone();
    }

    match (purity1, purity2) {
        // Pure is the "empty type" so this is a no-op
        (ty::purity::Poly::Fixed(Purity::Pure), other)
        | (other, ty::purity::Poly::Fixed(Purity::Pure)) => other.clone(),
        _ => {
            // Impure is the "top type" so this becomes impure
            Purity::Impure.into_poly()
        }
    }
}

pub fn poly_unify_iter<I>(tvars: &[ty::TVar], members: I) -> ty::Poly
where
    I: Iterator<Item = ty::Poly>,
{
    let ctx = PolyUnifyCtx { tvars };
    ctx.unify_ref_iter(vec![], members)
}

pub fn poly_unify_list(
    tvars: &[ty::TVar],
    list1: &ty::List<ty::Poly>,
    list2: &ty::List<ty::Poly>,
) -> UnifiedList<ty::Poly> {
    let ctx = PolyUnifyCtx { tvars };
    ctx.unify_list(list1, list2)
}

struct MonoUnifyCtx {}

impl UnifyCtx<ty::Mono> for MonoUnifyCtx {
    fn unify_ty_refs(&self, mono1: &ty::Mono, mono2: &ty::Mono) -> UnifiedTy<ty::Mono> {
        self.unify_ty(mono1, mono1.as_ty(), mono2, mono2.as_ty())
    }

    fn unify_purity_refs(&self, purity1: &Purity, purity2: &Purity) -> Purity {
        if purity1 == purity2 {
            *purity1
        } else {
            Purity::Impure
        }
    }

    fn intersect_list(
        &self,
        list1: &ty::List<ty::Mono>,
        list2: &ty::List<ty::Mono>,
    ) -> Result<ty::List<ty::Mono>, ty::intersect::Error> {
        ty::intersect::mono_intersect_list(list1, list2)
    }
}

pub fn mono_unify_list(
    list1: &ty::List<ty::Mono>,
    list2: &ty::List<ty::Mono>,
) -> UnifiedList<ty::Mono> {
    let ctx = MonoUnifyCtx {};
    ctx.unify_list(list1, list2)
}

#[cfg(test)]
mod test {
    use super::*;

    fn poly_for_str(datum_str: &str) -> ty::Poly {
        use hir;
        hir::poly_for_str(datum_str)
    }

    fn poly_unify(tvars: &[ty::TVar], poly1: &ty::Poly, poly2: &ty::Poly) -> UnifiedTy<ty::Poly> {
        let ctx = PolyUnifyCtx { tvars };
        ctx.unify_ty_refs(poly1, poly2)
    }

    fn assert_discerned(ty_str1: &str, ty_str2: &str) {
        let poly1 = poly_for_str(ty_str1);
        let poly2 = poly_for_str(ty_str2);

        assert_eq!(UnifiedTy::Discerned, poly_unify(&[], &poly1, &poly2));
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

        assert_eq!(UnifiedTy::Merged(expected), poly_unify(&[], &poly1, &poly2));
    }

    fn assert_merged_iter(expected_str: &str, ty_strs: &[&str]) {
        let expected = poly_for_str(expected_str);
        let polys = ty_strs.iter().map(|&s| poly_for_str(s));

        assert_eq!(expected, poly_unify_iter(&[], polys));
    }

    fn bounded_polys(bound_str1: &str, bound_str2: &str) -> (Vec<ty::TVar>, ty::Poly, ty::Poly) {
        let bound1 = poly_for_str(bound_str1);
        let bound2 = poly_for_str(bound_str2);

        let tvars = vec![
            ty::TVar::new("poly1".into(), bound1),
            ty::TVar::new("poly2".into(), bound2),
        ];

        let poly1 = ty::Poly::Var(ty::TVarId::new(0));
        let poly2 = ty::Poly::Var(ty::TVarId::new(1));

        (tvars, poly1, poly2)
    }

    fn assert_poly_bound_merged(expected_str: &str, bound_str1: &str, bound_str2: &str) {
        let expected = poly_for_str(expected_str);
        let (tvars, poly1, poly2) = bounded_polys(bound_str1, bound_str2);

        let result = poly_unify(&tvars, &poly1, &poly2);

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
            UnifiedTy::Discerned,
            poly_unify(&tvars, &poly1, &poly2),
            "Poly bounds are not discernible"
        );
    }

    #[test]
    fn disjoint_types() {
        assert_discerned("Str", "Sym");
    }

    #[test]
    fn two_sym_types() {
        assert_discerned("'foo", "'bar");
    }

    #[test]
    fn literal_sym_and_any_sym() {
        assert_merged("Sym", "Sym", "'foo");
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
        assert_merged("(Type? Str)", "(Type? Str)", "(Type? Str)");
        assert_merged("(Any -> Bool)", "(Type? Str)", "(Type? Sym)");
        assert_merged("(Int -> Any)", "(Int -> Any)", "(Type? Sym)");
        assert_merged("(... ->! Bool)", "(... ->! Bool)", "(Type? Sym)");
    }

    #[test]
    fn set_types() {
        assert_merged("(Setof Bool)", "(Setof true)", "(Setof false)");
        assert_merged("(Setof (RawU Str Sym))", "(Setof Str)", "(Setof Sym)");
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
            "(RawU Char Int Str Sym)",
            "(RawU Char Int)",
            "(RawU Str Sym)",
        );
        assert_merged(
            "(RawU true (... -> (RawU Float Int)))",
            "(RawU true (Int -> Float))",
            "(RawU true (Float -> Int))",
        );
        assert_merged("(RawU 'foo 'bar Bool)", "(RawU 'foo 'bar)", "Bool");
        assert_merged("Sym", "(RawU 'foo 'bar)", "Sym");
        assert_merged("(RawU Int Sym)", "(RawU 'foo 'bar Int)", "Sym");
        assert_merged("Sym", "(RawU)", "Sym");
        assert_merged("(RawU)", "(RawU)", "(RawU)");

        assert_merged(
            "(RawU Char Int Str Sym)",
            "(RawU Char Int)",
            "(RawU Str Sym)",
        );
    }

    #[test]
    fn unify_iter() {
        assert_merged_iter("(RawU)", &[]);
        assert_merged_iter("Sym", &["Sym"]);
        assert_merged_iter("Bool", &["true", "false"]);
        assert_merged_iter(
            "(Setof (RawU Str Sym Int))",
            &["(Setof Str)", "(Setof Sym)", "(Setof Int)"],
        );

        assert_merged_iter(
            "(... -> (RawU Sym Str))",
            &["(Str -> Sym)", "(RawU)", "(Sym -> Str)"],
        );
    }

    #[test]
    fn list_types() {
        assert_merged("(Listof Any)", "(List Any)", "(Listof Any)");
        assert_discerned("(List Any)", "(List Any Any)");
        assert_merged("(List (RawU Sym Str))", "(List Sym)", "(List Str)");
        assert_discerned("(List Str)", "(List Str Str Str ...)");
        assert_merged(
            "(List Int (RawU Float Sym Str) ...)",
            "(List Int Sym ...)",
            "(List Int Float Str ...)",
        );

        assert_merged("(Listof Int)", "(List Int Int ...)", "(List)");
        assert_merged("(Listof Sym)", "(List)", "(List Sym Sym ...)");
    }

    #[test]
    fn vec_types() {
        assert_merged("(Vectorof Bool)", "(Vector true)", "(Vectorof false)");
        assert_discerned("(Vector Int Sym)", "(Vector 'bar Int Str)");
    }

    #[test]
    fn poly_bounds() {
        assert_poly_bound_merged("Bool", "true", "false");

        assert_poly_bound_discerned("Sym", "Str");

        assert_poly_bound_discerned("Sym", "Sym");
        assert_poly_bound_discerned("Sym", "'foo");

        assert_poly_bound_discerned("(Int -> Float)", "(Int -> Float)");
        assert_poly_bound_discerned("Any", "(Int -> Float)");
        assert_poly_bound_discerned("(Listof Sym)", "(Listof Sym)");
        assert_poly_bound_discerned("(Listof Sym)", "(Listof Str)");
        assert_poly_bound_discerned("(Setof Sym)", "(Setof Str)");

        // This does not have subtypes; it can be unified
        assert_poly_bound_merged(
            "(Any ... -> (RawU))",
            "(Any ... -> (RawU))",
            "(Any ... -> (RawU))",
        );
    }

    #[test]
    fn polymorphic_funs() {
        let ptype1_unbounded = ty::Poly::Var(ty::TVarId::new(0));
        let ptype2_string = ty::Poly::Var(ty::TVarId::new(1));

        let tvars = [
            ty::TVar::new("PAny".into(), poly_for_str("Any")),
            ty::TVar::new("PStr".into(), poly_for_str("Str")),
        ];

        // #{A} (A -> A)
        let pidentity_fun = ty::Fun::new(
            ty::purity::PVarIds::monomorphic(),
            ty::TVarId::new(0)..ty::TVarId::new(1),
            ty::TopFun::new(Purity::Pure.into_poly(), ptype1_unbounded.clone()),
            ty::List::new(Box::new([ptype1_unbounded.clone()]), None),
        ).into_ty_ref();

        // #{[A : Str]} (A ->! A)
        let pidentity_impure_string_fun = ty::Fun::new(
            ty::purity::PVarIds::monomorphic(),
            ty::TVarId::new(1)..ty::TVarId::new(2),
            ty::TopFun::new(Purity::Impure.into_poly(), ptype2_string.clone()),
            ty::List::new(Box::new([ptype2_string.clone()]), None),
        ).into_ty_ref();

        assert_eq!(
            UnifiedTy::Merged(pidentity_fun.clone()),
            poly_unify(&tvars, &pidentity_fun, &pidentity_fun)
        );

        let top_impure_fun = poly_for_str("(... ->! Any)");
        assert_eq!(
            UnifiedTy::Merged(top_impure_fun),
            poly_unify(&tvars, &pidentity_fun, &pidentity_impure_string_fun)
        );
    }

    #[test]
    fn purity_refs() {
        let purity_pure = Purity::Pure.into_poly();
        let purity_impure = Purity::Impure.into_poly();
        let purity_var1 = ty::purity::Poly::Var(ty::purity::PVarId::new(1));
        let purity_var2 = ty::purity::Poly::Var(ty::purity::PVarId::new(2));

        assert_eq!(purity_pure, poly_unify_purity(&purity_pure, &purity_pure));

        assert_eq!(
            purity_impure,
            poly_unify_purity(&purity_impure, &purity_impure)
        );

        assert_eq!(purity_var1, poly_unify_purity(&purity_var1, &purity_var1));

        assert_eq!(
            purity_impure,
            poly_unify_purity(&purity_pure, &purity_impure)
        );

        assert_eq!(purity_var1, poly_unify_purity(&purity_pure, &purity_var1));

        assert_eq!(
            purity_impure,
            poly_unify_purity(&purity_impure, &purity_var1)
        );

        assert_eq!(purity_impure, poly_unify_purity(&purity_var1, &purity_var2));
    }
}
