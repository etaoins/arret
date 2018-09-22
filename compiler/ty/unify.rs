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

use crate::ty;
use crate::ty::purity;
use crate::ty::purity::Purity;

#[derive(Debug, PartialEq)]
pub enum UnifiedTy<S: ty::TyRef> {
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

pub trait Unifiable: ty::TyRef {
    fn unify_ty_refs(tvars: &ty::TVars, mono1: &Self, mono2: &Self) -> UnifiedTy<Self>;
}

impl Unifiable for ty::Mono {
    fn unify_ty_refs(tvars: &ty::TVars, mono1: &ty::Mono, mono2: &ty::Mono) -> UnifiedTy<ty::Mono> {
        unify_ty(tvars, mono1, mono1.as_ty(), mono2, mono2.as_ty())
    }
}

impl Unifiable for ty::Poly {
    fn unify_ty_refs(tvars: &ty::TVars, poly1: &ty::Poly, poly2: &ty::Poly) -> UnifiedTy<ty::Poly> {
        if let (ty::Poly::Fixed(ty1), ty::Poly::Fixed(ty2)) = (&poly1, &poly2) {
            // We can invoke full simplfication logic if we have fixed types
            unify_ty(tvars, poly1, ty1, poly2, ty2)
        } else {
            // Leave these separate
            UnifiedTy::Discerned
        }
    }
}

fn try_list_to_exact_pair<S: Unifiable>(list: &ty::List<S>) -> Option<&S> {
    if let Some(ref rest) = list.rest {
        if list.fixed.len() == 1 && &list.fixed[0] == rest.as_ref() {
            return Some(rest);
        }
    }

    None
}

/// Unifies a member in to an existing vector of members
///
/// It is assumed `output_members` refers to members of an already unified union.
fn union_push<S: Unifiable>(tvars: &ty::TVars, output_members: &mut Vec<S>, new_member: S) {
    for i in 0..output_members.len() {
        match S::unify_ty_refs(tvars, &output_members[i], &new_member) {
            UnifiedTy::Merged(merged_member) => {
                // Our merged type may now unify with one of the already processed members of the
                // union. Remove the member we merged with and recurse using the merged member.
                output_members.swap_remove(i);
                return union_push(tvars, output_members, merged_member);
            }
            UnifiedTy::Discerned => {}
        }
    }

    output_members.push(new_member);
}

/// Extends an existing union with new members
///
/// `existing_members` are assumed to be the members of an existing union. If there is no existing
/// union this must be empty. This is an optimisation to avoid processing the already unified
/// members.
fn union_extend<S, I>(tvars: &ty::TVars, existing_members: Vec<S>, new_members: I) -> S
where
    S: Unifiable,
    I: Iterator<Item = S>,
{
    let mut output_members = existing_members;

    for new_member in new_members {
        union_push(tvars, &mut output_members, new_member);
    }

    S::from_vec(output_members)
}

fn unify_top_fun<S: Unifiable>(
    tvars: &ty::TVars,
    top_fun1: &ty::TopFun,
    top_fun2: &ty::TopFun,
) -> UnifiedTy<S> {
    let unified_purity = unify_purity_refs(top_fun1.purity(), top_fun2.purity());
    let unified_ret = unify_to_ty_ref(tvars, top_fun1.ret(), top_fun2.ret());

    UnifiedTy::Merged(ty::TopFun::new(unified_purity, unified_ret).into_ty_ref())
}

fn unify_fun<S: Unifiable>(tvars: &ty::TVars, fun1: &ty::Fun, fun2: &ty::Fun) -> UnifiedTy<S> {
    let unified_purity = unify_purity_refs(fun1.purity(), fun2.purity());

    if fun1.is_monomorphic() && fun2.is_monomorphic() {
        let unified_ret = unify_to_ty_ref(tvars, fun1.ret(), fun2.ret());

        match ty::intersect::intersect_list(tvars, fun1.params(), fun2.params()) {
            Ok(unified_params) => UnifiedTy::Merged(
                ty::Fun::new(
                    purity::PVars::new(),
                    ty::TVars::new(),
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
        UnifiedTy::Merged(ty::TopFun::new(unified_purity, ty::Ty::Any.into_ty_ref()).into_ty_ref())
    }
}

fn unify_ty<S: Unifiable>(
    tvars: &ty::TVars,
    ref1: &S,
    ty1: &ty::Ty<S>,
    ref2: &S,
    ty2: &ty::Ty<S>,
) -> UnifiedTy<S> {
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
            let unified_ty_ref = unify_to_ty_ref(tvars, ty_ref1.as_ref(), ty_ref2.as_ref());
            UnifiedTy::Merged(ty::Ty::Set(Box::new(unified_ty_ref)).into_ty_ref())
        }

        // Map type
        (ty::Ty::Map(map1), ty::Ty::Map(map2)) => {
            let unified_key_ref = unify_to_ty_ref(tvars, map1.key(), map2.key());
            let unified_val_ref = unify_to_ty_ref(tvars, map1.value(), map2.value());

            UnifiedTy::Merged(
                ty::Ty::Map(Box::new(ty::Map::new(unified_key_ref, unified_val_ref))).into_ty_ref(),
            )
        }

        // Vector types
        (ty::Ty::Vector(members1), ty::Ty::Vector(members2)) => {
            if members1.len() != members2.len() {
                // We can quickly check vector lengths at runtime
                UnifiedTy::Discerned
            } else {
                let unified_members = members1
                    .iter()
                    .zip(members2.iter())
                    .map(|(member1, member2)| unify_to_ty_ref(tvars, member1, member2))
                    .collect::<Vec<S>>();

                UnifiedTy::Merged(ty::Ty::Vector(unified_members.into_boxed_slice()).into_ty_ref())
            }
        }
        (ty::Ty::Vectorof(member1), ty::Ty::Vectorof(member2)) => UnifiedTy::Merged(
            ty::Ty::Vectorof(Box::new(unify_to_ty_ref(
                tvars,
                member1.as_ref(),
                member2.as_ref(),
            ))).into_ty_ref(),
        ),
        (ty::Ty::Vector(members1), ty::Ty::Vectorof(member2))
        | (ty::Ty::Vectorof(member2), ty::Ty::Vector(members1)) => {
            let unified_member = union_extend(
                tvars,
                vec![member2.as_ref().clone()],
                members1.iter().cloned(),
            );

            UnifiedTy::Merged(ty::Ty::Vectorof(Box::new(unified_member)).into_ty_ref())
        }

        // Function types
        (ty::Ty::TopFun(top_fun1), ty::Ty::TopFun(top_fun2)) => {
            unify_top_fun(tvars, top_fun1, top_fun2)
        }
        (ty::Ty::Fun(fun), ty::Ty::TopFun(top_fun))
        | (ty::Ty::TopFun(top_fun), ty::Ty::Fun(fun)) => {
            unify_top_fun(tvars, fun.top_fun(), top_fun)
        }
        (ty::Ty::TyPred(_), ty::Ty::TopFun(top_fun))
        | (ty::Ty::TopFun(top_fun), ty::Ty::TyPred(_)) => {
            unify_top_fun(tvars, &ty::TopFun::new_for_ty_pred(), top_fun)
        }

        (ty::Ty::Fun(fun1), ty::Ty::Fun(fun2)) => unify_fun(tvars, fun1, fun2),
        (ty::Ty::TyPred(_), ty::Ty::Fun(fun)) | (ty::Ty::Fun(fun), ty::Ty::TyPred(_)) => {
            unify_fun(tvars, &ty::Fun::new_for_ty_pred(), fun)
        }

        (ty::Ty::TyPred(_), ty::Ty::TyPred(_)) => {
            UnifiedTy::Merged(ty::Ty::Fun(Box::new(ty::Fun::new_for_ty_pred())).into_ty_ref())
        }

        // Union types
        (ty::Ty::Union(members1), ty::Ty::Union(members2)) => {
            let new_union = union_extend(tvars, members1.to_vec(), members2.iter().cloned());
            UnifiedTy::Merged(new_union)
        }
        (ty::Ty::Union(members1), _) => {
            let new_union = union_extend(tvars, members1.to_vec(), iter::once(ref2).cloned());
            UnifiedTy::Merged(new_union)
        }
        (_, ty::Ty::Union(members2)) => {
            let new_union = union_extend(tvars, members2.to_vec(), iter::once(ref1).cloned());
            UnifiedTy::Merged(new_union)
        }

        // List types
        (ty::Ty::List(list1), ty::Ty::List(list2)) => match unify_list(tvars, list1, list2) {
            UnifiedList::Discerned => UnifiedTy::Discerned,
            UnifiedList::Merged(merged_list) => {
                UnifiedTy::Merged(ty::Ty::List(merged_list).into_ty_ref())
            }
        },

        _ => UnifiedTy::Discerned,
    }
}

pub fn unify_purity_refs(purity1: &purity::Poly, purity2: &purity::Poly) -> purity::Poly {
    if purity1 == purity2 {
        return purity1.clone();
    }

    match (purity1, purity2) {
        // Pure is the "empty type" so this is a no-op
        (purity::Poly::Fixed(Purity::Pure), other) | (other, purity::Poly::Fixed(Purity::Pure)) => {
            other.clone()
        }
        _ => {
            // Impure is the "top type" so this becomes impure
            Purity::Impure.into_poly()
        }
    }
}

pub fn unify_to_ty_ref<S: Unifiable>(tvars: &ty::TVars, ty_ref1: &S, ty_ref2: &S) -> S {
    match S::unify_ty_refs(tvars, ty_ref1, ty_ref2) {
        UnifiedTy::Merged(ty_ref) => ty_ref,
        UnifiedTy::Discerned => {
            ty::Ty::Union(Box::new([ty_ref1.clone(), ty_ref2.clone()])).into_ty_ref()
        }
    }
}

/// Unifies an iterator of types in to a new type
pub fn unify_ty_ref_iter<S, I>(tvars: &ty::TVars, new_members: I) -> S
where
    S: Unifiable,
    I: Iterator<Item = S>,
{
    union_extend(tvars, vec![], new_members)
}

pub fn unify_list<S: Unifiable>(
    tvars: &ty::TVars,
    list1: &ty::List<S>,
    list2: &ty::List<S>,
) -> UnifiedList<S> {
    if list1.is_empty() {
        if let Some(member) = try_list_to_exact_pair(list2) {
            return UnifiedList::Merged(ty::List::new(Box::new([]), Some(member.clone())));
        }
    } else if list2.is_empty() {
        if let Some(member) = try_list_to_exact_pair(list1) {
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

        merged_fixed.push(unify_to_ty_ref(tvars, fixed1, fixed2));
    }

    let merged_rest = if fixed_iter1.len() > 0
        || fixed_iter2.len() > 0
        || list1.rest().is_some()
        || list2.rest().is_some()
    {
        // Merge all remaining fixed and rest args together
        let rest_iter = fixed_iter1
            .chain(fixed_iter2.chain(list1.rest().into_iter().chain(list2.rest().into_iter())));

        Some(unify_ty_ref_iter(tvars, rest_iter.cloned()))
    } else {
        None
    };

    UnifiedList::Merged(ty::List::new(merged_fixed.into_boxed_slice(), merged_rest))
}

#[cfg(test)]
mod test {
    use super::*;

    fn poly_for_str(datum_str: &str) -> ty::Poly {
        use crate::hir;
        hir::poly_for_str(datum_str)
    }

    fn assert_discerned(ty_str1: &str, ty_str2: &str) {
        let poly1 = poly_for_str(ty_str1);
        let poly2 = poly_for_str(ty_str2);

        assert_eq!(
            UnifiedTy::Discerned,
            ty::Poly::unify_ty_refs(&ty::TVars::new(), &poly1, &poly2)
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
            ty::is_a::ty_ref_is_a(&ty::TVars::new(), &poly1, &expected)
        );
        assert_eq!(
            ty::is_a::Result::Yes,
            ty::is_a::ty_ref_is_a(&ty::TVars::new(), &poly2, &expected)
        );

        assert_eq!(
            UnifiedTy::Merged(expected),
            ty::Poly::unify_ty_refs(&ty::TVars::new(), &poly1, &poly2)
        );
    }

    fn assert_merged_iter(expected_str: &str, ty_strs: &[&str]) {
        let expected = poly_for_str(expected_str);
        let polys = ty_strs.iter().map(|&s| poly_for_str(s));

        assert_eq!(expected, unify_ty_ref_iter(&ty::TVars::new(), polys));
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
    fn polymorphic_funs() {
        let pidentity_fun = poly_for_str("(All #{A} A -> A)");
        assert_eq!(
            UnifiedTy::Merged(pidentity_fun.clone()),
            ty::Poly::unify_ty_refs(&ty::TVars::new(), &pidentity_fun, &pidentity_fun)
        );

        let pidentity_impure_string_fun = poly_for_str("(All #{[A : Str]} A ->! A)");
        let top_impure_fun = poly_for_str("(... ->! Any)");
        assert_eq!(
            UnifiedTy::Merged(top_impure_fun),
            ty::Poly::unify_ty_refs(
                &ty::TVars::new(),
                &pidentity_fun,
                &pidentity_impure_string_fun
            )
        );
    }

    #[test]
    fn purity_refs() {
        let purity_pure = Purity::Pure.into_poly();
        let purity_impure = Purity::Impure.into_poly();

        let pvar_id1 = purity::PVarId::alloc();
        let purity_var1 = purity::Poly::Var(pvar_id1);

        let pvar_id2 = purity::PVarId::alloc();
        let purity_var2 = purity::Poly::Var(pvar_id2);

        assert_eq!(purity_pure, unify_purity_refs(&purity_pure, &purity_pure));

        assert_eq!(
            purity_impure,
            unify_purity_refs(&purity_impure, &purity_impure)
        );

        assert_eq!(purity_var1, unify_purity_refs(&purity_var1, &purity_var1));

        assert_eq!(
            purity_impure,
            unify_purity_refs(&purity_pure, &purity_impure)
        );

        assert_eq!(purity_var1, unify_purity_refs(&purity_pure, &purity_var1));

        assert_eq!(
            purity_impure,
            unify_purity_refs(&purity_impure, &purity_var1)
        );

        assert_eq!(purity_impure, unify_purity_refs(&purity_var1, &purity_var2));
    }
}
