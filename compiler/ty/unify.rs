//! Builds new types by unifying zero or more input types
//!
//! Every type can be distinguished from every other type at runtime. This would allow our most
//! naive implementation to simply detect any duplicate or subtypes and remove them.
//!
//! However, while every type can be tested at runtime some type checks are very expensive. A
//! pathological case would be testing if a long `(List & Any)` is a `(List & Int)`. We need to
//! allow these checks for completeness but they should be discouraged. To that end, any types
//! that would be expensive to distinguish at runtime are merged by this code. This ensures in the
//! general case it should be quick to test for individual members of a union.
use std::cmp;
use std::iter;

use crate::ty;
use crate::ty::purity;
use crate::ty::purity::Purity;
use crate::ty::record;
use crate::ty::ty_args::TyArgs;
use crate::ty::var_usage::Variance;
use crate::ty::Ty;

#[derive(Debug, PartialEq)]
enum UnifiedTy<M: ty::PM> {
    /// The types are distinct and have no clean simplification
    ///
    /// An example would be Str and Sym.
    Discerned,

    /// The types can be simplified in to a single non-union type
    ///
    /// A trivial example would be Sym and 'foo because of their subtype relationship. More complex
    /// per-type logic exists, especially surrounding sequences.
    Merged(ty::Ref<M>),
}

#[derive(Debug, PartialEq)]
pub enum UnifiedList<M: ty::PM> {
    Discerned,
    Merged(ty::List<M>),
}

fn unify_ty_refs<M: ty::PM>(ref1: &ty::Ref<M>, ref2: &ty::Ref<M>) -> UnifiedTy<M> {
    if let (ty::Ref::Fixed(ty1), ty::Ref::Fixed(ty2)) = (&ref1, &ref2) {
        // We can invoke full simplification logic if we have fixed types
        unify_ty(ref1, ty1, ref2, ty2)
    } else if ty::is_a::ty_ref_is_a(ref1, ref2) {
        UnifiedTy::Merged(ref2.clone())
    } else if ty::is_a::ty_ref_is_a(ref2, ref1) {
        UnifiedTy::Merged(ref1.clone())
    } else {
        // Leave these separate
        UnifiedTy::Discerned
    }
}

fn try_list_to_exact_pair<M: ty::PM>(list: &ty::List<M>) -> Option<&ty::Ref<M>> {
    if list.fixed.len() == 1 && &list.fixed[0] == list.rest.as_ref() {
        Some(list.rest.as_ref())
    } else {
        None
    }
}

/// Unifies a member in to an existing vector of members
///
/// It is assumed `output_members` refers to members of an already unified union.
fn union_push<M: ty::PM>(output_members: &mut Vec<ty::Ref<M>>, new_member: ty::Ref<M>) {
    for i in 0..output_members.len() {
        match unify_ty_refs(&output_members[i], &new_member) {
            UnifiedTy::Merged(merged_member) => {
                // Our merged type may now unify with one of the already processed members of the
                // union. Remove the member we merged with and recurse using the merged member.
                output_members.swap_remove(i);
                return union_push(output_members, merged_member);
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
fn union_extend<M, I>(existing_members: Vec<ty::Ref<M>>, new_members: I) -> ty::Ref<M>
where
    M: ty::PM,
    I: Iterator<Item = ty::Ref<M>>,
{
    let mut output_members = existing_members;

    for new_member in new_members {
        union_push(&mut output_members, new_member);
    }

    ty::Ref::from_vec(output_members)
}

fn unify_top_fun<M: ty::PM>(top_fun1: &ty::TopFun, top_fun2: &ty::TopFun) -> UnifiedTy<M> {
    let unified_purity = unify_purity_refs(top_fun1.purity(), top_fun2.purity());
    let unified_ret = unify_to_ty_ref(top_fun1.ret(), top_fun2.ret());

    UnifiedTy::Merged(ty::TopFun::new(unified_purity, unified_ret).into())
}

fn unify_fun<M: ty::PM>(fun1: &ty::Fun, fun2: &ty::Fun) -> UnifiedTy<M> {
    let unified_purity = unify_purity_refs(fun1.purity(), fun2.purity());

    if fun1.has_polymorphic_vars() || fun2.has_polymorphic_vars() {
        // TODO: We could do better here by finding our upper bound and unifying them
        // Preserving the polymorphicness would be very complex
        UnifiedTy::Merged(ty::TopFun::new(unified_purity, Ty::Any.into()).into())
    } else {
        let unified_ret = unify_to_ty_ref(fun1.ret(), fun2.ret());

        match ty::intersect::intersect_list(fun1.params(), fun2.params()) {
            Ok(unified_params) => UnifiedTy::Merged(
                ty::Fun::new_mono(unified_params, unified_purity, unified_ret).into(),
            ),
            Err(ty::intersect::Error::Disjoint) => {
                UnifiedTy::Merged(ty::TopFun::new(unified_purity, unified_ret).into())
            }
        }
    }
}

fn unify_record_field_purities<M: ty::PM>(
    variance: Variance,
    pvar: &purity::PVarId,
    ty_args1: &TyArgs<M>,
    ty_args2: &TyArgs<M>,
) -> purity::Ref {
    use crate::ty::intersect::intersect_purity_refs;
    use crate::ty::is_a::purity_refs_equivalent;

    let purity_ref1 = &ty_args1.pvar_purities()[pvar];
    let purity_ref2 = &ty_args2.pvar_purities()[pvar];

    match variance {
        Variance::Covariant => unify_purity_refs(purity_ref1, purity_ref2),
        Variance::Contravariant => intersect_purity_refs(purity_ref1, purity_ref2),
        Variance::Invariant => {
            if purity_refs_equivalent(purity_ref1, purity_ref2) {
                purity_ref1.clone()
            } else {
                Purity::Impure.into()
            }
        }
    }
}

fn unify_record_field_ty_refs<M: ty::PM>(
    variance: Variance,
    tvar: &ty::TVarId,
    ty_args1: &TyArgs<M>,
    ty_args2: &TyArgs<M>,
) -> UnifiedTy<M> {
    use crate::ty::intersect::intersect_ty_refs;
    use crate::ty::is_a::ty_refs_equivalent;

    let ty_ref1 = &ty_args1.tvar_types()[tvar];
    let ty_ref2 = &ty_args2.tvar_types()[tvar];

    match variance {
        Variance::Covariant => unify_ty_refs(ty_ref1, ty_ref2),
        Variance::Contravariant => match intersect_ty_refs(ty_ref1, ty_ref2) {
            Ok(intersected) => UnifiedTy::Merged(intersected),
            Err(_) => UnifiedTy::Discerned,
        },
        Variance::Invariant => {
            if ty_refs_equivalent(ty_ref1, ty_ref2) {
                UnifiedTy::Merged(ty_ref1.clone())
            } else {
                UnifiedTy::Discerned
            }
        }
    }
}

fn unify_record_instance<M: ty::PM>(
    instance1: &record::Instance<M>,
    instance2: &record::Instance<M>,
) -> UnifiedTy<M> {
    use crate::ty::record::PolyParam;
    use std::collections::HashMap;

    if instance1.cons() != instance2.cons() {
        return UnifiedTy::Merged(Ty::TopRecord.into());
    }

    let mut merged_pvar_purities = HashMap::new();
    let mut merged_tvar_types = HashMap::new();

    for poly_param in instance1.cons().poly_params() {
        match poly_param {
            PolyParam::PVar(variance, pvar) => {
                merged_pvar_purities.insert(
                    pvar.clone(),
                    unify_record_field_purities(
                        *variance,
                        pvar,
                        instance1.ty_args(),
                        instance2.ty_args(),
                    ),
                );
            }
            PolyParam::TVar(variance, tvar) => {
                let unified_ty = unify_record_field_ty_refs(
                    *variance,
                    tvar,
                    instance1.ty_args(),
                    instance2.ty_args(),
                );

                match unified_ty {
                    UnifiedTy::Merged(merged) => {
                        merged_tvar_types.insert(tvar.clone(), merged);
                    }
                    UnifiedTy::Discerned => return UnifiedTy::Discerned,
                }
            }
            PolyParam::Pure(_) | PolyParam::TFixed(_, _) => {}
        }
    }

    UnifiedTy::Merged(
        record::Instance::new(
            instance1.cons().clone(),
            TyArgs::new(merged_pvar_purities, merged_tvar_types),
        )
        .into(),
    )
}

fn unify_ty<M: ty::PM>(
    ref1: &ty::Ref<M>,
    ty1: &Ty<M>,
    ref2: &ty::Ref<M>,
    ty2: &Ty<M>,
) -> UnifiedTy<M> {
    if ty1 == ty2 {
        return UnifiedTy::Merged(ref1.clone());
    }
    match (ty1, ty2) {
        // Handle supertype relationships
        (_, Ty::Any) | (Ty::Any, _) => UnifiedTy::Merged(Ty::Any.into()),
        (Ty::LitSym(_), Ty::Sym) | (Ty::Sym, Ty::LitSym(_)) => UnifiedTy::Merged(Ty::Sym.into()),
        (Ty::LitBool(_), Ty::Bool) | (Ty::Bool, Ty::LitBool(_)) => {
            UnifiedTy::Merged(Ty::Bool.into())
        }
        (Ty::Float, Ty::Num) | (Ty::Num, Ty::Float) => UnifiedTy::Merged(Ty::Num.into()),
        (Ty::Int, Ty::Num) | (Ty::Num, Ty::Int) => UnifiedTy::Merged(Ty::Num.into()),

        // Simplify (U true false) => Bool
        (Ty::LitBool(true), Ty::LitBool(false)) | (Ty::LitBool(false), Ty::LitBool(true)) => {
            UnifiedTy::Merged(Ty::Bool.into())
        }

        // Simplify (U Float Int) => Num
        (Ty::Float, Ty::Int) | (Ty::Int, Ty::Float) => UnifiedTy::Merged(Ty::Num.into()),

        // Set type
        (Ty::Set(ty_ref1), Ty::Set(ty_ref2)) => {
            let unified_ty_ref = unify_to_ty_ref(ty_ref1.as_ref(), ty_ref2.as_ref());
            UnifiedTy::Merged(Ty::Set(Box::new(unified_ty_ref)).into())
        }

        // Map type
        (Ty::Map(map1), Ty::Map(map2)) => {
            let unified_key_ref = unify_to_ty_ref(map1.key(), map2.key());
            let unified_val_ref = unify_to_ty_ref(map1.value(), map2.value());

            UnifiedTy::Merged(ty::Map::new(unified_key_ref, unified_val_ref).into())
        }

        // Vector types
        (Ty::Vector(members1), Ty::Vector(members2)) => {
            if members1.len() != members2.len() {
                // We can quickly check vector lengths at runtime
                UnifiedTy::Discerned
            } else {
                let unified_members = members1
                    .iter()
                    .zip(members2.iter())
                    .map(|(member1, member2)| unify_to_ty_ref(member1, member2))
                    .collect();

                UnifiedTy::Merged(Ty::Vector(unified_members).into())
            }
        }
        (Ty::Vectorof(member1), Ty::Vectorof(member2)) => UnifiedTy::Merged(
            Ty::Vectorof(Box::new(unify_to_ty_ref(
                member1.as_ref(),
                member2.as_ref(),
            )))
            .into(),
        ),
        (Ty::Vector(members1), Ty::Vectorof(member2))
        | (Ty::Vectorof(member2), Ty::Vector(members1)) => {
            let unified_member =
                union_extend(vec![member2.as_ref().clone()], members1.iter().cloned());

            UnifiedTy::Merged(Ty::Vectorof(Box::new(unified_member)).into())
        }

        // Function types
        (Ty::TopFun(top_fun1), Ty::TopFun(top_fun2)) => unify_top_fun(top_fun1, top_fun2),
        (Ty::Fun(fun), Ty::TopFun(top_fun)) | (Ty::TopFun(top_fun), Ty::Fun(fun)) => {
            unify_top_fun(fun.top_fun(), top_fun)
        }
        (Ty::TyPred(_), Ty::TopFun(top_fun))
        | (Ty::TopFun(top_fun), Ty::TyPred(_))
        | (Ty::EqPred, Ty::TopFun(top_fun))
        | (Ty::TopFun(top_fun), Ty::EqPred) => unify_top_fun(&ty::TopFun::new_for_pred(), top_fun),

        (Ty::Fun(fun1), Ty::Fun(fun2)) => unify_fun(fun1, fun2),
        (Ty::TyPred(_), Ty::Fun(fun)) | (Ty::Fun(fun), Ty::TyPred(_)) => {
            unify_fun(&ty::Fun::new_for_ty_pred(), fun)
        }
        (Ty::EqPred, Ty::Fun(fun)) | (Ty::Fun(fun), Ty::EqPred) => {
            unify_fun(&ty::Fun::new_for_eq_pred(), fun)
        }

        (Ty::TyPred(_), Ty::TyPred(_)) => {
            UnifiedTy::Merged(Ty::Fun(Box::new(ty::Fun::new_for_ty_pred())).into())
        }

        // Union types
        (Ty::Union(members1), Ty::Union(members2)) => {
            let new_union = union_extend(members1.to_vec(), members2.iter().cloned());
            UnifiedTy::Merged(new_union)
        }
        (Ty::Union(members1), _) => {
            let new_union = union_extend(members1.to_vec(), iter::once(ref2).cloned());
            UnifiedTy::Merged(new_union)
        }
        (_, Ty::Union(members2)) => {
            let new_union = union_extend(members2.to_vec(), iter::once(ref1).cloned());
            UnifiedTy::Merged(new_union)
        }

        // List types
        (Ty::List(list1), Ty::List(list2)) => match unify_list(list1, list2) {
            UnifiedList::Discerned => UnifiedTy::Discerned,
            UnifiedList::Merged(merged_list) => UnifiedTy::Merged(merged_list.into()),
        },

        // Record types
        (Ty::Record(instance1), Ty::Record(instance2)) => {
            unify_record_instance(instance1, instance2)
        }

        _ => UnifiedTy::Discerned,
    }
}

pub fn unify_purity_refs(purity1: &purity::Ref, purity2: &purity::Ref) -> purity::Ref {
    if purity1 == purity2 {
        return purity1.clone();
    }

    match (purity1, purity2) {
        // Pure is the "empty type" so this is a no-op
        (purity::Ref::Fixed(Purity::Pure), other) | (other, purity::Ref::Fixed(Purity::Pure)) => {
            other.clone()
        }
        _ => {
            // Impure is the "top type" so this becomes impure
            Purity::Impure.into()
        }
    }
}

pub fn unify_to_ty_ref<M: ty::PM>(ty_ref1: &ty::Ref<M>, ty_ref2: &ty::Ref<M>) -> ty::Ref<M> {
    match unify_ty_refs(ty_ref1, ty_ref2) {
        UnifiedTy::Merged(ty_ref) => ty_ref,
        UnifiedTy::Discerned => Ty::Union(Box::new([ty_ref1.clone(), ty_ref2.clone()])).into(),
    }
}

/// Unifies an iterator of types in to a new type
pub fn unify_ty_ref_iter<M, I>(new_members: I) -> ty::Ref<M>
where
    M: ty::PM,
    I: Iterator<Item = ty::Ref<M>>,
{
    union_extend(vec![], new_members)
}

pub fn unify_list<M: ty::PM>(list1: &ty::List<M>, list2: &ty::List<M>) -> UnifiedList<M> {
    if list1.is_empty() {
        if let Some(member) = try_list_to_exact_pair(list2) {
            return UnifiedList::Merged(ty::List::new_uniform(member.clone()));
        }
    } else if list2.is_empty() {
        if let Some(member) = try_list_to_exact_pair(list1) {
            return UnifiedList::Merged(ty::List::new_uniform(member.clone()));
        }
    }

    if list1.has_disjoint_arity(list2) {
        return UnifiedList::Discerned;
    }

    let mut fixed_iter1 = list1.fixed().iter();
    let mut fixed_iter2 = list2.fixed().iter();

    let mut merged_fixed: Vec<ty::Ref<M>> =
        Vec::with_capacity(cmp::min(fixed_iter1.len(), fixed_iter2.len()));

    while fixed_iter1.len() > 0 && fixed_iter2.len() > 0 {
        let fixed1 = fixed_iter1.next().unwrap();
        let fixed2 = fixed_iter2.next().unwrap();

        merged_fixed.push(unify_to_ty_ref(fixed1, fixed2));
    }

    // Merge all remaining fixed and rest args together
    let rest_iter = fixed_iter1
        .chain(fixed_iter2.chain(iter::once(list1.rest()).chain(iter::once(list2.rest()))));

    let merged_rest = unify_ty_ref_iter(rest_iter.cloned());

    UnifiedList::Merged(ty::List::new(merged_fixed.into_boxed_slice(), merged_rest))
}

#[cfg(test)]
mod test {
    use super::*;
    use crate::hir::{poly_for_str, tvar_bounded_by};
    use arret_syntax::span::EMPTY_SPAN;

    fn assert_discerned(ty_str1: &str, ty_str2: &str) {
        let poly1 = poly_for_str(ty_str1);
        let poly2 = poly_for_str(ty_str2);

        assert_eq!(UnifiedTy::Discerned, unify_ty_refs(&poly1, &poly2));
    }

    fn assert_merged(expected_str: &str, ty_str1: &str, ty_str2: &str) {
        let expected = poly_for_str(expected_str);
        let poly1 = poly_for_str(ty_str1);
        let poly2 = poly_for_str(ty_str2);

        // This is the basic invariant we're testing - each of our input types satisfies the merged
        // type
        assert_eq!(true, ty::is_a::ty_ref_is_a(&poly1, &expected));
        assert_eq!(true, ty::is_a::ty_ref_is_a(&poly2, &expected));

        assert_eq!(UnifiedTy::Merged(expected), unify_ty_refs(&poly1, &poly2));
    }

    fn assert_merged_iter(expected_str: &str, ty_strs: &[&str]) {
        let expected = poly_for_str(expected_str);
        let polys = ty_strs.iter().map(|&s| poly_for_str(s));

        assert_eq!(expected, unify_ty_ref_iter(polys));
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
    fn num_types() {
        assert_merged("Int", "Int", "Int");
        assert_merged("Num", "Int", "Float");
        assert_merged("Num", "Float", "Int");
    }

    #[test]
    fn top_fun_types() {
        assert_merged("(... ->! Bool)", "(... ->! true)", "(... -> false)");
    }

    #[test]
    fn fun_types() {
        // Parameters are contravariant and Float/Int are disjoint
        assert_merged("(... -> Num)", "(Float -> Int)", "(Int -> Float)");

        assert_merged("(true -> Num)", "(Bool -> Int)", "(true -> Float)");
        assert_merged("(->! Int)", "(-> Int)", "(->! Int)");
        assert_merged("(->! Bool)", "(-> true)", "(->! false)");

        assert_merged("(... ->! Bool)", "(... -> true)", "(->! false)");
    }

    #[test]
    fn ty_pred_types() {
        assert_merged("str?", "str?", "str?");
        assert_merged("(Any -> Bool)", "str?", "sym?");
        assert_merged("(Int -> Any)", "(Int -> Any)", "sym?");
        assert_merged("(... ->! Bool)", "(... ->! Bool)", "sym?");
    }

    #[test]
    fn eq_pred_type() {
        assert_merged("=", "=", "=");
        assert_merged("(Int Int -> Any)", "(Int Int -> Any)", "=");
        assert_merged("(... ->! Bool)", "(... ->! Bool)", "=");
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
            "(RawU true (... -> Num))",
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
        assert_merged("(List & Any)", "(List Any)", "(List & Any)");
        assert_discerned("(List Any)", "(List Any Any)");
        assert_merged("(List (RawU Sym Str))", "(List Sym)", "(List Str)");
        assert_discerned("(List Str)", "(List Str Str & Str)");
        assert_merged(
            "(List Int & (RawU Float Sym Str))",
            "(List Int & Sym)",
            "(List Int Float & Str)",
        );

        assert_merged("(List & Int)", "(List Int & Int)", "(List)");
        assert_merged("(List & Sym)", "(List)", "(List Sym & Sym)");
    }

    #[test]
    fn vec_types() {
        assert_merged("(Vectorof Bool)", "(Vector true)", "(Vectorof false)");
        assert_discerned("(Vector Int Sym)", "(Vector 'bar Int Str)");
    }

    #[test]
    fn polymorphic_funs() {
        let pidentity_fun = poly_for_str("(All #{A} A -> A)");
        let pidentity_impure_string_fun = poly_for_str("(All #{[A Str]} A ->! A)");
        let top_impure_fun = poly_for_str("(... ->! Any)");

        assert_eq!(
            UnifiedTy::Merged(pidentity_fun.clone()),
            unify_ty_refs(&pidentity_fun, &pidentity_fun)
        );

        assert_eq!(
            UnifiedTy::Merged(top_impure_fun.clone()),
            unify_ty_refs(&pidentity_fun, &pidentity_impure_string_fun)
        );

        assert_eq!(
            UnifiedTy::Merged(top_impure_fun.clone()),
            unify_ty_refs(&pidentity_fun, &top_impure_fun)
        );
    }

    #[test]
    fn purity_refs() {
        use arret_syntax::span::EMPTY_SPAN;

        let purity_pure = Purity::Pure.into();
        let purity_impure = Purity::Impure.into();

        let pvar1 = purity::PVar::new(EMPTY_SPAN, "test".into());
        let purity_var1 = purity::Ref::Var(pvar1);

        let pvar2 = purity::PVar::new(EMPTY_SPAN, "test".into());
        let purity_var2 = purity::Ref::Var(pvar2);

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

    #[test]
    fn related_poly_bounds() {
        let ptype1_unbounded = tvar_bounded_by(Ty::Any.into());
        let ptype2_bounded_by_1 = tvar_bounded_by(ptype1_unbounded.clone());

        assert_eq!(
            UnifiedTy::Merged(ptype1_unbounded.clone()),
            unify_ty_refs(&ptype1_unbounded, &ptype1_unbounded)
        );

        assert_eq!(
            UnifiedTy::Merged(ptype2_bounded_by_1.clone()),
            unify_ty_refs(&ptype2_bounded_by_1, &ptype2_bounded_by_1)
        );

        assert_eq!(
            UnifiedTy::Merged(ptype1_unbounded.clone()),
            unify_ty_refs(&ptype2_bounded_by_1, &ptype1_unbounded)
        );

        assert_eq!(
            UnifiedTy::Merged(ptype1_unbounded.clone()),
            unify_ty_refs(&ptype1_unbounded, &ptype2_bounded_by_1,)
        );
    }

    #[test]
    fn record_instances() {
        use crate::ty::ty_args::TyArgs;
        use std::collections::HashMap;

        let tvar1 = ty::TVar::new(EMPTY_SPAN, "tvar1".into(), Ty::Any.into());
        let tvar2 = ty::TVar::new(EMPTY_SPAN, "tvar2".into(), Ty::Any.into());

        let cons1 = record::Cons::new(
            EMPTY_SPAN,
            "cons1".into(),
            "cons1?".into(),
            Some(Box::new([record::PolyParam::TVar(
                Variance::Covariant,
                tvar1.clone(),
            )])),
            Box::new([record::Field::new(
                EMPTY_SPAN,
                "cons1-field1".into(),
                tvar1.clone().into(),
            )]),
        );

        let cons2 = record::Cons::new(
            EMPTY_SPAN,
            "cons2".into(),
            "cons2?".into(),
            Some(Box::new([
                record::PolyParam::TVar(Variance::Covariant, tvar1.clone()),
                record::PolyParam::TVar(Variance::Contravariant, tvar2.clone()),
            ])),
            Box::new([
                record::Field::new(EMPTY_SPAN, "cons2-covariant".into(), tvar1.clone().into()),
                record::Field::new(
                    EMPTY_SPAN,
                    "cons2-contravariant".into(),
                    tvar2.clone().into(),
                ),
            ]),
        );

        let float_instance1_poly: ty::Ref<ty::Poly> = record::Instance::new(
            cons1.clone(),
            TyArgs::new(
                HashMap::new(),
                std::iter::once((tvar1.clone(), Ty::Float.into())).collect(),
            ),
        )
        .into();

        let float_bool_instance2_poly: ty::Ref<ty::Poly> = record::Instance::new(
            cons2.clone(),
            TyArgs::new(
                HashMap::new(),
                std::iter::once((tvar1.clone(), Ty::Float.into()))
                    .chain(std::iter::once((tvar2.clone(), Ty::Bool.into())))
                    .collect(),
            ),
        )
        .into();

        let int_false_instance2_poly: ty::Ref<ty::Poly> = record::Instance::new(
            cons2.clone(),
            TyArgs::new(
                HashMap::new(),
                std::iter::once((tvar1.clone(), Ty::Int.into()))
                    .chain(std::iter::once((tvar2.clone(), Ty::LitBool(false).into())))
                    .collect(),
            ),
        )
        .into();

        let num_false_instance2_poly: ty::Ref<ty::Poly> = record::Instance::new(
            cons2.clone(),
            TyArgs::new(
                HashMap::new(),
                std::iter::once((tvar1.clone(), Ty::Num.into()))
                    .chain(std::iter::once((tvar2.clone(), Ty::LitBool(false).into())))
                    .collect(),
            ),
        )
        .into();

        // Different record constructors
        assert_eq!(
            UnifiedTy::Merged(Ty::TopRecord.into()),
            unify_ty_refs(&float_instance1_poly, &float_bool_instance2_poly)
        );

        // Different instances of same constructor
        assert_eq!(
            UnifiedTy::Merged(num_false_instance2_poly),
            unify_ty_refs(&int_false_instance2_poly, &float_bool_instance2_poly)
        );
    }
}
