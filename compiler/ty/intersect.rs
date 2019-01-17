use std::cmp;
use std::iter;
use std::result;

use crate::ty;
use crate::ty::list_iter::ListIterator;
use crate::ty::purity;
use crate::ty::purity::Purity;

#[derive(PartialEq, Debug)]
pub enum Error {
    Disjoint,
}

type Result<S> = result::Result<S, Error>;

pub trait Intersectable: ty::TyRef {
    fn intersect_ty_refs(tvars: &ty::TVars, ty_ref1: &Self, ty_ref2: &Self) -> Result<Self>;
}

impl Intersectable for ty::Mono {
    fn intersect_ty_refs(
        tvars: &ty::TVars,
        mono1: &ty::Mono,
        mono2: &ty::Mono,
    ) -> Result<ty::Mono> {
        if ty::is_a::ty_ref_is_a(tvars, mono1, mono2).to_bool() {
            return Ok(mono1.clone());
        } else if ty::is_a::ty_ref_is_a(tvars, mono2, mono1).to_bool() {
            return Ok(mono2.clone());
        }

        non_subty_intersect(tvars, mono1, mono1.as_ty(), mono2, mono2.as_ty())
    }
}

impl Intersectable for ty::Poly {
    fn intersect_ty_refs(
        tvars: &ty::TVars,
        poly1: &ty::Poly,
        poly2: &ty::Poly,
    ) -> Result<ty::Poly> {
        if ty::is_a::ty_ref_is_a(tvars, poly1, poly2).to_bool() {
            return Ok(poly1.clone());
        } else if ty::is_a::ty_ref_is_a(tvars, poly2, poly1).to_bool() {
            return Ok(poly2.clone());
        }

        match (poly1, poly2) {
            (ty::Poly::Fixed(ty1), ty::Poly::Fixed(ty2)) => {
                // We can invoke full intersection logic if we have fixed types
                non_subty_intersect(tvars, poly1, ty1, poly2, ty2)
            }
            _ => {
                // TODO: Can we be more clever here?
                Err(Error::Disjoint)
            }
        }
    }
}

fn unify_list(
    tvars: &ty::TVars,
    list1: &ty::List<ty::Poly>,
    list2: &ty::List<ty::Poly>,
) -> Result<ty::List<ty::Poly>> {
    match ty::unify::unify_list(tvars, list1, list2) {
        ty::unify::UnifiedList::Merged(merged) => Ok(merged),
        ty::unify::UnifiedList::Discerned => Err(Error::Disjoint),
    }
}

fn intersect_purity_refs(purity1: &purity::Poly, purity2: &purity::Poly) -> purity::Poly {
    if purity1 == purity2 {
        purity1.clone()
    } else {
        Purity::Pure.into_poly()
    }
}

/// Intersects a vector of refs with an iterator
///
/// `lefts` is a slice as it needs to be iterated over multiple times. `rights` is only visited
/// once so it can be an arbitrary iterator.
fn intersect_ref_iter<'a, S, I>(tvars: &ty::TVars, lefts: &[S], rights: I) -> Result<S>
where
    S: Intersectable + 'a,
    I: Iterator<Item = &'a S>,
{
    let mut intersected_types: Vec<S> = vec![];

    for right in rights {
        for left in lefts {
            match intersect_ty_refs(tvars, left, right) {
                Err(Error::Disjoint) => {}
                Ok(intersected) => {
                    intersected_types.push(intersected);
                }
            }
        }
    }

    match intersected_types.len() {
        0 => Err(Error::Disjoint),
        1 => Ok(intersected_types.pop().unwrap()),
        _ => Ok(ty::Ty::Union(intersected_types.into_boxed_slice()).into_ty_ref()),
    }
}

/// Intersects two types under the assumption that they are not subtypes
fn non_subty_intersect<S: Intersectable>(
    tvars: &ty::TVars,
    ref1: &S,
    ty1: &ty::Ty<S>,
    ref2: &S,
    ty2: &ty::Ty<S>,
) -> Result<S> {
    match (ty1, ty2) {
        // Union types
        (ty::Ty::Union(refs1), ty::Ty::Union(refs2)) => {
            intersect_ref_iter(tvars, refs1, refs2.iter())
        }
        (ty::Ty::Union(refs1), _) => intersect_ref_iter(tvars, refs1, iter::once(ref2)),
        (_, ty::Ty::Union(refs2)) => intersect_ref_iter(tvars, refs2, iter::once(ref1)),

        // Set type
        (ty::Ty::Set(member1), ty::Ty::Set(member2)) => Ok(ty::Ty::Set(Box::new(
            intersect_ty_refs(tvars, member1.as_ref(), member2.as_ref())?,
        ))
        .into_ty_ref()),

        // Map type
        (ty::Ty::Map(map1), ty::Ty::Map(map2)) => Ok(ty::Ty::Map(Box::new(ty::Map::new(
            intersect_ty_refs(tvars, map1.key(), map2.key())?,
            intersect_ty_refs(tvars, map1.value(), map2.value())?,
        )))
        .into_ty_ref()),

        // Vector types
        (ty::Ty::Vectorof(member1), ty::Ty::Vectorof(member2)) => Ok(ty::Ty::Vectorof(Box::new(
            intersect_ty_refs(tvars, member1.as_ref(), member2.as_ref())?,
        ))
        .into_ty_ref()),
        (ty::Ty::Vector(members1), ty::Ty::Vector(members2)) => {
            if members1.len() != members2.len() {
                Err(Error::Disjoint)
            } else {
                let intersected_members = members1
                    .iter()
                    .zip(members2.iter())
                    .map(|(member1, member2)| intersect_ty_refs(tvars, member1, member2))
                    .collect::<Result<Box<[S]>>>()?;

                Ok(ty::Ty::Vector(intersected_members).into_ty_ref())
            }
        }
        (ty::Ty::Vectorof(member1), ty::Ty::Vector(members2))
        | (ty::Ty::Vector(members2), ty::Ty::Vectorof(member1)) => {
            let intersected_members = members2
                .iter()
                .map(|member2| intersect_ty_refs(tvars, member1.as_ref(), member2))
                .collect::<Result<Box<[S]>>>()?;

            Ok(ty::Ty::Vector(intersected_members).into_ty_ref())
        }

        // List types
        (ty::Ty::List(list1), ty::Ty::List(list2)) => {
            Ok(ty::Ty::List(intersect_list(tvars, list1, list2)?).into_ty_ref())
        }

        // Function types
        (ty::Ty::TopFun(top_fun1), ty::Ty::TopFun(top_fun2)) => {
            let intersected_purity = intersect_purity_refs(top_fun1.purity(), top_fun2.purity());
            let intersected_ret = intersect_ty_refs(tvars, top_fun1.ret(), top_fun2.ret())?;

            Ok(ty::TopFun::new(intersected_purity, intersected_ret).into_ty_ref())
        }
        (ty::Ty::TopFun(top_fun), ty::Ty::Fun(fun))
        | (ty::Ty::Fun(fun), ty::Ty::TopFun(top_fun)) => {
            if fun.has_polymorphic_vars() {
                // TODO: This might be possible but we would have to recalculate the tvars for
                // the intersected function
                return Err(Error::Disjoint);
            }

            let intersected_purity = intersect_purity_refs(top_fun.purity(), fun.purity());
            let intersected_params = fun.params().clone();
            let intersected_ret = intersect_ty_refs(tvars, top_fun.ret(), fun.ret())?;

            Ok(ty::Fun::new(
                purity::PVars::new(),
                ty::TVars::new(),
                ty::TopFun::new(intersected_purity, intersected_ret),
                intersected_params,
            )
            .into_ty_ref())
        }
        (ty::Ty::Fun(fun1), ty::Ty::Fun(fun2)) => {
            if fun1.has_polymorphic_vars() || fun2.has_polymorphic_vars() {
                // TODO: Same issue as top functions
                Err(Error::Disjoint)
            } else {
                let intersected_purity = intersect_purity_refs(fun1.purity(), fun2.purity());
                let intersected_params = unify_list(tvars, fun1.params(), fun2.params())?;
                let intersected_ret = intersect_ty_refs(tvars, fun1.ret(), fun2.ret())?;

                Ok(ty::Fun::new(
                    purity::PVars::new(),
                    ty::TVars::new(),
                    ty::TopFun::new(intersected_purity, intersected_ret),
                    intersected_params,
                )
                .into_ty_ref())
            }
        }
        (_, _) => Err(Error::Disjoint),
    }
}

pub fn intersect_list<S: Intersectable>(
    tvars: &ty::TVars,
    list1: &ty::List<S>,
    list2: &ty::List<S>,
) -> Result<ty::List<S>> {
    if list1.has_disjoint_arity(&list2) {
        return Err(ty::intersect::Error::Disjoint);
    }

    let mut iter1 = ListIterator::new(list1);
    let mut iter2 = ListIterator::new(list2);

    let mut merged_fixed: Vec<S> =
        Vec::with_capacity(cmp::max(iter1.fixed_len(), iter2.fixed_len()));

    while iter1.fixed_len() > 0 || iter2.fixed_len() > 0 {
        let next1 = iter1.next().unwrap();
        let next2 = iter2.next().unwrap();

        let merged_next = intersect_ty_refs(tvars, next1, next2)?;
        merged_fixed.push(merged_next);
    }

    let merged_rest = match (list1.rest(), list2.rest()) {
        (Some(rest1), Some(rest2)) => Some(intersect_ty_refs(tvars, rest1, rest2)?),
        _ => None,
    };

    Ok(ty::List::new(merged_fixed.into_boxed_slice(), merged_rest))
}

pub fn intersect_ty_refs<S: Intersectable>(
    tvars: &ty::TVars,
    ty_ref1: &S,
    ty_ref2: &S,
) -> Result<S> {
    S::intersect_ty_refs(tvars, ty_ref1, ty_ref2)
}

#[cfg(test)]
mod test {
    use super::*;
    use crate::hir::poly_for_str;

    fn assert_disjoint(ty_str1: &str, ty_str2: &str) {
        let poly1 = poly_for_str(ty_str1);
        let poly2 = poly_for_str(ty_str2);

        assert_eq!(
            Error::Disjoint,
            intersect_ty_refs(&ty::TVars::new(), &poly1, &poly2).unwrap_err()
        );
    }

    fn assert_merged(expected_str: &str, ty_str1: &str, ty_str2: &str) {
        let expected = poly_for_str(expected_str);
        let poly1 = poly_for_str(ty_str1);
        let poly2 = poly_for_str(ty_str2);

        // This is the basic invariant we're testing - each of our merged type satisfies each of
        // our input types.
        assert_eq!(
            ty::is_a::Result::Yes,
            ty::is_a::ty_ref_is_a(&ty::TVars::new(), &expected, &poly1),
            "The expected type does not definitely satisfy the first input type; the test is incorrect"
        );
        assert_eq!(
            ty::is_a::Result::Yes,
            ty::is_a::ty_ref_is_a(&ty::TVars::new(), &expected, &poly2),
            "The expected type does not definitely satisfy the second input type; the test is incorrect"
        );

        assert_eq!(
            expected,
            intersect_ty_refs(&ty::TVars::new(), &poly1, &poly2).unwrap()
        );
    }

    #[test]
    fn disjoint_types() {
        assert_disjoint("Sym", "Str");
    }

    #[test]
    fn simple_subtypes() {
        assert_merged("true", "Bool", "true");
        assert_merged("Float", "Num", "Float");
        assert_merged("Bool", "Bool", "Any");
    }

    #[test]
    fn union_types() {
        assert_merged("'bar", "(RawU 'foo 'bar)", "(RawU 'bar 'baz)");
        assert_merged(
            "(RawU 'bar 'baz)",
            "(RawU 'foo 'bar 'baz)",
            "(RawU 'bar 'baz 'foobar)",
        );
        assert_merged("true", "(RawU true 'foo)", "Bool");
    }

    #[test]
    fn map_types() {
        assert_disjoint("(Map Int Float)", "(Map Float Int)");
        assert_merged(
            "(Map 'foo Int)",
            "(Map (RawU 'foo 'bar) Int)",
            "(Map (RawU 'foo 'baz) Int)",
        );
    }

    #[test]
    fn set_types() {
        assert_disjoint("(Setof Sym)", "(Setof Str)");
        assert_merged(
            "(Setof 'foo)",
            "(Setof (RawU 'foo 'bar))",
            "(Setof (RawU 'foo 'baz))",
        );
    }

    #[test]
    fn list_types() {
        assert_disjoint("(List Sym)", "(List Str)");
        assert_merged("(List Sym Sym)", "(List Any Sym)", "(List Sym ...)");
        assert_merged(
            "(List false true)",
            "(List Bool true)",
            "(List false Bool Any ...)",
        );

        assert_disjoint("(List Sym Sym)", "(List Sym)");
    }

    #[test]
    fn vec_types() {
        assert_disjoint("(Vector Int)", "(Vector Float)");
        assert_merged("(Vector true)", "(Vector Bool)", "(Vectorof true)");
        assert_merged("(Vectorof false)", "(Vectorof Bool)", "(Vectorof false)");
    }

    #[test]
    fn top_fun_types() {
        assert_disjoint("(... -> Float)", "(... -> Int)");
        assert_merged("(... -> true)", "(... -> Bool)", "(... ->! true)");
    }

    #[test]
    fn fun_types() {
        assert_merged("(Num -> Int)", "(Float -> Int)", "(Int -> Int)");
        assert_disjoint("(Str -> Sym)", "(Str Str -> Sym)");
        assert_merged("(-> true)", "(-> Bool)", "(->! true)");
        assert_merged("(Bool -> Str)", "(true -> Str)", "(false ->! Str)");

        assert_merged("(->! true)", "(... ->! true)", "(->! Any)");
    }

    #[test]
    fn ty_pred_types() {
        assert_disjoint("str?", "sym?");
        assert_merged("str?", "str?", "str?");
        assert_merged("str?", "str?", "(Any -> Bool)");
        assert_merged("str?", "str?", "(... -> Bool)");
    }

    #[test]
    fn eq_pred_types() {
        assert_merged("=", "=", "=");
        assert_merged("=", "=", "(Any Any -> Bool)");
        assert_merged("=", "=", "(... -> Bool)");
    }

    #[test]
    fn polymorphic_funs() {
        let pidentity_fun = poly_for_str("(All #{A} A -> A)");
        let pidentity_impure_bool_fun = poly_for_str("(All #{[A Bool]} A ->! A)");
        let top_pure_fun = poly_for_str("(... -> Any)");

        // We should intersect polymorphic functions with themselves
        assert_eq!(
            pidentity_fun.clone(),
            intersect_ty_refs(&ty::TVars::new(), &pidentity_fun, &pidentity_fun).unwrap()
        );

        // The intersection of the pure identity function and the top pure function is the identity
        // function
        assert_eq!(
            pidentity_fun.clone(),
            intersect_ty_refs(&ty::TVars::new(), &pidentity_fun, &top_pure_fun).unwrap()
        );

        // The intersection of the pure identity function and the impure string function is the
        // identity function
        // TODO: This seems like it should be (Str -> Str)
        assert_eq!(
            pidentity_fun.clone(),
            intersect_ty_refs(
                &ty::TVars::new(),
                &pidentity_fun,
                &pidentity_impure_bool_fun
            )
            .unwrap()
        );

        // These have no subtype relationship
        // TODO: This also seems like it should be (Str -> Str)
        assert_eq!(
            Error::Disjoint,
            intersect_ty_refs(&ty::TVars::new(), &pidentity_impure_bool_fun, &top_pure_fun)
                .unwrap_err()
        );
    }
}
