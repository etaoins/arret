use std::cmp;
use std::iter;
use std::result;

use crate::ty;
use crate::ty::list_iter::ListIterator;
use crate::ty::purity;
use crate::ty::purity::Purity;
use crate::ty::record;
use crate::ty::ty_args::TyArgs;
use crate::ty::var_usage::Variance;
use crate::ty::Ty;

#[derive(PartialEq, Debug)]
pub enum Error {
    Disjoint,
}

type Result<S> = result::Result<S, Error>;

/// Flattens an intersection between two type references
///
/// This has no type logic; it only flattens the structure of the refs.
fn flatten_ref_intersect<M: ty::Pm>(ref1: &ty::Ref<M>, ref2: &ty::Ref<M>) -> ty::Ref<M> {
    let mut members: Vec<ty::Ref<M>> = vec![];

    if let Some(Ty::Intersect(members1)) = ref1.try_to_fixed() {
        members.extend(members1.iter().cloned());
    } else {
        members.push(ref1.clone());
    }

    if let Some(Ty::Intersect(members2)) = ref2.try_to_fixed() {
        members.extend(members2.iter().cloned());
    } else {
        members.push(ref2.clone());
    }

    match members.len() {
        0 => Ty::Any.into(),
        1 => members.pop().unwrap(),
        _ => Ty::Intersect(members.into_boxed_slice()).into(),
    }
}

fn unify_list(
    list1: &ty::List<ty::Poly>,
    list2: &ty::List<ty::Poly>,
) -> Result<ty::List<ty::Poly>> {
    match ty::unify::unify_list(list1, list2) {
        ty::unify::UnifiedList::Merged(merged) => Ok(merged),
        ty::unify::UnifiedList::Discerned => Err(Error::Disjoint),
    }
}

/// Intersects a vector of refs with an iterator
///
/// `lefts` is a slice as it needs to be iterated over multiple times. `rights` is only visited
/// once so it can be an arbitrary iterator.
fn intersect_union_iter<'a, M, I>(lefts: &[ty::Ref<M>], rights: I) -> Result<ty::Ref<M>>
where
    M: ty::Pm + 'a,
    I: Iterator<Item = &'a ty::Ref<M>>,
{
    let mut intersected_types: Vec<ty::Ref<M>> = vec![];

    for right in rights {
        for left in lefts {
            match intersect_ty_refs(left, right) {
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
        _ => Ok(Ty::Union(intersected_types.into_boxed_slice()).into()),
    }
}

fn intersect_ty_ref_iter<'a, M, I>(mut ty_refs: I) -> Result<ty::Ref<M>>
where
    M: ty::Pm + 'a,
    I: Iterator<Item = &'a ty::Ref<M>>,
{
    let mut acc = if let Some(acc) = ty_refs.next() {
        acc.clone()
    } else {
        return Ok(Ty::Any.into());
    };

    for ty_ref in ty_refs {
        acc = intersect_ty_refs(&acc, ty_ref)?;
    }
    Ok(acc)
}

fn intersect_record_field_purities<M: ty::Pm>(
    variance: Variance,
    pvar: &purity::PVarId,
    ty_args1: &TyArgs<M>,
    ty_args2: &TyArgs<M>,
) -> Result<purity::Ref> {
    use crate::ty::is_a::purity_refs_equivalent;
    use crate::ty::unify::unify_purity_refs;

    let purity_ref1 = &ty_args1.pvar_purities()[pvar];
    let purity_ref2 = &ty_args2.pvar_purities()[pvar];

    match variance {
        Variance::Covariant => Ok(intersect_purity_refs(purity_ref1, purity_ref2)),
        Variance::Contravariant => Ok(unify_purity_refs(purity_ref1, purity_ref2)),
        Variance::Invariant => {
            if purity_refs_equivalent(purity_ref1, purity_ref2) {
                Ok(purity_ref1.clone())
            } else {
                Err(Error::Disjoint)
            }
        }
    }
}

fn intersect_record_field_ty_refs<M: ty::Pm>(
    variance: Variance,
    tvar: &ty::TVarId,
    ty_args1: &TyArgs<M>,
    ty_args2: &TyArgs<M>,
) -> Result<ty::Ref<M>> {
    use crate::ty::is_a::ty_refs_equivalent;
    use crate::ty::unify::unify_to_ty_ref;

    let ty_ref1 = &ty_args1.tvar_types()[tvar];
    let ty_ref2 = &ty_args2.tvar_types()[tvar];

    match variance {
        Variance::Covariant => intersect_ty_refs(ty_ref1, ty_ref2),
        Variance::Contravariant => Ok(unify_to_ty_ref(ty_ref1, ty_ref2)),
        Variance::Invariant => {
            if ty_refs_equivalent(ty_ref1, ty_ref2) {
                Ok(ty_ref1.clone())
            } else {
                Err(Error::Disjoint)
            }
        }
    }
}

fn intersect_record_instance<M: ty::Pm>(
    instance1: &record::Instance<M>,
    instance2: &record::Instance<M>,
) -> Result<record::Instance<M>> {
    use crate::ty::record::PolyParam;
    use std::collections::HashMap;

    if instance1.cons() != instance2.cons() {
        return Err(Error::Disjoint);
    }

    let mut merged_pvar_purities = HashMap::new();
    let mut merged_tvar_types = HashMap::new();

    for poly_param in instance1.cons().poly_params() {
        match poly_param {
            PolyParam::PVar(variance, pvar) => {
                merged_pvar_purities.insert(
                    pvar.clone(),
                    intersect_record_field_purities(
                        *variance,
                        pvar,
                        instance1.ty_args(),
                        instance2.ty_args(),
                    )?,
                );
            }
            PolyParam::TVar(variance, tvar) => {
                merged_tvar_types.insert(
                    tvar.clone(),
                    intersect_record_field_ty_refs(
                        *variance,
                        tvar,
                        instance1.ty_args(),
                        instance2.ty_args(),
                    )?,
                );
            }
            PolyParam::Pure(_) | PolyParam::TFixed(_, _) => {}
        }
    }

    Ok(record::Instance::new(
        instance1.cons().clone(),
        TyArgs::new(merged_pvar_purities, merged_tvar_types),
    ))
}

/// Intersects two types under the assumption that they are not subtypes
fn non_subty_intersect<M: ty::Pm>(
    ref1: &ty::Ref<M>,
    ty1: &Ty<M>,
    ref2: &ty::Ref<M>,
    ty2: &Ty<M>,
) -> Result<ty::Ref<M>> {
    match (ty1, ty2) {
        // Union types
        (Ty::Union(refs1), Ty::Union(refs2)) => intersect_union_iter(refs1, refs2.iter()),
        (Ty::Union(refs1), _) => intersect_union_iter(refs1, iter::once(ref2)),
        (_, Ty::Union(refs2)) => intersect_union_iter(refs2, iter::once(ref1)),

        // Intersection types
        (Ty::Intersect(refs1), Ty::Intersect(refs2)) => {
            intersect_ty_ref_iter(refs1.iter().chain(refs2.iter()))
        }
        (Ty::Intersect(refs1), _) => {
            let mut acc = ref2.clone();
            for ty_ref in refs1.iter() {
                acc = intersect_ty_refs(&acc, ty_ref)?;
            }
            Ok(acc)
        }
        (_, Ty::Intersect(refs2)) => {
            let mut acc = ref1.clone();
            for ty_ref in refs2.iter() {
                acc = intersect_ty_refs(&acc, ty_ref)?;
            }
            Ok(acc)
        }

        // Set type
        (Ty::Set(member1), Ty::Set(member2)) => Ok(Ty::Set(Box::new(intersect_ty_refs(
            member1.as_ref(),
            member2.as_ref(),
        )?))
        .into()),

        // Map type
        (Ty::Map(map1), Ty::Map(map2)) => Ok(ty::Map::new(
            intersect_ty_refs(map1.key(), map2.key())?,
            intersect_ty_refs(map1.value(), map2.value())?,
        )
        .into()),

        // Vector types
        (Ty::Vectorof(member1), Ty::Vectorof(member2)) => Ok(Ty::Vectorof(Box::new(
            intersect_ty_refs(member1.as_ref(), member2.as_ref())?,
        ))
        .into()),
        (Ty::Vector(members1), Ty::Vector(members2)) => {
            if members1.len() != members2.len() {
                Err(Error::Disjoint)
            } else {
                let intersected_members = members1
                    .iter()
                    .zip(members2.iter())
                    .map(|(member1, member2)| intersect_ty_refs(member1, member2))
                    .collect::<Result<Box<[ty::Ref<M>]>>>()?;

                Ok(Ty::Vector(intersected_members).into())
            }
        }
        (Ty::Vectorof(member1), Ty::Vector(members2))
        | (Ty::Vector(members2), Ty::Vectorof(member1)) => {
            let intersected_members = members2
                .iter()
                .map(|member2| intersect_ty_refs(member1.as_ref(), member2))
                .collect::<Result<Box<[ty::Ref<M>]>>>()?;

            Ok(Ty::Vector(intersected_members).into())
        }

        // List types
        (Ty::List(list1), Ty::List(list2)) => Ok(intersect_list(list1, list2)?.into()),

        // Function types
        (Ty::TopFun(top_fun1), Ty::TopFun(top_fun2)) => {
            let intersected_purity = intersect_purity_refs(top_fun1.purity(), top_fun2.purity());
            let intersected_ret = intersect_ty_refs(top_fun1.ret(), top_fun2.ret())?;

            Ok(ty::TopFun::new(intersected_purity, intersected_ret).into())
        }
        (Ty::TopFun(top_fun), Ty::Fun(fun)) | (Ty::Fun(fun), Ty::TopFun(top_fun)) => {
            if fun.has_polymorphic_vars() {
                // TODO: This might be possible but we would have to recalculate the tvars for
                // the intersected function
                return Err(Error::Disjoint);
            }

            let intersected_purity = intersect_purity_refs(top_fun.purity(), fun.purity());
            let intersected_params = fun.params().clone();
            let intersected_ret = intersect_ty_refs(top_fun.ret(), fun.ret())?;

            Ok(ty::Fun::new_mono(intersected_params, intersected_purity, intersected_ret).into())
        }
        (Ty::Fun(fun1), Ty::Fun(fun2)) => {
            if fun1.has_polymorphic_vars() || fun2.has_polymorphic_vars() {
                // TODO: Same issue as top functions
                Err(Error::Disjoint)
            } else {
                let intersected_purity = intersect_purity_refs(fun1.purity(), fun2.purity());
                let intersected_params = unify_list(fun1.params(), fun2.params())?;
                let intersected_ret = intersect_ty_refs(fun1.ret(), fun2.ret())?;

                Ok(
                    ty::Fun::new_mono(intersected_params, intersected_purity, intersected_ret)
                        .into(),
                )
            }
        }
        (Ty::Record(instance1), Ty::Record(instance2)) => {
            Ok(Ty::Record(Box::new(intersect_record_instance(instance1, instance2)?)).into())
        }
        (_, _) => Err(Error::Disjoint),
    }
}

pub fn intersect_list<M: ty::Pm>(list1: &ty::List<M>, list2: &ty::List<M>) -> Result<ty::List<M>> {
    if list1.has_disjoint_arity(list2) {
        return Err(ty::intersect::Error::Disjoint);
    }

    let mut iter1 = ListIterator::new(list1);
    let mut iter2 = ListIterator::new(list2);

    let mut merged_fixed: Vec<ty::Ref<M>> =
        Vec::with_capacity(cmp::max(iter1.fixed_len(), iter2.fixed_len()));

    while iter1.fixed_len() > 0 || iter2.fixed_len() > 0 {
        let next1 = iter1.next().unwrap();
        let next2 = iter2.next().unwrap();

        let merged_next = intersect_ty_refs(next1, next2)?;
        merged_fixed.push(merged_next);
    }

    let merged_rest = intersect_ty_refs(list1.rest(), list2.rest())?;
    Ok(ty::List::new(merged_fixed.into_boxed_slice(), merged_rest))
}

pub fn intersect_ty_refs<M: ty::Pm>(
    ty_ref1: &ty::Ref<M>,
    ty_ref2: &ty::Ref<M>,
) -> Result<ty::Ref<M>> {
    if ty::is_a::ty_ref_is_a(ty_ref1, ty_ref2) {
        return Ok(ty_ref1.clone());
    } else if ty::is_a::ty_ref_is_a(ty_ref2, ty_ref1) {
        return Ok(ty_ref2.clone());
    }

    match (ty_ref1, ty_ref2) {
        (ty::Ref::Fixed(ty1), ty::Ref::Fixed(ty2)) => {
            // We can invoke full intersection logic if we have fixed types
            non_subty_intersect(ty_ref1, ty1, ty_ref2, ty2)
        }
        _ => {
            let bound1 = ty_ref1.resolve_to_ty();
            let bound2 = ty_ref2.resolve_to_ty();

            // Make sure the bounds aren't disjoint
            // We can't simply `non_subty_intersect` because the bounds may be subtypes
            intersect_ty_refs(&bound1.clone().into(), &bound2.clone().into())?;

            Ok(flatten_ref_intersect(ty_ref1, ty_ref2))
        }
    }
}

pub fn intersect_purity_refs(purity1: &purity::Ref, purity2: &purity::Ref) -> purity::Ref {
    if purity1 == purity2 {
        purity1.clone()
    } else {
        Purity::Pure.into()
    }
}

#[cfg(test)]
mod test {
    use super::*;

    use crate::hir::{poly_for_str, tvar_bounded_by};
    use crate::source::EMPTY_SPAN;

    fn assert_disjoint_poly(poly1: &ty::Ref<ty::Poly>, poly2: &ty::Ref<ty::Poly>) {
        assert_eq!(
            Error::Disjoint,
            intersect_ty_refs(poly1, poly2).unwrap_err()
        );
    }

    fn assert_merged_poly(
        expected: &ty::Ref<ty::Poly>,
        poly1: &ty::Ref<ty::Poly>,
        poly2: &ty::Ref<ty::Poly>,
    ) {
        // This is the basic invariant we're testing - each of our merged type satisfies each of
        // our input types.
        assert!(
            ty::is_a::ty_ref_is_a(expected, poly1),
            "The expected type does not definitely satisfy the first input type; the test is incorrect"
        );
        assert!(
            ty::is_a::ty_ref_is_a(expected, poly2),
            "The expected type does not definitely satisfy the second input type; the test is incorrect"
        );

        assert_eq!(expected, &intersect_ty_refs(poly1, poly2).unwrap());
    }

    fn assert_disjoint(ty_str1: &str, ty_str2: &str) {
        let poly1 = poly_for_str(ty_str1);
        let poly2 = poly_for_str(ty_str2);

        assert_disjoint_poly(&poly1, &poly2)
    }

    fn assert_merged(expected_str: &str, ty_str1: &str, ty_str2: &str) {
        let expected = poly_for_str(expected_str);
        let poly1 = poly_for_str(ty_str1);
        let poly2 = poly_for_str(ty_str2);

        assert_merged_poly(&expected, &poly1, &poly2);
    }

    fn assert_disjoint_iter(ty_strs: &[&str]) {
        let polys: Vec<_> = ty_strs.iter().map(|&s| poly_for_str(s)).collect();

        assert_eq!(
            Error::Disjoint,
            intersect_ty_ref_iter(polys.iter()).unwrap_err()
        );
    }

    fn assert_merged_iter(expected_str: &str, ty_strs: &[&str]) {
        let expected = poly_for_str(expected_str);
        let polys: Vec<_> = ty_strs.iter().map(|&s| poly_for_str(s)).collect();

        assert_eq!(expected, intersect_ty_ref_iter(polys.iter()).unwrap());
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
    fn intersect_types() {
        let ptype = tvar_bounded_by(Ty::Any.into());

        let any_int = poly_for_str("Int");
        let any_float = poly_for_str("Float");

        // These two intersections become disjoint
        assert_eq!(
            Error::Disjoint,
            intersect_ty_refs::<ty::Poly>(
                &Ty::Intersect(Box::new([ptype.clone(), any_int])).into(),
                &Ty::Intersect(Box::new([ptype, any_float])).into(),
            )
            .unwrap_err()
        )
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
        assert_merged("(List Sym Sym)", "(List Any Sym)", "(List & Sym)");
        assert_merged(
            "(List false true)",
            "(List Bool true)",
            "(List false Bool & Any)",
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
    fn unbounded_poly_var() {
        let ptype1 = tvar_bounded_by(Ty::Any.into());
        let ptype2 = tvar_bounded_by(Ty::Any.into());

        let ptype_intersect = Ty::Intersect(Box::new([ptype1.clone(), ptype2.clone()])).into();
        let any_sym = poly_for_str("Sym");

        // These are equal; it should just return the original type
        assert_merged_poly(&ptype1, &ptype1, &ptype1);

        // These create an intersect type
        assert_merged_poly(
            &Ty::Intersect(Box::new([ptype1.clone(), ptype2.clone()])).into(),
            &ptype1,
            &ptype2,
        );

        assert_merged_poly(
            &Ty::Intersect(Box::new([any_sym.clone(), ptype2.clone()])).into(),
            &any_sym,
            &ptype2,
        );

        // These extend an existing intersection
        assert_merged_poly(
            &Ty::Intersect(Box::new([any_sym.clone(), ptype1, ptype2])).into(),
            &any_sym,
            &ptype_intersect,
        );
    }

    #[test]
    fn bounded_poly_vars() {
        let ptype1_any = tvar_bounded_by(Ty::Any.into());
        let ptype2_sym = tvar_bounded_by(Ty::Sym.into());
        let ptype3_str = tvar_bounded_by(Ty::Str.into());

        let any_sym = poly_for_str("Sym");

        assert_merged_poly(
            &Ty::Intersect(Box::new([ptype1_any.clone(), ptype2_sym.clone()])).into(),
            &ptype1_any,
            &ptype2_sym,
        );

        assert_merged_poly(&ptype2_sym, &any_sym, &ptype2_sym);

        // These have disjoint bounds
        assert_disjoint_poly(&ptype2_sym, &ptype3_str);
        assert_disjoint_poly(&ptype3_str, &any_sym);
    }

    #[test]
    fn polymorphic_funs() {
        let pidentity_fun = poly_for_str("(All #{A} A -> A)");
        let pidentity_impure_bool_fun = poly_for_str("(All #{[A Bool]} A ->! A)");
        let top_pure_fun = poly_for_str("(... -> Any)");

        // We should intersect polymorphic functions with themselves
        assert_merged_poly(&pidentity_fun, &pidentity_fun, &pidentity_fun);

        // The intersection of the pure identity function and the top pure function is the identity
        // function
        assert_merged_poly(&pidentity_fun, &pidentity_fun, &top_pure_fun);

        // The intersection of the pure identity function and the impure bool identity function is
        // the identity function
        // TODO: This seems like it should be `(All #{[A Bool]} A -> A)`
        assert_merged_poly(&pidentity_fun, &pidentity_fun, &pidentity_impure_bool_fun);

        // These have no subtype relationship
        // TODO: This also seems like it should be `(All #{[A Bool]} A -> A)`
        assert_disjoint_poly(&pidentity_impure_bool_fun, &top_pure_fun);
    }

    #[test]
    fn intersect_iter() {
        assert_merged_iter("Any", &[]);
        assert_merged_iter("Sym", &["Sym"]);
        assert_merged_iter("true", &["true", "Bool"]);
        assert_disjoint_iter(&["true", "false"]);
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
            cons1,
            TyArgs::new(
                HashMap::new(),
                std::iter::once((tvar1.clone(), Ty::Float.into())).collect(),
            ),
        )
        .into();

        let float_true_instance2_poly: ty::Ref<ty::Poly> = record::Instance::new(
            cons2.clone(),
            TyArgs::new(
                HashMap::new(),
                std::iter::once((tvar1.clone(), Ty::Float.into()))
                    .chain(std::iter::once((tvar2.clone(), Ty::LitBool(true).into())))
                    .collect(),
            ),
        )
        .into();

        let int_true_instance2_poly: ty::Ref<ty::Poly> = record::Instance::new(
            cons2.clone(),
            TyArgs::new(
                HashMap::new(),
                std::iter::once((tvar1.clone(), Ty::Int.into()))
                    .chain(std::iter::once((tvar2.clone(), Ty::LitBool(true).into())))
                    .collect(),
            ),
        )
        .into();

        let int_bool_instance2_poly: ty::Ref<ty::Poly> = record::Instance::new(
            cons2.clone(),
            TyArgs::new(
                HashMap::new(),
                std::iter::once((tvar1.clone(), Ty::Int.into()))
                    .chain(std::iter::once((tvar2.clone(), Ty::Bool.into())))
                    .collect(),
            ),
        )
        .into();

        let num_bool_instance2_poly: ty::Ref<ty::Poly> = record::Instance::new(
            cons2,
            TyArgs::new(
                HashMap::new(),
                std::iter::once((tvar1, Ty::Num.into()))
                    .chain(std::iter::once((tvar2, Ty::Bool.into())))
                    .collect(),
            ),
        )
        .into();

        // Different record constructors
        assert_disjoint_poly(&float_instance1_poly, &float_true_instance2_poly);

        // Disjoint record instances
        assert_disjoint_poly(&float_true_instance2_poly, &int_bool_instance2_poly);

        // Intersectable record types
        assert_merged_poly(
            &int_bool_instance2_poly,
            &int_true_instance2_poly,
            &num_bool_instance2_poly,
        )
    }
}
