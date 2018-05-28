use std::cmp;
use std::iter;
use std::result;

use ty;
use ty::list_iter::ListIterator;
use ty::purity::Purity;
use ty::TVarIds;

#[derive(PartialEq, Debug)]
pub enum Error {
    Disjoint,
}

type Result<S> = result::Result<S, Error>;

trait IntersectCtx<S>
where
    S: ty::TyRef,
{
    fn intersect_ty_refs(&self, &S, &S) -> Result<S>;
    fn intersect_purity_refs(&self, &S::PRef, &S::PRef) -> S::PRef;
    fn unify_list(&self, &ty::List<S>, &ty::List<S>) -> Result<ty::List<S>>;

    /// Intersects a vector of refs with an iterator
    ///
    /// `lefts` is a slice as it needs to be iterated over multiple times. `rights` is only visited
    /// once so it can be an arbitrary iterator.
    fn intersect_ref_iter<'a, I>(&self, lefts: &[S], rights: I) -> Result<S>
    where
        S: 'a,
        I: Iterator<Item = &'a S>,
    {
        let mut intersected_types: Vec<S> = vec![];

        for right in rights {
            for left in lefts {
                match self.intersect_ty_refs(left, right) {
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

    fn intersect_list(&self, list1: &ty::List<S>, list2: &ty::List<S>) -> Result<ty::List<S>> {
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

            let merged_next = self.intersect_ty_refs(next1, next2)?;
            merged_fixed.push(merged_next);
        }

        let merged_rest = match (list1.rest(), list2.rest()) {
            (Some(rest1), Some(rest2)) => Some(self.intersect_ty_refs(rest1, rest2)?),
            _ => None,
        };

        Ok(ty::List::new(merged_fixed.into_boxed_slice(), merged_rest))
    }

    /// Intersects two types under the assumption that they are not subtypes
    fn non_subty_intersect(
        &self,
        ref1: &S,
        ty1: &ty::Ty<S>,
        ref2: &S,
        ty2: &ty::Ty<S>,
    ) -> Result<S> {
        match (ty1, ty2) {
            // Union types
            (ty::Ty::Union(refs1), ty::Ty::Union(refs2)) => {
                self.intersect_ref_iter(refs1, refs2.iter())
            }
            (ty::Ty::Union(refs1), _) => self.intersect_ref_iter(refs1, iter::once(ref2)),
            (_, ty::Ty::Union(refs2)) => self.intersect_ref_iter(refs2, iter::once(ref1)),

            // Set type
            (ty::Ty::Set(member1), ty::Ty::Set(member2)) => {
                Ok(ty::Ty::Set(Box::new(self.intersect_ty_refs(member1, member2)?)).into_ty_ref())
            }

            // Map type
            (ty::Ty::Map(map1), ty::Ty::Map(map2)) => Ok(ty::Ty::Map(Box::new(ty::Map::new(
                self.intersect_ty_refs(map1.key(), map2.key())?,
                self.intersect_ty_refs(map1.value(), map2.value())?,
            ))).into_ty_ref()),

            // Vector types
            (ty::Ty::Vecof(member1), ty::Ty::Vecof(member2)) => Ok(ty::Ty::Vecof(Box::new(
                self.intersect_ty_refs(member1, member2)?,
            )).into_ty_ref()),
            (ty::Ty::Vec(members1), ty::Ty::Vec(members2)) => {
                if members1.len() != members2.len() {
                    Err(Error::Disjoint)
                } else {
                    let intersected_members = members1
                        .iter()
                        .zip(members2.iter())
                        .map(|(member1, member2)| self.intersect_ty_refs(member1, member2))
                        .collect::<Result<Vec<S>>>()?;

                    Ok(ty::Ty::Vec(intersected_members.into_boxed_slice()).into_ty_ref())
                }
            }
            (ty::Ty::Vecof(member1), ty::Ty::Vec(members2))
            | (ty::Ty::Vec(members2), ty::Ty::Vecof(member1)) => {
                let intersected_members = members2
                    .iter()
                    .map(|member2| self.intersect_ty_refs(member1, member2))
                    .collect::<Result<Vec<S>>>()?;

                Ok(ty::Ty::Vec(intersected_members.into_boxed_slice()).into_ty_ref())
            }

            // List types
            (ty::Ty::List(list1), ty::Ty::List(list2)) => {
                Ok(ty::Ty::List(self.intersect_list(list1, list2)?).into_ty_ref())
            }

            // Function types
            (ty::Ty::TopFun(top_fun1), ty::Ty::TopFun(top_fun2)) => {
                let intersected_purity =
                    self.intersect_purity_refs(top_fun1.purity(), top_fun2.purity());
                let intersected_ret = self.intersect_ty_refs(top_fun1.ret(), top_fun2.ret())?;

                Ok(ty::TopFun::new(intersected_purity, intersected_ret).into_ty_ref())
            }
            (ty::Ty::TopFun(top_fun), ty::Ty::Fun(fun))
            | (ty::Ty::Fun(fun), ty::Ty::TopFun(top_fun)) => {
                if !fun.is_monomorphic() {
                    // TODO: This might be possible but we would have to recalculate the tvars for
                    // the intersected function
                    return Err(Error::Disjoint);
                }

                let intersected_purity = self.intersect_purity_refs(top_fun.purity(), fun.purity());
                let intersected_params = fun.params().clone();
                let intersected_ret = self.intersect_ty_refs(top_fun.ret(), fun.ret())?;

                Ok(ty::Fun::new(
                    ty::purity::PVarIds::monomorphic(),
                    S::TVarIds::monomorphic(),
                    ty::TopFun::new(intersected_purity, intersected_ret),
                    intersected_params,
                ).into_ty_ref())
            }
            (ty::Ty::Fun(fun1), ty::Ty::Fun(fun2)) => {
                if fun1.is_monomorphic() && fun2.is_monomorphic() {
                    let intersected_purity =
                        self.intersect_purity_refs(fun1.purity(), fun2.purity());
                    let intersected_params = self.unify_list(fun1.params(), fun2.params())?;
                    let intersected_ret = self.intersect_ty_refs(fun1.ret(), fun2.ret())?;

                    Ok(ty::Fun::new(
                        ty::purity::PVarIds::monomorphic(),
                        S::TVarIds::monomorphic(),
                        ty::TopFun::new(intersected_purity, intersected_ret),
                        intersected_params,
                    ).into_ty_ref())
                } else {
                    // TODO: Same issue as top functions
                    Err(Error::Disjoint)
                }
            }
            (_, _) => Err(Error::Disjoint),
        }
    }
}

struct PolyIntersectCtx<'a> {
    tvars: &'a [ty::TVar],
}

impl<'a> IntersectCtx<ty::Poly> for PolyIntersectCtx<'a> {
    fn intersect_ty_refs(&self, poly1: &ty::Poly, poly2: &ty::Poly) -> Result<ty::Poly> {
        if ty::is_a::poly_is_a(self.tvars, poly1, poly2).to_bool() {
            return Ok(poly1.clone());
        } else if ty::is_a::poly_is_a(self.tvars, poly2, poly1).to_bool() {
            return Ok(poly2.clone());
        }

        // Determine if we're dealing with fixed types or polymorphic bounds
        let resolved1 = ty::resolve::resolve_poly_ty(self.tvars, poly1);
        let resolved2 = ty::resolve::resolve_poly_ty(self.tvars, poly2);

        match (resolved1, resolved2) {
            (ty::resolve::Result::Fixed(ty1), ty::resolve::Result::Fixed(ty2)) => {
                // We can invoke full intersection logic if we have fixed types
                self.non_subty_intersect(poly1, ty1, poly2, ty2)
            }
            _ => {
                // TODO: Can we be more clever here?
                Err(Error::Disjoint)
            }
        }
    }

    fn intersect_purity_refs(
        &self,
        purity1: &ty::purity::Poly,
        purity2: &ty::purity::Poly,
    ) -> ty::purity::Poly {
        if purity1 == purity2 {
            purity1.clone()
        } else {
            Purity::Pure.into_poly()
        }
    }

    fn unify_list(
        &self,
        list1: &ty::List<ty::Poly>,
        list2: &ty::List<ty::Poly>,
    ) -> Result<ty::List<ty::Poly>> {
        match ty::unify::poly_unify_list(self.tvars, list1, list2) {
            Ok(ty::unify::UnifiedList::Merged(merged)) => Ok(merged),
            _ => Err(Error::Disjoint),
        }
    }
}

pub fn poly_intersect(tvars: &[ty::TVar], poly1: &ty::Poly, poly2: &ty::Poly) -> Result<ty::Poly> {
    let ctx = PolyIntersectCtx { tvars };
    ctx.intersect_ty_refs(poly1, poly2)
}

pub fn poly_intersect_list(
    tvars: &[ty::TVar],
    list1: &ty::List<ty::Poly>,
    list2: &ty::List<ty::Poly>,
) -> Result<ty::List<ty::Poly>> {
    let ctx = PolyIntersectCtx { tvars };
    ctx.intersect_list(list1, list2)
}

struct MonoIntersectCtx {}

impl<'a> IntersectCtx<ty::Mono> for MonoIntersectCtx {
    fn intersect_ty_refs(&self, mono1: &ty::Mono, mono2: &ty::Mono) -> Result<ty::Mono> {
        if ty::is_a::mono_is_a(mono1, mono2).to_bool() {
            return Ok(mono1.clone());
        } else if ty::is_a::mono_is_a(mono2, mono1).to_bool() {
            return Ok(mono2.clone());
        }

        self.non_subty_intersect(mono1, mono1.as_ty(), mono2, mono2.as_ty())
    }

    fn intersect_purity_refs(&self, purity1: &Purity, purity2: &Purity) -> Purity {
        if purity1 == purity2 {
            *purity1
        } else {
            Purity::Pure
        }
    }

    fn unify_list(
        &self,
        list1: &ty::List<ty::Mono>,
        list2: &ty::List<ty::Mono>,
    ) -> Result<ty::List<ty::Mono>> {
        match ty::unify::mono_unify_list(list1, list2) {
            Ok(ty::unify::UnifiedList::Merged(merged)) => Ok(merged),
            _ => Err(Error::Disjoint),
        }
    }
}

pub fn mono_intersect_list(
    list1: &ty::List<ty::Mono>,
    list2: &ty::List<ty::Mono>,
) -> Result<ty::List<ty::Mono>> {
    let ctx = MonoIntersectCtx {};
    ctx.intersect_list(list1, list2)
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
            Error::Disjoint,
            poly_intersect(&[], &poly1, &poly2).unwrap_err()
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
            ty::is_a::poly_is_a(&[], &expected, &poly1),
            "The expected type does not definitely satisfy the first input type; the test is incorrect"
        );
        assert_eq!(
            ty::is_a::Result::Yes,
            ty::is_a::poly_is_a(&[], &expected, &poly2),
            "The expected type does not definitely satisfy the second input type; the test is incorrect"
        );

        assert_eq!(expected, poly_intersect(&[], &poly1, &poly2).unwrap());
    }

    #[test]
    fn disjoint_types() {
        assert_disjoint("Symbol", "String");
    }

    #[test]
    fn simple_subtypes() {
        assert_merged("true", "Bool", "true");
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
        assert_disjoint("(Setof Symbol)", "(Setof String)");
        assert_merged(
            "(Setof 'foo)",
            "(Setof (RawU 'foo 'bar))",
            "(Setof (RawU 'foo 'baz))",
        );
    }

    #[test]
    fn list_types() {
        assert_disjoint("(List Symbol)", "(List String)");
        assert_merged(
            "(List Symbol Symbol)",
            "(List Any Symbol)",
            "(List Symbol ...)",
        );
        assert_merged(
            "(List false true)",
            "(List Bool true)",
            "(List false Bool Any ...)",
        );

        assert_disjoint("(List Symbol Symbol)", "(List Symbol)");
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
        assert_merged(
            "((RawU Float Int) -> Int)",
            "(Float -> Int)",
            "(Int -> Int)",
        );
        assert_disjoint("(String -> Symbol)", "(String String -> Symbol)");
        assert_merged("(-> true)", "(-> Bool)", "(->! true)");
        assert_merged("(Bool -> String)", "(true -> String)", "(false ->! String)");

        assert_merged("(->! true)", "(... ->! true)", "(->! Any)");
    }

    #[test]
    fn ty_pred_types() {
        assert_disjoint("(Type? String)", "(Type? Symbol)");
        assert_merged("(Type? String)", "(Type? String)", "(Type? String)");

        assert_merged("(Type? String)", "(Type? String)", "(Any -> Bool)");

        assert_merged("(Type? String)", "(Type? String)", "(... -> Bool)");
    }

    #[test]
    fn polymorphic_funs() {
        let ptype1_unbounded = ty::Poly::Var(ty::TVarId::new(0));
        let ptype2_string = ty::Poly::Var(ty::TVarId::new(1));

        let tvars = [
            ty::TVar::new("TAny".into(), poly_for_str("Any")),
            ty::TVar::new("TString".into(), poly_for_str("String")),
        ];

        // (All A (A -> A))
        let pidentity_fun = ty::Fun::new(
            ty::purity::PVarIds::monomorphic(),
            ty::TVarId::new(0)..ty::TVarId::new(1),
            ty::TopFun::new(Purity::Pure.into_poly(), ptype1_unbounded.clone()),
            ty::List::new(Box::new([ptype1_unbounded.clone()]), None),
        ).into_ty_ref();

        // (All [A : String] (A ->! A))
        let pidentity_impure_string_fun = ty::Fun::new(
            ty::purity::PVarIds::monomorphic(),
            ty::TVarId::new(1)..ty::TVarId::new(2),
            ty::TopFun::new(Purity::Impure.into_poly(), ptype2_string.clone()),
            ty::List::new(Box::new([ptype2_string.clone()]), None),
        ).into_ty_ref();

        let top_pure_fun = poly_for_str("(... -> Any)");

        // We should intersect polymorphic functions with themselves
        assert_eq!(
            pidentity_fun.clone(),
            poly_intersect(&tvars, &pidentity_fun, &pidentity_fun).unwrap()
        );

        // The intersection of the pure identity function and the top pure function is the identity
        // function
        assert_eq!(
            pidentity_fun.clone(),
            poly_intersect(&tvars, &pidentity_fun, &top_pure_fun).unwrap()
        );

        // The intersections of the two polymorphic functions is disjoint
        assert_eq!(
            Error::Disjoint,
            poly_intersect(&tvars, &pidentity_fun, &pidentity_impure_string_fun).unwrap_err()
        );

        // Pure and impure functions are disjoint
        // TODO: There is a better answer here of `(All [A : String] (A ->! A))`. This is just
        // checking we don't create a nonsense type.
        assert_eq!(
            Error::Disjoint,
            poly_intersect(&tvars, &pidentity_impure_string_fun, &top_pure_fun).unwrap_err()
        );
    }
}
