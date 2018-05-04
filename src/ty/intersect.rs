use std::iter;
use std::result;

use ty;
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
    fn unify_ty_refs(&self, &S, &S) -> Result<ty::unify::UnifiedTy<S>>;
    fn intersect_purity_refs(&self, &S::PRef, &S::PRef) -> S::PRef;

    fn intersect_list_cons_refs(
        &self,
        list_ref: &S,
        member_ref: &S,
        car_ref: &S,
        cdr_ref: &S,
    ) -> Result<S> {
        let intersected_car = self.intersect_ty_refs(member_ref, car_ref)?;
        let intersected_cdr = self.intersect_ty_refs(list_ref, cdr_ref)?;

        Ok(S::from_ty(ty::Ty::Cons(
            Box::new(intersected_car),
            Box::new(intersected_cdr),
        )))
    }

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
            _ => Ok(S::from_ty(ty::Ty::Union(intersected_types))),
        }
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
            (&ty::Ty::Union(ref refs1), &ty::Ty::Union(ref refs2)) => {
                self.intersect_ref_iter(refs1, refs2.iter())
            }
            (&ty::Ty::Union(ref refs1), _) => self.intersect_ref_iter(refs1, iter::once(ref2)),
            (_, &ty::Ty::Union(ref refs2)) => self.intersect_ref_iter(refs2, iter::once(ref1)),

            // Set type
            (&ty::Ty::Set(ref member1), &ty::Ty::Set(ref member2)) => Ok(S::from_ty(ty::Ty::Set(
                Box::new(self.intersect_ty_refs(member1, member2)?),
            ))),

            // Map type
            (&ty::Ty::Map(ref key1, ref value1), &ty::Ty::Map(ref key2, ref value2)) => {
                Ok(S::from_ty(ty::Ty::Map(
                    Box::new(self.intersect_ty_refs(key1, key2)?),
                    Box::new(self.intersect_ty_refs(value1, value2)?),
                )))
            }

            // Vector types
            (&ty::Ty::Vecof(ref member1), &ty::Ty::Vecof(ref member2)) => Ok(S::from_ty(
                ty::Ty::Vecof(Box::new(self.intersect_ty_refs(member1, member2)?)),
            )),
            (&ty::Ty::Vec(ref members1), &ty::Ty::Vec(ref members2)) => {
                if members1.len() != members2.len() {
                    Err(Error::Disjoint)
                } else {
                    let intersected_members = members1
                        .iter()
                        .zip(members2.iter())
                        .map(|(member1, member2)| self.intersect_ty_refs(member1, member2))
                        .collect::<Result<Vec<S>>>()?;

                    Ok(S::from_ty(ty::Ty::Vec(intersected_members)))
                }
            }
            (&ty::Ty::Vecof(ref member1), &ty::Ty::Vec(ref members2))
            | (&ty::Ty::Vec(ref members2), &ty::Ty::Vecof(ref member1)) => {
                let intersected_members = members2
                    .iter()
                    .map(|member2| self.intersect_ty_refs(member1, member2))
                    .collect::<Result<Vec<S>>>()?;

                Ok(S::from_ty(ty::Ty::Vec(intersected_members)))
            }

            // List types
            (&ty::Ty::Listof(ref member1), &ty::Ty::Listof(ref member2)) => Ok(S::from_ty(
                ty::Ty::Listof(Box::new(self.intersect_ty_refs(member1, member2)?)),
            )),
            (&ty::Ty::Cons(ref car1, ref cdr1), &ty::Ty::Cons(ref car2, ref cdr2)) => {
                Ok(S::from_ty(ty::Ty::Cons(
                    Box::new(self.intersect_ty_refs(car1, car2)?),
                    Box::new(self.intersect_ty_refs(cdr1, cdr2)?),
                )))
            }
            (&ty::Ty::Listof(ref member), &ty::Ty::Cons(ref car, ref cdr)) => {
                self.intersect_list_cons_refs(ref1, member, car, cdr)
            }
            (&ty::Ty::Cons(ref car, ref cdr), &ty::Ty::Listof(ref member)) => {
                self.intersect_list_cons_refs(ref2, member, car, cdr)
            }

            // Function type
            (&ty::Ty::Fun(ref fun1), &ty::Ty::Fun(ref fun2)) => {
                if fun1.is_polymorphic() || fun2.is_polymorphic() {
                    // TODO: This might be possible but we would have to recalculate the tvars for
                    // the intersected function
                    return Err(Error::Disjoint);
                }

                let intersected_purity = self.intersect_purity_refs(fun1.purity(), fun2.purity());
                let intersected_params = match self.unify_ty_refs(fun1.params(), fun2.params())? {
                    ty::unify::UnifiedTy::Merged(merged) => merged,
                    ty::unify::UnifiedTy::Discerned => {
                        return Err(Error::Disjoint);
                    }
                };
                let intersected_ret = self.intersect_ty_refs(fun1.ret(), fun2.ret())?;

                Ok(S::from_ty(ty::Ty::new_fun(
                    intersected_purity,
                    S::TVarIds::empty(),
                    intersected_params,
                    intersected_ret,
                )))
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

        match (&resolved1, &resolved2) {
            (&ty::resolve::Result::Fixed(ty1), &ty::resolve::Result::Fixed(ty2)) => {
                // We can invoke full intersection logic if we have fixed types
                self.non_subty_intersect(poly1, ty1, poly2, ty2)
            }
            _ => {
                // TODO: Can we be more clever here?
                Err(Error::Disjoint)
            }
        }
    }

    fn unify_ty_refs(
        &self,
        poly1: &ty::Poly,
        poly2: &ty::Poly,
    ) -> Result<ty::unify::UnifiedTy<ty::Poly>> {
        ty::unify::poly_unify(self.tvars, poly1, poly2).map_err(|_| Error::Disjoint)
    }

    fn intersect_purity_refs(&self, purity1: &Purity, purity2: &Purity) -> Purity {
        match (purity1, purity2) {
            (&Purity::Impure, &Purity::Impure) => Purity::Impure,
            _ => Purity::Pure,
        }
    }
}

pub fn poly_intersect<'a>(
    tvars: &'a [ty::TVar],
    poly1: &'a ty::Poly,
    poly2: &'a ty::Poly,
) -> Result<ty::Poly> {
    let ctx = PolyIntersectCtx { tvars };
    ctx.intersect_ty_refs(poly1, poly2)
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

    fn unify_ty_refs(
        &self,
        mono1: &ty::Mono,
        mono2: &ty::Mono,
    ) -> Result<ty::unify::UnifiedTy<ty::Mono>> {
        ty::unify::mono_unify(mono1, mono2).map_err(|_| Error::Disjoint)
    }

    fn intersect_purity_refs(&self, purity1: &Purity, purity2: &Purity) -> Purity {
        match (purity1, purity2) {
            (&Purity::Impure, &Purity::Impure) => Purity::Impure,
            _ => Purity::Pure,
        }
    }
}

pub fn mono_intersect<'a>(mono1: &'a ty::Mono, mono2: &'a ty::Mono) -> Result<ty::Mono> {
    let ctx = MonoIntersectCtx {};
    ctx.intersect_ty_refs(mono1, mono2)
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
    fn cons_types() {
        assert_merged("(Cons true false)", "(Cons true Bool)", "(Cons Bool false)");
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
    fn fun_types() {
        assert_disjoint("(Float -> Int)", "(Int -> Int)");
        assert_merged("(-> true)", "(-> Bool)", "(->! true)");
        assert_merged("(Bool -> String)", "(true -> String)", "(false ->! String)");

        assert_disjoint("(String -> Symbol)", "(String String -> Symbol)");
    }

    #[test]
    fn ty_pred_types() {
        assert_disjoint("(Type? String)", "(Type? Symbol)");
        assert_merged("(Type? String)", "(Type? String)", "(Type? String)");
    }

    #[test]
    fn polymorphic_funs() {
        let ptype1_unbounded = ty::Poly::Var(ty::TVarId::new(0));
        let ptype2_string = ty::Poly::Var(ty::TVarId::new(1));

        let tvars = [
            ty::TVar::new("TAny".to_owned(), poly_for_str("Any")),
            ty::TVar::new("TString".to_owned(), poly_for_str("String")),
        ];

        // (All A (A -> A))
        let pidentity_fun = ty::Ty::new_fun(
            Purity::Pure,
            ty::TVarId::new(0)..ty::TVarId::new(1),
            ty::Ty::new_simple_list_type(vec![ptype1_unbounded.clone()].into_iter(), None),
            ptype1_unbounded.clone(),
        ).into_poly();

        // (All A (A A -> (Cons A A))
        let panys_to_cons = ty::Ty::new_fun(
            Purity::Pure,
            ty::TVarId::new(0)..ty::TVarId::new(1),
            ty::Ty::new_simple_list_type(
                vec![ptype1_unbounded.clone(), ptype1_unbounded.clone()].into_iter(),
                None,
            ),
            ty::Ty::Cons(
                Box::new(ptype1_unbounded.clone()),
                Box::new(ptype1_unbounded.clone()),
            ).into_poly(),
        ).into_poly();

        // (All [A : String] (A ->! A))
        let pidentity_impure_string_fun = ty::Ty::new_fun(
            Purity::Impure,
            ty::TVarId::new(1)..ty::TVarId::new(2),
            ty::Ty::new_simple_list_type(vec![ptype2_string.clone()].into_iter(), None),
            ptype2_string.clone(),
        ).into_poly();

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
            poly_intersect(&tvars, &pidentity_fun, &panys_to_cons).unwrap_err()
        );
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
