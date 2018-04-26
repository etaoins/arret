use std::iter;
use std::result::Result;

use ty;

#[derive(PartialEq, Debug)]
pub enum IntersectedTy<S> {
    /// The types are disjoint; their intersection would be the empty union
    Disjoint,

    /// The types can be intersected in to a new type
    Merged(S),
}

impl<S> IntersectedTy<S>
where
    S: ty::TyRef,
{
    pub fn into_ty_ref(self) -> S {
        match self {
            IntersectedTy::Disjoint => S::from_ty(ty::Ty::Union(vec![])),
            IntersectedTy::Merged(merged) => merged,
        }
    }
}

trait IntersectCtx<S>
where
    S: ty::TyRef,
{
    fn intersect_ref(&self, &S, &S) -> IntersectedTy<S>;
    fn unify_ref(&self, &S, &S) -> Result<ty::unify::UnifiedTy<S>, ()>;

    fn intersect_list_cons_refs(
        &self,
        list_ref: &S,
        member_ref: &S,
        car_ref: &S,
        cdr_ref: &S,
    ) -> IntersectedTy<S> {
        let intersected_car = self.intersect_ref(member_ref, car_ref).into_ty_ref();
        let intersected_cdr = self.intersect_ref(list_ref, cdr_ref).into_ty_ref();

        IntersectedTy::Merged(S::from_ty(ty::Ty::Cons(
            Box::new(intersected_car),
            Box::new(intersected_cdr),
        )))
    }

    /// Intersects a vector of refs with an iterator
    ///
    /// The `left` is a `Vec` as it needs to be iterated over multiple times. `right` is only
    /// visited once so it can be an arbitrary iterator.
    fn intersect_refs<'a, I>(&self, lefts: &[S], rights: I) -> IntersectedTy<S>
    where
        S: 'a,
        I: Iterator<Item = &'a S>,
    {
        let mut intersected_types: Vec<S> = vec![];

        for right in rights {
            for left in lefts {
                match self.intersect_ref(left, right) {
                    IntersectedTy::Disjoint => {}
                    IntersectedTy::Merged(intersected) => {
                        intersected_types.push(intersected);
                    }
                }
            }
        }

        match intersected_types.len() {
            0 => IntersectedTy::Disjoint,
            1 => IntersectedTy::Merged(intersected_types.pop().unwrap()),
            _ => IntersectedTy::Merged(S::from_ty(ty::Ty::Union(intersected_types))),
        }
    }

    /// Intersects two types under the assumption that they are not subtypes
    fn non_subty_intersect(
        &self,
        ref1: &S,
        ty1: &ty::Ty<S>,
        ref2: &S,
        ty2: &ty::Ty<S>,
    ) -> IntersectedTy<S> {
        match (ty1, ty2) {
            // Union types
            (&ty::Ty::Union(ref refs1), &ty::Ty::Union(ref refs2)) => {
                self.intersect_refs(refs1, refs2.iter())
            }
            (&ty::Ty::Union(ref refs1), _) => self.intersect_refs(refs1, iter::once(ref2)),
            (_, &ty::Ty::Union(ref refs2)) => self.intersect_refs(refs2, iter::once(ref1)),

            // Set type
            (&ty::Ty::Set(ref member1), &ty::Ty::Set(ref member2)) => {
                IntersectedTy::Merged(S::from_ty(ty::Ty::Set(Box::new(self.intersect_ref(
                    member1, member2,
                ).into_ty_ref()))))
            }

            // Map type
            (&ty::Ty::Map(ref key1, ref value1), &ty::Ty::Map(ref key2, ref value2)) => {
                IntersectedTy::Merged(S::from_ty(ty::Ty::Map(
                    Box::new(self.intersect_ref(key1, key2).into_ty_ref()),
                    Box::new(self.intersect_ref(value1, value2).into_ty_ref()),
                )))
            }

            // Vector types
            (&ty::Ty::Vecof(ref member1), &ty::Ty::Vecof(ref member2)) => {
                IntersectedTy::Merged(S::from_ty(ty::Ty::Vecof(Box::new(self.intersect_ref(
                    member1, member2,
                ).into_ty_ref()))))
            }
            (&ty::Ty::Vec(ref members1), &ty::Ty::Vec(ref members2)) => {
                if members1.len() != members2.len() {
                    IntersectedTy::Disjoint
                } else {
                    let intersected_members = members1
                        .iter()
                        .zip(members2.iter())
                        .map(|(member1, member2)| {
                            self.intersect_ref(member1, member2).into_ty_ref()
                        })
                        .collect::<Vec<S>>();

                    IntersectedTy::Merged(S::from_ty(ty::Ty::Vec(intersected_members)))
                }
            }
            (&ty::Ty::Vecof(ref member1), &ty::Ty::Vec(ref members2))
            | (&ty::Ty::Vec(ref members2), &ty::Ty::Vecof(ref member1)) => {
                let intersected_members = members2
                    .iter()
                    .map(|member2| self.intersect_ref(member1, member2).into_ty_ref())
                    .collect::<Vec<S>>();

                IntersectedTy::Merged(S::from_ty(ty::Ty::Vec(intersected_members)))
            }

            // List types
            (&ty::Ty::Listof(ref member1), &ty::Ty::Listof(ref member2)) => {
                IntersectedTy::Merged(S::from_ty(ty::Ty::Listof(Box::new(self.intersect_ref(
                    member1, member2,
                ).into_ty_ref()))))
            }
            (&ty::Ty::Cons(ref car1, ref cdr1), &ty::Ty::Cons(ref car2, ref cdr2)) => {
                IntersectedTy::Merged(S::from_ty(ty::Ty::Cons(
                    Box::new(self.intersect_ref(car1, car2).into_ty_ref()),
                    Box::new(self.intersect_ref(cdr1, cdr2).into_ty_ref()),
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
                let intersected_impure = fun1.impure() && fun2.impure();
                let intersected_params = match self.unify_ref(fun1.params(), fun2.params()) {
                    Ok(ty::unify::UnifiedTy::Merged(merged)) => merged,
                    Ok(ty::unify::UnifiedTy::Discerned) => S::from_ty(ty::Ty::Union(vec![
                        fun1.params().clone(),
                        fun2.params().clone(),
                    ])),
                    _ => {
                        return IntersectedTy::Disjoint;
                    }
                };
                let intersected_ret = self.intersect_ref(fun1.ret(), fun2.ret()).into_ty_ref();

                IntersectedTy::Merged(S::from_ty(ty::Ty::new_fun(
                    intersected_impure,
                    intersected_params,
                    intersected_ret,
                )))
            }
            (_, _) => IntersectedTy::Disjoint,
        }
    }
}

struct PolyIntersectCtx<'a> {
    pvars: &'a [ty::PVar],
}

impl<'a> IntersectCtx<ty::Poly> for PolyIntersectCtx<'a> {
    fn intersect_ref(&self, poly1: &ty::Poly, poly2: &ty::Poly) -> IntersectedTy<ty::Poly> {
        if ty::is_a::poly_is_a(self.pvars, poly1, poly2).to_bool() {
            return IntersectedTy::Merged(poly1.clone());
        } else if ty::is_a::poly_is_a(self.pvars, poly2, poly1).to_bool() {
            return IntersectedTy::Merged(poly2.clone());
        }

        // Determine if we're dealing with fixed types or polymorphic bounds
        let resolved1 = ty::resolve::resolve_poly_ty(self.pvars, poly1);
        let resolved2 = ty::resolve::resolve_poly_ty(self.pvars, poly2);

        match (&resolved1, &resolved2) {
            (&ty::resolve::Result::Fixed(ty1), &ty::resolve::Result::Fixed(ty2)) => {
                // We can invoke full intersection logic if we have fixed types
                self.non_subty_intersect(poly1, ty1, poly2, ty2)
            }
            _ => {
                // TODO: Can we be more clever here?
                IntersectedTy::Disjoint
            }
        }
    }

    fn unify_ref(
        &self,
        poly1: &ty::Poly,
        poly2: &ty::Poly,
    ) -> Result<ty::unify::UnifiedTy<ty::Poly>, ()> {
        ty::unify::poly_unify(self.pvars, poly1, poly2).map_err(|_| ())
    }
}

pub fn poly_intersect<'a>(
    pvars: &'a [ty::PVar],
    poly1: &'a ty::Poly,
    poly2: &'a ty::Poly,
) -> IntersectedTy<ty::Poly> {
    let ctx = PolyIntersectCtx { pvars };
    ctx.intersect_ref(poly1, poly2)
}

struct MonoIntersectCtx {}

impl<'a> IntersectCtx<ty::Mono> for MonoIntersectCtx {
    fn intersect_ref(&self, mono1: &ty::Mono, mono2: &ty::Mono) -> IntersectedTy<ty::Mono> {
        if ty::is_a::mono_is_a(mono1, mono2).to_bool() {
            return IntersectedTy::Merged(mono1.clone());
        } else if ty::is_a::mono_is_a(mono2, mono1).to_bool() {
            return IntersectedTy::Merged(mono2.clone());
        }

        self.non_subty_intersect(mono1, mono1.as_ty(), mono2, mono2.as_ty())
    }

    fn unify_ref(
        &self,
        mono1: &ty::Mono,
        mono2: &ty::Mono,
    ) -> Result<ty::unify::UnifiedTy<ty::Mono>, ()> {
        ty::unify::mono_unify(mono1, mono2).map_err(|_| ())
    }
}

pub fn mono_intersect<'a>(mono1: &'a ty::Mono, mono2: &'a ty::Mono) -> IntersectedTy<ty::Mono> {
    let ctx = MonoIntersectCtx {};
    ctx.intersect_ref(mono1, mono2)
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

        assert_eq!(IntersectedTy::Disjoint, poly_intersect(&[], &poly1, &poly2));
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

        assert_eq!(
            IntersectedTy::Merged(expected),
            poly_intersect(&[], &poly1, &poly2)
        );
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
        assert_merged("(Map (RawU) (RawU))", "(Map Int Float)", "(Map Float Int)");
        assert_merged(
            "(Map 'foo Int)",
            "(Map (RawU 'foo 'bar) Int)",
            "(Map (RawU 'foo 'baz) Int)",
        );
    }

    #[test]
    fn set_types() {
        assert_merged("(Setof (RawU))", "(Setof Symbol)", "(Setof String)");
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
        assert_merged("(List (RawU))", "(List Symbol)", "(List String)");
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

        // This is tricky - we can dereference the first element but we can't go further
        //
        // This requires `(Cons)` syntax as it's no longer a simple list type
        assert_merged(
            "(Cons Symbol (RawU))",
            "(List Symbol Symbol)",
            "(List Symbol)",
        );
    }

    #[test]
    fn vec_types() {
        assert_merged("(Vector (RawU))", "(Vector Int)", "(Vector Float)");
        assert_merged("(Vector true)", "(Vector Bool)", "(Vectorof true)");
        assert_merged("(Vectorof false)", "(Vectorof Bool)", "(Vectorof false)");
    }

    #[test]
    fn fun_types() {
        // TODO: Our list union code is trying to be helpful by making the param lists disjoint to
        // allow for runtime type checks. However, it just ends up producing a weird (but correct)
        // type.
        assert_merged(
            "(Fn (RawU (List Float) (List Int)) (RawU))",
            "(Float -> Int)",
            "(Int -> Float)",
        );
        assert_merged("(-> true)", "(-> Bool)", "(->! true)");
        assert_merged("(Bool -> String)", "(true -> String)", "(false ->! String)");

        assert_merged(
            "(Fn (RawU (List String) (List String String)) Symbol)",
            "(String -> Symbol)",
            "(String String -> Symbol)",
        );
    }

    #[test]
    fn ty_pred_types() {
        assert_disjoint("(Type? String)", "(Type? Symbol)");
        assert_merged("(Type? String)", "(Type? String)", "(Type? String)");
    }
}
