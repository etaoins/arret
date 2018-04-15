use std::iter;
use std::result::Result;

use ty;
use ty::seq_ty_iter::{ListTyIterator, RevVecTyIterator, SeqTyIterator};

#[derive(PartialEq, Debug)]
pub enum IntersectedTy<S> {
    Disjoint,
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

enum IntersectedSeq<S> {
    Disjoint,
    Merged(Vec<S>, Option<S>),
}

trait IntersectCtx<S>
where
    S: ty::TyRef,
{
    fn intersect_ref(&self, &S, &S) -> IntersectedTy<S>;
    fn unify_ref(&self, &S, &S) -> Result<ty::unify::UnifiedTy<S>, ()>;

    fn intersect_seq<'a, I>(&self, mut iter1: I, mut iter2: I) -> IntersectedSeq<S>
    where
        S: 'a,
        I: SeqTyIterator<'a, S>,
    {
        let range1 = iter1.size_range();
        let range2 = iter2.size_range();

        if range2.start > range1.end || range2.end < range1.start {
            return IntersectedSeq::Disjoint;
        }

        let mut merged_fixed: Vec<S> = vec![];
        while iter1.fixed_len() > 0 || iter2.fixed_len() > 0 {
            let next1 = iter1.next().unwrap();
            let next2 = iter2.next().unwrap();

            let merged_next = self.intersect_ref(next1, next2);
            merged_fixed.push(merged_next.into_ty_ref());
        }

        let merged_rest = if iter1.is_infinite() && iter2.is_infinite() {
            let rest1 = iter1.next().unwrap();
            let rest2 = iter2.next().unwrap();

            Some(self.intersect_ref(rest1, rest2).into_ty_ref())
        } else {
            None
        };

        IntersectedSeq::Merged(merged_fixed, merged_rest)
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
            (&ty::Ty::Union(ref refs1), &ty::Ty::Union(ref refs2)) => {
                self.intersect_refs(refs1, refs2.iter())
            }
            (&ty::Ty::Union(ref refs1), _) => self.intersect_refs(refs1, iter::once(ref2)),
            (_, &ty::Ty::Union(ref refs2)) => self.intersect_refs(refs2, iter::once(ref1)),
            (&ty::Ty::Set(ref member1), &ty::Ty::Set(ref member2)) => IntersectedTy::Merged(
                S::from_ty(ty::Ty::Set(Box::new(
                    self.intersect_ref(member1, member2).into_ty_ref(),
                ))),
            ),
            (&ty::Ty::Map(ref key1, ref value1), &ty::Ty::Map(ref key2, ref value2)) => {
                IntersectedTy::Merged(S::from_ty(ty::Ty::Map(
                    Box::new(self.intersect_ref(key1, key2).into_ty_ref()),
                    Box::new(self.intersect_ref(value1, value2).into_ty_ref()),
                )))
            }
            (&ty::Ty::Vec(ref begin1, ref fixed1), &ty::Ty::Vec(ref begin2, ref fixed2)) => {
                match self.intersect_seq(
                    RevVecTyIterator::new(begin1, fixed1),
                    RevVecTyIterator::new(begin2, fixed2),
                ) {
                    IntersectedSeq::Merged(mut fixed, begin) => {
                        fixed.reverse();
                        IntersectedTy::Merged(S::from_ty(ty::Ty::Vec(begin.map(Box::new), fixed)))
                    }
                    IntersectedSeq::Disjoint => IntersectedTy::Disjoint,
                }
            }
            (&ty::Ty::List(ref fixed1, ref rest1), &ty::Ty::List(ref fixed2, ref rest2)) => {
                match self.intersect_seq(
                    ListTyIterator::new(fixed1, rest1),
                    ListTyIterator::new(fixed2, rest2),
                ) {
                    IntersectedSeq::Merged(fixed, rest) => {
                        IntersectedTy::Merged(S::from_ty(ty::Ty::List(fixed, rest.map(Box::new))))
                    }
                    IntersectedSeq::Disjoint => IntersectedTy::Disjoint,
                }
            }
            (&ty::Ty::Fun(ref fun1), &ty::Ty::Fun(ref fun2)) => {
                let intersected_impure = fun1.impure() && fun2.impure();
                let intersected_params = match self.unify_ref(fun1.params(), fun2.params()) {
                    Ok(ty::unify::UnifiedTy::Merged(merged)) => merged,
                    Ok(ty::unify::UnifiedTy::Disjoint) => S::from_ty(ty::Ty::Union(vec![
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
    fn list_types() {
        assert_merged("(List (RawU))", "(List Symbol)", "(List String)");
        assert_disjoint("(List Symbol Symbol)", "(List Symbol)");
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
    }

    #[test]
    fn vec_types() {
        assert_merged("(Vector (RawU))", "(Vector Int)", "(Vector Float)");
        assert_merged(
            "(Vector true false)",
            "(Vector Symbol ... true Bool)",
            "(Vector Bool false)",
        );
    }

    #[test]
    fn fun_types() {
        // TODO: Our list union code is trying to be helpful by making the param lists disjoint to
        // allow for runtime type checks. However, it just ends up producing a weird (but correct)
        // type.
        assert_merged(
            "(-> (RawU (List Float) (List Int)) (RawU))",
            "(Float -> Int)",
            "(Int -> Float)",
        );
        assert_merged("(-> true)", "(-> Bool)", "(->! true)");
        assert_merged("(Bool -> String)", "(true -> String)", "(false ->! String)");

        assert_merged(
            "(-> (RawU (List String) (List String String)) Symbol)",
            "(String -> Symbol)",
            "(String String -> Symbol)",
        );
    }
}
