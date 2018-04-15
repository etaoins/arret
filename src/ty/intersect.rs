use std::iter;
use std::result;

use ty;
use ty::seq_ty_iter::{ListTyIterator, RevVecTyIterator, SeqTyIterator};

#[derive(Debug, PartialEq)]
pub enum Error {
    /// The two types cannot be intersected as they are disjoint
    Disjoint,
}

pub type Result<S> = result::Result<S, Error>;

struct IntersectedSeq<S>(Vec<S>, Option<S>);

trait IntersectCtx<S>
where
    S: ty::TyRef,
{
    fn intersect_ref(&self, &S, &S) -> Result<S>;
    fn unify_ref(&self, &S, &S) -> ty::unify::Result<ty::unify::UnifiedTy<S>, S>;

    fn intersect_seq<'a, I>(&self, mut iter1: I, mut iter2: I) -> Result<IntersectedSeq<S>>
    where
        S: 'a,
        I: SeqTyIterator<'a, S>,
    {
        let range1 = iter1.size_range();
        let range2 = iter2.size_range();

        if range2.start > range1.end || range2.end < range1.start {
            return Err(Error::Disjoint);
        }

        let mut merged_fixed: Vec<S> = vec![];
        while iter1.fixed_len() > 0 || iter2.fixed_len() > 0 {
            let next1 = iter1.next().unwrap();
            let next2 = iter2.next().unwrap();

            let merged_next = self.intersect_ref(next1, next2)?;
            merged_fixed.push(merged_next);
        }

        let merged_rest = if iter1.is_infinite() && iter2.is_infinite() {
            let rest1 = iter1.next().unwrap();
            let rest2 = iter2.next().unwrap();

            Some(self.intersect_ref(rest1, rest2)?)
        } else {
            None
        };

        Ok(IntersectedSeq(merged_fixed, merged_rest))
    }

    /// Intersects a vector of refs with an iterator
    ///
    /// The `left` is a `Vec` as it needs to be iterated over multiple times. `right` is only
    /// visited once so it can be an arbitrary iterator.
    fn intersect_refs<'a, I>(&self, lefts: &[S], rights: I) -> Result<S>
    where
        S: 'a,
        I: Iterator<Item = &'a S>,
    {
        let mut intersected_types: Vec<S> = vec![];

        for right in rights {
            for left in lefts {
                match self.intersect_ref(left, right) {
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
            (&ty::Ty::Union(ref refs1), &ty::Ty::Union(ref refs2)) => {
                self.intersect_refs(refs1, refs2.iter())
            }
            (&ty::Ty::Union(ref refs1), _) => self.intersect_refs(refs1, iter::once(ref2)),
            (_, &ty::Ty::Union(ref refs2)) => self.intersect_refs(refs2, iter::once(ref1)),
            (&ty::Ty::Set(ref member1), &ty::Ty::Set(ref member2)) => Ok(S::from_ty(ty::Ty::Set(
                Box::new(self.intersect_ref(member1, member2)?),
            ))),
            (&ty::Ty::Hash(ref key1, ref value1), &ty::Ty::Hash(ref key2, ref value2)) => {
                Ok(S::from_ty(ty::Ty::Hash(
                    Box::new(self.intersect_ref(key1, key2)?),
                    Box::new(self.intersect_ref(value1, value2)?),
                )))
            }
            (&ty::Ty::Vec(ref begin1, ref fixed1), &ty::Ty::Vec(ref begin2, ref fixed2)) => {
                self.intersect_seq(
                    RevVecTyIterator::new(begin1, fixed1),
                    RevVecTyIterator::new(begin2, fixed2),
                ).map(|IntersectedSeq(mut fixed, begin)| {
                    fixed.reverse();
                    S::from_ty(ty::Ty::Vec(begin.map(Box::new), fixed))
                })
            }
            (&ty::Ty::List(ref fixed1, ref rest1), &ty::Ty::List(ref fixed2, ref rest2)) => {
                self.intersect_seq(
                    ListTyIterator::new(fixed1, rest1),
                    ListTyIterator::new(fixed2, rest2),
                ).map(|IntersectedSeq(fixed, rest)| {
                    S::from_ty(ty::Ty::List(fixed, rest.map(Box::new)))
                })
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
                        // TODO: Do we need to handle erased types differently?
                        return Err(Error::Disjoint);
                    }
                };
                let intersected_ret = self.intersect_ref(fun1.ret(), fun2.ret())?;

                Ok(S::from_ty(ty::Ty::new_fun(
                    intersected_impure,
                    intersected_params,
                    intersected_ret,
                )))
            }
            (_, _) => Err(Error::Disjoint),
        }
    }
}

struct PolyIntersectCtx<'a> {
    pvars: &'a [ty::PVar],
}

impl<'a> IntersectCtx<ty::Poly> for PolyIntersectCtx<'a> {
    fn intersect_ref(&self, poly1: &ty::Poly, poly2: &ty::Poly) -> Result<ty::Poly> {
        if ty::is_a::poly_is_a(self.pvars, poly1, poly2).to_bool() {
            return Ok(poly1.clone());
        } else if ty::is_a::poly_is_a(self.pvars, poly2, poly1).to_bool() {
            return Ok(poly2.clone());
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
                Err(Error::Disjoint)
            }
        }
    }

    fn unify_ref(
        &self,
        poly1: &ty::Poly,
        poly2: &ty::Poly,
    ) -> ty::unify::Result<ty::unify::UnifiedTy<ty::Poly>, ty::Poly> {
        ty::unify::poly_unify(self.pvars, poly1, poly2)
    }
}

pub fn poly_intersect<'a>(
    pvars: &'a [ty::PVar],
    poly1: &'a ty::Poly,
    poly2: &'a ty::Poly,
) -> Result<ty::Poly> {
    let ctx = PolyIntersectCtx { pvars };
    ctx.intersect_ref(poly1, poly2)
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
            ty::is_a::poly_is_a(&[], &expected, &poly1)
        );
        assert_eq!(
            ty::is_a::Result::Yes,
            ty::is_a::poly_is_a(&[], &expected, &poly2)
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
    fn hash_types() {
        assert_disjoint("(Hash Int Float)", "(Hash Float Int)");
        assert_merged(
            "(Hash 'foo Int)",
            "(Hash (RawU 'foo 'bar) Int)",
            "(Hash (RawU 'foo 'baz) Int)",
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
        assert_disjoint("(Vector Int)", "(Vector Float)");
        assert_merged(
            "(Vector true false)",
            "(Vector Symbol ... true Bool)",
            "(Vector Bool false)",
        );
    }

    #[test]
    fn fun_types() {
        assert_disjoint("(Float -> Int)", "(Int -> Float)");
        assert_merged("(-> true)", "(-> Bool)", "(->! true)");
        assert_merged("(Bool -> String)", "(true -> String)", "(false ->! String)");

        assert_merged(
            "(-> (RawU (List String) (List String String)) Symbol)",
            "(String -> Symbol)",
            "(String String -> Symbol)",
        );
    }
}
