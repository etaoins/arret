use syntax::datum::Datum;
use ty;

trait DatumTyContext<S>
where
    S: ty::TyRef,
{
    fn unify_ref_iter<I>(&self, members: I) -> S
    where
        I: Iterator<Item = S>;

    fn new_simple_list_type<I>(fixed: I, rest: Option<S>) -> ty::Ty<S>
    where
        I: DoubleEndedIterator<Item = S>,
    {
        let tail_poly = rest.map(|t| ty::Ty::Listof(Box::new(t)))
            .unwrap_or_else(|| ty::Ty::Nil);

        fixed.rev().fold(tail_poly, |tail_ty, fixed_ref| {
            ty::Ty::Cons(Box::new(fixed_ref), Box::new(tail_ty.into_ref()))
        })
    }

    fn ref_for_datum(&self, datum: &Datum) -> S
    where
        S: ty::TyRef,
    {
        (match *datum {
            Datum::Bool(_, val) => ty::Ty::LitBool(val),
            Datum::Sym(_, ref val) => ty::Ty::LitSym(val.clone()),
            Datum::Char(_, _) => ty::Ty::Char,
            Datum::Int(_, _) => ty::Ty::Int,
            Datum::Float(_, _) => ty::Ty::Float,
            Datum::Str(_, _) => ty::Ty::Str,
            Datum::List(_, ref vs) => {
                Self::new_simple_list_type(vs.iter().map(|datum| self.ref_for_datum(datum)), None)
            }
            Datum::Vec(_, ref vs) => {
                ty::Ty::Vec(vs.iter().map(|v| self.ref_for_datum(v)).collect())
            }
            Datum::Set(_, ref vs) => {
                // Without function, begin or rest types we should never hit erasure
                let unified_type = self.unify_ref_iter(vs.iter().map(|v| self.ref_for_datum(v)));

                ty::Ty::Set(Box::new(unified_type))
            }
            Datum::Map(_, ref vs) => {
                let unified_key =
                    self.unify_ref_iter(vs.iter().map(|&(ref k, _)| self.ref_for_datum(k)));

                let unified_value =
                    self.unify_ref_iter(vs.iter().map(|&(_, ref v)| self.ref_for_datum(v)));

                ty::Ty::Map(Box::new(unified_key), Box::new(unified_value))
            }
        }).into_ref()
    }
}

struct MonoDatumTyContext {}

impl DatumTyContext<ty::Mono> for MonoDatumTyContext {
    fn unify_ref_iter<I>(&self, iter: I) -> ty::Mono
    where
        I: Iterator<Item = ty::Mono>,
    {
        ty::unify::mono_unify_iter(iter).unwrap()
    }
}

pub fn mono_for_datum(datum: &Datum) -> ty::Mono {
    let ctx = MonoDatumTyContext {};
    ctx.ref_for_datum(datum)
}

struct PolyDatumTyContext {}

impl DatumTyContext<ty::Poly> for PolyDatumTyContext {
    fn unify_ref_iter<I>(&self, iter: I) -> ty::Poly
    where
        I: Iterator<Item = ty::Poly>,
    {
        ty::unify::poly_unify_iter(&[], iter).unwrap()
    }
}

pub fn poly_for_datum(datum: &Datum) -> ty::Poly {
    let ctx = PolyDatumTyContext {};
    ctx.ref_for_datum(datum)
}

#[cfg(test)]
mod test {
    use super::*;
    use hir::poly_for_str;

    fn mono_for_str(ty_str: &str) -> ty::Mono {
        use std::collections::HashMap;

        let poly = poly_for_str(ty_str).unwrap();
        ty::subst::subst(&HashMap::new(), &HashMap::new(), &poly).unwrap()
    }

    fn assert_mono_for_str(ty_str: &str, datum_str: &str) {
        use syntax::parser::datum_from_str;
        let datum = datum_from_str(datum_str).unwrap();

        assert_eq!(mono_for_str(ty_str), mono_for_datum(&datum));
        assert_eq!(poly_for_str(ty_str).unwrap(), poly_for_datum(&datum));
    }

    #[test]
    fn trivial_types() {
        assert_mono_for_str("Int", "1");
        assert_mono_for_str("Int", "-51");
        assert_mono_for_str("Char", "\\newline");
        assert_mono_for_str("String", r#""Test string""#);
    }

    #[test]
    fn bool_literal() {
        assert_mono_for_str("true", "true");
        assert_mono_for_str("false", "false");
    }

    #[test]
    fn sym_literal() {
        assert_mono_for_str("'foo", "foo");
    }

    #[test]
    fn fixed_list() {
        assert_mono_for_str("(List)", "()");
        assert_mono_for_str("(List Int Int 'foo)", "(1 2 foo)");
    }

    #[test]
    fn fixed_vec() {
        assert_mono_for_str("(Vector)", "[]");
        assert_mono_for_str("(Vector false Int 'foo)", "[false 2 foo]");
    }

    #[test]
    fn fixed_set() {
        assert_mono_for_str("(Setof (RawU))", "#{}");
        assert_mono_for_str("(Setof Bool)", "#{true false}");
    }

    #[test]
    fn fixed_map() {
        assert_mono_for_str("(Map (RawU) (RawU))", "{}");
        assert_mono_for_str("(Map Bool (RawU Int 'foo))", "{true 1, false foo}");
    }
}
