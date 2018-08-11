use syntax::datum::Datum;
use crate::ty;

trait DatumTyCtx<S: ty::TyRef> {
    fn unify_ref_iter<I>(&self, members: I) -> S
    where
        I: Iterator<Item = S>;

    fn ref_for_datum(&self, datum: &Datum) -> S {
        (match datum {
            Datum::Bool(_, val) => ty::Ty::LitBool(*val),
            Datum::Sym(_, val) => ty::Ty::LitSym(val.clone()),
            Datum::Char(_, _) => ty::Ty::Char,
            Datum::Int(_, _) => ty::Ty::Int,
            Datum::Float(_, _) => ty::Ty::Float,
            Datum::Str(_, _) => ty::Ty::Str,
            Datum::List(_, vs) => ty::Ty::List(ty::List::new(
                vs.iter()
                    .map(|datum| self.ref_for_datum(datum))
                    .collect::<Vec<S>>()
                    .into_boxed_slice(),
                None,
            )),
            Datum::Vec(_, vs) => ty::Ty::Vector(
                vs.iter()
                    .map(|v| self.ref_for_datum(v))
                    .collect::<Vec<S>>()
                    .into_boxed_slice(),
            ),
            Datum::Set(_, vs) => {
                let unified_type = self.unify_ref_iter(vs.iter().map(|v| self.ref_for_datum(v)));
                ty::Ty::Set(Box::new(unified_type))
            }
            Datum::Map(_, vs) => {
                let unified_key =
                    self.unify_ref_iter(vs.iter().map(|(k, _)| self.ref_for_datum(k)));

                let unified_value =
                    self.unify_ref_iter(vs.iter().map(|(_, v)| self.ref_for_datum(v)));

                ty::Ty::Map(Box::new(ty::Map::new(unified_key, unified_value)))
            }
        }).into_ty_ref()
    }
}

struct PolyDatumTyCtx {}

impl DatumTyCtx<ty::Poly> for PolyDatumTyCtx {
    fn unify_ref_iter<I>(&self, iter: I) -> ty::Poly
    where
        I: Iterator<Item = ty::Poly>,
    {
        ty::unify::poly_unify_iter(&[], iter)
    }
}

pub fn poly_for_datum(datum: &Datum) -> ty::Poly {
    let ctx = PolyDatumTyCtx {};
    ctx.ref_for_datum(datum)
}

#[cfg(test)]
mod test {
    use super::*;
    use crate::hir::poly_for_str;

    fn assert_poly_for_str(ty_str: &str, datum_str: &str) {
        use syntax::parser::datum_from_str;
        let datum = datum_from_str(datum_str).unwrap();

        assert_eq!(poly_for_str(ty_str), poly_for_datum(&datum));
    }

    #[test]
    fn trivial_types() {
        assert_poly_for_str("Int", "1");
        assert_poly_for_str("Int", "-51");
        assert_poly_for_str("Char", "\\newline");
        assert_poly_for_str("Str", r#""Test string""#);
    }

    #[test]
    fn bool_literal() {
        assert_poly_for_str("true", "true");
        assert_poly_for_str("false", "false");
    }

    #[test]
    fn sym_literal() {
        assert_poly_for_str("'foo", "foo");
    }

    #[test]
    fn fixed_list() {
        assert_poly_for_str("()", "()");
        assert_poly_for_str("(List Int Int 'foo)", "(1 2 foo)");
    }

    #[test]
    fn fixed_vec() {
        assert_poly_for_str("[]", "[]");
        assert_poly_for_str("(Vector false Int 'foo)", "[false 2 foo]");
    }

    #[test]
    fn fixed_set() {
        assert_poly_for_str("(Setof (RawU))", "#{}");
        assert_poly_for_str("(Setof Bool)", "#{true false}");
    }

    #[test]
    fn fixed_map() {
        assert_poly_for_str("(Map (RawU) (RawU))", "{}");
        assert_poly_for_str("(Map Bool (RawU Int 'foo))", "{true 1, false foo}");
    }
}
