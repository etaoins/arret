use crate::ty;
use syntax::datum::Datum;

pub fn ty_ref_for_datum<S: ty::unify::Unifiable>(datum: &Datum) -> S {
    (match datum {
        Datum::Bool(_, val) => ty::Ty::LitBool(*val),
        Datum::Sym(_, val) => ty::Ty::LitSym(val.clone()),
        Datum::Char(_, _) => ty::Ty::Char,
        Datum::Int(_, _) => ty::Ty::Int,
        Datum::Float(_, _) => ty::Ty::Float,
        Datum::Str(_, _) => ty::Ty::Str,
        Datum::List(_, vs) => ty::Ty::List(ty::List::new(
            vs.iter()
                .map(|datum| ty_ref_for_datum(datum))
                .collect::<Vec<S>>()
                .into_boxed_slice(),
            None,
        )),
        Datum::Vector(_, vs) => ty::Ty::Vector(
            vs.iter()
                .map(|v| ty_ref_for_datum(v))
                .collect::<Vec<S>>()
                .into_boxed_slice(),
        ),
        Datum::Set(_, vs) => {
            let unified_type = ty::unify::unify_ty_ref_iter(
                &ty::TVars::new(),
                vs.iter().map(|v| ty_ref_for_datum(v)),
            );
            ty::Ty::Set(Box::new(unified_type))
        }
        Datum::Map(_, vs) => {
            let unified_key = ty::unify::unify_ty_ref_iter(
                &ty::TVars::new(),
                vs.iter().map(|(k, _)| ty_ref_for_datum(k)),
            );

            let unified_value = ty::unify::unify_ty_ref_iter(
                &ty::TVars::new(),
                vs.iter().map(|(_, v)| ty_ref_for_datum(v)),
            );

            ty::Ty::Map(Box::new(ty::Map::new(unified_key, unified_value)))
        }
    }).into_ty_ref()
}

#[cfg(test)]
mod test {
    use super::*;
    use crate::hir::poly_for_str;

    fn assert_poly_for_str(ty_str: &str, datum_str: &str) {
        use syntax::parser::datum_from_str;
        let datum = datum_from_str(datum_str).unwrap();

        assert_eq!(poly_for_str(ty_str), ty_ref_for_datum(&datum));
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
