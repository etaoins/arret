use syntax::datum::Datum;
use ty;

pub fn mono_for_datum(datum: &Datum) -> ty::Mono {
    match *datum {
        Datum::Bool(_, val) => ty::Ty::LitBool(val).into_mono(),
        Datum::Sym(_, ref val) => ty::Ty::LitSym(val.clone()).into_mono(),
        Datum::Char(_, _) => ty::Ty::Char.into_mono(),
        Datum::Int(_, _) => ty::Ty::Int.into_mono(),
        Datum::Float(_, _) => ty::Ty::Float.into_mono(),
        Datum::Str(_, _) => ty::Ty::Str.into_mono(),
        Datum::List(_, ref vs) => {
            ty::Ty::List(vs.iter().map(mono_for_datum).collect(), None).into_mono()
        }
        Datum::Vec(_, ref vs) => {
            ty::Ty::Vec(None, vs.iter().map(mono_for_datum).collect()).into_mono()
        }
        Datum::Set(_, ref vs) => {
            // Without function, begin or rest types we should never hit erasure
            let unified_type = ty::unify::mono_unify_iter(vs.iter().map(mono_for_datum)).unwrap();
            ty::Ty::Set(Box::new(unified_type)).into_mono()
        }
        Datum::Map(_, ref vs) => {
            let unified_key =
                ty::unify::mono_unify_iter(vs.iter().map(|&(ref k, _)| mono_for_datum(k))).unwrap();

            let unified_value =
                ty::unify::mono_unify_iter(vs.iter().map(|&(_, ref v)| mono_for_datum(v))).unwrap();

            ty::Ty::Hash(Box::new(unified_key), Box::new(unified_value)).into_mono()
        }
    }
}

#[cfg(test)]
mod test {
    use super::*;

    fn mono_for_str(ty_str: &str) -> ty::Mono {
        use std::collections::HashMap;
        use hir;

        let poly = hir::poly_for_str(ty_str).unwrap();
        ty::subst::subst(&poly, &HashMap::new()).unwrap()
    }

    fn assert_mono_for_str(ty_str: &str, datum_str: &str) {
        use syntax::parser::datum_from_str;
        let datum = datum_from_str(datum_str).unwrap();

        assert_eq!(mono_for_str(ty_str), mono_for_datum(&datum));
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
    fn fixed_hash() {
        assert_mono_for_str("(Hash (RawU) (RawU))", "{}");
        assert_mono_for_str("(Hash Bool (RawU Int 'foo))", "{true 1, false foo}");
    }
}
