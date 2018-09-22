use crate::ty;
use crate::ty::purity::Purity;

fn poly_ty_has_subtypes(poly_ty: &ty::Ty<ty::Poly>) -> bool {
    match poly_ty {
        ty::Ty::Any | ty::Ty::Bool | ty::Ty::Sym | ty::Ty::TopFun(_) => true,
        ty::Ty::Char
        | ty::Ty::Float
        | ty::Ty::Int
        | ty::Ty::LitBool(_)
        | ty::Ty::LitSym(_)
        | ty::Ty::Str => false,
        ty::Ty::Fun(fun) => {
            fun.purity() != &Purity::Pure.into_poly()
                || !fun.params().fixed().is_empty()
                || fun.params().rest() != Some(&ty::Ty::Any.into_poly())
                || poly_has_subtypes(&fun.ret())
        }
        ty::Ty::TyPred(_) => false,
        ty::Ty::Map(map) => [map.key(), map.value()]
            .iter()
            .any(|poly| poly_has_subtypes(poly)),
        ty::Ty::Set(member) => poly_has_subtypes(member),
        ty::Ty::Vector(members) => members.iter().any(|member| poly_has_subtypes(member)),
        ty::Ty::Union(members) => !members.is_empty(),
        ty::Ty::List(list) => {
            // Any arbitrary fixed length list is a subtype of a list with rest
            list.rest().is_some() || list.fixed().iter().any(|fixed| poly_has_subtypes(fixed))
        }
        ty::Ty::Vectorof(_) => {
            // Any arbitrary fixed length vector is a subtype of this vector
            true
        }
    }
}

pub fn poly_has_subtypes(poly: &ty::Poly) -> bool {
    use crate::ty::TyRef;

    poly.try_to_fixed()
        .map(|poly_ty| poly_ty_has_subtypes(poly_ty))
        .unwrap_or(true)
}

#[cfg(test)]
mod test {
    use super::*;

    fn poly_for_str(datum_str: &str) -> ty::Poly {
        use crate::hir;
        hir::poly_for_str(datum_str)
    }

    fn str_has_subtypes(datum_str: &str) -> bool {
        let poly = poly_for_str(datum_str);
        poly_has_subtypes(&poly)
    }

    #[test]
    fn poly_subtypes() {
        assert_eq!(true, str_has_subtypes("Any"));
        assert_eq!(true, str_has_subtypes("Bool"));
        assert_eq!(false, str_has_subtypes("true"));
        assert_eq!(false, str_has_subtypes("Char"));
        assert_eq!(false, str_has_subtypes("Float"));
        assert_eq!(false, str_has_subtypes("Str"));
        assert_eq!(true, str_has_subtypes("Sym"));

        assert_eq!(false, str_has_subtypes("(Any ... -> true)"));
        assert_eq!(true, str_has_subtypes("(Any ... ->! true)"));
        assert_eq!(true, str_has_subtypes("(Any -> true)"));
        assert_eq!(true, str_has_subtypes("(Int ... -> true)"));
        assert_eq!(true, str_has_subtypes("(Any ... -> Any)"));

        assert_eq!(true, str_has_subtypes("(Map Sym Int)"));
        assert_eq!(false, str_has_subtypes("(Map Float Int)"));

        assert_eq!(true, str_has_subtypes("(List Sym Int)"));
        assert_eq!(true, str_has_subtypes("(List Str Int ...)"));
        assert_eq!(false, str_has_subtypes("(List Str Int)"));

        assert_eq!(true, str_has_subtypes("(Setof Sym)"));
        assert_eq!(false, str_has_subtypes("(Setof Float)"));

        assert_eq!(true, str_has_subtypes("(Vectorof false)"));
        assert_eq!(false, str_has_subtypes("(Vector false true)"));

        assert_eq!(false, str_has_subtypes("(RawU)"));
    }
}
