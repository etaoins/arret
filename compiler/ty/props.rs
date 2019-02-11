use crate::ty;
use crate::ty::purity::Purity;
use crate::ty::TyRef;

fn ty_has_subtypes<S: TyRef>(ty: &ty::Ty<S>) -> bool {
    match ty {
        ty::Ty::Any | ty::Ty::Bool | ty::Ty::Num | ty::Ty::Sym | ty::Ty::TopFun(_) => true,
        ty::Ty::Char
        | ty::Ty::Float
        | ty::Ty::Int
        | ty::Ty::LitBool(_)
        | ty::Ty::LitSym(_)
        | ty::Ty::Str
        | ty::Ty::TyPred(_)
        | ty::Ty::EqPred => false,
        ty::Ty::Fun(fun) => {
            fun.purity() != &Purity::Pure.into()
                || !fun.params().fixed().is_empty()
                || fun.params().rest() != Some(&ty::Ty::Any.into())
                || has_subtypes(fun.ret())
        }
        ty::Ty::Map(map) => has_subtypes(map.key()) || has_subtypes(map.value()),
        ty::Ty::Set(member) => has_subtypes(member.as_ref()),
        ty::Ty::Vector(members) => members.iter().any(has_subtypes),
        ty::Ty::Union(members) => !members.is_empty(),
        ty::Ty::List(list) => {
            // Any arbitrary fixed length list is a subtype of a list with rest
            list.rest().is_some() || list.fixed().iter().any(has_subtypes)
        }
        ty::Ty::Vectorof(_) => {
            // Any arbitrary fixed length vector is a subtype of this vector
            true
        }
        ty::Ty::Intersect(_) => {
            // If we're correctly normalised we should have subtypes
            true
        }
    }
}

pub fn has_subtypes<S: TyRef>(ty_ref: &S) -> bool {
    ty_ref
        .try_to_fixed()
        .map(|ty| ty_has_subtypes(ty))
        .unwrap_or(true)
}

fn ty_is_literal<S: TyRef>(ty: &ty::Ty<S>) -> bool {
    match ty {
        ty::Ty::LitBool(_) | ty::Ty::LitSym(_) => true,
        ty::Ty::Vector(members) => members.iter().all(is_literal),
        ty::Ty::List(list) => list.rest().is_none() && list.fixed().iter().all(is_literal),
        _ => false,
    }
}

pub fn is_literal<S: TyRef>(ty_ref: &S) -> bool {
    ty_ref
        .try_to_fixed()
        .map(|ty| ty_is_literal(ty))
        .unwrap_or(false)
}

#[cfg(test)]
mod test {
    use super::*;
    use crate::hir::poly_for_str;

    fn str_has_subtypes(datum_str: &str) -> bool {
        let poly = poly_for_str(datum_str);
        has_subtypes(&poly)
    }

    fn str_is_literal(datum_str: &str) -> bool {
        let poly = poly_for_str(datum_str);
        is_literal(&poly)
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
        assert_eq!(true, str_has_subtypes("Num"));

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

        let tvar_id = ty::TVarId::alloc();
        assert_eq!(true, has_subtypes(&ty::Poly::Var(tvar_id)));
    }

    #[test]
    fn poly_literal() {
        assert_eq!(false, str_is_literal("Any"));
        assert_eq!(false, str_is_literal("Bool"));
        assert_eq!(true, str_is_literal("true"));
        assert_eq!(false, str_is_literal("Char"));
        assert_eq!(false, str_is_literal("Float"));
        assert_eq!(false, str_is_literal("Str"));
        assert_eq!(false, str_is_literal("Sym"));

        assert_eq!(false, str_is_literal("(Any ... -> true)"));

        assert_eq!(false, str_is_literal("(Map Sym Int)"));
        assert_eq!(false, str_is_literal("(Map false true)"));

        assert_eq!(false, str_is_literal("(List Sym Int)"));
        assert_eq!(false, str_is_literal("(List Str Int ...)"));
        assert_eq!(true, str_is_literal("(List true false)"));
        assert_eq!(false, str_is_literal("(List true false ...)"));
        assert_eq!(true, str_is_literal("()"));

        assert_eq!(false, str_is_literal("(Setof ())"));
        assert_eq!(false, str_is_literal("(Setof Float)"));

        assert_eq!(false, str_is_literal("(Vectorof false)"));
        assert_eq!(true, str_is_literal("(Vector false true)"));

        let tvar_id = ty::TVarId::alloc();
        assert_eq!(false, is_literal(&ty::Poly::Var(tvar_id)));
    }
}
