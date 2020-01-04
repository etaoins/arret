use crate::ty;
use crate::ty::purity::Purity;
use crate::ty::var_usage::Variance;
use crate::ty::Ty;

fn ty_has_subtypes<M: ty::PM>(ty: &Ty<M>) -> bool {
    match ty {
        Ty::Any
        | Ty::Bool
        | Ty::Num
        | Ty::Sym
        | Ty::TopFun(_)
        | Ty::TopRecord
        | Ty::RecordClass(_) => true,

        Ty::Char
        | Ty::Float
        | Ty::Int
        | Ty::LitBool(_)
        | Ty::LitSym(_)
        | Ty::Str
        | Ty::TyPred(_)
        | Ty::EqPred => false,

        Ty::Fun(fun) => {
            fun.purity() != &Purity::Pure.into()
                || !fun.params().fixed().is_empty()
                || fun.params().rest() != &Ty::Any.into()
                || has_subtypes(fun.ret())
        }
        Ty::Map(map) => has_subtypes(map.key()) || has_subtypes(map.value()),
        Ty::Set(member) => has_subtypes(member.as_ref()),
        Ty::Vector(members) => members.iter().any(has_subtypes),
        Ty::Union(members) => !members.is_empty(),
        Ty::List(list) => {
            // Any arbitrary fixed length list is a subtype of a list with rest
            list.has_rest() || list.fixed().iter().any(has_subtypes)
        }

        // Any record type supporting variance has subtypes
        Ty::Record(instance) => instance
            .cons()
            .poly_params()
            .iter()
            .any(|poly_param| poly_param.variance() != Variance::Invariant),

        Ty::Vectorof(_) => {
            // Any arbitrary fixed length vector is a subtype of this vector
            true
        }
        Ty::Intersect(_) => {
            // If we're correctly normalised we should have subtypes
            true
        }
    }
}

pub fn has_subtypes<M: ty::PM>(ty_ref: &ty::Ref<M>) -> bool {
    ty_ref
        .try_to_fixed()
        .map(|ty| ty_has_subtypes(ty))
        .unwrap_or(true)
}

fn ty_is_literal<M: ty::PM>(ty: &Ty<M>) -> bool {
    match ty {
        Ty::LitBool(_) | Ty::LitSym(_) => true,
        Ty::Vector(members) => members.iter().all(is_literal),
        Ty::List(list) => !list.has_rest() && list.fixed().iter().all(is_literal),
        _ => false,
    }
}

pub fn is_literal<M: ty::PM>(ty_ref: &ty::Ref<M>) -> bool {
    ty_ref
        .try_to_fixed()
        .map(|ty| ty_is_literal(ty))
        .unwrap_or(false)
}

#[cfg(test)]
mod test {
    use super::*;

    use crate::hir::poly_for_str;
    use crate::source::empty_span;
    use crate::ty::record;
    use crate::ty::ty_args::TyArgs;

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

        assert_eq!(false, str_has_subtypes("(& Any -> true)"));
        assert_eq!(true, str_has_subtypes("(& Any ->! true)"));
        assert_eq!(true, str_has_subtypes("(Any -> true)"));
        assert_eq!(true, str_has_subtypes("(& Int -> true)"));
        assert_eq!(true, str_has_subtypes("(& Any -> Any)"));

        assert_eq!(true, str_has_subtypes("(Map Sym Int)"));
        assert_eq!(false, str_has_subtypes("(Map Float Int)"));

        assert_eq!(true, str_has_subtypes("(List Sym Int)"));
        assert_eq!(true, str_has_subtypes("(List Str & Int)"));
        assert_eq!(false, str_has_subtypes("(List Str Int)"));

        assert_eq!(true, str_has_subtypes("(Setof Sym)"));
        assert_eq!(false, str_has_subtypes("(Setof Float)"));

        assert_eq!(true, str_has_subtypes("(Vectorof false)"));
        assert_eq!(false, str_has_subtypes("(Vector false true)"));

        assert_eq!(false, str_has_subtypes("(RawU)"));

        let tvar = ty::TVar::new(empty_span(), "test".into(), Ty::Any.into());
        assert_eq!(true, has_subtypes(&tvar.into()));
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

        assert_eq!(false, str_is_literal("(& Any -> true)"));

        assert_eq!(false, str_is_literal("(Map Sym Int)"));
        assert_eq!(false, str_is_literal("(Map false true)"));

        assert_eq!(false, str_is_literal("(List Sym Int)"));
        assert_eq!(false, str_is_literal("(List Str & Int)"));
        assert_eq!(true, str_is_literal("(List true false)"));
        assert_eq!(false, str_is_literal("(List true & false)"));
        assert_eq!(true, str_is_literal("()"));

        assert_eq!(false, str_is_literal("(Setof ())"));
        assert_eq!(false, str_is_literal("(Setof Float)"));

        assert_eq!(false, str_is_literal("(Vectorof false)"));
        assert_eq!(true, str_is_literal("(Vector false true)"));

        let tvar = ty::TVar::new(empty_span(), "test".into(), Ty::Any.into());
        assert_eq!(false, is_literal(&tvar.into()));
    }

    #[test]
    fn mono_record_type() {
        let mono_record_cons = record::Cons::new(
            empty_span(),
            "record_cons".into(),
            "record_cons?".into(),
            None,
            Box::new([record::Field::new(
                empty_span(),
                "num".into(),
                Ty::Num.into(),
            )]),
        );

        let int_record_instance_ref: ty::Ref<ty::Poly> =
            record::Instance::new(mono_record_cons, TyArgs::empty()).into();

        assert_eq!(false, has_subtypes(&int_record_instance_ref));
        assert_eq!(false, is_literal(&int_record_instance_ref));
    }

    #[test]
    fn poly_record_type() {
        let tvar = ty::TVar::new(empty_span(), "tvar".into(), Ty::Any.into());

        let poly_record_cons = record::Cons::new(
            empty_span(),
            "record_cons".into(),
            "record_cons?".into(),
            Some(Box::new([record::PolyParam::TVar(
                Variance::Covariant,
                tvar.clone(),
            )])),
            Box::new([record::Field::new(empty_span(), "num".into(), tvar.into())]),
        );

        let poly_record_instance_ref: ty::Ref<ty::Poly> =
            record::Instance::new(poly_record_cons, TyArgs::empty()).into();

        assert_eq!(true, has_subtypes(&poly_record_instance_ref));
        assert_eq!(false, is_literal(&poly_record_instance_ref));
    }
}
