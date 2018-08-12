use crate::ty;
use crate::ty::purity::Purity;

pub enum Result<'ty> {
    Fixed(&'ty ty::Ty<ty::Poly>),
    Bound(&'ty ty::Ty<ty::Poly>),
}

impl<'ty> Result<'ty> {
    pub fn as_ty(&self) -> &'ty ty::Ty<ty::Poly> {
        match self {
            Result::Fixed(ty) => ty,
            Result::Bound(ty) => ty,
        }
    }
}

fn poly_ty_has_subtypes(tvars: &[ty::TVar], poly_ty: &ty::Ty<ty::Poly>) -> bool {
    match poly_ty {
        ty::Ty::Any | ty::Ty::Bool | ty::Ty::Sym | ty::Ty::TopFun(_) => true,
        ty::Ty::Char
        | ty::Ty::Float
        | ty::Ty::Int
        | ty::Ty::LitBool(_)
        | ty::Ty::LitSym(_)
        | ty::Ty::Str => false,
        ty::Ty::Fun(fun) => {
            (fun.purity() != &Purity::Pure.into_poly())
                || !fun.params().fixed().is_empty()
                || fun.params().rest() != Some(&ty::Ty::Any.into_poly())
                || poly_has_subtypes(tvars, fun.ret())
        }
        ty::Ty::TyPred(_) => false,
        ty::Ty::Map(map) => [map.key(), map.value()]
            .iter()
            .any(|poly| poly_has_subtypes(tvars, poly)),
        ty::Ty::Set(member) => poly_has_subtypes(tvars, member),
        ty::Ty::Vector(members) => members
            .iter()
            .any(|member| poly_has_subtypes(tvars, member)),
        ty::Ty::Union(members) => !members.is_empty(),
        ty::Ty::List(list) => {
            // Any arbitrary fixed length list is a subtype of a list with rest
            list.rest().is_some() || list
                .fixed()
                .iter()
                .any(|fixed| poly_has_subtypes(tvars, fixed))
        }
        ty::Ty::Vectorof(_) => {
            // Any arbitrary fixed length vector is a subtype of this vector
            true
        }
    }
}

fn poly_has_subtypes(tvars: &[ty::TVar], poly: &ty::Poly) -> bool {
    let poly_ty = resolve_poly_ty(tvars, poly).as_ty();
    poly_ty_has_subtypes(tvars, poly_ty)
}

fn resolve_tvar_id_bound(tvars: &[ty::TVar], tvar_id: ty::TVarId) -> &ty::Ty<ty::Poly> {
    match tvars[tvar_id.to_usize()].bound {
        ty::Poly::Fixed(ref fixed_ty) => fixed_ty,
        ty::Poly::Var(tvar_id) => resolve_tvar_id_bound(tvars, tvar_id),
    }
}

/// Resolves a Poly to either a fixed type or polymorphic variable's bound
///
/// If a type variable is bounded by a type with no subtypes (e.g. Str or LitBool) then it's treated
/// as fixed.
pub fn resolve_poly_ty<'ty>(tvars: &'ty [ty::TVar], poly: &'ty ty::Poly) -> Result<'ty> {
    match poly {
        ty::Poly::Fixed(ty) => Result::Fixed(ty),
        ty::Poly::Var(tvar_id) => {
            let ty = resolve_tvar_id_bound(tvars, *tvar_id);

            if poly_ty_has_subtypes(tvars, ty) {
                Result::Bound(ty)
            } else {
                Result::Fixed(ty)
            }
        }
    }
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
        poly_has_subtypes(&[], &poly)
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

        assert_eq!(false, poly_has_subtypes(&[], &ty::Ty::never().into_poly()));
    }
}
