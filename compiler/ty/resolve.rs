use crate::ty;

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

fn resolve_tvar_id_bound(tvars: &ty::TVars, tvar_id: ty::TVarId) -> &ty::Ty<ty::Poly> {
    match tvars
        .get(&tvar_id)
        .expect("TVar not found while resolving TVarId")
        .bound
    {
        ty::Poly::Fixed(ref fixed_ty) => fixed_ty,
        ty::Poly::Var(tvar_id) => resolve_tvar_id_bound(tvars, tvar_id),
    }
}

/// Resolves a Poly to either a fixed type or polymorphic variable's bound
///
/// If a type variable is bounded by a type with no subtypes (e.g. Str or LitBool) then it's treated
/// as fixed.
pub fn resolve_poly_ty<'ty>(tvars: &'ty ty::TVars, poly: &'ty ty::Poly) -> Result<'ty> {
    match poly {
        ty::Poly::Fixed(ty) => Result::Fixed(ty),
        ty::Poly::Var(tvar_id) => {
            let ty = resolve_tvar_id_bound(tvars, *tvar_id);
            Result::Bound(ty)
        }
    }
}
