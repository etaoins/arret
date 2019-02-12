use crate::ty;

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
pub fn resolve_poly_ty<'ty>(tvars: &'ty ty::TVars, poly: &'ty ty::Poly) -> &'ty ty::Ty<ty::Poly> {
    match poly {
        ty::Poly::Fixed(ty) => ty,
        ty::Poly::Var(tvar_id) => resolve_tvar_id_bound(tvars, *tvar_id),
    }
}
