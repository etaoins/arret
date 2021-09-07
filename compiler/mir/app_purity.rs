use std::collections::HashMap;

use crate::ty;
use crate::ty::purity;
use crate::ty::purity::Purity;

fn resolve_ref_to_purity(
    pvar_purities: &HashMap<purity::PVarId, purity::Ref>,
    poly: &purity::Ref,
) -> Purity {
    match poly {
        purity::Ref::Fixed(purity) => *purity,
        purity::Ref::Var(pvar) => {
            let inner_ref = pvar_purities
                .get(pvar)
                .expect("Unable to find PVar determining fun apply purity");

            resolve_ref_to_purity(pvar_purities, inner_ref)
        }
    }
}

/// Returns the purity for a fun application
pub fn fun_app_purity(
    pvar_purities: &HashMap<purity::PVarId, purity::Ref>,
    fun_purity: &purity::Ref,
    fun_ret_ty: &ty::Ref<ty::Poly>,
) -> Purity {
    if fun_ret_ty.is_never() {
        // This is a hack for things like `panic`. Pure funs are allowed to panic but if they
        // return `(U)` they're likely only called to terminate the program. Without this `panic`
        // would be optimised away.
        return Purity::Impure;
    }

    resolve_ref_to_purity(pvar_purities, fun_purity)
}
