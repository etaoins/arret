use std::collections::HashMap;

use crate::ty;
use crate::ty::purity;
use crate::ty::purity::Purity;

/// Polymorphic type arguments to a polymorphic function
///
/// These type arguments are still polymorphic which can happen if the bound refers to another
/// type variable.
pub struct PolyTyArgs {
    pvar_purities: HashMap<purity::PVarId, purity::Poly>,
    tvar_types: HashMap<ty::TVarId, ty::Poly>,
}

impl PolyTyArgs {
    pub fn new(
        pvar_purities: HashMap<purity::PVarId, purity::Poly>,
        tvar_types: HashMap<ty::TVarId, ty::Poly>,
    ) -> PolyTyArgs {
        PolyTyArgs {
            pvar_purities,
            tvar_types,
        }
    }

    /// Returns the args for the passed pvars/tvars where all args are set to their upper bound
    pub fn from_upper_bound(pvars: &purity::PVars, tvars: &ty::TVars) -> PolyTyArgs {
        let pvar_purities = pvars
            .keys()
            .map(|pvar_id| (*pvar_id, Purity::Impure.into_poly()))
            .collect();

        let tvar_types = tvars
            .iter()
            .map(|(tvar_id, tvar)| (*tvar_id, tvar.bound.clone()))
            .collect();

        PolyTyArgs {
            pvar_purities,
            tvar_types,
        }
    }

    pub fn get_pvar_purity(&self, pvar_id: purity::PVarId) -> Option<&purity::Poly> {
        self.pvar_purities.get(&pvar_id)
    }

    pub fn get_tvar_type(&self, tvar_id: ty::TVarId) -> Option<&ty::Poly> {
        self.tvar_types.get(&tvar_id)
    }
}

/// Monomorphic type arguments to a polymorphic function
///
/// These type arguments are monomorphic which means they've been fully resolved and don't refer
/// to any polymorphic variables.
pub struct MonoTyArgs {
    pvar_purities: HashMap<purity::PVarId, Purity>,
    tvar_types: HashMap<ty::TVarId, ty::Ty<ty::Mono>>,
}

impl MonoTyArgs {
    pub fn empty() -> MonoTyArgs {
        MonoTyArgs {
            pvar_purities: HashMap::new(),
            tvar_types: HashMap::new(),
        }
    }

    pub fn pvar_purity(&self, pvar_id: purity::PVarId) -> &Purity {
        &self.pvar_purities[&pvar_id]
    }

    pub fn tvar_type(&self, tvar_id: ty::TVarId) -> &ty::Ty<ty::Mono> {
        &self.tvar_types[&tvar_id]
    }

    pub fn get_pvar_purity(&self, pvar_id: purity::PVarId) -> Option<&Purity> {
        self.pvar_purities.get(&pvar_id)
    }

    pub fn get_tvar_type(&self, tvar_id: ty::TVarId) -> Option<&ty::Ty<ty::Mono>> {
        self.tvar_types.get(&tvar_id)
    }
}
