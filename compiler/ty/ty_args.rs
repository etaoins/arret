use std::collections::HashMap;

use crate::ty;
use crate::ty::purity;
use crate::ty::purity::Purity;

/// Polymorphic type arguments to a polymorphic function
///
/// These type arguments are still polymorphic which can happen if the bound refers to another
/// type variable.
#[derive(PartialEq, Clone, Debug)]
pub struct PolyTyArgs {
    pvar_purities: HashMap<purity::PVarId, purity::Poly>,
    tvar_types: HashMap<ty::TVarId, ty::Ref<ty::Poly>>,
}

impl PolyTyArgs {
    pub fn new(
        pvar_purities: HashMap<purity::PVarId, purity::Poly>,
        tvar_types: HashMap<ty::TVarId, ty::Ref<ty::Poly>>,
    ) -> PolyTyArgs {
        PolyTyArgs {
            pvar_purities,
            tvar_types,
        }
    }

    pub fn empty() -> PolyTyArgs {
        PolyTyArgs {
            pvar_purities: HashMap::new(),
            tvar_types: HashMap::new(),
        }
    }

    /// Returns the args for the passed pvars/tvars where all args are set to their upper bound
    pub fn from_upper_bound(pvars: &purity::PVars, tvars: &ty::TVars) -> PolyTyArgs {
        let pvar_purities = pvars
            .keys()
            .map(|pvar_id| (*pvar_id, Purity::Impure.into()))
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

    pub fn pvar_purities(&self) -> &HashMap<purity::PVarId, purity::Poly> {
        &self.pvar_purities
    }

    pub fn tvar_types(&self) -> &HashMap<ty::TVarId, ty::Ref<ty::Poly>> {
        &self.tvar_types
    }
}

/// Monomorphic type arguments to a polymorphic function
///
/// These type arguments are monomorphic which means they've been fully resolved and don't refer
/// to any polymorphic variables.
#[derive(PartialEq, Clone, Debug)]
pub struct MonoTyArgs {
    pvar_purities: HashMap<purity::PVarId, purity::Poly>,
    tvar_types: HashMap<ty::TVarId, ty::Ty<ty::Mono>>,
}

impl MonoTyArgs {
    pub fn new(
        pvar_purities: HashMap<purity::PVarId, purity::Poly>,
        tvar_types: HashMap<ty::TVarId, ty::Ty<ty::Mono>>,
    ) -> MonoTyArgs {
        MonoTyArgs {
            pvar_purities,
            tvar_types,
        }
    }

    pub fn empty() -> MonoTyArgs {
        MonoTyArgs {
            pvar_purities: HashMap::new(),
            tvar_types: HashMap::new(),
        }
    }

    pub fn pvar_purities(&self) -> &HashMap<purity::PVarId, purity::Poly> {
        &self.pvar_purities
    }

    pub fn tvar_types(&self) -> &HashMap<ty::TVarId, ty::Ty<ty::Mono>> {
        &self.tvar_types
    }
}
