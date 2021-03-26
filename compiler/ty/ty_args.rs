use std::collections::HashMap;

use crate::ty;
use crate::ty::purity;
use crate::ty::purity::Purity;

/// Type arguments to a polymorphic function or substitution
#[derive(PartialEq, Clone, Debug)]
pub struct TyArgs<M: ty::Pm> {
    pvar_purities: HashMap<purity::PVarId, purity::Ref>,
    tvar_types: HashMap<ty::TVarId, ty::Ref<M>>,
}

impl<M: ty::Pm> TyArgs<M> {
    pub fn new(
        pvar_purities: HashMap<purity::PVarId, purity::Ref>,
        tvar_types: HashMap<ty::TVarId, ty::Ref<M>>,
    ) -> Self {
        Self {
            pvar_purities,
            tvar_types,
        }
    }

    pub fn empty() -> Self {
        Self {
            pvar_purities: HashMap::new(),
            tvar_types: HashMap::new(),
        }
    }

    pub fn pvar_purities(&self) -> &HashMap<purity::PVarId, purity::Ref> {
        &self.pvar_purities
    }

    pub fn tvar_types(&self) -> &HashMap<ty::TVarId, ty::Ref<M>> {
        &self.tvar_types
    }
}

impl TyArgs<ty::Poly> {
    /// Returns the args for the passed pvars/tvars where all args are set to their upper bound
    pub fn from_upper_bound(pvars: &[purity::PVarId], tvars: &[ty::TVarId]) -> Self {
        let pvar_purities = pvars
            .iter()
            .map(|pvar| (pvar.clone(), Purity::Impure.into()))
            .collect();

        let tvar_types = tvars
            .iter()
            .map(|tvar| (tvar.clone(), tvar.bound.clone()))
            .collect();

        Self {
            pvar_purities,
            tvar_types,
        }
    }
}
