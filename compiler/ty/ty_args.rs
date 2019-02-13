use std::collections::HashMap;

use crate::ty;
use crate::ty::purity;
use crate::ty::purity::Purity;

/// Type arguments to a polymorphic function
///
/// These type arguments are still polymorphic which can happen if the bound refers to another
/// type variable.
#[derive(PartialEq, Clone, Debug)]
pub struct TyArgs<M: ty::PM> {
    pvar_purities: HashMap<purity::PVarId, purity::Ref>,
    tvar_types: HashMap<ty::TVarId, ty::Ref<M>>,
}

impl<M: ty::PM> TyArgs<M> {
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
    pub fn from_upper_bound(pvars: &purity::PVars, tvars: &ty::TVars) -> Self {
        let pvar_purities = pvars
            .keys()
            .map(|pvar_id| (*pvar_id, Purity::Impure.into()))
            .collect();

        let tvar_types = tvars
            .iter()
            .map(|(tvar_id, tvar)| (*tvar_id, tvar.bound.clone()))
            .collect();

        Self {
            pvar_purities,
            tvar_types,
        }
    }
}
