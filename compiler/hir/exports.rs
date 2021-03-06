use std::collections::HashMap;

use arret_syntax::datum::DataStr;

use crate::hir::prim::PRIM_EXPORTS;
use crate::hir::scope::Binding;
use crate::hir::types::TY_EXPORTS;

pub type Exports = HashMap<DataStr, Binding>;

pub fn prims_exports() -> Exports {
    PRIM_EXPORTS
        .iter()
        .map(|(name, binding)| ((*name).into(), binding.clone()))
        .collect()
}

pub fn tys_exports() -> Exports {
    TY_EXPORTS
        .iter()
        .map(|(name, binding)| ((*name).into(), binding.clone()))
        .collect()
}
