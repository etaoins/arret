use std::collections::HashMap;

use hir::prim::PRIM_EXPORTS;
use hir::scope::Binding;
use hir::types::TY_EXPORTS;

pub type Exports = HashMap<Box<str>, Binding>;

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
