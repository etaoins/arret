use hir::scope::Binding;
use std::collections::HashMap;

use hir::prim::PRIM_EXPORTS;
use hir::types::TY_EXPORTS;

#[derive(PartialEq, Debug)]
pub struct Module {
    exports: HashMap<Box<str>, Binding>,
}

impl Module {
    pub fn new(exports: HashMap<Box<str>, Binding>) -> Module {
        Module { exports }
    }

    pub fn prims_module() -> Module {
        Module::new(
            PRIM_EXPORTS
                .iter()
                .map(|(name, binding)| ((*name).into(), binding.clone()))
                .collect(),
        )
    }

    pub fn tys_module() -> Module {
        Module::new(
            TY_EXPORTS
                .iter()
                .map(|(name, binding)| ((*name).into(), binding.clone()))
                .collect(),
        )
    }

    pub fn exports(&self) -> &HashMap<Box<str>, Binding> {
        &self.exports
    }
}
