use hir::prim::insert_prim_exports;
use hir::scope::Binding;
use hir::types::insert_ty_exports;
use std::collections::HashMap;

#[derive(PartialEq, Debug)]
pub struct Module {
    exports: HashMap<String, Binding>,
}

impl Module {
    pub fn new(exports: HashMap<String, Binding>) -> Module {
        Module { exports }
    }

    pub fn prims_module() -> Module {
        let mut prim_exports = HashMap::<String, Binding>::new();
        insert_prim_exports(&mut prim_exports);

        Module::new(prim_exports)
    }

    pub fn tys_module() -> Module {
        let mut type_exports = HashMap::<String, Binding>::new();
        insert_ty_exports(&mut type_exports);

        Module::new(type_exports)
    }

    pub fn exports(&self) -> &HashMap<String, Binding> {
        &self.exports
    }
}
