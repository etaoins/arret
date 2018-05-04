use hir::destruc::Destruc;
use hir::prim::insert_prim_exports;
use hir::scope::Binding;
use hir::types::insert_ty_exports;
use hir::Expr;
use std::collections::HashMap;
use syntax::span::Span;

#[derive(PartialEq, Debug)]
pub struct ModuleDef {
    span: Span,
    destruc: Destruc,
    value: Expr,
}

impl ModuleDef {
    pub fn new(span: Span, destruc: Destruc, value: Expr) -> ModuleDef {
        ModuleDef {
            span,
            destruc,
            value,
        }
    }

    #[cfg(test)]
    pub fn into_value(self) -> Expr {
        self.value
    }
}

#[derive(PartialEq, Debug)]
pub struct Module {
    defs: Vec<ModuleDef>,
    exports: HashMap<String, Binding>,
}

impl Module {
    pub fn new(defs: Vec<ModuleDef>, exports: HashMap<String, Binding>) -> Module {
        Module { defs, exports }
    }

    pub fn prims_module() -> Module {
        let mut prim_exports = HashMap::<String, Binding>::new();
        insert_prim_exports(&mut prim_exports);

        Module::new(vec![], prim_exports)
    }

    pub fn tys_module() -> Module {
        let mut type_exports = HashMap::<String, Binding>::new();
        insert_ty_exports(&mut type_exports);

        Module::new(vec![], type_exports)
    }

    #[cfg(test)]
    pub fn into_defs(self) -> Vec<ModuleDef> {
        self.defs
    }

    pub fn exports(&self) -> &HashMap<String, Binding> {
        &self.exports
    }
}
