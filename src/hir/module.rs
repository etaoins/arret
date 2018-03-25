use std::collections::HashMap;
use hir::Expr;
use hir::types::insert_ty_exports;
use hir::scope::{insert_prim_exports, Binding};

#[derive(PartialEq, Debug)]
pub struct Module {
    body_expr: Expr,
    exports: HashMap<String, Binding>,
}

impl Module {
    pub fn new(body_expr: Expr, exports: HashMap<String, Binding>) -> Module {
        Module { body_expr, exports }
    }

    pub fn prims_module() -> Module {
        let mut prim_exports = HashMap::<String, Binding>::new();
        insert_prim_exports(&mut prim_exports);

        Module::new(Expr::Do(vec![]), prim_exports)
    }

    pub fn tys_module() -> Module {
        let mut type_exports = HashMap::<String, Binding>::new();
        insert_ty_exports(&mut type_exports);

        Module::new(Expr::Do(vec![]), type_exports)
    }

    pub fn into_body_expr(self) -> Expr {
        self.body_expr
    }

    pub fn exports(&self) -> &HashMap<String, Binding> {
        &self.exports
    }
}
