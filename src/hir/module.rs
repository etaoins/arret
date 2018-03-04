use std::collections::HashMap;
use hir::Expr;
use hir::scope::{insert_primitive_exports, Binding};

#[derive(PartialEq, Debug)]
pub struct Module {
    body_expr: Expr,
    exports: HashMap<String, Binding>,
}

impl Module {
    pub fn new(body_expr: Expr, exports: HashMap<String, Binding>) -> Module {
        Module { body_expr, exports }
    }

    pub fn primitives_module() -> Module {
        let mut primitive_exports = HashMap::<String, Binding>::new();
        insert_primitive_exports(&mut primitive_exports);

        Module::new(Expr::Do(vec![]), primitive_exports)
    }

    pub fn into_body_expr(self) -> Expr {
        self.body_expr
    }

    pub fn exports(&self) -> &HashMap<String, Binding> {
        &self.exports
    }
}
