use std::collections::HashMap;
use hir::Expr;
use hir::scope::Binding;

#[derive(Debug, PartialEq)]
pub struct Module {
    pub body_expr: Expr,
    pub exports: HashMap<String, Binding>,
}
