use crate::hir;

/// Returns if an expression can have a side effect or cause a type error
///
/// This is used for very basic dead code elimination during type checking.
pub fn expr_can_side_effect(expr: &hir::Expr<hir::Inferred>) -> bool {
    use hir::ExprKind;
    match &expr.kind {
        ExprKind::Ref(_, _)
        | ExprKind::Lit(_)
        | ExprKind::EqPred(_)
        | ExprKind::TyPred(_, _)
        | ExprKind::RecordCons(_, _)
        | ExprKind::Fun(_)
        | ExprKind::RustFun(_) => false,
        ExprKind::Do(exprs) => exprs.iter().any(expr_can_side_effect),
        ExprKind::MacroExpand(_, inner) => expr_can_side_effect(inner),
        // These can trigger type errors even if they only contain pure expressions
        ExprKind::Cond(cond) => {
            expr_can_side_effect(&cond.test_expr)
                || expr_can_side_effect(&cond.true_expr)
                || expr_can_side_effect(&cond.false_expr)
        }
        ExprKind::Let(let_expr) => {
            expr_can_side_effect(&let_expr.value_expr) || expr_can_side_effect(&let_expr.body_expr)
        }
        ExprKind::App(_) => true,
    }
}
