use crate::hir;

/// Visits an expression and all of its subexpressions
pub fn visit_exprs<'a, P: hir::Phase, F>(expr: &'a hir::Expr<P>, visitor: &mut F)
where
    F: FnMut(&'a hir::Expr<P>),
{
    visitor(expr);

    use crate::hir::ExprKind;
    match &expr.kind {
        ExprKind::Cond(cond) => {
            visit_exprs(&cond.test_expr, visitor);
            visit_exprs(&cond.true_expr, visitor);
            visit_exprs(&cond.false_expr, visitor);
        }
        ExprKind::Fun(fun) => {
            visit_exprs(&fun.body_expr, visitor);
        }
        ExprKind::App(app) => {
            visit_exprs(&app.fun_expr, visitor);
            for fixed_arg_expr in &app.fixed_arg_exprs {
                visit_exprs(fixed_arg_expr, visitor);
            }
            for rest_arg_expr in &app.rest_arg_expr {
                visit_exprs(rest_arg_expr, visitor);
            }
        }
        ExprKind::Recur(recur) => {
            for fixed_arg_expr in &recur.fixed_arg_exprs {
                visit_exprs(fixed_arg_expr, visitor);
            }
            for rest_arg_expr in &recur.rest_arg_expr {
                visit_exprs(rest_arg_expr, visitor);
            }
        }
        ExprKind::Let(hir_let) => {
            visit_exprs(&hir_let.value_expr, visitor);
            visit_exprs(&hir_let.body_expr, visitor);
        }
        ExprKind::Do(exprs) => {
            for expr in exprs {
                visit_exprs(expr, visitor);
            }
        }
        ExprKind::MacroExpand(_, expr) => {
            visit_exprs(expr, visitor);
        }
        ExprKind::ExportRef(_, _)
        | ExprKind::LocalRef(_, _)
        | ExprKind::Lit(_)
        | ExprKind::RustFun(_)
        | ExprKind::TyPred(_, _)
        | ExprKind::EqPred(_)
        | ExprKind::RecordCons(_, _)
        | ExprKind::FieldAccessor(_) => {
            // Terminal expression
        }
    };
}
