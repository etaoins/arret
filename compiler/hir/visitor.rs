use crate::hir;

/// Visits an expression and all of its subexpressions
pub fn visit_exprs<'a, P: hir::Phase, F>(expr: &'a hir::Expr<P>, visitor: &mut F)
where
    F: FnMut(&'a hir::Expr<P>),
{
    visitor(expr);

    match expr {
        hir::Expr::Cond(_, cond) => {
            visit_exprs(&cond.test_expr, visitor);
            visit_exprs(&cond.true_expr, visitor);
            visit_exprs(&cond.false_expr, visitor);
        }
        hir::Expr::Fun(_, fun) => {
            visit_exprs(&fun.body_expr, visitor);
        }
        hir::Expr::App(_, app) => {
            visit_exprs(&app.fun_expr, visitor);
            for fixed_arg_expr in &app.fixed_arg_exprs {
                visit_exprs(fixed_arg_expr, visitor);
            }
            for rest_arg_expr in &app.rest_arg_expr {
                visit_exprs(rest_arg_expr, visitor);
            }
        }
        hir::Expr::Let(_, hir_let) => {
            visit_exprs(&hir_let.value_expr, visitor);
            visit_exprs(&hir_let.body_expr, visitor);
        }
        hir::Expr::Do(exprs) => {
            for expr in exprs {
                visit_exprs(&expr, visitor);
            }
        }
        hir::Expr::MacroExpand(_, expr) => {
            visit_exprs(expr, visitor);
        }
        hir::Expr::Ref(_, _)
        | hir::Expr::Lit(_)
        | hir::Expr::RustFun(_, _)
        | hir::Expr::TyPred(_, _) => {
            // Terminal expression
        }
    };
}
