use crate::hir;
use crate::ty;
use crate::ty::purity::Purity;
use crate::ty::Ty;

/// Returns if an expression can have a side effect
///
/// This is used for very basic dead code elimination during type checking.
pub fn expr_can_side_effect(expr: &hir::Expr<hir::Inferred>) -> bool {
    use hir::ExprKind;
    match &expr.kind {
        ExprKind::LocalRef(_, _)
        | ExprKind::ExportRef(_, _)
        | ExprKind::Lit(_)
        | ExprKind::EqPred(_)
        | ExprKind::TyPred(_, _)
        | ExprKind::RecordCons(_, _)
        | ExprKind::FieldAccessor(_)
        | ExprKind::Fun(_)
        | ExprKind::RustFun(_) => false,
        ExprKind::Do(exprs) => exprs.iter().any(expr_can_side_effect),
        ExprKind::MacroExpand(_, inner) => expr_can_side_effect(inner),
        ExprKind::Cond(cond) => {
            expr_can_side_effect(&cond.test_expr)
                || expr_can_side_effect(&cond.true_expr)
                || expr_can_side_effect(&cond.false_expr)
        }
        ExprKind::Let(let_expr) => {
            expr_can_side_effect(&let_expr.value_expr) || expr_can_side_effect(&let_expr.body_expr)
        }
        ExprKind::App(app) => {
            if let ty::Ref::Fixed(Ty::Fun(ref fun_type)) = app.fun_expr.result_ty {
                fun_type.top_fun().purity() != &Purity::Pure.into()
                    || fun_type.ret().is_never()
                    || app.fixed_arg_exprs.iter().any(expr_can_side_effect)
                    || app.rest_arg_expr.iter().any(expr_can_side_effect)
            } else {
                true
            }
        }
        ExprKind::Recur(_) => {
            // We don't know if a `(recur)` is pure without knowing the function it appears in.
            // However, by definition `(recur)` always occurs in a position where its value becomes
            // the return value of a function. This means that in practice it can never be
            // eliminated anyway.
            true
        }
    }
}

#[cfg(test)]
mod test {
    use super::*;

    use crate::source::EMPTY_SPAN;
    use crate::ty::ty_args::TyArgs;

    #[test]
    fn pure_app_expr() {
        let app_expr = hir::Expr::<hir::Inferred> {
            result_ty: ty::List::empty().into(),
            kind: hir::ExprKind::App(Box::new(hir::App {
                span: EMPTY_SPAN,
                fun_expr: hir::Expr {
                    result_ty: ty::Fun::new_mono(
                        ty::List::empty(),
                        Purity::Pure.into(),
                        ty::List::empty().into(),
                    )
                    .into(),
                    kind: hir::ExprKind::Do(vec![]),
                },
                ty_args: TyArgs::empty(),
                fixed_arg_exprs: vec![],
                rest_arg_expr: None,
            })),
        };

        assert_eq!(false, expr_can_side_effect(&app_expr));
    }

    #[test]
    fn impure_app_expr() {
        let app_expr = hir::Expr::<hir::Inferred> {
            result_ty: ty::List::empty().into(),
            kind: hir::ExprKind::App(Box::new(hir::App {
                span: EMPTY_SPAN,
                fun_expr: hir::Expr {
                    result_ty: ty::Fun::new_mono(
                        ty::List::empty(),
                        Purity::Impure.into(),
                        ty::List::empty().into(),
                    )
                    .into(),
                    kind: hir::ExprKind::Do(vec![]),
                },
                ty_args: TyArgs::empty(),
                fixed_arg_exprs: vec![],
                rest_arg_expr: None,
            })),
        };

        assert_eq!(true, expr_can_side_effect(&app_expr));
    }
}
