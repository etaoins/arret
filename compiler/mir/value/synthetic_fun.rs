use std::rc::Rc;

use syntax::span::EMPTY_SPAN;

use crate::hir;
use crate::mir::closure::Closure;
use crate::mir::value;
use crate::ty;
use crate::ty::purity;
use crate::ty::purity::Purity;

pub fn eq_pred_arret_fun() -> value::ArretFun {
    let left_var_id = hir::VarId::alloc();
    let right_var_id = hir::VarId::alloc();

    let fixed_params = [("left", left_var_id), ("right", right_var_id)]
        .iter()
        .map(|(name, var_id)| {
            hir::destruc::Destruc::Scalar(
                EMPTY_SPAN,
                hir::destruc::Scalar::new(Some(*var_id), (*name).into(), ty::Ty::Any.into_poly()),
            )
        })
        .collect();

    value::ArretFun {
        span: EMPTY_SPAN,
        source_name: Some("=".to_owned()),
        closure: Closure::empty(),
        fun_expr: Rc::new(hir::Fun {
            pvars: purity::PVars::new(),
            tvars: ty::TVars::new(),

            purity: Purity::Pure.into_poly(),
            params: hir::destruc::List::new(fixed_params, None),
            ret_ty: ty::Ty::Bool.into_poly(),

            body_expr: hir::Expr::App(
                EMPTY_SPAN,
                Box::new(hir::App {
                    ret_ty: ty::Ty::Bool.into_poly(),
                    fun_expr: hir::Expr::EqPred(EMPTY_SPAN),
                    fixed_arg_exprs: vec![
                        hir::Expr::Ref(EMPTY_SPAN, left_var_id),
                        hir::Expr::Ref(EMPTY_SPAN, right_var_id),
                    ],
                    rest_arg_expr: None,
                }),
            ),
        }),
    }
}

pub fn ty_pred_arret_fun(test_ty: ty::pred::TestTy) -> value::ArretFun {
    let subject_var_id = hir::VarId::alloc();

    value::ArretFun {
        span: EMPTY_SPAN,
        source_name: Some(test_ty.to_str().to_owned()),
        closure: Closure::empty(),
        fun_expr: Rc::new(hir::Fun {
            pvars: purity::PVars::new(),
            tvars: ty::TVars::new(),

            purity: Purity::Pure.into_poly(),
            params: hir::destruc::List::new(
                vec![hir::destruc::Destruc::Scalar(
                    EMPTY_SPAN,
                    hir::destruc::Scalar::new(
                        Some(subject_var_id),
                        "subject".into(),
                        ty::Ty::Any.into_poly(),
                    ),
                )],
                None,
            ),
            ret_ty: ty::Ty::Bool.into_poly(),

            body_expr: hir::Expr::App(
                EMPTY_SPAN,
                Box::new(hir::App {
                    ret_ty: ty::Ty::Bool.into_poly(),
                    fun_expr: hir::Expr::TyPred(EMPTY_SPAN, test_ty),
                    fixed_arg_exprs: vec![hir::Expr::Ref(EMPTY_SPAN, subject_var_id)],
                    rest_arg_expr: None,
                }),
            ),
        }),
    }
}
