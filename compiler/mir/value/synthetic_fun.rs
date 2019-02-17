use std::rc::Rc;

use syntax::span::EMPTY_SPAN;

use crate::hir;
use crate::mir::closure::Closure;
use crate::mir::value;
use crate::ty;
use crate::ty::purity;
use crate::ty::purity::Purity;
use crate::ty::ty_args::TyArgs;

pub fn eq_pred_arret_fun() -> value::ArretFun {
    let left_var_id = hir::VarId::alloc();
    let right_var_id = hir::VarId::alloc();

    let fixed_params = [("left", left_var_id), ("right", right_var_id)]
        .iter()
        .map(|(name, var_id)| {
            hir::destruc::Destruc::Scalar(
                EMPTY_SPAN,
                hir::destruc::Scalar::new(Some(*var_id), (*name).into(), ty::Ty::Any.into()),
            )
        })
        .collect();

    value::ArretFun {
        id: value::ArretFunId::alloc(),
        span: EMPTY_SPAN,
        source_name: Some("=".to_owned()),
        env_ty_args: TyArgs::empty(),
        closure: Closure::empty(),
        fun_expr: Rc::new(hir::Fun {
            pvar_ids: purity::PVarIds::new(),
            tvar_ids: ty::TVarIds::new(),

            purity: Purity::Pure.into(),
            params: hir::destruc::List::new(fixed_params, None),
            ret_ty: ty::Ty::Bool.into(),

            body_expr: hir::Expr {
                span: EMPTY_SPAN,
                result_ty: ty::Ty::Bool.into(),
                kind: hir::ExprKind::App(Box::new(hir::App {
                    fun_expr: hir::Expr {
                        span: EMPTY_SPAN,
                        result_ty: ty::Ty::EqPred.into(),
                        kind: hir::ExprKind::EqPred,
                    },
                    ty_args: TyArgs::empty(),
                    fixed_arg_exprs: vec![
                        hir::Expr {
                            span: EMPTY_SPAN,
                            result_ty: ty::Ty::Any.into(),
                            kind: hir::ExprKind::Ref(left_var_id),
                        },
                        hir::Expr {
                            span: EMPTY_SPAN,
                            result_ty: ty::Ty::Any.into(),
                            kind: hir::ExprKind::Ref(right_var_id),
                        },
                    ],
                    rest_arg_expr: None,
                })),
            },
        }),
    }
}

pub fn ty_pred_arret_fun(test_ty: ty::pred::TestTy) -> value::ArretFun {
    let subject_var_id = hir::VarId::alloc();

    value::ArretFun {
        id: value::ArretFunId::alloc(),
        span: EMPTY_SPAN,
        source_name: Some(test_ty.to_str().to_owned()),
        env_ty_args: TyArgs::empty(),
        closure: Closure::empty(),
        fun_expr: Rc::new(hir::Fun {
            pvar_ids: purity::PVarIds::new(),
            tvar_ids: ty::TVarIds::new(),

            purity: Purity::Pure.into(),
            params: hir::destruc::List::new(
                vec![hir::destruc::Destruc::Scalar(
                    EMPTY_SPAN,
                    hir::destruc::Scalar::new(
                        Some(subject_var_id),
                        "subject".into(),
                        ty::Ty::Any.into(),
                    ),
                )],
                None,
            ),
            ret_ty: ty::Ty::Bool.into(),

            body_expr: hir::Expr {
                span: EMPTY_SPAN,
                result_ty: ty::Ty::Bool.into(),
                kind: hir::ExprKind::App(Box::new(hir::App {
                    fun_expr: hir::Expr {
                        span: EMPTY_SPAN,
                        result_ty: ty::Ty::TyPred(test_ty).into(),
                        kind: hir::ExprKind::TyPred(test_ty),
                    },
                    ty_args: TyArgs::empty(),
                    fixed_arg_exprs: vec![hir::Expr {
                        span: EMPTY_SPAN,
                        result_ty: ty::Ty::Any.into(),
                        kind: hir::ExprKind::Ref(subject_var_id),
                    }],
                    rest_arg_expr: None,
                })),
            },
        }),
    }
}
