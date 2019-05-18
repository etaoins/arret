use arret_syntax::span::EMPTY_SPAN;
use std::collections::HashMap;

use crate::hir;
use crate::mir::closure::Closure;
use crate::mir::value;
use crate::ty;
use crate::ty::purity;
use crate::ty::purity::Purity;
use crate::ty::ty_args::TyArgs;

fn new_eq_pred_arret_fun() -> value::ArretFun {
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

    value::ArretFun::new(
        Some("=".into()),
        TyArgs::empty(),
        Closure::empty(),
        hir::Fun {
            span: EMPTY_SPAN,

            pvar_ids: purity::PVarIds::new(),
            tvar_ids: ty::TVarIds::new(),

            purity: Purity::Pure.into(),
            params: hir::destruc::List::new(fixed_params, None),
            ret_ty: ty::Ty::Bool.into(),

            body_expr: hir::Expr {
                result_ty: ty::Ty::Bool.into(),
                kind: hir::ExprKind::App(Box::new(hir::App {
                    span: EMPTY_SPAN,
                    fun_expr: hir::Expr {
                        result_ty: ty::Ty::EqPred.into(),
                        kind: hir::ExprKind::EqPred(EMPTY_SPAN),
                    },
                    ty_args: TyArgs::empty(),
                    fixed_arg_exprs: vec![
                        hir::Expr {
                            result_ty: ty::Ty::Any.into(),
                            kind: hir::ExprKind::Ref(EMPTY_SPAN, left_var_id),
                        },
                        hir::Expr {
                            result_ty: ty::Ty::Any.into(),
                            kind: hir::ExprKind::Ref(EMPTY_SPAN, right_var_id),
                        },
                    ],
                    rest_arg_expr: None,
                })),
            },
        },
    )
}

fn new_ty_pred_arret_fun(test_ty: ty::pred::TestTy) -> value::ArretFun {
    let subject_var_id = hir::VarId::alloc();

    value::ArretFun::new(
        Some(test_ty.to_string().into()),
        TyArgs::empty(),
        Closure::empty(),
        hir::Fun {
            span: EMPTY_SPAN,

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
                result_ty: ty::Ty::Bool.into(),
                kind: hir::ExprKind::App(Box::new(hir::App {
                    span: EMPTY_SPAN,
                    fun_expr: hir::Expr {
                        result_ty: ty::Ty::TyPred(test_ty.clone()).into(),
                        kind: hir::ExprKind::TyPred(EMPTY_SPAN, test_ty),
                    },
                    ty_args: TyArgs::empty(),
                    fixed_arg_exprs: vec![hir::Expr {
                        result_ty: ty::Ty::Any.into(),
                        kind: hir::ExprKind::Ref(EMPTY_SPAN, subject_var_id),
                    }],
                    rest_arg_expr: None,
                })),
            },
        },
    )
}

#[derive(Default)]
pub struct SyntheticFuns {
    eq_pred_arret_fun: Option<value::ArretFun>,
    ty_pred_arret_fun: HashMap<ty::pred::TestTy, value::ArretFun>,
}

impl SyntheticFuns {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn eq_pred_arret_fun(&mut self) -> &value::ArretFun {
        self.eq_pred_arret_fun
            .get_or_insert_with(new_eq_pred_arret_fun)
    }

    pub fn ty_pred_arret_fun(&mut self, test_ty: ty::pred::TestTy) -> &value::ArretFun {
        self.ty_pred_arret_fun
            .entry(test_ty.clone())
            .or_insert_with(|| new_ty_pred_arret_fun(test_ty))
    }
}
