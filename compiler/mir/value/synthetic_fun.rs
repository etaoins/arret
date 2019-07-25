use arret_syntax::span::EMPTY_SPAN;
use std::collections::HashMap;

use crate::hir;
use crate::mir::closure::Closure;
use crate::mir::value;
use crate::ty;
use crate::ty::purity;
use crate::ty::purity::Purity;
use crate::ty::record;
use crate::ty::ty_args::TyArgs;
use crate::ty::Ty;

fn new_eq_pred_arret_fun() -> value::ArretFun {
    let left_var_id = hir::VarId::alloc();
    let right_var_id = hir::VarId::alloc();

    let fixed_params = [("left", left_var_id), ("right", right_var_id)]
        .iter()
        .map(|(name, var_id)| {
            hir::destruc::Destruc::Scalar(
                EMPTY_SPAN,
                hir::destruc::Scalar::new(Some(*var_id), (*name).into(), Ty::Any.into()),
            )
        })
        .collect();

    value::ArretFun::new(
        Some("=".into()),
        TyArgs::empty(),
        Closure::empty(),
        hir::Fun {
            span: EMPTY_SPAN,

            pvars: purity::PVars::new(),
            tvars: ty::TVars::new(),

            purity: Purity::Pure.into(),
            params: hir::destruc::List::new(fixed_params, None),
            ret_ty: Ty::Bool.into(),

            body_expr: hir::Expr {
                result_ty: Ty::Bool.into(),
                kind: hir::ExprKind::App(Box::new(hir::App {
                    span: EMPTY_SPAN,
                    fun_expr: hir::Expr {
                        result_ty: Ty::EqPred.into(),
                        kind: hir::ExprKind::EqPred(EMPTY_SPAN),
                    },
                    ty_args: TyArgs::empty(),
                    fixed_arg_exprs: vec![
                        hir::Expr {
                            result_ty: Ty::Any.into(),
                            kind: hir::ExprKind::Ref(EMPTY_SPAN, left_var_id),
                        },
                        hir::Expr {
                            result_ty: Ty::Any.into(),
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

            pvars: purity::PVars::new(),
            tvars: ty::TVars::new(),

            purity: Purity::Pure.into(),
            params: hir::destruc::List::new(
                vec![hir::destruc::Destruc::Scalar(
                    EMPTY_SPAN,
                    hir::destruc::Scalar::new(
                        Some(subject_var_id),
                        "subject".into(),
                        Ty::Any.into(),
                    ),
                )],
                None,
            ),
            ret_ty: Ty::Bool.into(),

            body_expr: hir::Expr {
                result_ty: Ty::Bool.into(),
                kind: hir::ExprKind::App(Box::new(hir::App {
                    span: EMPTY_SPAN,
                    fun_expr: hir::Expr {
                        result_ty: Ty::TyPred(test_ty.clone()).into(),
                        kind: hir::ExprKind::TyPred(EMPTY_SPAN, test_ty),
                    },
                    ty_args: TyArgs::empty(),
                    fixed_arg_exprs: vec![hir::Expr {
                        result_ty: Ty::Any.into(),
                        kind: hir::ExprKind::Ref(EMPTY_SPAN, subject_var_id),
                    }],
                    rest_arg_expr: None,
                })),
            },
        },
    )
}

fn new_record_cons_arret_fun(cons: &record::ConsId) -> value::ArretFun {
    let record::ConsPolymorphicVars {
        ty_args,
        pvars,
        tvars,
    } = cons.polymorphic_vars();

    let cons_fun_ty = record::Cons::value_cons_fun_type(cons);
    let record_instance_ty = Ty::Record(Box::new(record::Instance::new(
        cons.clone(),
        ty_args.clone(),
    )));

    let param_var_ids: Vec<hir::VarId> = hir::VarId::alloc_iter(cons.fields().len()).collect();

    let params = hir::destruc::List::new(
        cons.fields()
            .iter()
            .zip(param_var_ids.iter())
            .map(|(field, param_var_id)| {
                hir::destruc::Destruc::Scalar(
                    EMPTY_SPAN,
                    hir::destruc::Scalar::new(
                        Some(*param_var_id),
                        field.name().clone(),
                        field.ty_ref().clone(),
                    ),
                )
            })
            .collect(),
        None,
    );

    let cons_arg_exprs = cons
        .fields()
        .iter()
        .zip(param_var_ids.iter())
        .map(|(field, param_var_id)| hir::Expr {
            result_ty: field.ty_ref().clone(),
            kind: hir::ExprKind::Ref(EMPTY_SPAN, *param_var_id),
        })
        .collect();

    value::ArretFun::new(
        Some(cons.value_cons_name().clone()),
        // These are the environment type args, not our own
        TyArgs::empty(),
        Closure::empty(),
        hir::Fun {
            span: EMPTY_SPAN,

            pvars,
            tvars,

            purity: Purity::Pure.into(),
            params,
            ret_ty: record_instance_ty.clone().into(),

            body_expr: hir::Expr {
                result_ty: record_instance_ty.clone().into(),
                kind: hir::ExprKind::App(Box::new(hir::App {
                    span: EMPTY_SPAN,
                    fun_expr: hir::Expr {
                        result_ty: cons_fun_ty.into(),
                        kind: hir::ExprKind::RecordCons(EMPTY_SPAN, cons.clone()),
                    },
                    ty_args: ty_args.clone(),
                    fixed_arg_exprs: cons_arg_exprs,
                    rest_arg_expr: None,
                })),
            },
        },
    )
}

fn new_field_accessor_arret_fun(cons: &record::ConsId, field_index: usize) -> value::ArretFun {
    let record::ConsPolymorphicVars {
        ty_args,
        pvars,
        tvars,
    } = cons.polymorphic_vars();

    let field = &cons.fields()[field_index];
    let accessor_fun_ty = field.accessor_fun_type(cons);
    let record_instance_ty = Ty::Record(Box::new(record::Instance::new(
        cons.clone(),
        ty_args.clone(),
    )));

    let record_var_id = hir::VarId::alloc();
    value::ArretFun::new(
        Some(format!("{}-{}", cons.value_cons_name(), field.name()).into()),
        // These are the environment type args, not our own
        TyArgs::empty(),
        Closure::empty(),
        hir::Fun {
            span: EMPTY_SPAN,

            pvars,
            tvars,

            purity: Purity::Pure.into(),
            params: hir::destruc::List::new(
                vec![hir::destruc::Destruc::Scalar(
                    EMPTY_SPAN,
                    hir::destruc::Scalar::new(
                        Some(record_var_id),
                        cons.value_cons_name().clone(),
                        record_instance_ty.clone().into(),
                    ),
                )],
                None,
            ),
            ret_ty: field.ty_ref().clone(),

            body_expr: hir::Expr {
                result_ty: field.ty_ref().clone(),
                kind: hir::ExprKind::App(Box::new(hir::App {
                    span: EMPTY_SPAN,
                    fun_expr: hir::Expr {
                        result_ty: accessor_fun_ty.into(),
                        kind: hir::ExprKind::FieldAccessor(Box::new(hir::FieldAccessor {
                            span: EMPTY_SPAN,
                            record_cons: cons.clone(),
                            field_index,
                        })),
                    },
                    ty_args: ty_args.clone(),
                    fixed_arg_exprs: vec![hir::Expr {
                        result_ty: record_instance_ty.into(),
                        kind: hir::ExprKind::Ref(EMPTY_SPAN, record_var_id),
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
    record_cons_arret_fun: HashMap<record::ConsId, value::ArretFun>,
    field_accessor_arret_fun: HashMap<(record::ConsId, usize), value::ArretFun>,
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

    pub fn record_cons_arret_fun(&mut self, cons: &record::ConsId) -> &value::ArretFun {
        self.record_cons_arret_fun
            .entry(cons.clone())
            .or_insert_with(|| new_record_cons_arret_fun(cons))
    }

    pub fn field_accessor_arret_fun(
        &mut self,
        cons: &record::ConsId,
        field_index: usize,
    ) -> &value::ArretFun {
        let lookup_key = (cons.clone(), field_index);

        self.field_accessor_arret_fun
            .entry(lookup_key)
            .or_insert_with(|| new_field_accessor_arret_fun(cons, field_index))
    }
}
