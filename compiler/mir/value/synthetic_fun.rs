use std::collections::HashMap;

use arret_syntax::datum::DataStr;
use arret_syntax::span::EMPTY_SPAN;

use crate::hir;
use crate::hir::var_id::ModuleVarIdAlloc;
use crate::mir::env_values::EnvValues;
use crate::mir::value;
use crate::ty;
use crate::ty::purity;
use crate::ty::purity::Purity;
use crate::ty::record;
use crate::ty::ty_args::TyArgs;
use crate::ty::Ty;

struct ExprParam {
    source_name: DataStr,
    poly_type: ty::Ref<ty::Poly>,
}

fn wrap_poly_expr_in_arret_fun(
    source_name: DataStr,
    ty_args: TyArgs<ty::Poly>,
    pvars: purity::PVars,
    tvars: ty::TVars,
    expr_params: &[ExprParam],
    ret_ty: ty::Ref<ty::Poly>,
    wrapped_expr: hir::Expr<hir::Inferred>,
) -> value::ArretFun {
    let mvia = ModuleVarIdAlloc::new();

    let expr_params_with_var_id: Vec<(&ExprParam, hir::VarId)> = expr_params
        .iter()
        .map(|expr_param| (expr_param, mvia.alloc()))
        .collect();

    let params = hir::destruc::List::new(
        expr_params_with_var_id
            .iter()
            .map(|(expr_param, param_var_id)| {
                hir::destruc::Destruc::Scalar(
                    EMPTY_SPAN,
                    hir::destruc::Scalar::new(
                        Some(*param_var_id),
                        expr_param.source_name.clone(),
                        expr_param.poly_type.clone(),
                    ),
                )
            })
            .collect(),
        None,
    );

    let fixed_arg_exprs = expr_params_with_var_id
        .iter()
        .map(|(expr_param, param_var_id)| hir::Expr {
            result_ty: expr_param.poly_type.clone(),
            kind: hir::ExprKind::Ref(EMPTY_SPAN, *param_var_id),
        })
        .collect();

    value::ArretFun::new(
        Some(source_name),
        // These are the environment type args, not our own
        TyArgs::empty(),
        EnvValues::empty(),
        hir::Fun {
            span: EMPTY_SPAN,

            pvars,
            tvars,

            purity: Purity::Pure.into(),
            params,
            ret_ty: ret_ty.clone(),
            ret_ty_span: None,

            body_expr: hir::Expr {
                result_ty: ret_ty,
                kind: hir::ExprKind::App(Box::new(hir::App {
                    span: EMPTY_SPAN,
                    fun_expr: wrapped_expr,
                    ty_args,
                    fixed_arg_exprs,
                    rest_arg_expr: None,
                })),
            },
        },
    )
}

fn wrap_mono_expr_in_arret_fun(
    source_name: DataStr,
    expr_params: &[ExprParam],
    ret_ty: ty::Ref<ty::Poly>,
    wrapped_expr: hir::Expr<hir::Inferred>,
) -> value::ArretFun {
    wrap_poly_expr_in_arret_fun(
        source_name,
        TyArgs::empty(),
        purity::PVars::new(),
        ty::TVars::new(),
        expr_params,
        ret_ty,
        wrapped_expr,
    )
}

fn new_eq_pred_arret_fun() -> value::ArretFun {
    let expr_params = [
        ExprParam {
            source_name: "left".into(),
            poly_type: Ty::Any.into(),
        },
        ExprParam {
            source_name: "right".into(),
            poly_type: Ty::Any.into(),
        },
    ];

    let wrapped_expr = hir::Expr {
        result_ty: Ty::EqPred.into(),
        kind: hir::ExprKind::EqPred(EMPTY_SPAN),
    };

    wrap_mono_expr_in_arret_fun("=".into(), &expr_params, Ty::Bool.into(), wrapped_expr)
}

fn new_ty_pred_arret_fun(test_ty: ty::pred::TestTy) -> value::ArretFun {
    let expr_params = [ExprParam {
        source_name: "subject".into(),
        poly_type: Ty::Any.into(),
    }];

    let wrapped_expr = hir::Expr {
        result_ty: Ty::TyPred(test_ty.clone()).into(),
        kind: hir::ExprKind::TyPred(EMPTY_SPAN, test_ty.clone()),
    };

    wrap_mono_expr_in_arret_fun(
        test_ty.to_string().into(),
        &expr_params,
        Ty::Bool.into(),
        wrapped_expr,
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

    let expr_params: Vec<ExprParam> = cons
        .fields()
        .iter()
        .map(|field| ExprParam {
            source_name: field.name().clone(),
            poly_type: field.ty_ref().clone(),
        })
        .collect();

    let wrapped_expr = hir::Expr {
        result_ty: cons_fun_ty.into(),
        kind: hir::ExprKind::RecordCons(EMPTY_SPAN, cons.clone()),
    };

    wrap_poly_expr_in_arret_fun(
        cons.value_cons_name().clone(),
        ty_args,
        pvars,
        tvars,
        &expr_params,
        record_instance_ty.into(),
        wrapped_expr,
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

    let expr_params = &[ExprParam {
        source_name: cons.value_cons_name().clone(),
        poly_type: record_instance_ty.clone().into(),
    }];

    let wrapped_expr = hir::Expr {
        result_ty: accessor_fun_ty.into(),
        kind: hir::ExprKind::FieldAccessor(Box::new(hir::FieldAccessor {
            span: EMPTY_SPAN,
            record_cons: cons.clone(),
            field_index,
        })),
    };

    wrap_poly_expr_in_arret_fun(
        format!("{}-{}", cons.value_cons_name(), field.name()).into(),
        ty_args,
        pvars,
        tvars,
        expr_params,
        record_instance_ty.into(),
        wrapped_expr,
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
