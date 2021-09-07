use std::collections::HashMap;

use codespan_reporting::diagnostic::Diagnostic;

use arret_syntax::datum::Datum;
use arret_syntax::span::{FileId, Span};

use crate::ty;
use crate::ty::purity;
use crate::CompileCtx;

use crate::context::ModuleImports;
use crate::hir::destruc;
use crate::hir::error::{Error, ErrorKind, ExpectedSym, Result};
use crate::hir::exports::Exports;
use crate::hir::import;
use crate::hir::macros::{expand_macro, lower_macro_rules};
use crate::hir::ns::{Ident, NsDataIter, NsDatum};
use crate::hir::prim::Prim;
use crate::hir::records::lower_record;
use crate::hir::scope::{Binding, Scope};
use crate::hir::types::{lower_poly, lower_polymorphic_var_set, try_lower_purity};
use crate::hir::util::{expect_one_arg, expect_spanned_ns_ident, try_take_rest_arg};
use crate::hir::var_id::{ExportId, LocalIdAlloc};
use crate::hir::Lowered;
use crate::hir::{
    App, Cond, DeclPurity, DeclTy, Def, Expr, ExprKind, FieldAccessor, Fun, Let, LocalId, Recur,
};

#[cfg(test)]
use crate::source::EMPTY_SPAN;

/// Module lowered to HIR
pub struct LoweredModule {
    /// Defs in the order the were lowered
    pub defs: Vec<Def<Lowered>>,

    pub exports: Exports,
    pub main_local_id: Option<LocalId>,
}

struct DeferredDef {
    span: Span,
    macro_invocation_span: Option<Span>,
    destruc: destruc::Destruc<Lowered>,
    value_datum: NsDatum,
}

struct DeferredExport {
    span: Span,
    ident: Ident,
}

enum DeferredModulePrim {
    Def(DeferredDef),
    Exports(Vec<DeferredExport>),
}

impl DeferredModulePrim {
    fn with_macro_invocation_span(self, span: Span) -> DeferredModulePrim {
        match self {
            DeferredModulePrim::Def(deferred_def) => DeferredModulePrim::Def(DeferredDef {
                macro_invocation_span: Some(span),
                ..deferred_def
            }),
            other => other,
        }
    }
}

pub(crate) enum LoweredReplDatum {
    /// One or more modules were imported
    Import(ModuleImports),
    /// An evaluable definition
    EvaluableDef(Def<Lowered>),
    /// A non-evalable definition handled by HIR lowering
    NonEvaluableDef,
    /// An expression
    Expr(Expr<Lowered>),
}

// This would be less ugly as Result<!> once it's stabilised
fn lower_user_compile_error(span: Span, arg_iter: NsDataIter) -> Error {
    match expect_one_arg(span, arg_iter) {
        Ok(NsDatum::Str(_, user_message)) => Error::new(span, ErrorKind::UserError(user_message)),
        Ok(other) => Error::new(
            other.span(),
            ErrorKind::ExpectedCompileErrorString(other.description()),
        ),
        Err(error) => error,
    }
}

fn lower_macro(
    scope: &mut Scope<'_>,
    self_datum: NsDatum,
    transformer_spec: NsDatum,
) -> Result<()> {
    let (self_span, self_ident) = expect_spanned_ns_ident(self_datum, "new macro name")?;

    let macro_rules_data = if let NsDatum::List(span, vs) = transformer_spec {
        let mut transformer_data = vs.into_vec();

        let macro_type_datum = if let Some(macro_type_datum) = transformer_data.first() {
            macro_type_datum
        } else {
            return Err(Error::new(span, ErrorKind::NoMacroType));
        };

        if let Some(Binding::Prim(Prim::MacroRules)) = scope.get_datum(macro_type_datum) {
        } else {
            return Err(Error::new(macro_type_datum.span(), ErrorKind::BadMacroType));
        }

        transformer_data.remove(0);
        transformer_data
    } else {
        return Err(Error::new(
            transformer_spec.span(),
            ErrorKind::ExpectedMacroSpecList(transformer_spec.description()),
        ));
    };

    let mac = lower_macro_rules(scope, &self_ident, macro_rules_data)?;
    scope.insert_binding(self_span, self_ident, Binding::Macro(None, mac))?;

    Ok(())
}

fn lower_defmacro(scope: &mut Scope<'_>, span: Span, mut arg_iter: NsDataIter) -> Result<()> {
    if arg_iter.len() != 2 {
        return Err(Error::new(
            span,
            ErrorKind::WrongDefLikeArgCount("defmacro"),
        ));
    }

    let self_datum = arg_iter.next().unwrap();
    let transformer_spec = arg_iter.next().unwrap();

    lower_macro(scope, self_datum, transformer_spec)
}

fn lower_letmacro(
    lia: &LocalIdAlloc,
    scope: &Scope<'_>,
    span: Span,
    arg_iter: NsDataIter,
) -> Result<Expr<Lowered>> {
    lower_let_like(lia, scope, span, arg_iter, lower_macro, |expr, _| expr)
}

fn lower_type(scope: &mut Scope<'_>, self_datum: NsDatum, ty_datum: NsDatum) -> Result<()> {
    let (span, ident) = expect_spanned_ns_ident(self_datum, "new type name")?;
    let ty = lower_poly(scope, ty_datum)?;

    scope.insert_binding(span, ident, Binding::Ty(ty))?;
    Ok(())
}

fn lower_deftype(scope: &mut Scope<'_>, span: Span, mut arg_iter: NsDataIter) -> Result<()> {
    if arg_iter.len() != 2 {
        return Err(Error::new(span, ErrorKind::WrongDefLikeArgCount("deftype")));
    }

    let self_datum = arg_iter.next().unwrap();
    let ty_datum = arg_iter.next().unwrap();

    lower_type(scope, self_datum, ty_datum)
}

fn lower_lettype(
    lia: &LocalIdAlloc,
    scope: &Scope<'_>,
    span: Span,
    arg_iter: NsDataIter,
) -> Result<Expr<Lowered>> {
    lower_let_like(lia, scope, span, arg_iter, lower_type, |expr, _| expr)
}

fn lower_defrecord(scope: &mut Scope<'_>, span: Span, mut arg_iter: NsDataIter) -> Result<()> {
    if arg_iter.len() != 2 {
        return Err(Error::new(span, ErrorKind::WrongDefRecordArgCount));
    }

    let ty_cons_datum = arg_iter.next().unwrap();
    let value_cons_datum = arg_iter.next().unwrap();

    lower_record(scope, ty_cons_datum, value_cons_datum)
}

fn lower_letrecord(
    lia: &LocalIdAlloc,
    scope: &Scope<'_>,
    span: Span,
    arg_iter: NsDataIter,
) -> Result<Expr<Lowered>> {
    lower_let_like(lia, scope, span, arg_iter, lower_record, |expr, _| expr)
}

/// Lowers an identifier in to a scalar destruc with the passed type
fn lower_ident_destruc(
    lia: &LocalIdAlloc,
    scope: &mut Scope<'_>,
    span: Span,
    ident: Ident,
    decl_ty: DeclTy,
) -> Result<destruc::Scalar<Lowered>> {
    if ident.is_underscore() {
        Ok(destruc::Scalar::new(None, ident.into_name(), decl_ty))
    } else {
        let local_id = lia.alloc();
        let source_name = ident.name().clone();

        scope.insert_local(span, ident, local_id)?;

        Ok(destruc::Scalar::new(Some(local_id), source_name, decl_ty))
    }
}

fn lower_scalar_destruc(
    lia: &LocalIdAlloc,
    scope: &mut Scope<'_>,
    destruc_datum: NsDatum,
) -> Result<destruc::Scalar<Lowered>> {
    match destruc_datum {
        NsDatum::Ident(span, ident) => lower_ident_destruc(lia, scope, span, ident, DeclTy::Free),
        NsDatum::Vector(span, vs) => {
            let mut data = vs.into_vec();

            if data.len() != 2 {
                return Err(Error::new(span, ErrorKind::NoVecDestruc));
            }

            let ty = lower_poly(scope, data.pop().unwrap())?;

            let (span, ident) = expect_spanned_ns_ident(data.pop().unwrap(), "new variable name")?;
            lower_ident_destruc(lia, scope, span, ident, ty.into())
        }
        _ => Err(Error::new(destruc_datum.span(), ErrorKind::BadRestDestruc)),
    }
}

fn lower_list_destruc(
    lia: &LocalIdAlloc,
    scope: &mut Scope<'_>,
    mut data_iter: NsDataIter,
) -> Result<destruc::List<Lowered>> {
    let rest = try_take_rest_arg(&mut data_iter);

    let fixed_destrucs = data_iter
        .map(|v| lower_destruc(lia, scope, v))
        .collect::<Result<Vec<destruc::Destruc<Lowered>>>>()?;

    let rest_destruc = match rest {
        Some(rest) => Some(Box::new(lower_scalar_destruc(lia, scope, rest)?)),
        None => None,
    };

    Ok(destruc::List::new(fixed_destrucs, rest_destruc))
}

fn lower_destruc(
    lia: &LocalIdAlloc,
    scope: &mut Scope<'_>,
    destruc_datum: NsDatum,
) -> Result<destruc::Destruc<Lowered>> {
    match destruc_datum {
        NsDatum::Ident(span, _) | NsDatum::Vector(span, _) => {
            lower_scalar_destruc(lia, scope, destruc_datum)
                .map(|scalar| destruc::Destruc::Scalar(span, scalar))
        }

        NsDatum::List(span, vs) => lower_list_destruc(lia, scope, vs.into_vec().into_iter())
            .map(|list_destruc| destruc::Destruc::List(span, list_destruc)),

        NsDatum::Keyword(span, _) => Err(Error::new(
            span,
            ErrorKind::ExpectedSym(
                ExpectedSym {
                    found: "keyword",
                    usage: "new variable name",
                }
                .into(),
            ),
        )),
        _ => Err(Error::new(destruc_datum.span(), ErrorKind::BadListDestruc)),
    }
}

fn lower_let_like<B, C, O>(
    lia: &LocalIdAlloc,
    outer_scope: &Scope<'_>,
    span: Span,
    mut arg_iter: NsDataIter,
    binder: B,
    fold_output: C,
) -> Result<Expr<Lowered>>
where
    B: Fn(&mut Scope<'_>, NsDatum, NsDatum) -> Result<O>,
    C: Fn(Expr<Lowered>, O) -> Expr<Lowered>,
{
    let bindings_datum = arg_iter
        .next()
        .ok_or_else(|| Error::new(span, ErrorKind::NoBindingVec))?;

    let bindings_data = if let NsDatum::Vector(_, vs) = bindings_datum {
        vs.into_vec()
    } else {
        return Err(Error::new(
            bindings_datum.span(),
            ErrorKind::BindingsNotVec(bindings_datum.description()),
        ));
    };

    let mut scope = outer_scope.child();
    let mut outputs = Vec::<O>::with_capacity(bindings_data.len() / 2);

    let mut bindings_iter = bindings_data.into_iter();
    while let Some(target_datum) = bindings_iter.next() {
        let value_datum = bindings_iter
            .next()
            .ok_or_else(|| Error::new(target_datum.span(), ErrorKind::UnevenBindingVec))?;

        outputs.push(binder(&mut scope, target_datum, value_datum)?);
    }

    let body_expr = lower_body(lia, &scope, arg_iter)?;

    // This is to build nested `Let` expressions. Types/macros don't need this
    Ok(outputs.into_iter().rfold(body_expr, fold_output))
}

fn lower_body(
    lia: &LocalIdAlloc,
    scope: &Scope<'_>,
    body_data: NsDataIter,
) -> Result<Expr<Lowered>> {
    let mut flattened_exprs = vec![];

    for body_datum in body_data {
        match lower_expr(lia, scope, body_datum)? {
            Expr {
                kind: ExprKind::Do(mut exprs),
                ..
            } => {
                flattened_exprs.append(&mut exprs);
            }
            other => {
                flattened_exprs.push(other);
            }
        }
    }

    if flattened_exprs.len() == 1 {
        Ok(flattened_exprs.pop().unwrap())
    } else {
        Ok(ExprKind::Do(flattened_exprs).into())
    }
}

fn lower_let(
    lia: &LocalIdAlloc,
    scope: &Scope<'_>,
    span: Span,
    arg_iter: NsDataIter,
) -> Result<Expr<Lowered>> {
    lower_let_like(
        lia,
        scope,
        span,
        arg_iter,
        |scope, target_datum, value_datum| {
            let value_expr = lower_expr(lia, scope, value_datum)?;
            let destruc = lower_destruc(lia, scope, target_datum)?;
            Ok((destruc, value_expr))
        },
        |body_expr, (destruc, value_expr)| {
            ExprKind::Let(Box::new(Let {
                span,
                destruc,
                value_expr,
                body_expr,
            }))
            .into()
        },
    )
}

fn lower_fun(
    lia: &LocalIdAlloc,
    outer_scope: &Scope<'_>,
    span: Span,
    mut arg_iter: NsDataIter,
) -> Result<Expr<Lowered>> {
    let mut fun_scope = outer_scope.child();

    let mut next_datum = arg_iter
        .next()
        .ok_or_else(|| Error::new(span, ErrorKind::NoParamDecl))?;

    // We can either begin with a set of type variables or a list of parameters
    let (pvars, tvars) = if let NsDatum::Set(_, vs) = next_datum {
        next_datum = arg_iter
            .next()
            .ok_or_else(|| Error::new(span, ErrorKind::NoParamDecl))?;

        lower_polymorphic_var_set(outer_scope, &mut fun_scope, vs.into_vec().into_iter())?
    } else {
        (purity::PVars::new(), ty::TVars::new())
    };

    // Pull out our params
    let params = match next_datum {
        NsDatum::List(_, vs) => lower_list_destruc(lia, &mut fun_scope, vs.into_vec().into_iter())?,
        other => {
            return Err(Error::new(
                other.span(),
                ErrorKind::ExpectedParamList(other.description()),
            ));
        }
    };

    // Determine if we have a purity and return type after the parameters, eg (param) -> RetTy
    let mut purity = DeclPurity::Free;
    let mut ret_ty = DeclTy::Free;
    let mut ret_ty_span = None;

    if arg_iter.len() >= 2 {
        if let Some(poly_purity) = try_lower_purity(&fun_scope, &arg_iter.as_slice()[0]) {
            arg_iter.next();
            purity = poly_purity.into();

            match arg_iter.next().unwrap() {
                NsDatum::Ident(_, ref ident) if ident.is_underscore() => {}
                ret_datum => {
                    ret_ty_span = Some(ret_datum.span());
                    ret_ty = lower_poly(&fun_scope, ret_datum)?.into();
                }
            }
        }
    }

    // Extract the body
    let body_expr = lower_body(lia, &fun_scope, arg_iter)?;

    Ok(ExprKind::Fun(Box::new(Fun {
        span,
        pvars,
        tvars,
        purity,
        params,
        ret_ty,
        ret_ty_span,
        body_expr,
    }))
    .into())
}

fn lower_expr_prim_apply(
    lia: &LocalIdAlloc,
    scope: &Scope<'_>,
    span: Span,
    prim: Prim,
    mut arg_iter: NsDataIter,
) -> Result<Expr<Lowered>> {
    match prim {
        Prim::Def | Prim::DefMacro | Prim::DefType | Prim::ImportPlaceholder | Prim::DefRecord => {
            Err(Error::new(span, ErrorKind::DefOutsideBody))
        }
        Prim::Let => lower_let(lia, scope, span, arg_iter),
        Prim::LetMacro => lower_letmacro(lia, scope, span, arg_iter),
        Prim::LetType => lower_lettype(lia, scope, span, arg_iter),
        Prim::LetRecord => lower_letrecord(lia, scope, span, arg_iter),
        Prim::Export => Err(Error::new(span, ErrorKind::ExportOutsideModule)),
        Prim::Quote => {
            let literal_datum = expect_one_arg(span, arg_iter)?;
            Ok(literal_datum.into_syntax_datum().into())
        }
        Prim::Fun => lower_fun(lia, scope, span, arg_iter),
        Prim::If => {
            if arg_iter.len() != 3 {
                return Err(Error::new(span, ErrorKind::WrongCondArgCount));
            }

            Ok(ExprKind::Cond(Box::new(Cond {
                span,
                test_expr: lower_expr(lia, scope, arg_iter.next().unwrap())?,
                true_expr: lower_expr(lia, scope, arg_iter.next().unwrap())?,
                false_expr: lower_expr(lia, scope, arg_iter.next().unwrap())?,
            }))
            .into())
        }
        Prim::Do => lower_body(lia, scope, arg_iter),
        Prim::Recur => lower_recur(lia, scope, span, arg_iter),
        Prim::CompileError => Err(lower_user_compile_error(span, arg_iter)),
        Prim::MacroRules | Prim::All => {
            Err(Error::new(span, ErrorKind::ExpectedValue("primitive")))
        }
    }
}

fn lower_expr_apply(
    lia: &LocalIdAlloc,
    scope: &Scope<'_>,
    span: Span,
    fun_expr: Expr<Lowered>,
    mut arg_iter: NsDataIter,
) -> Result<Expr<Lowered>> {
    let rest_arg_datum = try_take_rest_arg(&mut arg_iter);

    let fixed_arg_exprs = arg_iter
        .map(|arg_datum| lower_expr(lia, scope, arg_datum))
        .collect::<Result<Vec<Expr<Lowered>>>>()?;

    let rest_arg_expr = match rest_arg_datum {
        Some(rest_arg_datum) => Some(lower_expr(lia, scope, rest_arg_datum)?),
        None => None,
    };

    Ok(ExprKind::App(Box::new(App {
        span,
        fun_expr,
        ty_args: (),
        fixed_arg_exprs,
        rest_arg_expr,
    }))
    .into())
}

fn lower_recur(
    lia: &LocalIdAlloc,
    scope: &Scope<'_>,
    span: Span,
    mut arg_iter: NsDataIter,
) -> Result<Expr<Lowered>> {
    let rest_arg_datum = try_take_rest_arg(&mut arg_iter);

    let fixed_arg_exprs = arg_iter
        .map(|arg_datum| lower_expr(lia, scope, arg_datum))
        .collect::<Result<Vec<Expr<Lowered>>>>()?;

    let rest_arg_expr = match rest_arg_datum {
        Some(rest_arg_datum) => Some(lower_expr(lia, scope, rest_arg_datum)?),
        None => None,
    };

    Ok(ExprKind::Recur(Box::new(Recur {
        span,
        fixed_arg_exprs,
        rest_arg_expr,
    }))
    .into())
}

fn lower_expr(lia: &LocalIdAlloc, scope: &Scope<'_>, datum: NsDatum) -> Result<Expr<Lowered>> {
    match datum {
        NsDatum::Ident(span, ident) => match scope.get_or_err(span, &ident)? {
            Binding::Var(Some(module_id), local_id) => {
                Ok(ExprKind::ExportRef(span, ExportId::new(*module_id, *local_id)).into())
            }
            Binding::Var(None, local_id) => Ok(ExprKind::LocalRef(span, *local_id).into()),
            Binding::TyPred(test_ty) => Ok(ExprKind::TyPred(span, test_ty.clone()).into()),
            Binding::EqPred => Ok(ExprKind::EqPred(span).into()),
            Binding::RecordValueCons(record_cons) => {
                Ok(ExprKind::RecordCons(span, record_cons.clone()).into())
            }
            Binding::FieldAccessor(record_cons, field_index) => {
                Ok(ExprKind::FieldAccessor(Box::new(FieldAccessor {
                    span,
                    record_cons: record_cons.clone(),
                    field_index: *field_index,
                }))
                .into())
            }
            other => Err(Error::new(
                span,
                ErrorKind::ExpectedValue(other.description()),
            )),
        },
        NsDatum::List(span, vs) => {
            let mut data_iter = vs.into_vec().into_iter();

            let fn_datum = if let Some(fn_datum) = data_iter.next() {
                fn_datum
            } else {
                return Ok(Datum::List(span, Box::new([])).into());
            };

            if let NsDatum::Ident(fn_span, ref ident) = fn_datum {
                match scope.get_or_err(fn_span, ident)? {
                    Binding::Prim(prim) => {
                        return lower_expr_prim_apply(lia, scope, span, *prim, data_iter);
                    }
                    Binding::Macro(module_id, mac) => {
                        let mut macro_scope = scope.child();

                        let expanded_datum = expand_macro(
                            &mut macro_scope,
                            span,
                            *module_id,
                            mac,
                            data_iter.as_slice(),
                        )?;

                        return lower_expr(lia, &macro_scope, expanded_datum)
                            .map(|expr| ExprKind::MacroExpand(span, Box::new(expr)).into())
                            .map_err(|e| e.with_macro_invocation_span(span));
                    }
                    _ => {}
                }
            }

            let fn_expr = lower_expr(lia, scope, fn_datum)?;
            lower_expr_apply(lia, scope, span, fn_expr, data_iter)
        }
        other => Ok(other.into_syntax_datum().into()),
    }
}

fn lower_module_prim_apply(
    lia: &LocalIdAlloc,
    scope: &mut Scope<'_>,
    span: Span,
    prim: Prim,
    mut arg_iter: NsDataIter,
) -> Result<Option<DeferredModulePrim>, Vec<Error>> {
    match prim {
        Prim::Export => {
            let deferred_exports = arg_iter
                .map(|datum| {
                    let (span, ident) = expect_spanned_ns_ident(datum, "identifier to export")?;
                    Ok(DeferredExport { span, ident })
                })
                .collect::<Result<Vec<DeferredExport>>>()?;

            Ok(Some(DeferredModulePrim::Exports(deferred_exports)))
        }
        Prim::Def => {
            if arg_iter.len() != 2 {
                return Err(vec![Error::new(
                    span,
                    ErrorKind::WrongDefLikeArgCount("def"),
                )]);
            }

            let destruc_datum = arg_iter.next().unwrap();
            let destruc = lower_destruc(lia, scope, destruc_datum)?;

            let value_datum = arg_iter.next().unwrap();

            let deferred_def = DeferredDef {
                span,
                macro_invocation_span: None,
                destruc,
                value_datum,
            };

            Ok(Some(DeferredModulePrim::Def(deferred_def)))
        }
        Prim::DefMacro => Ok(lower_defmacro(scope, span, arg_iter).map(|_| None)?),
        Prim::DefType => Ok(lower_deftype(scope, span, arg_iter).map(|_| None)?),
        Prim::DefRecord => Ok(lower_defrecord(scope, span, arg_iter).map(|_| None)?),
        Prim::CompileError => Err(vec![lower_user_compile_error(span, arg_iter)]),
        _ => Err(vec![Error::new(span, ErrorKind::NonDefInsideModule)]),
    }
}

fn lower_module_def(
    lia: &LocalIdAlloc,
    scope: &mut Scope<'_>,
    datum: NsDatum,
) -> Result<Option<DeferredModulePrim>, Vec<Error>> {
    let span = datum.span();

    if let NsDatum::List(span, vs) = datum {
        let mut data_iter = vs.into_vec().into_iter();

        if let Some(NsDatum::Ident(fn_span, ref ident)) = data_iter.next() {
            match scope.get_or_err(fn_span, ident)? {
                Binding::Prim(prim) => {
                    let prim = *prim;
                    return lower_module_prim_apply(lia, scope, span, prim, data_iter);
                }
                Binding::Macro(module_id, mac) => {
                    let module_id = *module_id;
                    let mac = &mac.clone();

                    let expanded_datum =
                        expand_macro(scope, span, module_id, mac, data_iter.as_slice())?;

                    return lower_module_def(lia, scope, expanded_datum)
                        .map(|def| def.map(|def| def.with_macro_invocation_span(span)))
                        .map_err(|errs| {
                            errs.into_iter()
                                .map(|e| e.with_macro_invocation_span(span))
                                .collect()
                        });
                }
                _ => {
                    // Non-def
                }
            }
        }
    }

    Err(vec![Error::new(span, ErrorKind::NonDefInsideModule)])
}

fn insert_import_bindings(
    imports: &ModuleImports,
    scope: &mut Scope<'_>,
    arg_data: &[Datum],
) -> Result<(), Vec<Error>> {
    for arg_datum in arg_data {
        let span = arg_datum.span();

        let parsed_import = import::parse_import_set(arg_datum)?;
        let import_module = &imports[parsed_import.module_name()];

        let exports = import::filter_imported_exports(parsed_import, &import_module.exports)?;

        scope.insert_bindings(
            span,
            exports.into_iter().map(|(name, binding)| {
                (
                    Ident::new(Scope::root_ns_id(), name),
                    binding.import_from(import_module.module_id),
                )
            }),
        )?;
    }

    Ok(())
}

pub(crate) fn lower_data(
    imports: &ModuleImports,
    data: &[Datum],
) -> Result<LoweredModule, Vec<Error>> {
    let lia = LocalIdAlloc::new();
    let mut scope = Scope::root();

    // Build up a list of errors to return at once
    let mut errors: Vec<Error> = vec![];

    // Extract all of our definitions.
    //
    // This occurs in two passes:
    // - Imports, types and macros are resolved immediately and cannot refer to bindings later
    //   in the body
    // - Definitions are resolved after the module has been loaded
    let mut deferred_exports = Vec::<DeferredExport>::new();
    let mut deferred_defs = Vec::<DeferredDef>::new();

    for input_datum in data {
        if let Some(arg_data) = import::try_extract_import_set(input_datum) {
            if let Err(mut new_errors) = insert_import_bindings(imports, &mut scope, arg_data) {
                errors.append(&mut new_errors);
            }

            continue;
        }

        let ns_datum = NsDatum::from_syntax_datum(input_datum);
        match lower_module_def(&lia, &mut scope, ns_datum) {
            Ok(Some(DeferredModulePrim::Exports(mut exports))) => {
                deferred_exports.append(&mut exports);
            }
            Ok(Some(DeferredModulePrim::Def(deferred_def))) => {
                deferred_defs.push(deferred_def);
            }
            Ok(None) => {}
            Err(mut new_errors) => {
                errors.append(&mut new_errors);
            }
        };
    }

    // Process any exports
    let mut exports = HashMap::with_capacity(deferred_exports.len());
    for deferred_export in deferred_exports {
        let DeferredExport { span, ident } = deferred_export;
        match scope.get_or_err(span, &ident) {
            Ok(binding) => {
                exports.insert(ident.into_name(), binding.clone());
            }
            Err(err) => {
                errors.push(err);
            }
        };
    }

    // And now process any deferred defs
    let mut defs = Vec::with_capacity(deferred_defs.len());
    for deferred_def in deferred_defs {
        match resolve_deferred_def(&lia, &scope, deferred_def) {
            Ok(def) => {
                defs.push(def);
            }
            Err(error) => {
                errors.push(error);
            }
        }
    }

    // Try to find `main!`. If we're not the entry module this will be ignored.
    let main_ident = Ident::new(Scope::root_ns_id(), "main!".into());
    let main_local_id = if let Some(Binding::Var(None, local_id)) = scope.get(&main_ident) {
        Some(*local_id)
    } else {
        None
    };

    if errors.is_empty() {
        Ok(LoweredModule {
            defs,
            exports,
            main_local_id,
        })
    } else {
        Err(errors)
    }
}

fn resolve_deferred_def(
    lia: &LocalIdAlloc,
    scope: &Scope<'_>,
    deferred_def: DeferredDef,
) -> Result<Def<Lowered>> {
    let DeferredDef {
        span,
        macro_invocation_span,
        destruc,
        value_datum,
    } = deferred_def;

    lower_expr(lia, scope, value_datum).map(|value_expr| Def {
        span,
        macro_invocation_span,
        destruc,
        value_expr,
    })
}

// REPL interface
pub(crate) fn lower_repl_datum(
    ccx: &CompileCtx,
    scope: &mut Scope<'_>,
    datum: &Datum,
) -> Result<LoweredReplDatum, Vec<Diagnostic<FileId>>> {
    use crate::reporting::errors_to_diagnostics;

    // For the purposes of type inference every datum is a new module
    let lia = LocalIdAlloc::new();

    if let Some(arg_data) = import::try_extract_import_set(datum) {
        let imports = ccx.imports_for_data(std::iter::once(datum))?;

        insert_import_bindings(&imports, scope, arg_data).map_err(errors_to_diagnostics)?;

        return Ok(LoweredReplDatum::Import(imports));
    }

    // Try interpreting this as a module def
    let ns_datum = NsDatum::from_syntax_datum(datum);
    match lower_module_def(&lia, scope, ns_datum.clone()) {
        Ok(Some(DeferredModulePrim::Def(deferred_def))) => {
            let def =
                resolve_deferred_def(&lia, scope, deferred_def).map_err(|err| vec![err.into()])?;

            Ok(LoweredReplDatum::EvaluableDef(def))
        }
        Ok(Some(DeferredModulePrim::Exports(_))) => {
            Err(vec![
                Error::new(datum.span(), ErrorKind::ExportInsideRepl).into()
            ])
        }
        Ok(None) => Ok(LoweredReplDatum::NonEvaluableDef),
        Err(mut errs) => {
            // `NonDefInsideModule` doesn't apply because we allow non-defs in the REPL
            errs.retain(|err| err.kind() != &ErrorKind::NonDefInsideModule);

            if errs.is_empty() {
                // Re-interpret as an expression
                let expr = lower_expr(&lia, scope, ns_datum).map_err(|err| vec![err.into()])?;

                Ok(LoweredReplDatum::Expr(expr))
            } else {
                Err(errors_to_diagnostics(errs))
            }
        }
    }
}

////
#[cfg(test)]
fn import_statement_for_module(names: &[&'static str]) -> Datum {
    Datum::List(
        EMPTY_SPAN,
        Box::new([
            Datum::Sym(EMPTY_SPAN, "import".into()),
            Datum::Vector(
                EMPTY_SPAN,
                names
                    .iter()
                    .map(|&n| Datum::Sym(EMPTY_SPAN, n.into()))
                    .collect(),
            ),
        ]),
    )
}

#[cfg(test)]
fn module_for_str(data_str: &str) -> Result<LoweredModule> {
    use std::iter;
    use std::sync::Arc;

    use arret_syntax::parser::data_from_str;

    use crate::context;
    use crate::hir::exports;
    use crate::hir::loader::ModuleName;

    let mut program_data = vec![];
    let mut imports: ModuleImports = HashMap::new();

    for (terminal_name, exports) in iter::once(("primitives", exports::prims_exports()))
        .chain(iter::once(("types", exports::tys_exports())))
    {
        program_data.push(import_statement_for_module(&[
            "arret",
            "internal",
            terminal_name,
        ]));

        imports.insert(
            ModuleName::new(
                "arret".into(),
                vec!["internal".into()],
                terminal_name.into(),
            ),
            Arc::new(context::prims_to_module(exports)),
        );
    }

    let mut test_data = data_from_str(None, data_str).unwrap();
    program_data.append(&mut test_data);

    imports.insert(
        ModuleName::new("arret".into(), vec!["internal".into()], "types".into()),
        Arc::new(context::prims_to_module(exports::tys_exports())),
    );

    lower_data(&imports, &program_data).map_err(|mut errors| errors.remove(0))
}

#[cfg(test)]
pub fn expr_for_str(data_str: &str) -> Expr<Lowered> {
    use arret_syntax::parser::datum_from_str;

    let lia = LocalIdAlloc::new();
    let scope = Scope::new_with_primitives();

    let test_datum = datum_from_str(None, data_str).unwrap();
    let test_nsdatum = NsDatum::from_syntax_datum(&test_datum);

    lower_expr(&lia, &scope, test_nsdatum).unwrap()
}

#[allow(clippy::many_single_char_names)]
#[cfg(test)]
mod test {
    use super::*;

    use arret_syntax::span::t2s;

    use crate::ty::purity::Purity;
    use crate::ty::Ty;

    #[test]
    fn self_quoting_bool() {
        let j = "false";
        let t = "^^^^^";

        let expected: Expr<_> = Datum::Bool(t2s(t), false).into();
        assert_eq!(expected, expr_for_str(j));
    }

    #[test]
    fn self_quoting_empty_list() {
        let j = "()";
        let t = "^^";

        let expected: Expr<_> = Datum::List(t2s(t), Box::new([])).into();
        assert_eq!(expected, expr_for_str(j));
    }

    #[test]
    fn quoted_datum_shorthand() {
        let j = "'foo";
        let t = " ^^^";

        let expected: Expr<_> = Datum::Sym(t2s(t), "foo".into()).into();
        assert_eq!(expected, expr_for_str(j));
    }

    #[test]
    fn quoted_datum_explicit() {
        let j = "(quote foo)";
        let t = "       ^^^ ";

        let expected: Expr<_> = Datum::Sym(t2s(t), "foo".into()).into();
        assert_eq!(expected, expr_for_str(j));
    }

    #[test]
    fn self_evaluating_keyword() {
        let j = ":foo";
        let t = "^^^^";

        let expected: Expr<_> = Datum::Sym(t2s(t), ":foo".into()).into();
        assert_eq!(expected, expr_for_str(j));
    }

    #[test]
    fn wildcard_let() {
        let j = "(let [_ 1])";
        let t = "      ^    ";
        let u = "^^^^^^^^^^^";
        let v = "        ^  ";

        let destruc =
            destruc::Destruc::Scalar(t2s(t), destruc::Scalar::new(None, "_".into(), DeclTy::Free));

        let expected: Expr<_> = ExprKind::Let(Box::new(Let {
            span: t2s(u),
            destruc,
            value_expr: Datum::Int(t2s(v), 1).into(),
            body_expr: ExprKind::Do(vec![]).into(),
        }))
        .into();

        assert_eq!(expected, expr_for_str(j));
    }

    #[test]
    fn empty_fn() {
        let j = "(fn ())";
        let t = "^^^^^^^";

        let expected: Expr<_> = ExprKind::Fun(Box::new(Fun {
            span: t2s(t),
            pvars: purity::PVars::new(),
            tvars: ty::TVars::new(),
            purity: DeclPurity::Free,
            params: destruc::List::new(vec![], None),
            ret_ty: DeclTy::Free,
            ret_ty_span: None,
            body_expr: ExprKind::Do(vec![]).into(),
        }))
        .into();

        assert_eq!(expected, expr_for_str(j));
    }

    #[test]
    fn empty_fn_with_purity() {
        let j = "(fn () -> _ 1)";
        let t = "^^^^^^^^^^^^^^";
        let u = "            ^ ";

        let expected: Expr<_> = ExprKind::Fun(Box::new(Fun {
            span: t2s(t),
            pvars: purity::PVars::new(),
            tvars: ty::TVars::new(),
            purity: Purity::Pure.into(),
            params: destruc::List::new(vec![], None),
            ret_ty: DeclTy::Free,
            ret_ty_span: None,
            body_expr: Datum::Int(t2s(u), 1).into(),
        }))
        .into();

        assert_eq!(expected, expr_for_str(j));
    }

    #[test]
    fn empty_fn_with_ret_ty() {
        let j = "(fn () -> Int 1)";
        let t = "^^^^^^^^^^^^^^^^";
        let u = "          ^^^   ";
        let v = "              ^ ";

        let expected: Expr<_> = ExprKind::Fun(Box::new(Fun {
            span: t2s(t),
            pvars: purity::PVars::new(),
            tvars: ty::TVars::new(),
            purity: Purity::Pure.into(),
            params: destruc::List::new(vec![], None),
            ret_ty: Ty::Int.into(),
            ret_ty_span: Some(t2s(u)),
            body_expr: Datum::Int(t2s(v), 1).into(),
        }))
        .into();

        assert_eq!(expected, expr_for_str(j));
    }

    #[test]
    fn fixed_expr_apply() {
        let j = "(1 2 3)";
        let t = "^^^^^^^";
        let u = " ^     ";
        let v = "   ^   ";
        let w = "     ^ ";

        let expected: Expr<_> = ExprKind::App(Box::new(App {
            span: t2s(t),
            fun_expr: Datum::Int(t2s(u), 1).into(),
            ty_args: (),
            fixed_arg_exprs: vec![Datum::Int(t2s(v), 2).into(), Datum::Int(t2s(w), 3).into()],
            rest_arg_expr: None,
        }))
        .into();

        assert_eq!(expected, expr_for_str(j));
    }

    #[test]
    fn rest_expr_apply() {
        let j = "(1 2 & 3)";
        let t = "^^^^^^^^^";
        let u = " ^       ";
        let v = "   ^     ";
        let w = "       ^ ";

        let expected: Expr<_> = ExprKind::App(Box::new(App {
            span: t2s(t),
            fun_expr: Datum::Int(t2s(u), 1).into(),
            ty_args: (),
            fixed_arg_exprs: vec![Datum::Int(t2s(v), 2).into()],
            rest_arg_expr: Some(Datum::Int(t2s(w), 3).into()),
        }))
        .into();

        assert_eq!(expected, expr_for_str(j));
    }

    #[test]
    fn recur_expr() {
        let j = "(recur 1 2 3)";
        let t = "^^^^^^^^^^^^^";
        let u = "       ^     ";
        let v = "         ^   ";
        let w = "           ^ ";

        let expected: Expr<_> = ExprKind::Recur(Box::new(Recur {
            span: t2s(t),
            fixed_arg_exprs: vec![
                Datum::Int(t2s(u), 1).into(),
                Datum::Int(t2s(v), 2).into(),
                Datum::Int(t2s(w), 3).into(),
            ],
            rest_arg_expr: None,
        }))
        .into();

        assert_eq!(expected, expr_for_str(j));
    }

    #[test]
    fn if_expr() {
        let j = "(if true 1 2)";
        let t = "^^^^^^^^^^^^^";
        let u = "    ^^^^     ";
        let v = "         ^   ";
        let w = "           ^ ";

        let expected: Expr<_> = ExprKind::Cond(Box::new(Cond {
            span: t2s(t),
            test_expr: ExprKind::Lit(Datum::Bool(t2s(u), true)).into(),
            true_expr: ExprKind::Lit(Datum::Int(t2s(v), 1)).into(),
            false_expr: ExprKind::Lit(Datum::Int(t2s(w), 2)).into(),
        }))
        .into();

        assert_eq!(expected, expr_for_str(j));
    }

    #[test]
    fn expand_trivial_macro() {
        let j = "(letmacro [one (macro-rules [() 1])] (one))";
        let t = "                                     ^^^^^ ";
        let u = "                                ^          ";

        let expected: Expr<_> =
            ExprKind::MacroExpand(t2s(t), Box::new(Datum::Int(t2s(u), 1).into())).into();
        assert_eq!(expected, expr_for_str(j));
    }

    #[test]
    fn mutual_module_def() {
        let j1 = "(export x y)";
        let j2 = "(def x y)";
        let j3 = "(def y x)";

        let j = &[j1, j2, j3].join("");

        let module = module_for_str(j).unwrap();
        assert_eq!(2, module.exports.len());
    }

    #[test]
    fn type_predicate() {
        let j = "bool?";
        let t = "^^^^^";

        let expected: Expr<_> = ExprKind::TyPred(t2s(t), ty::pred::TestTy::Bool).into();
        assert_eq!(expected, expr_for_str(j));
    }

    #[test]
    fn equality_predicate() {
        let j = "=";
        let t = "^";

        let expected: Expr<_> = ExprKind::EqPred(t2s(t)).into();
        assert_eq!(expected, expr_for_str(j));
    }
}
