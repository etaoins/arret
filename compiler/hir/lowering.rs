use std::collections::HashMap;
use std::sync::Arc;

use syntax::datum::Datum;
use syntax::span::{Span, EMPTY_SPAN};

use crate::rfi;
use crate::source::SourceFile;
use crate::ty;
use crate::ty::purity;
use crate::CompileCtx;

use crate::hir::destruc;
use crate::hir::error::{Error, ErrorKind, Result};
use crate::hir::exports::Exports;
use crate::hir::import::lower_import_set;
use crate::hir::loader::{load_module_by_name, LoadedModule, ModuleName};
use crate::hir::macros::{expand_macro, lower_macro_rules, MacroId};
use crate::hir::ns::{Ident, NsDataIter, NsDatum, NsId};
use crate::hir::prim::Prim;
use crate::hir::scope::{Binding, Scope};
use crate::hir::types::lower_polymorphic_vars;
use crate::hir::types::{lower_poly, try_lower_purity};
use crate::hir::util::{
    expect_arg_count, expect_ident_and_span, expect_one_arg, try_take_rest_arg,
};
use crate::hir::Lowered;
use crate::hir::{App, Cond, DeclPurity, DeclTy, Def, Expr, ExprKind, Fun, Let, VarId};

#[derive(Debug)]
struct LoweredModule {
    defs: Vec<Def<Lowered>>,
    exports: Exports,
    main_var_id: Option<VarId>,
}

pub struct LoweringCtx<'ccx> {
    ccx: &'ccx CompileCtx,
    rust_libraries: Vec<Arc<rfi::Library>>,
    module_exports: HashMap<ModuleName, Exports>,
    module_defs: Vec<Vec<Def<Lowered>>>,
}

pub struct LoweredProgram {
    pub defs: Vec<Vec<Def<Lowered>>>,
    pub rust_libraries: Vec<Arc<rfi::Library>>,
    pub main_var_id: VarId,
}

struct DeferredDef {
    span: Span,
    macro_invocation_span: Span,
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
                macro_invocation_span: span,
                ..deferred_def
            }),
            other => other,
        }
    }
}

pub enum LoweredReplDatum {
    Expr(Expr<Lowered>),
    Defs(Vec<Vec<Def<Lowered>>>),
}

#[derive(Clone, Copy)]
struct AppliedPrim {
    /// Primitive that was applied
    prim: Prim,

    /// Namespace the primitive identifier was in
    ///
    /// This is the namespace which (import) will place the new identifiers
    ns_id: NsId,

    /// Span of the entire application
    span: Span,
}

// This would be less ugly as Result<!> once it's stabilised
fn lower_user_compile_error(span: Span, arg_iter: NsDataIter) -> Error {
    match expect_one_arg(span, arg_iter) {
        Ok(NsDatum::Str(_, user_message)) => Error::new(span, ErrorKind::UserError(user_message)),
        Ok(_) => Error::new(span, ErrorKind::IllegalArg("string expected")),
        Err(error) => error,
    }
}

fn lower_macro(scope: &mut Scope, self_datum: NsDatum, transformer_spec: NsDatum) -> Result<()> {
    let (self_ident, self_span) = expect_ident_and_span(self_datum)?;

    let macro_rules_data = if let NsDatum::List(span, vs) = transformer_spec {
        let mut transformer_data = vs.into_vec();

        if transformer_data.first().and_then(|d| scope.get_datum(d))
            != Some(&Binding::Prim(Prim::MacroRules))
        {
            return Err(Error::new(
                span,
                ErrorKind::IllegalArg("unsupported macro type"),
            ));
        }

        transformer_data.remove(0);
        transformer_data
    } else {
        return Err(Error::new(
            transformer_spec.span(),
            ErrorKind::IllegalArg("macro specification must be a list"),
        ));
    };

    let mac = lower_macro_rules(scope, &self_ident, macro_rules_data)?;

    if self_ident.name() != "_" {
        scope.insert_binding(self_span, self_ident, Binding::Macro(MacroId::new(mac)))?;
    }

    Ok(())
}

fn lower_defmacro(scope: &mut Scope, span: Span, mut arg_iter: NsDataIter) -> Result<()> {
    expect_arg_count(span, 2, arg_iter.len())?;

    let self_datum = arg_iter.next().unwrap();
    let transformer_spec = arg_iter.next().unwrap();

    lower_macro(scope, self_datum, transformer_spec)
}

fn lower_letmacro(scope: &Scope, span: Span, arg_iter: NsDataIter) -> Result<Expr<Lowered>> {
    lower_let_like(scope, span, arg_iter, lower_macro, |expr, _| expr)
}

fn lower_type(scope: &mut Scope, self_datum: NsDatum, ty_datum: NsDatum) -> Result<()> {
    let (ident, span) = expect_ident_and_span(self_datum)?;
    let ty = lower_poly(scope, ty_datum)?;

    if ident.name() != "_" {
        scope.insert_binding(span, ident, Binding::Ty(ty))?;
    }

    Ok(())
}

fn lower_deftype(scope: &mut Scope, span: Span, mut arg_iter: NsDataIter) -> Result<()> {
    expect_arg_count(span, 2, arg_iter.len())?;

    let self_datum = arg_iter.next().unwrap();
    let ty_datum = arg_iter.next().unwrap();

    lower_type(scope, self_datum, ty_datum)
}

fn lower_lettype(scope: &Scope, span: Span, arg_iter: NsDataIter) -> Result<Expr<Lowered>> {
    lower_let_like(scope, span, arg_iter, lower_type, |expr, _| expr)
}

/// Lowers an identifier in to a scalar destruc with the passed type
fn lower_ident_destruc(
    scope: &mut Scope,
    span: Span,
    ident: Ident,
    decl_ty: DeclTy,
) -> Result<destruc::Scalar<Lowered>> {
    if ident.name() == "_" {
        Ok(destruc::Scalar::new(None, ident.into_data_name(), decl_ty))
    } else {
        let var_id = VarId::alloc();
        let source_name = ident.name().into();

        scope.insert_var(span, ident, var_id)?;
        Ok(destruc::Scalar::new(Some(var_id), source_name, decl_ty))
    }
}

fn lower_scalar_destruc(
    scope: &mut Scope,
    destruc_datum: NsDatum,
) -> Result<destruc::Scalar<Lowered>> {
    match destruc_datum {
        NsDatum::Ident(span, ident) => lower_ident_destruc(scope, span, ident, DeclTy::Free),
        NsDatum::Vector(span, vs) => {
            let mut data = vs.into_vec();

            if data.len() != 2 {
                return Err(Error::new(span, ErrorKind::NoVecDestruc));
            }

            let ty = lower_poly(scope, data.pop().unwrap())?;

            let (ident, span) = expect_ident_and_span(data.pop().unwrap())?;
            lower_ident_destruc(scope, span, ident, ty.into())
        }
        _ => Err(Error::new(
            destruc_datum.span(),
            ErrorKind::IllegalArg("expected a variable name or [name Type]"),
        )),
    }
}

fn lower_list_destruc(
    scope: &mut Scope,
    mut data_iter: NsDataIter,
) -> Result<destruc::List<Lowered>> {
    let rest = try_take_rest_arg(&mut data_iter);

    let fixed_destrucs = data_iter
        .map(|v| lower_destruc(scope, v))
        .collect::<Result<Vec<destruc::Destruc<Lowered>>>>()?;

    let rest_destruc = match rest {
        Some(rest) => Some(Box::new(lower_scalar_destruc(scope, rest)?)),
        None => None,
    };

    Ok(destruc::List::new(fixed_destrucs, rest_destruc))
}

fn lower_destruc(scope: &mut Scope, destruc_datum: NsDatum) -> Result<destruc::Destruc<Lowered>> {
    match destruc_datum {
        NsDatum::Ident(span, _) | NsDatum::Vector(span, _) => {
            lower_scalar_destruc(scope, destruc_datum)
                .map(|scalar| destruc::Destruc::Scalar(span, scalar))
        }
        NsDatum::List(span, vs) => lower_list_destruc(scope, vs.into_vec().into_iter())
            .map(|list_destruc| destruc::Destruc::List(span, list_destruc)),
        _ => Err(Error::new(
            destruc_datum.span(),
            ErrorKind::IllegalArg(
                "values can only be bound to variables or destructured into lists",
            ),
        )),
    }
}

fn lower_let_like<B, C, O>(
    outer_scope: &Scope,
    span: Span,
    mut arg_iter: NsDataIter,
    binder: B,
    fold_output: C,
) -> Result<Expr<Lowered>>
where
    B: Fn(&mut Scope, NsDatum, NsDatum) -> Result<O>,
    C: Fn(Expr<Lowered>, O) -> Expr<Lowered>,
{
    let bindings_datum = arg_iter
        .next()
        .ok_or_else(|| Error::new(span, ErrorKind::IllegalArg("bindings declaration missing")))?;

    let bindings_data = if let NsDatum::Vector(_, vs) = bindings_datum {
        vs.into_vec()
    } else {
        return Err(Error::new(
            bindings_datum.span(),
            ErrorKind::IllegalArg("binding vector expected"),
        ));
    };

    let mut scope = Scope::new_child(outer_scope);
    let mut outputs = Vec::<O>::with_capacity(bindings_data.len() / 2);

    let mut bindings_iter = bindings_data.into_iter();
    while let Some(target_datum) = bindings_iter.next() {
        let value_datum = bindings_iter.next().ok_or_else(|| {
            Error::new(
                target_datum.span(),
                ErrorKind::IllegalArg("binding vector must have an even number of forms"),
            )
        })?;

        outputs.push(binder(&mut scope, target_datum, value_datum)?);
    }

    let body_expr = lower_body(&scope, EMPTY_SPAN, arg_iter)?;

    // This is to build nested `Let` expressions. Types/macros don't need this
    Ok(outputs.into_iter().rfold(body_expr, fold_output))
}

fn lower_body(scope: &Scope, span: Span, body_data: NsDataIter) -> Result<Expr<Lowered>> {
    let mut flattened_exprs = vec![];

    for body_datum in body_data {
        match lower_expr(&scope, body_datum)? {
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
        Ok(Expr::new(span, ExprKind::Do(flattened_exprs)))
    }
}

fn lower_let(scope: &Scope, span: Span, arg_iter: NsDataIter) -> Result<Expr<Lowered>> {
    lower_let_like(
        scope,
        span,
        arg_iter,
        |scope, target_datum, value_datum| {
            let destruc = lower_destruc(scope, target_datum)?;
            let value_expr = lower_expr(scope, value_datum)?;
            Ok((destruc, value_expr))
        },
        |body_expr, (destruc, value_expr)| {
            Expr::new(
                span,
                ExprKind::Let(Box::new(Let {
                    destruc,
                    value_expr,
                    body_expr,
                })),
            )
        },
    )
}

fn lower_fun(outer_scope: &Scope, span: Span, mut arg_iter: NsDataIter) -> Result<Expr<Lowered>> {
    let mut fun_scope = Scope::new_child(outer_scope);

    let mut next_datum = arg_iter
        .next()
        .ok_or_else(|| Error::new(span, ErrorKind::IllegalArg("parameter declaration missing")))?;

    // We can either begin with a set of type variables or a list of parameters
    let (pvar_ids, tvar_ids) = if let NsDatum::Set(_, vs) = next_datum {
        next_datum = arg_iter.next().ok_or_else(|| {
            Error::new(
                span,
                ErrorKind::IllegalArg("type variables should be followed by parameters"),
            )
        })?;

        lower_polymorphic_vars(vs.into_vec().into_iter(), outer_scope, &mut fun_scope)?
    } else {
        (purity::PVarIds::new(), ty::TVarIds::new())
    };

    // Pull out our params
    let params = match next_datum {
        NsDatum::List(_, vs) => lower_list_destruc(&mut fun_scope, vs.into_vec().into_iter())?,
        _ => {
            return Err(Error::new(
                span,
                ErrorKind::IllegalArg("parameter list expected"),
            ));
        }
    };

    // Determine if we have a purity and return type after the parameters, eg (param) -> RetTy
    let mut purity = DeclPurity::Free;
    let mut ret_ty = DeclTy::Free;

    if arg_iter.len() >= 2 {
        if let Some(poly_purity) = try_lower_purity(&fun_scope, &arg_iter.as_slice()[0]) {
            arg_iter.next();
            purity = poly_purity.into();

            match arg_iter.next().unwrap() {
                NsDatum::Ident(_, ref ident) if ident.name() == "_" => {}
                ret_datum => {
                    ret_ty = lower_poly(&fun_scope, ret_datum)?.into();
                }
            }
        }
    }

    // Extract the body
    let body_expr = lower_body(&fun_scope, EMPTY_SPAN, arg_iter)?;

    Ok(Expr::new(
        span,
        ExprKind::Fun(Box::new(Fun {
            pvar_ids,
            tvar_ids,
            purity,
            params,
            ret_ty,
            body_expr,
        })),
    ))
}

fn lower_expr_prim_apply(
    scope: &Scope,
    span: Span,
    prim: Prim,
    mut arg_iter: NsDataIter,
) -> Result<Expr<Lowered>> {
    match prim {
        Prim::Def | Prim::DefMacro | Prim::DefType | Prim::Import => {
            Err(Error::new(span, ErrorKind::DefOutsideBody))
        }
        Prim::Let => lower_let(scope, span, arg_iter),
        Prim::LetMacro => lower_letmacro(scope, span, arg_iter),
        Prim::LetType => lower_lettype(scope, span, arg_iter),
        Prim::Export => Err(Error::new(span, ErrorKind::ExportOutsideModule)),
        Prim::Quote => {
            let literal_datum = expect_one_arg(span, arg_iter)?;
            Ok(literal_datum.into_syntax_datum().into())
        }
        Prim::Fun => lower_fun(scope, span, arg_iter),
        Prim::If => {
            expect_arg_count(span, 3, arg_iter.len())?;

            Ok(Expr::new(
                span,
                ExprKind::Cond(Box::new(Cond {
                    test_expr: lower_expr(scope, arg_iter.next().unwrap())?,
                    true_expr: lower_expr(scope, arg_iter.next().unwrap())?,
                    false_expr: lower_expr(scope, arg_iter.next().unwrap())?,
                })),
            ))
        }
        Prim::Do => lower_body(scope, span, arg_iter),
        Prim::CompileError => Err(lower_user_compile_error(span, arg_iter)),
        Prim::MacroRules | Prim::All => Err(Error::new(span, ErrorKind::PrimRef)),
    }
}

fn lower_expr_apply(
    scope: &Scope,
    span: Span,
    fun_expr: Expr<Lowered>,
    mut arg_iter: NsDataIter,
) -> Result<Expr<Lowered>> {
    let rest_arg_datum = try_take_rest_arg(&mut arg_iter);

    let fixed_arg_exprs = arg_iter
        .map(|arg_datum| lower_expr(scope, arg_datum))
        .collect::<Result<Vec<Expr<Lowered>>>>()?;

    let rest_arg_expr = match rest_arg_datum {
        Some(rest_arg_datum) => Some(lower_expr(scope, rest_arg_datum)?),
        None => None,
    };

    Ok(Expr::new(
        span,
        ExprKind::App(Box::new(App {
            fun_expr,
            ty_args: (),
            fixed_arg_exprs,
            rest_arg_expr,
        })),
    ))
}

fn lower_expr(scope: &Scope, datum: NsDatum) -> Result<Expr<Lowered>> {
    match datum {
        NsDatum::Ident(span, ident) => match scope.get_or_err(span, &ident)? {
            Binding::Var(id) => Ok(Expr::new(span, ExprKind::Ref(*id))),
            Binding::TyPred(test_ty) => Ok(Expr::new(span, ExprKind::TyPred(*test_ty))),
            Binding::EqPred => Ok(Expr::new(span, ExprKind::EqPred)),
            Binding::Prim(_) => Err(Error::new(span, ErrorKind::PrimRef)),
            Binding::Ty(_) | Binding::TyCons(_) | Binding::Purity(_) => {
                Err(Error::new(span, ErrorKind::TyRef))
            }
            Binding::Macro(_) => Err(Error::new(
                span,
                ErrorKind::MacroRef(ident.into_data_name()),
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
                        return lower_expr_prim_apply(scope, span, *prim, data_iter);
                    }
                    Binding::Macro(mac) => {
                        let mut macro_scope = Scope::new_child(scope);

                        let expanded_datum =
                            expand_macro(&mut macro_scope, span, mac, data_iter.as_slice())?;

                        return lower_expr(&macro_scope, expanded_datum)
                            .map(|expr| Expr::new(span, ExprKind::MacroExpand(Box::new(expr))))
                            .map_err(|e| e.with_macro_invocation_span(span));
                    }
                    _ => {}
                }
            }

            let fn_expr = lower_expr(scope, fn_datum)?;
            lower_expr_apply(scope, span, fn_expr, data_iter)
        }
        other => Ok(other.into_syntax_datum().into()),
    }
}

impl<'ccx> LoweringCtx<'ccx> {
    pub fn new(ccx: &'ccx CompileCtx) -> Self {
        use crate::hir::exports;

        let mut module_exports = HashMap::with_capacity(2);

        // These modules are always loaded
        module_exports.insert(
            ModuleName::new("arret".into(), vec!["internal".into()], "primitives".into()),
            exports::prims_exports(),
        );

        module_exports.insert(
            ModuleName::new("arret".into(), vec!["internal".into()], "types".into()),
            exports::tys_exports(),
        );

        Self {
            ccx,
            rust_libraries: vec![],
            module_exports,
            module_defs: vec![],
        }
    }

    fn load_module(&mut self, span: Span, module_name: ModuleName) -> Result<&Exports, Vec<Error>> {
        // TODO: An if-let or match here will cause the borrow to live past the return. This
        // prevents us from doing the insert below. We need to do a two-phase check instead.
        if self.module_exports.contains_key(&module_name) {
            return Ok(&self.module_exports[&module_name]);
        }

        let LoweredModule { exports, defs, .. } = {
            match load_module_by_name(self.ccx, span, &module_name)? {
                LoadedModule::Source(source_file) => {
                    let module_data = source_file.parsed().map_err(|err| vec![err.into()])?;
                    self.lower_module(module_data)?
                }
                LoadedModule::Rust(rfi_library) => self.include_rfi_library(span, rfi_library),
            }
        };

        self.module_defs.push(defs);
        Ok(self.module_exports.entry(module_name).or_insert(exports))
    }

    fn include_rfi_library(&mut self, span: Span, rfi_library: Arc<rfi::Library>) -> LoweredModule {
        use syntax::datum::DataStr;

        let exported_funs = rfi_library.exported_funs();

        let mut exports = HashMap::new();
        let mut defs = vec![];

        exports.reserve(exported_funs.len());
        defs.reserve(exported_funs.len());

        let var_ids = VarId::alloc_iter(exported_funs.len());
        for ((fun_name, rust_fun), var_id) in exported_funs.iter().zip(var_ids) {
            let fun_name_data_str: DataStr = (*fun_name).into();

            let def = Def {
                span,
                macro_invocation_span: EMPTY_SPAN,
                destruc: destruc::Destruc::Scalar(
                    span,
                    destruc::Scalar::new(
                        Some(var_id),
                        fun_name_data_str.clone(),
                        ty::Ty::Fun(Box::new(rust_fun.arret_fun_type().clone())).into(),
                    ),
                ),
                value_expr: Expr::new(span, ExprKind::RustFun(Box::new(rust_fun.clone()))),
            };

            defs.push(def);
            exports.insert(fun_name_data_str, Binding::Var(var_id));
        }

        self.rust_libraries.push(rfi_library);

        LoweredModule {
            defs,
            exports,
            main_var_id: None,
        }
    }

    fn lower_import(
        &mut self,
        scope: &mut Scope,
        ns_id: NsId,
        arg_iter: NsDataIter,
    ) -> Result<(), Vec<Error>> {
        for arg_datum in arg_iter {
            let span = arg_datum.span();

            let bindings = lower_import_set(arg_datum, |span, module_name| {
                Ok(self.load_module(span, module_name)?.clone())
            })?
            .into_iter()
            .map(|(name, binding)| (Ident::new(ns_id, name), binding));

            scope.insert_bindings(span, bindings)?;
        }

        Ok(())
    }

    fn lower_module_prim_apply(
        &mut self,
        scope: &mut Scope,
        applied_prim: AppliedPrim,
        mut arg_iter: NsDataIter,
    ) -> Result<Option<DeferredModulePrim>, Vec<Error>> {
        let AppliedPrim { prim, ns_id, span } = applied_prim;

        match prim {
            Prim::Export => {
                let deferred_exports = arg_iter
                    .map(|datum| {
                        if let NsDatum::Ident(span, ident) = datum {
                            Ok(DeferredExport { span, ident })
                        } else {
                            Err(Error::new(datum.span(), ErrorKind::ExpectedSym))
                        }
                    })
                    .collect::<Result<Vec<DeferredExport>>>()?;

                Ok(Some(DeferredModulePrim::Exports(deferred_exports)))
            }
            Prim::Def => {
                expect_arg_count(span, 2, arg_iter.len())?;

                let destruc_datum = arg_iter.next().unwrap();
                let destruc = lower_destruc(scope, destruc_datum)?;

                let value_datum = arg_iter.next().unwrap();

                let deferred_def = DeferredDef {
                    span,
                    macro_invocation_span: EMPTY_SPAN,
                    destruc,
                    value_datum,
                };

                Ok(Some(DeferredModulePrim::Def(deferred_def)))
            }
            Prim::DefMacro => Ok(lower_defmacro(scope, span, arg_iter).map(|_| None)?),
            Prim::DefType => Ok(lower_deftype(scope, span, arg_iter).map(|_| None)?),
            Prim::Import => self.lower_import(scope, ns_id, arg_iter).map(|_| None),
            Prim::CompileError => Err(vec![lower_user_compile_error(span, arg_iter)]),
            _ => Err(vec![Error::new(span, ErrorKind::NonDefInsideModule)]),
        }
    }

    fn lower_module_def(
        &mut self,
        scope: &mut Scope,
        datum: NsDatum,
    ) -> Result<Option<DeferredModulePrim>, Vec<Error>> {
        let span = datum.span();

        if let NsDatum::List(span, vs) = datum {
            let mut data_iter = vs.into_vec().into_iter();

            if let Some(NsDatum::Ident(fn_span, ref ident)) = data_iter.next() {
                match scope.get_or_err(fn_span, ident)? {
                    Binding::Prim(prim) => {
                        let applied_prim = AppliedPrim {
                            prim: *prim,
                            ns_id: ident.ns_id(),
                            span,
                        };

                        return self.lower_module_prim_apply(scope, applied_prim, data_iter);
                    }
                    Binding::Macro(mac) => {
                        let expanded_datum =
                            expand_macro(scope, span, &mac.clone(), data_iter.as_slice())?;

                        return self
                            .lower_module_def(scope, expanded_datum)
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

    fn resolve_deferred_def(scope: &Scope, deferred_def: DeferredDef) -> Result<Def<Lowered>> {
        let DeferredDef {
            span,
            macro_invocation_span,
            destruc,
            value_datum,
        } = deferred_def;

        lower_expr(&scope, value_datum).map(|value_expr| Def {
            span,
            macro_invocation_span,
            destruc,
            value_expr,
        })
    }

    fn lower_module(&mut self, data: &[Datum]) -> Result<LoweredModule, Vec<Error>> {
        let mut scope = Scope::empty();
        let ns_id = scope.alloc_ns_id();

        // The default scope only consists of (import)
        scope
            .insert_binding(
                EMPTY_SPAN,
                Ident::new(ns_id, "import".into()),
                Binding::Prim(Prim::Import),
            )
            .unwrap();

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
            let ns_datum = NsDatum::from_syntax_datum(ns_id, input_datum);
            match self.lower_module_def(&mut scope, ns_datum) {
                Ok(Some(DeferredModulePrim::Exports(mut exports))) => {
                    deferred_exports.append(&mut exports);
                }
                Ok(Some(DeferredModulePrim::Def(deferred_def))) => {
                    deferred_defs.push(deferred_def);
                }
                Ok(_) => {}
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
                    exports.insert(ident.into_data_name(), binding.clone());
                }
                Err(err) => {
                    errors.push(err);
                }
            };
        }

        // And now process any deferred defs
        let mut defs = Vec::with_capacity(deferred_defs.len());
        for deferred_def in deferred_defs {
            match Self::resolve_deferred_def(&scope, deferred_def) {
                Ok(def) => {
                    defs.push(def);
                }
                Err(error) => {
                    errors.push(error);
                }
            }
        }

        // Try to find `main!`. If we're not the root module this will be ignored.
        let main_ident = Ident::new(ns_id, "main!".into());
        let main_var_id = if let Some(Binding::Var(var_id)) = scope.get(&main_ident) {
            Some(*var_id)
        } else {
            None
        };

        if errors.is_empty() {
            Ok(LoweredModule {
                defs,
                exports,
                main_var_id,
            })
        } else {
            Err(errors)
        }
    }
}

// REPL interface
impl<'ccx> LoweringCtx<'ccx> {
    pub fn lower_repl_datum(
        &mut self,
        scope: &mut Scope,
        datum: NsDatum,
    ) -> Result<LoweredReplDatum, Vec<Error>> {
        use std::mem;

        // Try interpreting this as a module def
        match self.lower_module_def(scope, datum.clone()) {
            Ok(deferred_prims) => {
                let defs = deferred_prims
                    .into_iter()
                    .map(|deferred_prim| match deferred_prim {
                        DeferredModulePrim::Def(deferred_def) => {
                            Self::resolve_deferred_def(scope, deferred_def).map_err(|err| vec![err])
                        }
                        DeferredModulePrim::Exports(_) => {
                            Err(vec![Error::new(datum.span(), ErrorKind::ExportInsideRepl)])
                        }
                    })
                    .collect::<Result<Vec<Def<Lowered>>, Vec<Error>>>()?;

                let mut all_new_defs = mem::replace(&mut self.module_defs, vec![]);
                all_new_defs.push(defs);

                Ok(LoweredReplDatum::Defs(all_new_defs))
            }
            Err(errs) => {
                let non_def_errs = errs
                    .into_iter()
                    .filter(|err| err.kind() != &ErrorKind::NonDefInsideModule)
                    .collect::<Vec<Error>>();

                if non_def_errs.is_empty() {
                    // Re-interpret as an expression
                    let expr = lower_expr(scope, datum)?;
                    Ok(LoweredReplDatum::Expr(expr))
                } else {
                    Err(non_def_errs)
                }
            }
        }
    }
}

pub fn lower_program(
    ccx: &CompileCtx,
    source_file: &SourceFile,
) -> Result<LoweredProgram, Vec<Error>> {
    let file_span = source_file.span();

    let data = source_file.parsed().map_err(|err| vec![err.into()])?;

    let mut lcx = LoweringCtx::new(ccx);
    let root_module = lcx.lower_module(data)?;

    lcx.module_defs.push(root_module.defs);

    let main_var_id = if let Some(var_id) = root_module.main_var_id {
        var_id
    } else {
        return Err(vec![Error::new(file_span, ErrorKind::NoMainFun)]);
    };

    Ok(LoweredProgram {
        defs: lcx.module_defs,
        rust_libraries: lcx.rust_libraries,
        main_var_id,
    })
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
    use crate::PackagePaths;
    use syntax::parser::data_from_str;

    let mut test_data = data_from_str(data_str).unwrap();
    let mut program_data = vec![
        import_statement_for_module(&["arret", "internal", "primitives"]),
        import_statement_for_module(&["arret", "internal", "types"]),
    ];
    program_data.append(&mut test_data);

    let ccx = CompileCtx::new(PackagePaths::test_paths(None), true);
    let mut lcx = LoweringCtx::new(&ccx);

    lcx.lower_module(&program_data)
        .map_err(|mut errors| errors.remove(0))
}

#[cfg(test)]
pub fn expr_for_str(data_str: &str) -> Expr<Lowered> {
    use syntax::parser::datum_from_str;

    let test_ns_id = Scope::root_ns_id();
    let scope = Scope::new_with_primitives();

    let test_datum = datum_from_str(data_str).unwrap();
    let test_nsdatum = NsDatum::from_syntax_datum(test_ns_id, &test_datum);

    lower_expr(&scope, test_nsdatum).unwrap()
}

#[cfg(test)]
mod test {
    use super::*;
    use crate::ty::purity::Purity;
    use syntax::span::t2s;

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

        let expected = Expr::new(
            t2s(u),
            ExprKind::Let(Box::new(Let {
                destruc,
                value_expr: Datum::Int(t2s(v), 1).into(),
                body_expr: Expr::new(EMPTY_SPAN, ExprKind::Do(vec![])),
            })),
        );

        assert_eq!(expected, expr_for_str(j));
    }

    #[test]
    fn empty_fn() {
        let j = "(fn ())";
        let t = "^^^^^^^";

        let expected = Expr::new(
            t2s(t),
            ExprKind::Fun(Box::new(Fun {
                pvar_ids: purity::PVarIds::new(),
                tvar_ids: ty::TVarIds::new(),
                purity: DeclPurity::Free,
                params: destruc::List::new(vec![], None),
                ret_ty: DeclTy::Free,
                body_expr: Expr::new(EMPTY_SPAN, ExprKind::Do(vec![])),
            })),
        );

        assert_eq!(expected, expr_for_str(j));
    }

    #[test]
    fn empty_fn_with_purity() {
        let j = "(fn () -> _ 1)";
        let t = "^^^^^^^^^^^^^^";
        let u = "            ^ ";

        let expected = Expr::new(
            t2s(t),
            ExprKind::Fun(Box::new(Fun {
                pvar_ids: purity::PVarIds::new(),
                tvar_ids: ty::TVarIds::new(),
                purity: Purity::Pure.into(),
                params: destruc::List::new(vec![], None),
                ret_ty: DeclTy::Free,
                body_expr: Datum::Int(t2s(u), 1).into(),
            })),
        );

        assert_eq!(expected, expr_for_str(j));
    }

    #[test]
    fn empty_fn_with_ret_ty() {
        let j = "(fn () -> Int 1)";
        let t = "^^^^^^^^^^^^^^^^";
        let u = "              ^ ";

        let expected = Expr::new(
            t2s(t),
            ExprKind::Fun(Box::new(Fun {
                pvar_ids: purity::PVarIds::new(),
                tvar_ids: ty::TVarIds::new(),
                purity: Purity::Pure.into(),
                params: destruc::List::new(vec![], None),
                ret_ty: ty::Ty::Int.into(),
                body_expr: Datum::Int(t2s(u), 1).into(),
            })),
        );

        assert_eq!(expected, expr_for_str(j));
    }

    #[test]
    fn fixed_expr_apply() {
        let j = "(1 2 3)";
        let t = "^^^^^^^";
        let u = " ^     ";
        let v = "   ^   ";
        let w = "     ^ ";

        let expected = Expr::new(
            t2s(t),
            ExprKind::App(Box::new(App {
                fun_expr: Datum::Int(t2s(u), 1).into(),
                ty_args: (),
                fixed_arg_exprs: vec![Datum::Int(t2s(v), 2).into(), Datum::Int(t2s(w), 3).into()],
                rest_arg_expr: None,
            })),
        );

        assert_eq!(expected, expr_for_str(j));
    }

    #[test]
    fn rest_expr_apply() {
        let j = "(1 2 & 3)";
        let t = "^^^^^^^^^";
        let u = " ^       ";
        let v = "   ^     ";
        let w = "       ^ ";

        let expected = Expr::new(
            t2s(t),
            ExprKind::App(Box::new(App {
                fun_expr: Datum::Int(t2s(u), 1).into(),
                ty_args: (),
                fixed_arg_exprs: vec![Datum::Int(t2s(v), 2).into()],
                rest_arg_expr: Some(Datum::Int(t2s(w), 3).into()),
            })),
        );

        assert_eq!(expected, expr_for_str(j));
    }

    #[test]
    fn if_expr() {
        let j = "(if true 1 2)";
        let t = "^^^^^^^^^^^^^";
        let u = "    ^^^^     ";
        let v = "         ^   ";
        let w = "           ^ ";

        let expected = Expr::new(
            t2s(t),
            ExprKind::Cond(Box::new(Cond {
                test_expr: Expr::new(t2s(u), ExprKind::Lit(Datum::Bool(t2s(u), true))),
                true_expr: Expr::new(t2s(v), ExprKind::Lit(Datum::Int(t2s(v), 1))),
                false_expr: Expr::new(t2s(w), ExprKind::Lit(Datum::Int(t2s(w), 2))),
            })),
        );

        assert_eq!(expected, expr_for_str(j));
    }

    #[test]
    fn expand_trivial_macro() {
        let j = "(letmacro [one (macro-rules [() 1])] (one))";
        let t = "                                     ^^^^^ ";
        let u = "                                ^          ";

        let expected = Expr::new(
            t2s(t),
            ExprKind::MacroExpand(Box::new(Datum::Int(t2s(u), 1).into())),
        );
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

        let expected = Expr::new(t2s(t), ExprKind::TyPred(ty::pred::TestTy::Bool));
        assert_eq!(expected, expr_for_str(j));
    }

    #[test]
    fn equality_predicate() {
        let j = "=";
        let t = "^";

        let expected = Expr::new(t2s(t), ExprKind::EqPred);
        assert_eq!(expected, expr_for_str(j));
    }
}
