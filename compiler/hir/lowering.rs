use std::collections::{BTreeMap, HashMap};
use std::result;

use hir::destruc;
use hir::error::{Error, ErrorKind, Result};
use hir::import::lower_import_set;
use hir::loader::{load_module_by_name, parse_module_data, ModuleName};
use hir::macros::{expand_macro, lower_macro_rules, Macro};
use hir::module::Module;
use hir::ns::{Ident, NsDatum, NsId};
use hir::prim::Prim;
use hir::scope::{Binding, MacroId, Scope};
use hir::types::{lower_poly, try_lower_purity};
use hir::types::{lower_polymorphic_var, PolymorphicVar, PolymorphicVarKind};
use hir::util::{expect_arg_count, expect_ident_and_span, split_into_fixed_and_rest};
use hir::{App, Cond, Def, Expr, Fun, Let, VarIdCounter};
use source::{SourceFileId, SourceLoader};
use syntax::datum::Datum;
use syntax::span::{Span, EMPTY_SPAN};
use ty;
use ty::purity::Purity;

pub struct LoweringContext<'sl> {
    deferred_defs: Vec<DeferredDef>,

    var_id_counter: VarIdCounter,
    loaded_modules: BTreeMap<ModuleName, Module>,
    macros: Vec<Macro>,

    pvars: Vec<ty::purity::PVar>,
    tvars: Vec<ty::TVar>,

    source_loader: &'sl mut SourceLoader,
}

pub struct LoweredProgram {
    pub defs: Vec<Def<ty::Decl>>,
    pub pvars: Vec<ty::purity::PVar>,
    pub tvars: Vec<ty::TVar>,
}

impl From<Error> for Vec<Error> {
    fn from(error: Error) -> Vec<Error> {
        vec![error]
    }
}

struct DeferredDef(Span, destruc::Destruc<ty::Decl>, NsDatum);
struct DeferredExport(Span, Ident);

enum DeferredModulePrim {
    Def(DeferredDef),
    Export(DeferredExport),
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

impl<'sl> LoweringContext<'sl> {
    pub fn new(source_loader: &'sl mut SourceLoader) -> LoweringContext {
        let mut loaded_modules = BTreeMap::new();

        // These modules are always loaded
        loaded_modules.insert(
            ModuleName::new(vec!["risp".into(), "internal".into()], "primitives".into()),
            Module::prims_module(),
        );

        loaded_modules.insert(
            ModuleName::new(vec!["risp".into(), "internal".into()], "types".into()),
            Module::tys_module(),
        );

        LoweringContext {
            deferred_defs: vec![],
            var_id_counter: VarIdCounter::new(),
            loaded_modules,
            macros: vec![],
            pvars: vec![],
            tvars: vec![],
            source_loader,
        }
    }

    fn insert_pvar(&mut self, pvar: ty::purity::PVar) -> ty::purity::PVarId {
        ty::purity::PVarId::new_entry_id(&mut self.pvars, pvar)
    }

    fn insert_tvar(&mut self, tvar: ty::TVar) -> ty::TVarId {
        ty::TVarId::new_entry_id(&mut self.tvars, tvar)
    }

    // This would be less ugly as Result<!> once it's stabilised
    fn lower_user_compile_error(span: Span, mut arg_data: Vec<NsDatum>) -> Error {
        expect_arg_count(span, 1, arg_data.len())
            .err()
            .unwrap_or_else(|| {
                if let NsDatum::Str(_, user_message) = arg_data.pop().unwrap() {
                    Error::new(span, ErrorKind::UserError(user_message))
                } else {
                    Error::new(span, ErrorKind::IllegalArg("string expected"))
                }
            })
    }

    fn lower_macro(
        &mut self,
        scope: &mut Scope,
        self_datum: NsDatum,
        transformer_spec: NsDatum,
    ) -> Result<()> {
        let (self_ident, self_span) = expect_ident_and_span(self_datum)?;

        let macro_rules_data = if let NsDatum::List(span, vs) = transformer_spec {
            let mut transformer_data = vs.into_vec();

            if transformer_data.first().and_then(|d| scope.get_datum(d))
                != Some(Binding::Prim(Prim::MacroRules))
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

        let mac = lower_macro_rules(scope, macro_rules_data)?;

        if scope.get(&self_ident) != Some(Binding::Prim(Prim::Wildcard)) {
            let macro_id = MacroId::new_entry_id(&mut self.macros, mac);
            scope.insert_binding(self_span, self_ident, Binding::Macro(macro_id))?;
        }

        Ok(())
    }

    fn lower_defmacro(
        &mut self,
        scope: &mut Scope,
        span: Span,
        mut arg_data: Vec<NsDatum>,
    ) -> Result<()> {
        expect_arg_count(span, 2, arg_data.len())?;

        let transformer_spec = arg_data.pop().unwrap();
        let self_datum = arg_data.pop().unwrap();

        self.lower_macro(scope, self_datum, transformer_spec)
    }

    fn lower_letmacro(
        &mut self,
        scope: &Scope,
        span: Span,
        arg_data: Vec<NsDatum>,
    ) -> Result<Expr<ty::Decl>> {
        self.lower_let_like(scope, span, arg_data, Self::lower_macro, |expr, _| expr)
    }

    fn lower_type(
        &mut self,
        scope: &mut Scope,
        self_datum: NsDatum,
        ty_datum: NsDatum,
    ) -> Result<()> {
        let (ident, span) = expect_ident_and_span(self_datum)?;
        let ty = lower_poly(&self.tvars, scope, ty_datum)?;

        if scope.get(&ident) != Some(Binding::Prim(Prim::Wildcard)) {
            scope.insert_binding(span, ident, Binding::Ty(ty))?;
        }

        Ok(())
    }

    fn lower_deftype(
        &mut self,
        scope: &mut Scope,
        span: Span,
        mut arg_data: Vec<NsDatum>,
    ) -> Result<()> {
        expect_arg_count(span, 2, arg_data.len())?;

        let ty_datum = arg_data.pop().unwrap();
        let self_datum = arg_data.pop().unwrap();

        self.lower_type(scope, self_datum, ty_datum)
    }

    fn lower_lettype(
        &mut self,
        scope: &Scope,
        span: Span,
        arg_data: Vec<NsDatum>,
    ) -> Result<Expr<ty::Decl>> {
        self.lower_let_like(scope, span, arg_data, Self::lower_type, |expr, _| expr)
    }

    /// Lowers an identifier in to a scalar destruc with the passed type
    fn lower_ident_destruc(
        &mut self,
        scope: &mut Scope,
        span: Span,
        ident: Ident,
        decl_ty: ty::Decl,
    ) -> Result<destruc::Scalar<ty::Decl>> {
        match scope.get(&ident) {
            Some(Binding::Prim(Prim::Wildcard)) => {
                Ok(destruc::Scalar::new(None, ident.into_name(), decl_ty))
            }
            Some(Binding::Prim(Prim::Ellipsis)) => Err(Error::new(
                span,
                ErrorKind::IllegalArg(
                    "ellipsis can only be used to destructure the rest of a list",
                ),
            )),
            _ => {
                let var_id = self.var_id_counter.alloc();
                let source_name = ident.name().into();

                scope.insert_var(span, ident, var_id)?;

                Ok(destruc::Scalar::new(Some(var_id), source_name, decl_ty))
            }
        }
    }

    fn lower_scalar_destruc(
        &mut self,
        scope: &mut Scope,
        destruc_datum: NsDatum,
    ) -> Result<destruc::Scalar<ty::Decl>> {
        match destruc_datum {
            NsDatum::Ident(span, ident) => {
                self.lower_ident_destruc(scope, span, ident, ty::Decl::Free)
            }
            NsDatum::Vec(span, vs) => {
                let mut data = vs.into_vec();

                if data.len() != 3 {
                    return Err(Error::new(span, ErrorKind::NoVecDestruc));
                }

                // Make sure the middle element is a type colon
                if scope.get_datum(&data[1]) != Some(Binding::Prim(Prim::TyColon)) {
                    return Err(Error::new(span, ErrorKind::NoVecDestruc));
                }

                let ty = lower_poly(&self.tvars, scope, data.pop().unwrap())?;

                // Discard the type colon
                data.pop();

                let (ident, span) = expect_ident_and_span(data.pop().unwrap())?;
                self.lower_ident_destruc(scope, span, ident, ty.into_decl())
            }
            _ => Err(Error::new(
                destruc_datum.span(),
                ErrorKind::IllegalArg("expected a variable name or [name : Type]"),
            )),
        }
    }

    fn lower_list_destruc(
        &mut self,
        scope: &mut Scope,
        vs: Vec<NsDatum>,
    ) -> Result<destruc::List<ty::Decl>> {
        let (fixed, rest) = split_into_fixed_and_rest(scope, vs);

        let fixed_destrucs = fixed
            .into_iter()
            .map(|v| self.lower_destruc(scope, v))
            .collect::<Result<Vec<destruc::Destruc<ty::Decl>>>>()?;

        let rest_destruc = match rest {
            Some(rest) => Some(Box::new(self.lower_scalar_destruc(scope, rest)?)),
            None => None,
        };

        Ok(destruc::List::new(fixed_destrucs, rest_destruc))
    }

    fn lower_destruc(
        &mut self,
        scope: &mut Scope,
        destruc_datum: NsDatum,
    ) -> Result<destruc::Destruc<ty::Decl>> {
        match destruc_datum {
            NsDatum::Ident(span, _) | NsDatum::Vec(span, _) => self
                .lower_scalar_destruc(scope, destruc_datum)
                .map(|scalar| destruc::Destruc::Scalar(span, scalar)),
            NsDatum::List(span, vs) => self
                .lower_list_destruc(scope, vs.into_vec())
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
        &mut self,
        outer_scope: &Scope,
        span: Span,
        arg_data: Vec<NsDatum>,
        binder: B,
        fold_output: C,
    ) -> Result<Expr<ty::Decl>>
    where
        B: Fn(&mut Self, &mut Scope, NsDatum, NsDatum) -> Result<O>,
        C: Fn(Expr<ty::Decl>, O) -> Expr<ty::Decl>,
    {
        let mut arg_iter = arg_data.into_iter();
        let bindings_datum = arg_iter.next().ok_or_else(|| {
            Error::new(span, ErrorKind::IllegalArg("bindings declaration missing"))
        })?;

        let bindings_data = if let NsDatum::Vec(_, vs) = bindings_datum {
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

            outputs.push(binder(self, &mut scope, target_datum, value_datum)?);
        }

        let body_expr = self.lower_body(&scope, arg_iter)?;

        // This is to build nested `Let` expressions. Types/macros don't need this
        Ok(outputs.into_iter().rev().fold(body_expr, fold_output))
    }

    fn lower_body<I>(&mut self, scope: &Scope, body_data: I) -> Result<Expr<ty::Decl>>
    where
        I: IntoIterator<Item = NsDatum>,
    {
        let mut flattened_exprs = vec![];

        for body_datum in body_data {
            match self.lower_expr(&scope, body_datum)? {
                Expr::Do(mut exprs) => {
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
            Ok(Expr::Do(flattened_exprs))
        }
    }

    fn lower_let(
        &mut self,
        scope: &Scope,
        span: Span,
        arg_data: Vec<NsDatum>,
    ) -> Result<Expr<ty::Decl>> {
        self.lower_let_like(
            scope,
            span,
            arg_data,
            |inner_self, scope, target_datum, value_datum| {
                let destruc = inner_self.lower_destruc(scope, target_datum)?;
                let value_expr = inner_self.lower_expr(scope, value_datum)?;
                Ok((destruc, value_expr))
            },
            |body_expr, (destruc, value_expr)| {
                Expr::Let(
                    span,
                    Box::new(Let {
                        destruc,
                        value_expr,
                        body_expr,
                    }),
                )
            },
        )
    }

    fn lower_fun(
        &mut self,
        outer_scope: &Scope,
        span: Span,
        arg_data: Vec<NsDatum>,
    ) -> Result<Expr<ty::Decl>> {
        let mut arg_iter = arg_data.into_iter();

        let mut fun_scope = Scope::new_child(outer_scope);
        let pvar_id_start = ty::purity::PVarId::new(self.pvars.len());
        let tvar_id_start = ty::TVarId::new(self.tvars.len());

        let mut next_datum = arg_iter.next().ok_or_else(|| {
            Error::new(span, ErrorKind::IllegalArg("parameter declaration missing"))
        })?;

        // We can either begin with a set of type variables or a list of parameters
        if let NsDatum::Set(_, vs) = next_datum {
            for tvar_datum in vs.into_vec() {
                let PolymorphicVar { span, ident, kind } =
                    lower_polymorphic_var(&self.tvars, outer_scope, tvar_datum)?;

                let binding = match kind {
                    PolymorphicVarKind::TVar(tvar) => {
                        let tvar_id = self.insert_tvar(tvar);
                        Binding::Ty(ty::Poly::Var(tvar_id))
                    }
                    PolymorphicVarKind::PVar(pvar) => {
                        let pvar_id = self.insert_pvar(pvar);
                        Binding::Purity(ty::purity::Poly::Var(pvar_id))
                    }
                    PolymorphicVarKind::Pure => Binding::Purity(Purity::Pure.into_poly()),
                };

                fun_scope.insert_binding(span, ident, binding)?;
            }

            next_datum = arg_iter.next().ok_or_else(|| {
                Error::new(
                    span,
                    ErrorKind::IllegalArg("type variables should be followed by parameters"),
                )
            })?;
        };

        // We allocate tvar IDs sequentially so we can use a simple range to track them
        let pvar_ids = pvar_id_start..ty::purity::PVarId::new(self.pvars.len());
        let tvar_ids = tvar_id_start..ty::TVarId::new(self.tvars.len());

        // Pull out our params
        let params = match next_datum {
            NsDatum::List(_, vs) => self.lower_list_destruc(&mut fun_scope, vs.into_vec())?,
            _ => {
                return Err(Error::new(
                    span,
                    ErrorKind::IllegalArg("parameter list expected"),
                ));
            }
        };

        // Determine if we have a purity and return type after the parameters, eg (param) -> RetTy
        let mut purity = ty::purity::Decl::Free;
        let mut ret_ty = ty::Decl::Free;

        if arg_iter.len() >= 2 {
            if let Some(poly_purity) = try_lower_purity(&fun_scope, &arg_iter.as_slice()[0]) {
                arg_iter.next();
                purity = poly_purity.into_decl();

                let ret_datum = arg_iter.next().unwrap();
                ret_ty = lower_poly(&self.tvars, &fun_scope, ret_datum)?.into_decl();
            }
        }

        // Extract the body
        let body_expr = self.lower_body(&fun_scope, arg_iter)?;

        Ok(Expr::Fun(
            span,
            Box::new(Fun {
                pvar_ids,
                tvar_ids,
                purity,
                params,
                ret_ty,
                body_expr,
            }),
        ))
    }

    fn lower_expr_prim_apply(
        &mut self,
        scope: &Scope,
        span: Span,
        prim: Prim,
        mut arg_data: Vec<NsDatum>,
    ) -> Result<Expr<ty::Decl>> {
        match prim {
            Prim::Def | Prim::DefMacro | Prim::DefType | Prim::Import => {
                Err(Error::new(span, ErrorKind::DefOutsideBody))
            }
            Prim::Let => self.lower_let(scope, span, arg_data),
            Prim::LetMacro => self.lower_letmacro(scope, span, arg_data),
            Prim::LetType => self.lower_lettype(scope, span, arg_data),
            Prim::Export => Err(Error::new(span, ErrorKind::ExportOutsideModule)),
            Prim::Quote => {
                expect_arg_count(span, 1, arg_data.len())?;
                Ok(Expr::Lit(arg_data.pop().unwrap().into_syntax_datum()))
            }
            Prim::Fun => self.lower_fun(scope, span, arg_data),
            Prim::If => {
                expect_arg_count(span, 3, arg_data.len())?;

                Ok(Expr::Cond(
                    span,
                    Box::new(Cond {
                        false_expr: self.lower_expr(scope, arg_data.pop().unwrap())?,
                        true_expr: self.lower_expr(scope, arg_data.pop().unwrap())?,
                        test_expr: self.lower_expr(scope, arg_data.pop().unwrap())?,
                    }),
                ))
            }
            Prim::TyPred => {
                expect_arg_count(span, 1, arg_data.len())?;
                Ok(Expr::TyPred(
                    span,
                    lower_poly(&self.tvars, scope, arg_data.pop().unwrap())?,
                ))
            }
            Prim::Do => self.lower_body(scope, arg_data),
            Prim::CompileError => Err(Self::lower_user_compile_error(span, arg_data)),
            Prim::Ellipsis | Prim::Wildcard | Prim::MacroRules | Prim::TyColon => {
                Err(Error::new(span, ErrorKind::PrimRef))
            }
        }
    }

    fn load_module(
        &mut self,
        scope: &mut Scope,
        span: Span,
        module_name: &ModuleName,
    ) -> result::Result<&Module, Vec<Error>> {
        // TODO: This does a lot of hash lookups
        if !self.loaded_modules.contains_key(module_name) {
            let module_data = load_module_by_name(self.source_loader, span, module_name)?;
            let loaded_module = self.lower_module(scope, module_data)?;

            self.loaded_modules
                .insert(module_name.clone(), loaded_module);
        }

        Ok(&self.loaded_modules[module_name])
    }

    fn lower_import(
        &mut self,
        scope: &mut Scope,
        ns_id: NsId,
        arg_data: Vec<NsDatum>,
    ) -> Result<()> {
        for arg_datum in arg_data {
            let span = arg_datum.span();
            let bindings = lower_import_set(arg_datum, |span, module_name| {
                // TODO: Allow multiple errors to pass through an import
                Ok(self
                    .load_module(scope, span, module_name)
                    .map_err(|mut errors| errors.remove(0))?
                    .exports()
                    .clone())
            })?;

            for (name, binding) in bindings {
                scope.insert_binding(span, Ident::new(ns_id, name), binding)?;
            }
        }

        Ok(())
    }

    fn lower_expr_apply(
        &mut self,
        scope: &Scope,
        span: Span,
        fun_expr: Expr<ty::Decl>,
        arg_data: Vec<NsDatum>,
    ) -> Result<Expr<ty::Decl>> {
        let (fixed_arg_data, rest_arg_datum) = split_into_fixed_and_rest(scope, arg_data);

        let fixed_arg_exprs = fixed_arg_data
            .into_iter()
            .map(|arg_datum| self.lower_expr(scope, arg_datum))
            .collect::<Result<Vec<Expr<ty::Decl>>>>()?;

        let rest_arg_expr = match rest_arg_datum {
            Some(rest_arg_datum) => Some(self.lower_expr(scope, rest_arg_datum)?),
            None => None,
        };

        Ok(Expr::App(
            span,
            Box::new(App {
                fun_expr,
                fixed_arg_exprs,
                rest_arg_expr,
            }),
        ))
    }

    fn lower_expr(&mut self, scope: &Scope, datum: NsDatum) -> Result<Expr<ty::Decl>> {
        match datum {
            NsDatum::Ident(span, ref ident) => match scope.get(ident) {
                Some(Binding::Var(id)) => Ok(Expr::Ref(span, id)),
                Some(Binding::Prim(_)) => Err(Error::new(span, ErrorKind::PrimRef)),
                Some(Binding::Ty(_)) | Some(Binding::TyCons(_)) | Some(Binding::Purity(_)) => {
                    Err(Error::new(span, ErrorKind::TyRef))
                }
                Some(Binding::Macro(_)) => {
                    Err(Error::new(span, ErrorKind::MacroRef(ident.name().into())))
                }
                None => Err(Error::new(span, ErrorKind::UnboundSym(ident.name().into()))),
            },
            NsDatum::List(span, vs) => {
                let mut data = vs.into_vec();
                if data.is_empty() {
                    return Ok(Expr::Lit(Datum::List(span, Box::new([]))));
                }

                let arg_data = data.split_off(1);
                let fn_datum = data.pop().unwrap();

                match fn_datum {
                    NsDatum::Ident(fn_span, ref ident) => match scope.get(ident) {
                        Some(Binding::Prim(prim)) => {
                            self.lower_expr_prim_apply(scope, span, prim, arg_data)
                        }
                        Some(Binding::Macro(macro_id)) => {
                            let mut macro_scope = Scope::new_child(scope);

                            let expanded_datum = {
                                let mac = &self.macros[macro_id.to_usize()];

                                expand_macro(&mut macro_scope, span, mac, &arg_data)?
                            };

                            self.lower_expr(&macro_scope, expanded_datum)
                                .map_err(|e| e.with_macro_invocation_span(span))
                        }
                        Some(Binding::Var(id)) => {
                            self.lower_expr_apply(scope, span, Expr::Ref(span, id), arg_data)
                        }
                        Some(Binding::Ty(_))
                        | Some(Binding::TyCons(_))
                        | Some(Binding::Purity(_)) => Err(Error::new(span, ErrorKind::TyRef)),
                        None => Err(Error::new(
                            fn_span,
                            ErrorKind::UnboundSym(ident.name().into()),
                        )),
                    },
                    _ => {
                        let fn_expr = self.lower_expr(scope, fn_datum)?;
                        self.lower_expr_apply(scope, span, fn_expr, arg_data)
                    }
                }
            }
            other => Ok(Expr::Lit(other.into_syntax_datum())),
        }
    }

    fn lower_module_prim_apply(
        &mut self,
        scope: &mut Scope,
        applied_prim: AppliedPrim,
        mut arg_data: Vec<NsDatum>,
    ) -> Result<Vec<DeferredModulePrim>> {
        let AppliedPrim { prim, ns_id, span } = applied_prim;

        match prim {
            Prim::Export => {
                let deferred_exports = arg_data
                    .into_iter()
                    .map(|datum| {
                        if let NsDatum::Ident(span, ident) = datum {
                            Ok(DeferredModulePrim::Export(DeferredExport(span, ident)))
                        } else {
                            Err(Error::new(datum.span(), ErrorKind::ExpectedSym))
                        }
                    })
                    .collect::<Result<Vec<DeferredModulePrim>>>()?;

                Ok(deferred_exports)
            }
            Prim::Def => {
                expect_arg_count(span, 2, arg_data.len())?;

                let value_datum = arg_data.pop().unwrap();

                let destruc_datum = arg_data.pop().unwrap();
                let destruc = self.lower_destruc(scope, destruc_datum)?;

                Ok(vec![DeferredModulePrim::Def(DeferredDef(
                    span,
                    destruc,
                    value_datum,
                ))])
            }
            Prim::DefMacro => self.lower_defmacro(scope, span, arg_data).map(|_| vec![]),
            Prim::DefType => self.lower_deftype(scope, span, arg_data).map(|_| vec![]),
            Prim::Import => self.lower_import(scope, ns_id, arg_data).map(|_| vec![]),
            Prim::Do => {
                let mut deferred_prims = vec![];
                for arg_datum in arg_data {
                    deferred_prims.append(&mut self.lower_module_def(scope, arg_datum)?);
                }

                Ok(deferred_prims)
            }
            Prim::CompileError => Err(Self::lower_user_compile_error(span, arg_data)),
            _ => Err(Error::new(span, ErrorKind::NonDefInsideModule)),
        }
    }

    fn lower_module_def(
        &mut self,
        scope: &mut Scope,
        datum: NsDatum,
    ) -> Result<Vec<DeferredModulePrim>> {
        let span = datum.span();

        if let NsDatum::List(span, vs) = datum {
            let mut data = vs.into_vec();

            if !data.is_empty() {
                let arg_data = data.split_off(1);
                let fn_datum = data.pop().unwrap();

                if let NsDatum::Ident(fn_span, ref ident) = fn_datum {
                    match scope.get(ident) {
                        Some(Binding::Prim(prim)) => {
                            let applied_prim = AppliedPrim {
                                prim,
                                ns_id: ident.ns_id(),
                                span,
                            };

                            return self.lower_module_prim_apply(scope, applied_prim, arg_data);
                        }
                        Some(Binding::Macro(macro_id)) => {
                            let expanded_datum = {
                                let mac = &self.macros[macro_id.to_usize()];
                                expand_macro(scope, span, mac, &arg_data)?
                            };

                            return self
                                .lower_module_def(scope, expanded_datum)
                                .map_err(|e| e.with_macro_invocation_span(span));
                        }
                        Some(_) => {
                            // Non-def
                        }
                        None => {
                            return Err(Error::new(
                                fn_span,
                                ErrorKind::UnboundSym(ident.name().into()),
                            ));
                        }
                    }
                }
            }
        }

        Err(Error::new(span, ErrorKind::NonDefInsideModule))
    }

    fn lower_module(
        &mut self,
        scope: &mut Scope,
        data: Vec<Datum>,
    ) -> result::Result<Module, Vec<Error>> {
        let ns_id = scope.alloc_ns_id();
        let mut errors: Vec<Error> = vec![];

        // The default scope only consists of (import)
        scope
            .insert_binding(
                EMPTY_SPAN,
                Ident::new(ns_id, "import".into()),
                Binding::Prim(Prim::Import),
            )
            .unwrap();

        // Extract all of our definitions.
        //
        // This occurs in three passes:
        // - Imports, types and macros are resolved immediately and cannot refer to bindings later
        //   in the body
        // - Exports are resolved after the module has been loaded
        // - Other definitions are lowered at the end of the program once all bindings have been
        //   introduced.
        let mut deferred_exports = Vec::<DeferredExport>::new();
        for input_datum in data {
            let ns_datum = NsDatum::from_syntax_datum(ns_id, input_datum);
            match self.lower_module_def(scope, ns_datum) {
                Ok(new_deferred_prims) => {
                    for deferred_prim in new_deferred_prims {
                        match deferred_prim {
                            DeferredModulePrim::Export(deferred_export) => {
                                deferred_exports.push(deferred_export);
                            }
                            DeferredModulePrim::Def(deferred_def) => {
                                self.deferred_defs.push(deferred_def);
                            }
                        }
                    }
                }
                Err(error) => {
                    errors.push(error);
                }
            };
        }

        // Process any exports at the end of the module
        let mut exports = HashMap::<Box<str>, Binding>::new();
        for DeferredExport(span, ident) in deferred_exports {
            if let Some(binding) = scope.get(&ident) {
                exports.insert(ident.into_name(), binding);
            } else {
                errors.push(Error::new(span, ErrorKind::UnboundSym(ident.into_name())));
            }
        }

        if errors.is_empty() {
            Ok(Module::new(exports))
        } else {
            Err(errors)
        }
    }
}

pub fn lower_program(
    source_loader: &mut SourceLoader,
    source_file_id: SourceFileId,
) -> result::Result<LoweredProgram, Vec<Error>> {
    use std;

    let data = parse_module_data(source_loader.source_file(source_file_id))?;

    let mut root_scope = Scope::new_empty();
    let mut lcx = LoweringContext::new(source_loader);
    lcx.lower_module(&mut root_scope, data)?;

    let deferred_defs = std::mem::replace(&mut lcx.deferred_defs, vec![]);
    let mut defs: Vec<Def<ty::Decl>> = vec![];
    let mut errors: Vec<Error> = vec![];

    for DeferredDef(span, destruc, value_datum) in deferred_defs {
        match lcx.lower_expr(&root_scope, value_datum) {
            Ok(value_expr) => {
                defs.push(Def {
                    span,
                    destruc,
                    value_expr,
                });
            }
            Err(error) => {
                errors.push(error);
            }
        }
    }

    if !errors.is_empty() {
        Err(errors)
    } else {
        Ok(LoweredProgram {
            pvars: lcx.pvars,
            tvars: lcx.tvars,
            defs,
        })
    }
}

////
#[cfg(test)]
fn import_statement_for_module(names: &[&'static str]) -> Datum {
    Datum::List(
        EMPTY_SPAN,
        Box::new([
            Datum::Sym(EMPTY_SPAN, "import".into()),
            Datum::Vec(
                EMPTY_SPAN,
                names
                    .iter()
                    .map(|&n| Datum::Sym(EMPTY_SPAN, n.into()))
                    .collect::<Vec<Datum>>()
                    .into_boxed_slice(),
            ),
        ]),
    )
}

#[cfg(test)]
fn module_for_str(data_str: &str) -> Result<Module> {
    use syntax::parser::data_from_str;

    let mut root_scope = Scope::new_empty();

    let mut test_data = data_from_str(data_str).unwrap();
    let mut program_data = vec![
        import_statement_for_module(&["risp", "internal", "primitives"]),
        import_statement_for_module(&["risp", "internal", "types"]),
    ];
    program_data.append(&mut test_data);

    let mut source_loader = SourceLoader::new();
    let mut lcx = LoweringContext::new(&mut source_loader);

    lcx.lower_module(&mut root_scope, program_data)
        .map_err(|mut errors| errors.remove(0))
}

#[cfg(test)]
pub struct LoweredTestExpr {
    pub pvars: Vec<ty::purity::PVar>,
    pub tvars: Vec<ty::TVar>,
    pub expr: Expr<ty::Decl>,
}

#[cfg(test)]
pub fn lowered_expr_for_str(data_str: &str) -> LoweredTestExpr {
    use hir::prim::insert_prim_exports;
    use hir::types::insert_ty_exports;
    use syntax::parser::datum_from_str;

    let test_ns_id = NsId::new(1);
    let mut scope = Scope::new_empty();

    let mut ccx = SourceLoader::new();
    let mut lcx = LoweringContext::new(&mut ccx);

    // Extract our builtin exports
    let mut exports = HashMap::<Box<str>, Binding>::new();
    insert_prim_exports(&mut exports);
    insert_ty_exports(&mut exports);

    // Place them on our scope
    for (name, binding) in exports {
        scope
            .insert_binding(EMPTY_SPAN, Ident::new(test_ns_id, name), binding)
            .unwrap();
    }

    let test_datum = datum_from_str(data_str).unwrap();
    let test_nsdatum = NsDatum::from_syntax_datum(test_ns_id, test_datum);

    let expr = lcx.lower_expr(&scope, test_nsdatum).unwrap();

    LoweredTestExpr {
        pvars: lcx.pvars,
        tvars: lcx.tvars,
        expr,
    }
}

#[cfg(test)]
pub fn expr_for_str(data_str: &str) -> Expr<ty::Decl> {
    lowered_expr_for_str(data_str).expr
}

#[cfg(test)]
mod test {
    use super::*;
    use hir::VarId;
    use syntax::span::t2s;
    use ty;

    #[test]
    fn self_quoting_bool() {
        let j = "false";
        let t = "^^^^^";

        let expected = Expr::Lit(Datum::Bool(t2s(t), false));
        assert_eq!(expected, expr_for_str(j));
    }

    #[test]
    fn self_quoting_empty_list() {
        let j = "()";
        let t = "^^";

        let expected = Expr::Lit(Datum::List(t2s(t), Box::new([])));
        assert_eq!(expected, expr_for_str(j));
    }

    #[test]
    fn quoted_datum_shorthand() {
        let j = "'foo";
        let t = " ^^^";

        let expected = Expr::Lit(Datum::Sym(t2s(t), "foo".into()));
        assert_eq!(expected, expr_for_str(j));
    }

    #[test]
    fn quoted_datum_explicit() {
        let j = "(quote foo)";
        let t = "       ^^^ ";

        let expected = Expr::Lit(Datum::Sym(t2s(t), "foo".into()));
        assert_eq!(expected, expr_for_str(j));
    }

    #[test]
    fn basic_untyped_scalar_let() {
        let j = "(let [x 1] x)";
        let t = "      ^      ";
        let u = "^^^^^^^^^^^^^";
        let v = "        ^    ";
        let w = "           ^ ";

        let destruc = destruc::Destruc::Scalar(
            t2s(t),
            destruc::Scalar::new(Some(VarId(0)), "x".into(), ty::Decl::Free),
        );

        let expected = Expr::Let(
            t2s(u),
            Box::new(Let {
                destruc,
                value_expr: Expr::Lit(Datum::Int(t2s(v), 1)),
                body_expr: Expr::Ref(t2s(w), VarId(0)),
            }),
        );

        assert_eq!(expected, expr_for_str(j));
    }

    #[test]
    fn basic_typed_let() {
        let j = "(let [[x : true] true])";
        let t = "      ^^^^^^^^^^       ";
        let u = "^^^^^^^^^^^^^^^^^^^^^^^";
        let v = "                 ^^^^  ";

        let destruc = destruc::Destruc::Scalar(
            t2s(t),
            destruc::Scalar::new(
                Some(VarId(0)),
                "x".into(),
                ty::Ty::LitBool(true).into_decl(),
            ),
        );

        let expected = Expr::Let(
            t2s(u),
            Box::new(Let {
                destruc,
                value_expr: Expr::Lit(Datum::Bool(t2s(v), true)),
                body_expr: Expr::Do(vec![]),
            }),
        );

        assert_eq!(expected, expr_for_str(j));
    }

    #[test]
    fn wildcard_let() {
        let j = "(let [_ 1])";
        let t = "      ^    ";
        let u = "^^^^^^^^^^^";
        let v = "        ^  ";

        let destruc = destruc::Destruc::Scalar(
            t2s(t),
            destruc::Scalar::new(None, "_".into(), ty::Decl::Free),
        );

        let expected = Expr::Let(
            t2s(u),
            Box::new(Let {
                destruc,
                value_expr: Expr::Lit(Datum::Int(t2s(v), 1)),
                body_expr: Expr::Do(vec![]),
            }),
        );

        assert_eq!(expected, expr_for_str(j));
    }

    #[test]
    fn multi_let() {
        let j = "(let [x 1 y x])";
        let t = "      ^        ";
        let u = "          ^    ";
        let v = "^^^^^^^^^^^^^^^";
        let w = "        ^      ";
        let x = "            ^  ";

        let destruc_x = destruc::Destruc::Scalar(
            t2s(t),
            destruc::Scalar::new(Some(VarId(0)), "x".into(), ty::Decl::Free),
        );

        let destruc_y = destruc::Destruc::Scalar(
            t2s(u),
            destruc::Scalar::new(Some(VarId(1)), "y".into(), ty::Decl::Free),
        );

        let expected = Expr::Let(
            t2s(v),
            Box::new(Let {
                destruc: destruc_x,
                value_expr: Expr::Lit(Datum::Int(t2s(w), 1)),
                body_expr: Expr::Let(
                    t2s(v),
                    Box::new(Let {
                        destruc: destruc_y,
                        value_expr: Expr::Ref(t2s(x), VarId(0)),
                        body_expr: Expr::Do(vec![]),
                    }),
                ),
            }),
        );

        assert_eq!(expected, expr_for_str(j));
    }

    #[test]
    fn list_destruc_let() {
        let j = "(let [(x rest ...) '(1)] x)";
        let t = "      ^^^^^^^^^^^^         ";
        let u = "       ^                   ";
        let v = "^^^^^^^^^^^^^^^^^^^^^^^^^^^";
        let w = "                    ^^^    ";
        let x = "                     ^     ";
        let y = "                         ^ ";

        let destruc = destruc::Destruc::List(
            t2s(t),
            destruc::List::new(
                vec![destruc::Destruc::Scalar(
                    t2s(u),
                    destruc::Scalar::new(Some(VarId(0)), "x".into(), ty::Decl::Free),
                )],
                Some(Box::new(destruc::Scalar::new(
                    Some(VarId(1)),
                    "rest".into(),
                    ty::Decl::Free,
                ))),
            ),
        );

        let expected = Expr::Let(
            t2s(v),
            Box::new(Let {
                destruc,
                value_expr: Expr::Lit(Datum::List(t2s(w), Box::new([Datum::Int(t2s(x), 1)]))),
                body_expr: Expr::Ref(t2s(y), VarId(0)),
            }),
        );

        assert_eq!(expected, expr_for_str(j));
    }

    #[test]
    fn empty_fn() {
        let j = "(fn ())";
        let t = "^^^^^^^";

        let expected = Expr::Fun(
            t2s(t),
            Box::new(Fun {
                pvar_ids: ty::purity::PVarIds::monomorphic(),
                tvar_ids: ty::TVarIds::monomorphic(),
                purity: ty::purity::Decl::Free,
                params: destruc::List::new(vec![], None),
                ret_ty: ty::Decl::Free,
                body_expr: Expr::Do(vec![]),
            }),
        );

        assert_eq!(expected, expr_for_str(j));
    }

    #[test]
    fn empty_fn_with_ret_ty() {
        let j = "(fn () -> Int 1)";
        let t = "^^^^^^^^^^^^^^^^";
        let u = "              ^ ";

        let expected = Expr::Fun(
            t2s(t),
            Box::new(Fun {
                pvar_ids: ty::purity::PVarIds::monomorphic(),
                tvar_ids: ty::TVarIds::monomorphic(),
                purity: Purity::Pure.into_decl(),
                params: destruc::List::new(vec![], None),
                ret_ty: ty::Ty::Int.into_decl(),
                body_expr: Expr::Lit(Datum::Int(t2s(u), 1)),
            }),
        );

        assert_eq!(expected, expr_for_str(j));
    }

    #[test]
    fn identity_fn() {
        let j = "(fn (x) x)";
        let t = "     ^    ";
        let u = "^^^^^^^^^^";
        let v = "        ^ ";

        let param_var_id = VarId::new(0);
        let params = destruc::List::new(
            vec![destruc::Destruc::Scalar(
                t2s(t),
                destruc::Scalar::new(Some(param_var_id), "x".into(), ty::Decl::Free),
            )],
            None,
        );

        let expected = Expr::Fun(
            t2s(u),
            Box::new(Fun {
                pvar_ids: ty::purity::PVarIds::monomorphic(),
                tvar_ids: ty::TVarIds::monomorphic(),
                purity: ty::purity::Decl::Free,
                params,
                ret_ty: ty::Decl::Free,
                body_expr: Expr::Ref(t2s(v), param_var_id),
            }),
        );

        assert_eq!(expected, expr_for_str(j));
    }

    #[test]
    fn poly_purity_identity_fn() {
        let j = "(fn #{A [->_ : ->!]} ([x : A]) ->_ A x)";
        let t = "                      ^^^^^^^          ";
        let u = "^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^";
        let v = "                                     ^ ";

        let tvar_id = ty::TVarId::new(0);

        let param_var_id = VarId::new(0);
        let params = destruc::List::new(
            vec![destruc::Destruc::Scalar(
                t2s(t),
                destruc::Scalar::new(
                    Some(param_var_id),
                    "x".into(),
                    ty::Poly::Var(tvar_id).into_decl(),
                ),
            )],
            None,
        );

        let expected = Expr::Fun(
            t2s(u),
            Box::new(Fun {
                pvar_ids: ty::purity::PVarId::new(0)..ty::purity::PVarId::new(1),
                tvar_ids: ty::TVarId::new(0)..ty::TVarId::new(1),
                purity: ty::purity::Poly::Var(ty::purity::PVarId::new(0)).into_decl(),
                params,
                ret_ty: ty::Poly::Var(tvar_id).into_decl(),
                body_expr: Expr::Ref(t2s(v), param_var_id),
            }),
        );

        assert_eq!(expected, expr_for_str(j));
    }

    #[test]
    fn capturing_fn() {
        let j = "(let [x 1] (fn () x))";
        let t = "      ^              ";
        let u = "^^^^^^^^^^^^^^^^^^^^^";
        let v = "        ^            ";
        let w = "           ^^^^^^^^^ ";
        let x = "                  ^  ";

        let outer_var_id = VarId::new(0);
        let outer_destruc = destruc::Destruc::Scalar(
            t2s(t),
            destruc::Scalar::new(Some(outer_var_id), "x".into(), ty::Decl::Free),
        );

        let expected = Expr::Let(
            t2s(u),
            Box::new(Let {
                destruc: outer_destruc,
                value_expr: Expr::Lit(Datum::Int(t2s(v), 1)),
                body_expr: Expr::Fun(
                    t2s(w),
                    Box::new(Fun {
                        pvar_ids: ty::purity::PVarIds::monomorphic(),
                        tvar_ids: ty::TVarIds::monomorphic(),
                        purity: ty::purity::Decl::Free,
                        params: destruc::List::new(vec![], None),
                        ret_ty: ty::Decl::Free,
                        body_expr: Expr::Ref(t2s(x), outer_var_id),
                    }),
                ),
            }),
        );

        assert_eq!(expected, expr_for_str(j));
    }

    #[test]
    fn shadowing_fn() {
        let j = "(let [x 1] (fn (x) x))";
        let t = "      ^               ";
        let u = "        ^             ";
        let v = "                ^     ";
        let w = "^^^^^^^^^^^^^^^^^^^^^^";
        let x = "           ^^^^^^^^^^ ";
        let y = "                   ^  ";

        let outer_var_id = VarId::new(0);
        let outer_destruc = destruc::Destruc::Scalar(
            t2s(t),
            destruc::Scalar::new(Some(outer_var_id), "x".into(), ty::Decl::Free),
        );

        let param_var_id = VarId::new(1);
        let params = destruc::List::new(
            vec![destruc::Destruc::Scalar(
                t2s(v),
                destruc::Scalar::new(Some(param_var_id), "x".into(), ty::Decl::Free),
            )],
            None,
        );

        let expected = Expr::Let(
            t2s(w),
            Box::new(Let {
                destruc: outer_destruc,
                value_expr: Expr::Lit(Datum::Int(t2s(u), 1)),
                body_expr: Expr::Fun(
                    t2s(x),
                    Box::new(Fun {
                        pvar_ids: ty::purity::PVarIds::monomorphic(),
                        tvar_ids: ty::TVarIds::monomorphic(),
                        purity: ty::purity::Decl::Free,
                        params,
                        ret_ty: ty::Decl::Free,
                        body_expr: Expr::Ref(t2s(y), param_var_id),
                    }),
                ),
            }),
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

        let expected = Expr::App(
            t2s(t),
            Box::new(App {
                fun_expr: Expr::Lit(Datum::Int(t2s(u), 1)),
                fixed_arg_exprs: vec![
                    Expr::Lit(Datum::Int(t2s(v), 2)),
                    Expr::Lit(Datum::Int(t2s(w), 3)),
                ],
                rest_arg_expr: None,
            }),
        );

        assert_eq!(expected, expr_for_str(j));
    }

    #[test]
    fn rest_expr_apply() {
        let j = "(1 2 3 ...)";
        let t = "^^^^^^^^^^^";
        let u = " ^         ";
        let v = "   ^       ";
        let w = "     ^     ";

        let expected = Expr::App(
            t2s(t),
            Box::new(App {
                fun_expr: Expr::Lit(Datum::Int(t2s(u), 1)),
                fixed_arg_exprs: vec![Expr::Lit(Datum::Int(t2s(v), 2))],
                rest_arg_expr: Some(Expr::Lit(Datum::Int(t2s(w), 3))),
            }),
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

        let expected = Expr::Cond(
            t2s(t),
            Box::new(Cond {
                test_expr: Expr::Lit(Datum::Bool(t2s(u), true)),
                true_expr: Expr::Lit(Datum::Int(t2s(v), 1)),
                false_expr: Expr::Lit(Datum::Int(t2s(w), 2)),
            }),
        );

        assert_eq!(expected, expr_for_str(j));
    }

    #[test]
    fn simple_export() {
        let j = "(def x 1)(export x)";

        let var_id = VarId(0);
        let mut expected_exports = HashMap::new();
        expected_exports.insert("x".into(), Binding::Var(var_id));

        let expected = Module::new(expected_exports);
        assert_eq!(expected, module_for_str(j).unwrap());
    }

    #[test]
    fn expand_trivial_macro() {
        let j1 = "(letmacro [one (macro-rules [() 1])]";
        let t1 = "                                ^   ";
        let j2 = "(one)";
        let t2 = "     ";
        let j3 = ")";

        let j = &[j1, j2, j3].join("");
        let t = &[t1, t2].join("");

        let expected = Expr::Lit(Datum::Int(t2s(t), 1));
        assert_eq!(expected, expr_for_str(j));
    }

    #[test]
    fn trivial_lettype() {
        let j1 = "(lettype [MyTrue true]";
        let s1 = "                      ";

        let j2 = "(let [[x : MyTrue] true])";
        let t2 = "      ^^^^^^^^^^^^       ";
        let u2 = "^^^^^^^^^^^^^^^^^^^^^^^^^";
        let v2 = "                   ^^^^  ";

        let j3 = ")";

        let j = &[j1, j2, j3].join("");
        let t = &[s1, t2].join("");
        let u = &[s1, u2].join("");
        let v = &[s1, v2].join("");

        let destruc = destruc::Destruc::Scalar(
            t2s(t),
            destruc::Scalar::new(
                Some(VarId(0)),
                "x".into(),
                ty::Ty::LitBool(true).into_decl(),
            ),
        );

        let expected = Expr::Let(
            t2s(u),
            Box::new(Let {
                destruc,
                value_expr: Expr::Lit(Datum::Bool(t2s(v), true)),
                body_expr: Expr::Do(vec![]),
            }),
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
        assert_eq!(2, module.exports().len());
    }

    #[test]
    fn module_top_level_do() {
        let j1 = "(export x y)";
        let j2 = "(do (def x 1)";
        let j3 = "    (def y 2))";

        let j = &[j1, j2, j3].join("");

        let module = module_for_str(j).unwrap();
        assert_eq!(2, module.exports().len());
    }

    #[test]
    fn type_predicate() {
        let j = "(type-predicate true)";
        let t = "^^^^^^^^^^^^^^^^^^^^^";

        let expected = Expr::TyPred(t2s(t), ty::Ty::LitBool(true).into_poly());
        assert_eq!(expected, expr_for_str(j));
    }
}
