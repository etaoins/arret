use std::collections::{BTreeMap, HashMap};
use std::io::Read;
use std::result;

use ctx::CompileContext;
use hir::destruc;
use hir::error::{Error, ErrorKind, Result};
use hir::import::lower_import_set;
use hir::loader::{load_module_by_name, load_module_data, ModuleName};
use hir::macros::{expand_macro, lower_macro_rules, Macro};
use hir::module::Module;
use hir::ns::{Ident, NsDatum, NsId};
use hir::prim::Prim;
use hir::scope::{Binding, MacroId, Scope};
use hir::types::{lower_poly, lower_polymorphic_var, try_lower_purity, PolymorphicVar};
use hir::util::{
    expect_arg_count, expect_ident, expect_ident_and_span, pop_vec_front, split_into_fixed_and_rest,
};
use hir::{App, Cond, Def, Expr, Fun, Let, VarIdCounter};
use syntax::datum::Datum;
use syntax::span::{Span, EMPTY_SPAN};
use ty;
use ty::purity::Purity;

pub struct LoweringContext<'ccx> {
    deferred_defs: Vec<DeferredDef>,

    var_id_counter: VarIdCounter,
    loaded_modules: BTreeMap<ModuleName, Module>,
    macros: Vec<Macro>,

    pvars: Vec<ty::purity::PVar>,
    tvars: Vec<ty::TVar>,

    ccx: &'ccx mut CompileContext,
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

struct LoweredFunRetDecl {
    body_data: Vec<NsDatum>,
    purity: Option<ty::purity::Poly>,
    ret_ty: Option<ty::Poly>,
}

struct LetArgs {
    target_datum: NsDatum,
    value_datum: NsDatum,
    body_data: Vec<NsDatum>,
}

impl<'ccx> LoweringContext<'ccx> {
    pub fn new(ccx: &'ccx mut CompileContext) -> LoweringContext {
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
            ccx,
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
        expect_arg_count(span, &arg_data, 1)
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
        span: Span,
        self_datum: NsDatum,
        transformer_spec: NsDatum,
    ) -> Result<()> {
        let self_ident = expect_ident(self_datum)?;

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

        let mac = lower_macro_rules(scope, span, &self_ident, macro_rules_data)?;

        let macro_id = MacroId::new_entry_id(&mut self.macros, mac);
        scope.insert_binding(self_ident, Binding::Macro(macro_id));

        Ok(())
    }

    fn lower_defmacro(
        &mut self,
        scope: &mut Scope,
        span: Span,
        mut arg_data: Vec<NsDatum>,
    ) -> Result<()> {
        expect_arg_count(span, &arg_data, 2)?;

        let transformer_spec = arg_data.pop().unwrap();
        let self_datum = arg_data.pop().unwrap();

        self.lower_macro(scope, span, self_datum, transformer_spec)
    }

    fn lower_letmacro(
        &mut self,
        scope: &Scope,
        span: Span,
        arg_data: Vec<NsDatum>,
    ) -> Result<Expr<ty::Decl>> {
        let LetArgs {
            target_datum,
            value_datum,
            body_data,
        } = Self::expect_let_args(span, arg_data)?;

        let mut macro_scope = Scope::new_child(scope);
        self.lower_macro(&mut macro_scope, span, target_datum, value_datum)?;

        self.lower_body(&macro_scope, body_data)
    }

    fn lower_type(
        &mut self,
        scope: &mut Scope,
        self_datum: NsDatum,
        ty_datum: NsDatum,
    ) -> Result<()> {
        let ident = expect_ident(self_datum)?;
        let ty = lower_poly(&self.tvars, scope, ty_datum)?;

        scope.insert_binding(ident, Binding::Ty(ty));
        Ok(())
    }

    fn lower_deftype(
        &mut self,
        scope: &mut Scope,
        span: Span,
        mut arg_data: Vec<NsDatum>,
    ) -> Result<()> {
        expect_arg_count(span, &arg_data, 2)?;

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
        let LetArgs {
            target_datum,
            value_datum,
            body_data,
        } = Self::expect_let_args(span, arg_data)?;

        let mut type_scope = Scope::new_child(scope);
        self.lower_type(&mut type_scope, target_datum, value_datum)?;

        self.lower_body(&type_scope, body_data)
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

                scope.insert_var(ident, var_id);

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

    fn lower_fun_ret_decl(
        &mut self,
        fun_scope: &Scope,
        mut post_param_data: Vec<NsDatum>,
    ) -> Result<LoweredFunRetDecl> {
        if post_param_data.len() >= 2 {
            if let Some(purity) = try_lower_purity(fun_scope, &post_param_data[0]) {
                let body_data = post_param_data.split_off(2);
                let ret_datum = post_param_data.pop().unwrap();
                let ret_ty = lower_poly(&self.tvars, &fun_scope, ret_datum)?;

                return Ok(LoweredFunRetDecl {
                    body_data,
                    purity: Some(purity),
                    ret_ty: Some(ret_ty),
                });
            }
        }

        Ok(LoweredFunRetDecl {
            body_data: post_param_data,
            purity: None,
            ret_ty: None,
        })
    }

    fn expect_let_args(span: Span, arg_data: Vec<NsDatum>) -> Result<LetArgs> {
        if arg_data.is_empty() {
            return Err(Error::new(
                span,
                ErrorKind::IllegalArg("bindings declaration missing"),
            ));
        }

        let (bindings_datum, body_data) = pop_vec_front(arg_data);
        let mut bindings_data = if let NsDatum::Vec(span, vs) = bindings_datum {
            if vs.len() != 2 {
                return Err(Error::new(
                    span,
                    ErrorKind::IllegalArg("[target initialiser] expected"),
                ));
            }
            vs.into_vec()
        } else {
            return Err(Error::new(
                bindings_datum.span(),
                ErrorKind::IllegalArg("binding vector expected"),
            ));
        };

        let value_datum = bindings_data.pop().unwrap();
        let target_datum = bindings_data.pop().unwrap();

        Ok(LetArgs {
            target_datum,
            value_datum,
            body_data,
        })
    }

    fn lower_body(&mut self, scope: &Scope, body_data: Vec<NsDatum>) -> Result<Expr<ty::Decl>> {
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
        let LetArgs {
            target_datum,
            value_datum,
            body_data,
        } = Self::expect_let_args(span, arg_data)?;

        let mut let_scope = Scope::new_child(scope);

        let destruc = self.lower_destruc(&mut let_scope, target_datum)?;
        let value_expr = self.lower_expr(&let_scope, value_datum)?;
        let body_expr = self.lower_body(&let_scope, body_data)?;

        Ok(Expr::Let(
            span,
            Box::new(Let {
                destruc,
                value_expr,
                body_expr,
            }),
        ))
    }

    fn lower_fun(
        &mut self,
        scope: &Scope,
        span: Span,
        arg_data: Vec<NsDatum>,
    ) -> Result<Expr<ty::Decl>> {
        if arg_data.is_empty() {
            return Err(Error::new(
                span,
                ErrorKind::IllegalArg("parameter declaration missing"),
            ));
        }

        let mut fun_scope = Scope::new_child(scope);

        let (mut next_datum, mut tail_data) = pop_vec_front(arg_data);
        let pvar_id_start = ty::purity::PVarId::new(self.pvars.len());
        let tvar_id_start = ty::TVarId::new(self.tvars.len());

        // We can either begin with a set of type variables or a list of parameters
        if let NsDatum::Set(_, vs) = next_datum {
            for tvar_datum in vs.into_vec() {
                let (ident, polymorphic_var) =
                    lower_polymorphic_var(&self.tvars, scope, tvar_datum)?;

                match polymorphic_var {
                    PolymorphicVar::TVar(tvar) => {
                        let tvar_id = self.insert_tvar(tvar);
                        fun_scope.insert_binding(ident, Binding::Ty(ty::Poly::Var(tvar_id)));
                    }
                    PolymorphicVar::PVar(pvar) => {
                        let pvar_id = self.insert_pvar(pvar);
                        fun_scope
                            .insert_binding(ident, Binding::Purity(ty::purity::Poly::Var(pvar_id)));
                    }
                    PolymorphicVar::Pure => {
                        fun_scope.insert_binding(ident, Binding::Purity(Purity::Pure.into_poly()));
                    }
                }
            }

            if tail_data.is_empty() {
                return Err(Error::new(
                    span,
                    ErrorKind::IllegalArg("type variables should be followed by parameters"),
                ));
            }

            let (new_next_datum, new_tail_data) = pop_vec_front(tail_data);
            next_datum = new_next_datum;
            tail_data = new_tail_data;
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
        let LoweredFunRetDecl {
            body_data,
            purity,
            ret_ty,
        } = self.lower_fun_ret_decl(&fun_scope, tail_data)?;

        // Extract the body
        let body_expr = self.lower_body(&fun_scope, body_data)?;

        let purity = purity
            .map(ty::purity::Poly::into_decl)
            .unwrap_or(ty::purity::Decl::Free);

        // If we don't have a return type try to guess a span for the last expression so we can
        // locate inference errors
        let ret_ty = ret_ty
            .map(|ret_ty| ret_ty.into_decl())
            .unwrap_or(ty::Decl::Free);

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
                expect_arg_count(span, &arg_data, 1)?;
                Ok(Expr::Lit(arg_data.pop().unwrap().into_syntax_datum()))
            }
            Prim::Fun => self.lower_fun(scope, span, arg_data),
            Prim::If => {
                expect_arg_count(span, &arg_data, 3)?;

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
                expect_arg_count(span, &arg_data, 1)?;
                Ok(Expr::TyPred(
                    span,
                    lower_poly(&self.tvars, scope, arg_data.pop().unwrap())?,
                ))
            }
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
    ) -> Result<&Module> {
        // TODO: This does a lot of hash lookups
        if !self.loaded_modules.contains_key(module_name) {
            let module_data = load_module_by_name(self.ccx, span, module_name)?;
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
            let bindings = lower_import_set(arg_datum, |span, module_name| {
                Ok(self
                    .load_module(scope, span, module_name)?
                    .exports()
                    .clone())
            })?;

            for (name, binding) in bindings {
                scope.insert_binding(Ident::new(ns_id, name), binding);
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
                None => Err(Error::new(
                    span,
                    ErrorKind::UnboundSymbol(ident.name().into()),
                )),
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
                            ErrorKind::UnboundSymbol(ident.name().into()),
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
                            Err(Error::new(datum.span(), ErrorKind::ExpectedSymbol))
                        }
                    })
                    .collect::<Result<Vec<DeferredModulePrim>>>()?;

                Ok(deferred_exports)
            }
            Prim::Def => {
                expect_arg_count(span, &arg_data, 2)?;

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
                                ErrorKind::UnboundSymbol(ident.name().into()),
                            ));
                        }
                    }
                }
            }
        }

        Err(Error::new(span, ErrorKind::NonDefInsideModule))
    }

    fn lower_module(&mut self, scope: &mut Scope, data: Vec<Datum>) -> Result<Module> {
        let ns_id = scope.alloc_ns_id();

        // The default scope only consists of (import)
        scope.insert_binding(
            Ident::new(ns_id, "import".into()),
            Binding::Prim(Prim::Import),
        );

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
            let mut new_deferred_prims = self.lower_module_def(scope, ns_datum)?;

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

        // Process any exports at the end of the module
        let mut exports = HashMap::<Box<str>, Binding>::new();
        for DeferredExport(span, ident) in deferred_exports {
            if let Some(binding) = scope.get(&ident) {
                exports.insert(ident.into_name(), binding);
            } else {
                return Err(Error::new(
                    span,
                    ErrorKind::UnboundSymbol(ident.into_name()),
                ));
            }
        }

        Ok(Module::new(exports))
    }
}

pub fn lower_program(
    ccx: &mut CompileContext,
    display_name: String,
    input_reader: &mut Read,
) -> result::Result<LoweredProgram, Vec<Error>> {
    use std;

    let data = load_module_data(ccx, EMPTY_SPAN, display_name, input_reader)?;

    let mut root_scope = Scope::new_empty();
    let mut lcx = LoweringContext::new(ccx);
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

    let mut ccx = CompileContext::new();
    let mut lcx = LoweringContext::new(&mut ccx);
    lcx.lower_module(&mut root_scope, program_data)
}

#[cfg(test)]
pub struct LoweredTestExpr {
    pub pvars: Vec<ty::purity::PVar>,
    pub tvars: Vec<ty::TVar>,
    pub expr: Expr<ty::Decl>,
}

#[cfg(test)]
pub fn lowered_expr_for_str(data_str: &str) -> Result<LoweredTestExpr> {
    use hir::prim::insert_prim_exports;
    use hir::types::insert_ty_exports;
    use syntax::parser::datum_from_str;

    let test_ns_id = NsId::new(1);
    let mut scope = Scope::new_empty();

    let mut ccx = CompileContext::new();
    let mut lcx = LoweringContext::new(&mut ccx);

    let mut exports = HashMap::<Box<str>, Binding>::new();
    insert_prim_exports(&mut exports);
    insert_ty_exports(&mut exports);

    // Place them on our scope
    for (name, binding) in exports {
        scope.insert_binding(Ident::new(test_ns_id, name), binding);
    }

    // Wrap the test data in a function definition
    let test_datum = datum_from_str(data_str).unwrap();
    let test_nsdatum = NsDatum::from_syntax_datum(test_ns_id, test_datum);

    let expr = lcx.lower_expr(&scope, test_nsdatum)?;

    Ok(LoweredTestExpr {
        pvars: lcx.pvars,
        tvars: lcx.tvars,
        expr,
    })
}

#[cfg(test)]
pub fn expr_for_str(data_str: &str) -> Result<Expr<ty::Decl>> {
    lowered_expr_for_str(data_str).map(|body| body.expr)
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
        assert_eq!(expected, expr_for_str(j).unwrap());
    }

    #[test]
    fn self_quoting_empty_list() {
        let j = "()";
        let t = "^^";

        let expected = Expr::Lit(Datum::List(t2s(t), Box::new([])));
        assert_eq!(expected, expr_for_str(j).unwrap());
    }

    #[test]
    fn quoted_datum_shorthand() {
        let j = "'foo";
        let t = " ^^^";

        let expected = Expr::Lit(Datum::Sym(t2s(t), "foo".into()));
        assert_eq!(expected, expr_for_str(j).unwrap());
    }

    #[test]
    fn quoted_datum_explicit() {
        let j = "(quote foo)";
        let t = "       ^^^ ";

        let expected = Expr::Lit(Datum::Sym(t2s(t), "foo".into()));
        assert_eq!(expected, expr_for_str(j).unwrap());
    }

    #[test]
    fn quoted_multiple_data() {
        let j = "(quote 1 2 3)";
        let t = "^^^^^^^^^^^^^";

        let err = Error::new(t2s(t), ErrorKind::WrongArgCount(1));
        assert_eq!(err, expr_for_str(j).unwrap_err());
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

        assert_eq!(expected, expr_for_str(j).unwrap());
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

        assert_eq!(expected, expr_for_str(j).unwrap());
    }

    #[test]
    fn double_typed_var_def() {
        // We should fail to annotate a variable a second time
        let j = "(let [[[x : true] : false] 1])";
        let t = "       ^^^^^^^^^^             ";

        let err = Error::new(t2s(t), ErrorKind::ExpectedSymbol);
        assert_eq!(err, expr_for_str(j).unwrap_err());
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

        assert_eq!(expected, expr_for_str(j).unwrap());
    }

    #[test]
    fn bad_binding_let() {
        let j = "(let foo 1)";
        let t = "     ^^^   ";

        let err = Error::new(t2s(t), ErrorKind::IllegalArg("binding vector expected"));
        assert_eq!(err, expr_for_str(j).unwrap_err());
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

        assert_eq!(expected, expr_for_str(j).unwrap());
    }

    #[test]
    fn let_of_bad_destruc() {
        let j = "(let [1 1])";
        let t = "      ^    ";

        let err = Error::new(
            t2s(t),
            ErrorKind::IllegalArg(
                "values can only be bound to variables or destructured into lists",
            ),
        );
        assert_eq!(err, expr_for_str(j).unwrap_err());
    }

    #[test]
    fn let_of_vec_destruc() {
        let j = "(let [[x y] [1 2]])";
        let t = "      ^^^^^        ";

        let err = Error::new(t2s(t), ErrorKind::NoVecDestruc);
        assert_eq!(err, expr_for_str(j).unwrap_err());
    }

    #[test]
    fn def_outside_module_body() {
        let j = "(def x 1)";
        let t = "^^^^^^^^^";

        let err = Error::new(t2s(t), ErrorKind::DefOutsideBody);
        assert_eq!(err, expr_for_str(j).unwrap_err());
    }

    #[test]
    fn reference_prim() {
        let j = "def";
        let t = "^^^";

        let err = Error::new(t2s(t), ErrorKind::PrimRef);
        assert_eq!(err, expr_for_str(j).unwrap_err());
    }

    #[test]
    fn reference_unbound() {
        let j = "nopenopenope";
        let t = "^^^^^^^^^^^^";

        let err = Error::new(t2s(t), ErrorKind::UnboundSymbol("nopenopenope".into()));
        assert_eq!(err, expr_for_str(j).unwrap_err());
    }

    #[test]
    fn fn_without_param_decl() {
        let j = "(fn)";
        let t = "^^^^";

        let err = Error::new(
            t2s(t),
            ErrorKind::IllegalArg("parameter declaration missing"),
        );
        assert_eq!(err, expr_for_str(j).unwrap_err());
    }

    #[test]
    fn fn_with_bad_destruc_param() {
        let j = "(fn (1))";
        let t = "     ^  ";

        let err = Error::new(
            t2s(t),
            ErrorKind::IllegalArg(
                "values can only be bound to variables or destructured into lists",
            ),
        );
        assert_eq!(err, expr_for_str(j).unwrap_err());
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

        assert_eq!(expected, expr_for_str(j).unwrap());
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

        assert_eq!(expected, expr_for_str(j).unwrap());
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

        assert_eq!(expected, expr_for_str(j).unwrap());
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

        assert_eq!(expected, expr_for_str(j).unwrap());
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

        assert_eq!(expected, expr_for_str(j).unwrap());
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

        assert_eq!(expected, expr_for_str(j).unwrap());
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

        assert_eq!(expected, expr_for_str(j).unwrap());
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

        assert_eq!(expected, expr_for_str(j).unwrap());
    }

    #[test]
    fn empty_if() {
        let j = "(if)";
        let t = "^^^^";

        let err = Error::new(t2s(t), ErrorKind::WrongArgCount(3));
        assert_eq!(err, expr_for_str(j).unwrap_err());
    }

    #[test]
    fn if_without_test() {
        let j = "(if true)";
        let t = "^^^^^^^^^";

        let err = Error::new(t2s(t), ErrorKind::WrongArgCount(3));
        assert_eq!(err, expr_for_str(j).unwrap_err());
    }

    #[test]
    fn if_without_false_branch() {
        let j = "(if true 1)";
        let t = "^^^^^^^^^^^";

        let err = Error::new(t2s(t), ErrorKind::WrongArgCount(3));
        assert_eq!(err, expr_for_str(j).unwrap_err());
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

        assert_eq!(expected, expr_for_str(j).unwrap());
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
    fn export_unbound() {
        let j = "(export x)";
        let t = "        ^ ";

        let err = Error::new(t2s(t), ErrorKind::UnboundSymbol("x".into()));
        assert_eq!(err, module_for_str(j).unwrap_err());
    }

    #[test]
    fn letmacro_of_non_symbol() {
        let j = "(letmacro [1 (macro-rules ${})])";
        let t = "           ^                    ";

        let err = Error::new(t2s(t), ErrorKind::ExpectedSymbol);
        assert_eq!(err, expr_for_str(j).unwrap_err());
    }

    #[test]
    fn letmacro_of_non_list() {
        let j = "(letmacro [a b])";
        let t = "             ^  ";

        let err = Error::new(
            t2s(t),
            ErrorKind::IllegalArg("macro specification must be a list"),
        );
        assert_eq!(err, expr_for_str(j).unwrap_err());
    }

    #[test]
    fn letmacro_of_unsupported_type() {
        let j = "(letmacro [a (macro-fn #{})])";
        let t = "             ^^^^^^^^^^^^^^  ";

        let err = Error::new(t2s(t), ErrorKind::IllegalArg("unsupported macro type"));
        assert_eq!(err, expr_for_str(j).unwrap_err());
    }

    #[test]
    fn letmacro_with_duplicate_vars() {
        let j = "(letmacro [a (macro-rules #{} [[(a x x) x]])])";
        let t = "                                   ^          ";
        let u = "                                     ^        ";

        let err = Error::new(t2s(u), ErrorKind::DuplicateMacroVar("x".into(), t2s(t)));
        assert_eq!(err, expr_for_str(j).unwrap_err());
    }

    #[test]
    fn letmacro_with_bad_ellipsis() {
        let j = "(letmacro [a (macro-rules #{} [[(a ...) false]])])";
        let t = "                                   ^^^            ";

        let err = Error::new(
            t2s(t),
            ErrorKind::IllegalArg("ellipsis can only be used as part of a zero or more match"),
        );
        assert_eq!(err, expr_for_str(j).unwrap_err());
    }

    #[test]
    fn expand_macro_without_matching_rule() {
        let j1 = "(letmacro [one (macro-rules #{} [[(one) 1]])]";
        let t1 = "                                             ";
        let j2 = "(one extra-arg)";
        let t2 = "^^^^^^^^^^^^^^^";
        let j3 = ")";

        let j = &[j1, j2, j3].join("");
        let t = &[t1, t2].join("");

        let err = Error::new(t2s(t), ErrorKind::NoMacroRule);
        assert_eq!(err, expr_for_str(j).unwrap_err());
    }

    #[test]
    fn expand_trivial_macro() {
        let j1 = "(letmacro [one (macro-rules #{} [[(one) 1]])]";
        let t1 = "                                        ^    ";
        let j2 = "(one)";
        let t2 = "     ";
        let j3 = ")";

        let j = &[j1, j2, j3].join("");
        let t = &[t1, t2].join("");

        let expected = Expr::Lit(Datum::Int(t2s(t), 1));
        assert_eq!(expected, expr_for_str(j).unwrap());
    }

    #[test]
    fn expand_replacing_macro() {
        let j1 = "(letmacro [identity (macro-rules #{} [[(identity x) x]])]";
        let t1 = "                                                         ";
        let j2 = "(identity 1)";
        let t2 = "          ^ ";
        let j3 = ")";

        let j = &[j1, j2, j3].join("");
        let t = &[t1, t2].join("");

        let expected = Expr::Lit(Datum::Int(t2s(t), 1));
        assert_eq!(expected, expr_for_str(j).unwrap());
    }

    #[test]
    fn expand_two_value_replacement() {
        let j = "(letmacro [ret-two (macro-rules #{} [[(ret-two x y) [x y]]])] (ret-two 1 2))";
        let t = "                                                    ^^^^^                   ";
        let u = "                                                                       ^    ";
        let v = "                                                                         ^  ";

        let expected = Expr::Lit(Datum::Vec(
            t2s(t),
            Box::new([Datum::Int(t2s(u), 1), Datum::Int(t2s(v), 2)]),
        ));
        assert_eq!(expected, expr_for_str(j).unwrap());
    }

    #[test]
    fn expand_with_matching_literals() {
        let j = "(letmacro [for (macro-rules #{in} [[(for x in y) [x y]]])] (for 1 in 2))";
        let t = "                                                 ^^^^^                  ";
        let u = "                                                                ^       ";
        let v = "                                                                     ^  ";

        let expected = Expr::Lit(Datum::Vec(
            t2s(t),
            Box::new([Datum::Int(t2s(u), 1), Datum::Int(t2s(v), 2)]),
        ));
        assert_eq!(expected, expr_for_str(j).unwrap());
    }

    #[test]
    fn expand_with_escaped_ellipsis() {
        let j1 = "(letmacro [m (macro-rules #{} [[(m) '(... ...)]])]";
        let t1 = "                                          ^^^     ";
        let j2 = "(m)";
        let j3 = ")";

        let j = &[j1, j2, j3].join("");
        let t = t1;

        let expected = Expr::Lit(Datum::Sym(t2s(t), "...".into()));
        assert_eq!(expected, expr_for_str(j).unwrap());
    }

    #[test]
    fn expand_with_literal_underscore() {
        let j1 = "(letmacro [m (macro-rules #{_} [[(m _) 1][(m a) a]])]";
        let sp = "                                                     ";
        let j2 = "(m 2)";
        let t2 = "   ^ ";
        let j3 = ")";

        let j = &[j1, j2, j3].join("");
        let t = &[sp, t2].join("");

        let expected = Expr::Lit(Datum::Int(t2s(t), 2));
        assert_eq!(expected, expr_for_str(j).unwrap());
    }

    #[test]
    fn expand_with_non_matching_literals() {
        let j1 = "(letmacro [for (macro-rules #{in} [[(for x in y) [x y]]])]";
        let t1 = "                                                          ";
        let j2 = "(for 1 foo 2)";
        let t2 = "^^^^^^^^^^^^^";
        let j3 = ")";

        let j = &[j1, j2, j3].join("");
        let t = &[t1, t2].join("");

        let err = Error::new(t2s(t), ErrorKind::NoMacroRule);
        assert_eq!(err, expr_for_str(j).unwrap_err());
    }

    #[test]
    fn expand_with_wildcard() {
        let j1 = "(letmacro [third (macro-rules #{} [[(third _ _ x) x]])]";
        let t1 = "                                                       ";
        let j2 = "(third 1 2 3)";
        let t2 = "           ^ ";
        let j3 = ")";

        let j = &[j1, j2, j3].join("");
        let t = &[t1, t2].join("");

        let expected = Expr::Lit(Datum::Int(t2s(t), 3));
        assert_eq!(expected, expr_for_str(j).unwrap());
    }

    #[test]
    fn expand_recursive() {
        let j1 = "(letmacro [rec (macro-rules #{} [[(rec) 7] [(rec _) (rec)]])]";
        let t1 = "                                        ^                    ";
        let j2 = "(rec)";
        let t2 = "     ";
        let j3 = ")";

        let j = &[j1, j2, j3].join("");
        let t = &[t1, t2].join("");

        let expected = Expr::Lit(Datum::Int(t2s(t), 7));
        assert_eq!(expected, expr_for_str(j).unwrap());
    }

    #[test]
    fn expand_fixed_list_match() {
        let j1 = "(letmacro [ret-second (macro-rules #{} [[(ret-second (_ second _)) second]])]";
        let t1 = "                                                                             ";
        let j2 = "(ret-second (1 2 3))";
        let t2 = "               ^    ";
        let j3 = ")";

        let j = &[j1, j2, j3].join("");
        let t = &[t1, t2].join("");

        let expected = Expr::Lit(Datum::Int(t2s(t), 2));
        assert_eq!(expected, expr_for_str(j).unwrap());
    }

    #[test]
    fn expand_fixed_vector_match() {
        let j1 = "(letmacro [ret-third (macro-rules #{} [[(ret-third [_ _ third]) third]])]";
        let t1 = "                                                                         ";
        let j2 = "(ret-third [1 2 3])";
        let t2 = "                ^  ";
        let j3 = ")";

        let j = &[j1, j2, j3].join("");
        let t = &[t1, t2].join("");

        let expected = Expr::Lit(Datum::Int(t2s(t), 3));
        assert_eq!(expected, expr_for_str(j).unwrap());
    }

    #[test]
    fn expand_empty_set_match() {
        let j1 = "(letmacro [empty-set? (macro-rules #{} [[(empty-set? #{}) true]])]";
        let t1 = "                                                          ^^^^    ";
        let j2 = "(empty-set? #{})";
        let j3 = ")";

        let j = &[j1, j2, j3].join("");
        let t = t1;

        let expected = Expr::Lit(Datum::Bool(t2s(t), true));
        assert_eq!(expected, expr_for_str(j).unwrap());
    }

    #[test]
    fn expand_zero_or_more_set_match() {
        let j1 = "(letmacro [set->list (macro-rules #{} [[(set->list #{v ...}) '(v ...)]])]";
        let t1 = "                                                              ^^^^^^^    ";
        let sp = "                                                                         ";
        let j2 = "(set->list #{1 2 3})";
        let u2 = "             ^      ";
        let v2 = "               ^    ";
        let w2 = "                 ^  ";
        let j3 = ")";

        let j = &[j1, j2, j3].join("");
        let t = t1;
        let u = &[sp, u2].join("");
        let v = &[sp, v2].join("");
        let w = &[sp, w2].join("");

        let expected = Expr::Lit(Datum::List(
            t2s(t),
            Box::new([
                Datum::Int(t2s(u), 1),
                Datum::Int(t2s(v), 2),
                Datum::Int(t2s(w), 3),
            ]),
        ));
        assert_eq!(expected, expr_for_str(j).unwrap());
    }

    #[test]
    fn expand_fixed_set_match() {
        let j = "(letmacro [two-set? (macro-rules #{} [[(two-set? #{_ _}) false]])])";
        let t = "                                                 ^^^^^^            ";

        let err = Error::new(
            t2s(t),
            ErrorKind::IllegalArg("set patterns must either be empty or a zero or more match"),
        );
        assert_eq!(err, expr_for_str(j).unwrap_err());
    }

    #[test]
    fn expand_constant_match() {
        let j1 = "(letmacro [alph (macro-rules #{} [[(alph 1) 'a] [(alph 2) 'b] [(alph 3) 'c]])]";
        let t1 = "                                                           ^                  ";
        let j2 = "(alph 2)";
        let t2 = "        ";
        let j3 = ")";

        let j = &[j1, j2, j3].join("");
        let t = &[t1, t2].join("");

        let expected = Expr::Lit(Datum::Sym(t2s(t), "b".into()));
        assert_eq!(expected, expr_for_str(j).unwrap());
    }

    #[test]
    fn expand_terminal_zero_or_more_match() {
        let j1 =
            "(letmacro [return-all (macro-rules #{} [[(return-all values ...) '(values ...)]])]";
        let t1 =
            "                                                                  ^^^^^^^^^^^^    ";
        let sp =
            "                                                                                  ";
        let j2 = "(return-all 1 2 3)";
        let u2 = "            ^     ";
        let v2 = "              ^   ";
        let w2 = "                ^ ";
        let j3 = ")";

        let j = &[j1, j2, j3].join("");
        let t = t1;
        let u = &[sp, u2].join("");
        let v = &[sp, v2].join("");
        let w = &[sp, w2].join("");

        let expected = Expr::Lit(Datum::List(
            t2s(t),
            Box::new([
                Datum::Int(t2s(u), 1),
                Datum::Int(t2s(v), 2),
                Datum::Int(t2s(w), 3),
            ]),
        ));
        assert_eq!(expected, expr_for_str(j).unwrap());
    }

    #[test]
    fn expand_middle_zero_or_more_match() {
        let j1 = "(letmacro [mid (macro-rules #{} [[(mid [_ vals ... _]) [true vals ... false]]])]";
        let t1 = "                                                       ^^^^^^^^^^^^^^^^^^^^^    ";
        let u1 = "                                                        ^^^^                    ";
        let v1 = "                                                                      ^^^^^     ";
        let sp = "                                                                                ";
        let j2 = "(mid [1 2 3 4])";
        let w2 = "        ^      ";
        let x2 = "          ^    ";
        let j3 = ")";

        let j = &[j1, j2, j3].join("");
        let t = t1;
        let u = u1;
        let v = v1;
        let w = &[sp, w2].join("");
        let x = &[sp, x2].join("");

        let expected = Expr::Lit(Datum::Vec(
            t2s(t),
            Box::new([
                Datum::Bool(t2s(u), true),
                Datum::Int(t2s(w), 2),
                Datum::Int(t2s(x), 3),
                Datum::Bool(t2s(v), false),
            ]),
        ));
        assert_eq!(expected, expr_for_str(j).unwrap());
    }

    #[test]
    fn expand_multiple_zero_or_more() {
        let j1 = "(letmacro [vm (macro-rules #{} [[(vm (l ...) (r ...)) [r ... l ...]]])]";
        let t1 = "                                                      ^^^^^^^^^^^^^    ";
        let sp = "                                                                       ";
        let j2 = "(vm (1 2) (3 4))";
        let u2 = "     ^          ";
        let v2 = "       ^        ";
        let w2 = "           ^    ";
        let x2 = "             ^  ";
        let j3 = ")";

        let j = &[j1, j2, j3].join("");
        let t = t1;
        let u = &[sp, u2].join("");
        let v = &[sp, v2].join("");
        let w = &[sp, w2].join("");
        let x = &[sp, x2].join("");

        let expected = Expr::Lit(Datum::Vec(
            t2s(t),
            Box::new([
                Datum::Int(t2s(w), 3),
                Datum::Int(t2s(x), 4),
                Datum::Int(t2s(u), 1),
                Datum::Int(t2s(v), 2),
            ]),
        ));
        assert_eq!(expected, expr_for_str(j).unwrap());
    }

    #[test]
    fn expand_multiple_zero_or_more_in_same_pattern_seq() {
        let j = "(letmacro [vm (macro-rules #{} [[(vm (l ... r ...)) true]])])";
        let t = "                                      ^                      ";
        let u = "                                            ^                ";

        let err = Error::new(t2s(u), ErrorKind::MultipleZeroOrMoreMatch(t2s(t)));
        assert_eq!(err, expr_for_str(j).unwrap_err());
    }

    #[test]
    fn expand_subtemplate_without_matching_subpattern() {
        let j1 = "(letmacro [m (macro-rules #{} [[(m expr ...) (5 ...)]])])";
        let t1 = "                                             ^^^^^^^     ";
        let j2 = "(m 1 2 3 4)";
        let t2 = "           ";

        let j = &[j1, j2].join("");
        let t = &[t1, t2].join("");

        let err = Error::new(
            t2s(t),
            ErrorKind::IllegalArg("subtemplate does not include any macro variables"),
        );
        assert_eq!(err, expr_for_str(j).unwrap_err());
    }

    #[test]
    fn expand_subtemplate_matching_multiple_subpatterns() {
        let j =
            "(letmacro [m (macro-rules #{} [[(m (list1 ...) (list2 ...)) ([list1 list2] ...)]])])";
        let t =
            "                                                            ^^^^^^^^^^^^^^^^^^^     ";

        let err = Error::new(
            t2s(t),
            ErrorKind::IllegalArg(
                "subtemplate references macro variables from multiple subpatterns",
            ),
        );
        assert_eq!(err, expr_for_str(j).unwrap_err());
    }

    #[test]
    fn expand_nested_subpatterns() {
        let j1 = "(letmacro [m (macro-rules #{} [[(m (a b rest ...) ...) [(rest ... b a) ...]]])]";
        let t1 = "                                                       ^^^^^^^^^^^^^^^^^^^^    ";
        let u1 = "                                                        ^^^^^^^^^^^^^^         ";
        let sp = "                                                                               ";
        let j2 = "(m (1 2 3 4) (5 6))";
        let v2 = "    ^              ";
        let w2 = "      ^            ";
        let x2 = "        ^          ";
        let y2 = "          ^        ";
        let z2 = "              ^    ";
        let a2 = "                ^  ";
        let j3 = ")";

        let j = &[j1, j2, j3].join("");
        let t = t1;
        let u = u1;
        let v = &[sp, v2].join("");
        let w = &[sp, w2].join("");
        let x = &[sp, x2].join("");
        let y = &[sp, y2].join("");
        let z = &[sp, z2].join("");
        let a = &[sp, a2].join("");

        let expected = Expr::Lit(Datum::Vec(
            t2s(t),
            Box::new([
                Datum::List(
                    t2s(u),
                    Box::new([
                        Datum::Int(t2s(x), 3),
                        Datum::Int(t2s(y), 4),
                        Datum::Int(t2s(w), 2),
                        Datum::Int(t2s(v), 1),
                    ]),
                ),
                Datum::List(
                    t2s(u),
                    Box::new([Datum::Int(t2s(a), 6), Datum::Int(t2s(z), 5)]),
                ),
            ]),
        ));
        assert_eq!(expected, expr_for_str(j).unwrap());
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

        assert_eq!(expected, expr_for_str(j).unwrap());
    }

    #[test]
    fn literal_forbidden_in_module() {
        let j = "1";
        let t = "^";

        let err = Error::new(t2s(t), ErrorKind::NonDefInsideModule);
        assert_eq!(err, module_for_str(j).unwrap_err());
    }

    #[test]
    fn quote_forbidden_in_module() {
        let j = "'foo";
        let t = "^^^^";

        let err = Error::new(t2s(t), ErrorKind::NonDefInsideModule);
        assert_eq!(err, module_for_str(j).unwrap_err());
    }

    #[test]
    fn if_forbidden_in_module() {
        let j = "(if true 1 2)";
        let t = "^^^^^^^^^^^^^";

        let err = Error::new(t2s(t), ErrorKind::NonDefInsideModule);
        assert_eq!(err, module_for_str(j).unwrap_err());
    }

    #[test]
    fn fun_forbidden_in_module() {
        let j = "(fn () 1)";
        let t = "^^^^^^^^^";

        let err = Error::new(t2s(t), ErrorKind::NonDefInsideModule);
        assert_eq!(err, module_for_str(j).unwrap_err());
    }

    #[test]
    fn apply_forbidden_in_module() {
        let j = "((fn () 1))";
        let t = "^^^^^^^^^^^";

        let err = Error::new(t2s(t), ErrorKind::NonDefInsideModule);
        assert_eq!(err, module_for_str(j).unwrap_err());
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
    fn type_predicate() {
        let j = "(type-predicate true)";
        let t = "^^^^^^^^^^^^^^^^^^^^^";

        let expected = Expr::TyPred(t2s(t), ty::Ty::LitBool(true).into_poly());
        assert_eq!(expected, expr_for_str(j).unwrap());
    }
}
