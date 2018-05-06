use std::collections::{BTreeMap, HashMap};
use std::io::Read;

use ctx::CompileContext;
use hir::destruc;
use hir::error::{Error, ErrorKind, Result};
use hir::import::lower_import_set;
use hir::loader::{load_library_data, load_module_data, LibraryName};
use hir::macros::{expand_macro, lower_macro_rules, Macro};
use hir::module::{Module, ModuleDef};
use hir::ns::{Ident, NsDatum, NsId, NsIdAlloc};
use hir::prim::Prim;
use hir::scope::{Binding, MacroId, Scope};
use hir::types::{lower_poly, lower_tvar, try_lower_purity};
use hir::util::{
    expect_arg_count, expect_ident, expect_ident_and_span, pop_vec_front, split_into_fixed_and_rest,
};
use hir::{App, Cond, Expr, Fun, VarId};
use syntax::datum::Datum;
use syntax::span::{Span, EMPTY_SPAN};
use ty;

pub struct LoweringContext<'ccx> {
    curr_var_id: u32,
    ns_id_alloc: NsIdAlloc,
    loaded_libraries: BTreeMap<LibraryName, Module>,
    macros: Vec<Macro>,

    pvars: Vec<ty::purity::PVar>,
    tvars: Vec<ty::TVar>,
    free_purities: Vec<Span>,
    free_tys: Vec<Span>,

    ccx: &'ccx mut CompileContext,
}

pub struct LoweredProgram {
    /*
    tvars: Vec<ty::TVar>,
    free_tys: Vec<Span>,
    libraries: BTreeMap<LibraryName, Module>,*/
    entry_module: Module,
}

impl LoweredProgram {
    pub fn entry_module(&self) -> &Module {
        &self.entry_module
    }
}

enum DeferredModulePrim {
    Def(Span, destruc::Destruc, NsDatum),
    Export(Span, Ident),
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

impl<'ccx> LoweringContext<'ccx> {
    pub fn new(ccx: &'ccx mut CompileContext) -> LoweringContext {
        let mut loaded_libraries = BTreeMap::new();

        // These libraries are always loaded
        loaded_libraries.insert(
            LibraryName::new(
                vec!["risp".to_owned(), "internal".to_owned()],
                "primitives".to_owned(),
            ),
            Module::prims_module(),
        );

        loaded_libraries.insert(
            LibraryName::new(
                vec!["risp".to_owned(), "internal".to_owned()],
                "types".to_owned(),
            ),
            Module::tys_module(),
        );

        LoweringContext {
            curr_var_id: 0,
            ns_id_alloc: NsIdAlloc::new(),
            loaded_libraries,
            macros: vec![],
            pvars: vec![],
            tvars: vec![],
            free_tys: vec![],
            free_purities: vec![],
            ccx,
        }
    }

    fn insert_tvar(&mut self, tvar: ty::TVar) -> ty::TVarId {
        let tvar_id = ty::TVarId::new(self.tvars.len());
        self.tvars.push(tvar);

        tvar_id
    }

    fn insert_free_ty(&mut self, span: Span) -> ty::FreeTyId {
        let free_ty_id = ty::FreeTyId::new(self.free_tys.len());
        self.free_tys.push(span);

        free_ty_id
    }

    fn insert_free_purity(&mut self, span: Span) -> ty::purity::FreePurityId {
        let free_purity_id = ty::purity::FreePurityId::new(self.free_purities.len());
        self.free_purities.push(span);

        free_purity_id
    }

    fn alloc_var_id(&mut self) -> VarId {
        self.curr_var_id += 1;
        VarId::new(self.curr_var_id)
    }

    // This would be less ugly as Result<!> once it's stabilised
    fn lower_user_compile_error(span: Span, mut arg_data: Vec<NsDatum>) -> Error {
        expect_arg_count(span, &arg_data, 1)
            .err()
            .unwrap_or_else(|| {
                if let NsDatum::Str(_, user_message) = arg_data.pop().unwrap() {
                    Error::new(span, ErrorKind::UserError(user_message))
                } else {
                    Error::new(span, ErrorKind::IllegalArg("string expected".to_owned()))
                }
            })
    }

    fn lower_defmacro(
        &mut self,
        scope: &mut Scope,
        span: Span,
        mut arg_data: Vec<NsDatum>,
    ) -> Result<()> {
        expect_arg_count(span, &arg_data, 2)?;

        let transformer_spec = arg_data.pop().unwrap();
        let self_ident = expect_ident(arg_data.pop().unwrap())?;

        let macro_rules_data = if let NsDatum::List(span, mut vs) = transformer_spec {
            if vs.first().and_then(|d| scope.get_datum(d)) != Some(Binding::Prim(Prim::MacroRules))
            {
                return Err(Error::new(
                    span,
                    ErrorKind::IllegalArg("unsupported macro type".to_owned()),
                ));
            }

            vs.remove(0);
            vs
        } else {
            return Err(Error::new(
                transformer_spec.span(),
                ErrorKind::IllegalArg("macro specification must be a list".to_owned()),
            ));
        };

        let mac = lower_macro_rules(scope, span, &self_ident, macro_rules_data)?;

        let macro_id = MacroId::new(self.macros.len());
        self.macros.push(mac);
        scope.insert_binding(self_ident, Binding::Macro(macro_id));

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
        let ident = expect_ident(arg_data.pop().unwrap())?;

        let ty = lower_poly(&self.pvars, &self.tvars, scope, ty_datum)?;

        scope.insert_binding(ident, Binding::Ty(ty));
        Ok(())
    }

    /// Lowers an identifier in to a scalar destruc with the passed type
    fn lower_ident_destruc(
        &mut self,
        scope: &mut Scope,
        span: Span,
        ident: Ident,
        decl_ty: ty::Decl,
    ) -> Result<destruc::Scalar> {
        match scope.get(&ident) {
            Some(Binding::Prim(Prim::Wildcard)) => {
                Ok(destruc::Scalar::new(None, ident.into_name(), decl_ty))
            }
            Some(Binding::Prim(Prim::Ellipsis)) => Err(Error::new(
                span,
                ErrorKind::IllegalArg(
                    "ellipsis can only be used to destructure the rest of a list".to_owned(),
                ),
            )),
            _ => {
                let var_id = self.alloc_var_id();
                let source_name = ident.name().clone();

                scope.insert_var(ident, var_id);

                Ok(destruc::Scalar::new(Some(var_id), source_name, decl_ty))
            }
        }
    }

    fn lower_scalar_destruc(
        &mut self,
        scope: &mut Scope,
        destruc_datum: NsDatum,
    ) -> Result<destruc::Scalar> {
        match destruc_datum {
            NsDatum::Ident(span, ident) => {
                let free_ty_id = self.insert_free_ty(span);
                self.lower_ident_destruc(scope, span, ident, ty::Decl::Free(free_ty_id))
            }
            NsDatum::Vec(span, mut vs) => {
                if vs.len() != 3 {
                    return Err(Error::new(span, ErrorKind::NoVecDestruc));
                }

                // Make sure the middle element is a type colon
                if scope.get_datum(&vs[1]) != Some(Binding::Prim(Prim::TyColon)) {
                    return Err(Error::new(span, ErrorKind::NoVecDestruc));
                }

                let ty = lower_poly(&self.pvars, &self.tvars, scope, vs.pop().unwrap())?;

                // Discard the type colon
                vs.pop();

                let (ident, span) = expect_ident_and_span(vs.pop().unwrap())?;
                self.lower_ident_destruc(scope, span, ident, ty.into_decl())
            }
            _ => Err(Error::new(
                destruc_datum.span(),
                ErrorKind::IllegalArg("expected a variable name or [name : Type]".to_owned()),
            )),
        }
    }

    fn lower_list_destruc(&mut self, scope: &mut Scope, vs: Vec<NsDatum>) -> Result<destruc::List> {
        let (fixed, rest) = split_into_fixed_and_rest(scope, vs);

        let fixed_destrucs = fixed
            .into_iter()
            .map(|v| self.lower_destruc(scope, v))
            .collect::<Result<Vec<destruc::Destruc>>>()?;

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
    ) -> Result<destruc::Destruc> {
        match destruc_datum {
            NsDatum::Ident(span, _) | NsDatum::Vec(span, _) => {
                self.lower_scalar_destruc(scope, destruc_datum)
                    .map(|scalar| destruc::Destruc::Scalar(span, scalar))
            }
            NsDatum::List(span, vs) => self.lower_list_destruc(scope, vs)
                .map(|list_destruc| destruc::Destruc::List(span, list_destruc)),
            _ => Err(Error::new(
                destruc_datum.span(),
                ErrorKind::IllegalArg(
                    "values can only be bound to variables or destructured into lists".to_owned(),
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
                let ret_ty = lower_poly(&self.pvars, &self.tvars, &fun_scope, ret_datum)?;

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

    fn lower_fun(&mut self, scope: &Scope, span: Span, arg_data: Vec<NsDatum>) -> Result<Expr> {
        if arg_data.is_empty() {
            return Err(Error::new(
                span,
                ErrorKind::IllegalArg("parameter declaration missing".to_owned()),
            ));
        }

        let mut fun_scope = Scope::new_child(scope);

        let (mut next_datum, mut tail_data) = pop_vec_front(arg_data);
        let tvar_id_start = ty::TVarId::new(self.tvars.len());

        // We can either begin with a set of type variables or a list of parameters
        if let NsDatum::Set(_, vs) = next_datum {
            for tvar_datum in vs {
                let (ident, tvar) = lower_tvar(&self.pvars, &self.tvars, scope, tvar_datum)?;
                let tvar_id = self.insert_tvar(tvar);
                fun_scope.insert_binding(ident, Binding::Ty(ty::Poly::Var(tvar_id)));
            }

            if tail_data.is_empty() {
                return Err(Error::new(
                    span,
                    ErrorKind::IllegalArg(
                        "type variables should be followed by parameters".to_owned(),
                    ),
                ));
            }

            let (new_next_datum, new_tail_data) = pop_vec_front(tail_data);
            next_datum = new_next_datum;
            tail_data = new_tail_data;
        };

        // We allocate tvar IDs sequentially so we can use a simple range to track them
        let tvar_ids = tvar_id_start..ty::TVarId::new(self.tvars.len());

        // Pull out our params
        let params = match next_datum {
            NsDatum::List(_, vs) => self.lower_list_destruc(&mut fun_scope, vs)?,
            _ => {
                return Err(Error::new(
                    span,
                    ErrorKind::IllegalArg("parameter list expected".to_owned()),
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
        let body_exprs = body_data
            .into_iter()
            .map(|body_datum| self.lower_body_expr(&mut fun_scope, body_datum))
            .collect::<Result<Vec<Expr>>>()?;

        let body_expr = Expr::from_vec(body_exprs);

        let purity = purity
            .map(ty::purity::Poly::into_decl)
            .unwrap_or_else(|| ty::purity::Decl::Free(self.insert_free_purity(span)));

        // If we don't have a return type try to guess a span for the last expression so we can
        // locate inference errors
        let ret_ty = ret_ty.map(|ret_ty| ret_ty.into_decl()).unwrap_or_else(|| {
            let last_expr_span = body_expr.last_expr().and_then(|e| e.span()).unwrap_or(span);
            ty::Decl::Free(self.insert_free_ty(last_expr_span))
        });

        Ok(Expr::Fun(
            span,
            Fun {
                tvar_ids,
                purity,
                params,
                ret_ty,
                body_expr: Box::new(body_expr),
            },
        ))
    }

    fn lower_inner_prim_apply(
        &mut self,
        scope: &mut Scope,
        applied_prim: AppliedPrim,
        mut arg_data: Vec<NsDatum>,
    ) -> Result<Expr> {
        let AppliedPrim { prim, span, .. } = applied_prim;

        match prim {
            Prim::Def | Prim::DefMacro | Prim::DefType | Prim::Import => {
                Err(Error::new(span, ErrorKind::DefOutsideBody))
            }
            Prim::Export => Err(Error::new(span, ErrorKind::ExportOutsideModule)),
            Prim::Quote => {
                expect_arg_count(span, &arg_data, 1)?;
                Ok(Expr::Lit(arg_data.pop().unwrap().into_syntax_datum()))
            }
            Prim::Fun => self.lower_fun(scope, span, arg_data),
            Prim::If => {
                expect_arg_count(span, &arg_data, 3)?;

                macro_rules! pop_as_boxed_expr {
                    () => {
                        Box::new(self.lower_inner_expr(scope, arg_data.pop().unwrap())?)
                    };
                };

                Ok(Expr::Cond(
                    span,
                    Cond {
                        false_expr: pop_as_boxed_expr!(),
                        true_expr: pop_as_boxed_expr!(),
                        test_expr: pop_as_boxed_expr!(),
                    },
                ))
            }
            Prim::TyPred => {
                expect_arg_count(span, &arg_data, 1)?;
                Ok(Expr::TyPred(
                    span,
                    lower_poly(&self.pvars, &self.tvars, scope, arg_data.pop().unwrap())?,
                ))
            }
            Prim::CompileError => Err(Self::lower_user_compile_error(span, arg_data)),
            Prim::Ellipsis | Prim::Wildcard | Prim::MacroRules | Prim::TyColon => {
                Err(Error::new(span, ErrorKind::PrimRef))
            }
        }
    }

    fn load_library(
        &mut self,
        scope: &mut Scope,
        span: Span,
        library_name: &LibraryName,
    ) -> Result<&Module> {
        // TODO: This does a lot of hash lookups
        if !self.loaded_libraries.contains_key(library_name) {
            let library_data = load_library_data(self.ccx, span, library_name)?;
            let loaded_library = self.lower_module(scope, library_data)?;

            self.loaded_libraries
                .insert(library_name.clone(), loaded_library);
        }

        Ok(&self.loaded_libraries[library_name])
    }

    fn lower_import(
        &mut self,
        scope: &mut Scope,
        ns_id: NsId,
        arg_data: Vec<NsDatum>,
    ) -> Result<()> {
        for arg_datum in arg_data {
            let bindings = lower_import_set(arg_datum, |span, library_name| {
                Ok(self.load_library(scope, span, library_name)?
                    .exports()
                    .clone())
            })?;

            for (name, binding) in bindings {
                scope.insert_binding(Ident::new(ns_id, name), binding);
            }
        }

        Ok(())
    }

    fn lower_body_prim_apply(
        &mut self,
        scope: &mut Scope,
        applied_prim: AppliedPrim,
        mut arg_data: Vec<NsDatum>,
    ) -> Result<Expr> {
        let AppliedPrim { prim, ns_id, span } = applied_prim;

        match prim {
            Prim::Def => {
                expect_arg_count(span, &arg_data, 2)?;

                let value_datum = arg_data.pop().unwrap();
                let destruc_datum = arg_data.pop().unwrap();

                let value_expr = self.lower_inner_expr(scope, value_datum)?;
                let destruc = self.lower_destruc(scope, destruc_datum)?;

                Ok(Expr::Def(span, destruc, Box::new(value_expr)))
            }
            Prim::DefMacro => self.lower_defmacro(scope, span, arg_data)
                .map(|_| Expr::Do(vec![])),
            Prim::DefType => self.lower_deftype(scope, span, arg_data)
                .map(|_| Expr::Do(vec![])),
            Prim::Import => self.lower_import(scope, ns_id, arg_data)
                .map(|_| Expr::Do(vec![])),
            _ => self.lower_inner_prim_apply(scope, applied_prim, arg_data),
        }
    }

    fn lower_expr_apply(
        &mut self,
        scope: &mut Scope,
        span: Span,
        fun_expr: Expr,
        arg_data: Vec<NsDatum>,
    ) -> Result<Expr> {
        let (fixed_arg_data, rest_arg_datum) = split_into_fixed_and_rest(scope, arg_data);

        let fixed_arg_exprs = fixed_arg_data
            .into_iter()
            .map(|arg_datum| self.lower_inner_expr(scope, arg_datum))
            .collect::<Result<Vec<Expr>>>()?;

        let rest_arg_expr = match rest_arg_datum {
            Some(rest_arg_datum) => Some(Box::new(self.lower_inner_expr(scope, rest_arg_datum)?)),
            None => None,
        };

        Ok(Expr::App(
            span,
            App {
                fun_expr: Box::new(fun_expr),
                fixed_arg_exprs,
                rest_arg_expr,
            },
        ))
    }

    fn generic_lower_expr<F>(
        &mut self,
        scope: &mut Scope,
        datum: NsDatum,
        lower_prim_apply: F,
    ) -> Result<Expr>
    where
        F: Fn(&mut Self, &mut Scope, AppliedPrim, Vec<NsDatum>) -> Result<Expr>,
    {
        match datum {
            NsDatum::Ident(span, ref ident) => match scope.get(ident) {
                Some(Binding::Var(id)) => Ok(Expr::Ref(span, id)),
                Some(Binding::Prim(_)) => Err(Error::new(span, ErrorKind::PrimRef)),
                Some(Binding::Ty(_)) | Some(Binding::TyCons(_)) | Some(Binding::Purity(_)) => {
                    Err(Error::new(span, ErrorKind::TyRef))
                }
                Some(Binding::Macro(_)) => {
                    Err(Error::new(span, ErrorKind::MacroRef(ident.name().clone())))
                }
                None => Err(Error::new(
                    span,
                    ErrorKind::UnboundSymbol(ident.name().clone()),
                )),
            },
            NsDatum::List(span, mut vs) => {
                if vs.is_empty() {
                    return Ok(Expr::Lit(Datum::List(span, vec![])));
                }

                let arg_data = vs.split_off(1);
                let fn_datum = vs.pop().unwrap();

                match fn_datum {
                    NsDatum::Ident(fn_span, ref ident) => match scope.get(ident) {
                        Some(Binding::Prim(prim)) => {
                            let applied_prim = AppliedPrim {
                                prim,
                                ns_id: ident.ns_id(),
                                span,
                            };
                            lower_prim_apply(self, scope, applied_prim, arg_data)
                        }
                        Some(Binding::Macro(macro_id)) => {
                            let expanded_datum = {
                                let mac = &self.macros[macro_id.to_usize()];
                                expand_macro(&mut self.ns_id_alloc, scope, span, mac, &arg_data)?
                            };

                            self.generic_lower_expr(scope, expanded_datum, lower_prim_apply)
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
                            ErrorKind::UnboundSymbol(ident.name().clone()),
                        )),
                    },
                    _ => {
                        let fn_expr = self.lower_inner_expr(scope, fn_datum)?;
                        self.lower_expr_apply(scope, span, fn_expr, arg_data)
                    }
                }
            }
            other => Ok(Expr::Lit(other.into_syntax_datum())),
        }
    }

    fn lower_inner_expr(&mut self, scope: &mut Scope, datum: NsDatum) -> Result<Expr> {
        self.generic_lower_expr(scope, datum, LoweringContext::lower_inner_prim_apply)
    }

    fn lower_body_expr(&mut self, scope: &mut Scope, datum: NsDatum) -> Result<Expr> {
        self.generic_lower_expr(scope, datum, LoweringContext::lower_body_prim_apply)
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
                            Ok(DeferredModulePrim::Export(span, ident))
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

                Ok(vec![DeferredModulePrim::Def(span, destruc, value_datum)])
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

        if let NsDatum::List(span, mut vs) = datum {
            if !vs.is_empty() {
                let arg_data = vs.split_off(1);
                let fn_datum = vs.pop().unwrap();

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
                                expand_macro(&mut self.ns_id_alloc, scope, span, mac, &arg_data)?
                            };

                            return self.lower_module_def(scope, expanded_datum)
                                .map_err(|e| e.with_macro_invocation_span(span));
                        }
                        Some(_) => {
                            // Non-def
                        }
                        None => {
                            return Err(Error::new(
                                fn_span,
                                ErrorKind::UnboundSymbol(ident.name().clone()),
                            ));
                        }
                    }
                }
            }
        }

        Err(Error::new(span, ErrorKind::NonDefInsideModule))
    }

    fn lower_module(&mut self, scope: &mut Scope, data: Vec<Datum>) -> Result<Module> {
        let ns_id = self.ns_id_alloc.alloc();

        // The default scope only consists of (import)
        scope.insert_binding(
            Ident::new(ns_id, "import".to_owned()),
            Binding::Prim(Prim::Import),
        );

        // Extract all of our definitions.
        // Imports, types and macros are resolved immediate and cannot refer to bindings later in
        // the body. Exports and variable definitions (including defn) are deferred to a second
        // pass once all binding have been introduced. All other expressions are forbidden.
        let mut deferred_prims = Vec::<DeferredModulePrim>::new();
        for input_datum in data {
            let ns_datum = NsDatum::from_syntax_datum(ns_id, input_datum);
            let mut new_deferred_prims = self.lower_module_def(scope, ns_datum)?;

            deferred_prims.append(&mut new_deferred_prims);
        }

        // Process our variable definitions and exports
        let mut module_defs = Vec::<ModuleDef>::new();
        let mut exports = HashMap::<String, Binding>::new();
        for deferred_prim in deferred_prims {
            match deferred_prim {
                DeferredModulePrim::Def(span, destruc, value_datum) => {
                    let value_expr = self.lower_inner_expr(scope, value_datum)?;
                    module_defs.push(ModuleDef::new(span, destruc, value_expr));
                }
                DeferredModulePrim::Export(span, ident) => {
                    if let Some(binding) = scope.get(&ident) {
                        exports.insert(ident.into_name(), binding);
                    } else {
                        return Err(Error::new(
                            span,
                            ErrorKind::UnboundSymbol(ident.into_name()),
                        ));
                    }
                }
            }
        }

        Ok(Module::new(module_defs, exports))
    }

    fn lower_entry_module(
        &mut self,
        display_name: String,
        input_reader: &mut Read,
    ) -> Result<Module> {
        let mut root_scope = Scope::new_empty();

        let data = load_module_data(self.ccx, EMPTY_SPAN, display_name, input_reader)?;
        self.lower_module(&mut root_scope, data)
    }
}

pub fn lower_program(
    ccx: &mut CompileContext,
    display_name: String,
    input_reader: &mut Read,
) -> Result<LoweredProgram> {
    let mut lcx = LoweringContext::new(ccx);
    let entry_module = lcx.lower_entry_module(display_name, input_reader)?;

    Ok(LoweredProgram {
        //libraries: lcx.loaded_libraries,
        entry_module,
        //tvars: lcx.tvars,
        //free_tys: lcx.free_tys,
    })
}

////

#[cfg(test)]
mod test {
    use super::*;
    use syntax::parser::data_from_str;
    use syntax::span::t2s;
    use ty;
    use ty::purity::Purity;

    fn import_statement_for_library(names: &[&'static str]) -> Datum {
        Datum::List(
            EMPTY_SPAN,
            vec![
                Datum::Sym(EMPTY_SPAN, "import".to_owned()),
                Datum::Vec(
                    EMPTY_SPAN,
                    names
                        .iter()
                        .map(|n| Datum::Sym(EMPTY_SPAN, (*n).to_owned()))
                        .collect(),
                ),
            ],
        )
    }

    fn module_for_str(data_str: &str) -> Result<Module> {
        let mut root_scope = Scope::new_empty();

        let mut test_data = data_from_str(data_str).unwrap();
        let mut program_data = vec![
            import_statement_for_library(&["risp", "internal", "primitives"]),
            import_statement_for_library(&["risp", "internal", "types"]),
        ];
        program_data.append(&mut test_data);

        let mut ccx = CompileContext::new();
        let mut lcx = LoweringContext::new(&mut ccx);
        lcx.lower_module(&mut root_scope, program_data)
    }

    fn body_expr_for_str(data_str: &str) -> Result<Expr> {
        let mut root_scope = Scope::new_empty();

        // Wrap the test data in a function definition
        let mut test_data = data_from_str(data_str).unwrap();
        let mut main_function_data = vec![
            Datum::Sym(EMPTY_SPAN, "fn".to_owned()),
            Datum::List(EMPTY_SPAN, vec![]),
        ];

        main_function_data.append(&mut test_data);

        // Wrap the function definition in a program
        let program_data = vec![
            import_statement_for_library(&["risp", "internal", "primitives"]),
            import_statement_for_library(&["risp", "internal", "types"]),
            Datum::List(
                EMPTY_SPAN,
                vec![
                    Datum::Sym(EMPTY_SPAN, "def".to_owned()),
                    Datum::Sym(EMPTY_SPAN, "main!".to_owned()),
                    Datum::List(EMPTY_SPAN, main_function_data),
                ],
            ),
        ];

        let mut ccx = CompileContext::new();
        let mut lcx = LoweringContext::new(&mut ccx);
        let mut program_defs = lcx.lower_module(&mut root_scope, program_data)?.into_defs();

        assert_eq!(1, program_defs.len());
        let main_func_def = program_defs.pop().unwrap();

        if let Expr::Fun(_, main_func) = main_func_def.into_value() {
            Ok(*main_func.body_expr)
        } else {
            panic!("Unexpected program structure");
        }
    }

    #[test]
    fn self_quoting_bool() {
        let j = "false";
        let t = "^^^^^";

        let expected = Expr::Lit(Datum::Bool(t2s(t), false));
        assert_eq!(expected, body_expr_for_str(j).unwrap());
    }

    #[test]
    fn self_quoting_empty_list() {
        let j = "()";
        let t = "^^";

        let expected = Expr::Lit(Datum::List(t2s(t), vec![]));
        assert_eq!(expected, body_expr_for_str(j).unwrap());
    }

    #[test]
    fn quoted_datum_shorthand() {
        let j = "'foo";
        let t = " ^^^";

        let expected = Expr::Lit(Datum::Sym(t2s(t), "foo".to_owned()));
        assert_eq!(expected, body_expr_for_str(j).unwrap());
    }

    #[test]
    fn quoted_datum_explicit() {
        let j = "(quote foo)";
        let t = "       ^^^ ";

        let expected = Expr::Lit(Datum::Sym(t2s(t), "foo".to_owned()));
        assert_eq!(expected, body_expr_for_str(j).unwrap());
    }

    #[test]
    fn quoted_multiple_data() {
        let j = "(quote 1 2 3)";
        let t = "^^^^^^^^^^^^^";

        let err = Error::new(t2s(t), ErrorKind::WrongArgCount(1));
        assert_eq!(err, body_expr_for_str(j).unwrap_err());
    }

    #[test]
    fn basic_untyped_var_def() {
        let j = "(def x 1) x";
        let t = "     ^     ";
        let u = "^^^^^^^^^  ";
        let v = "       ^   ";
        let w = "          ^";

        let destruc = destruc::Destruc::Scalar(
            t2s(t),
            destruc::Scalar::new(
                Some(VarId(2)),
                "x".to_owned(),
                ty::Decl::Free(ty::FreeTyId::new(1)),
            ),
        );

        let expected = Expr::Do(vec![
            Expr::Def(t2s(u), destruc, Box::new(Expr::Lit(Datum::Int(t2s(v), 1)))),
            Expr::Ref(t2s(w), VarId(2)),
        ]);

        assert_eq!(expected, body_expr_for_str(j).unwrap());
    }

    #[test]
    fn basic_typed_var_def() {
        let j = "(def [x : true] true) x";
        let t = "     ^^^^^^^^^^        ";
        let u = "^^^^^^^^^^^^^^^^^^^^^  ";
        let v = "                ^^^^   ";
        let w = "                      ^";

        let destruc = destruc::Destruc::Scalar(
            t2s(t),
            destruc::Scalar::new(
                Some(VarId(2)),
                "x".to_owned(),
                ty::Ty::LitBool(true).into_decl(),
            ),
        );

        let expected = Expr::Do(vec![
            Expr::Def(
                t2s(u),
                destruc,
                Box::new(Expr::Lit(Datum::Bool(t2s(v), true))),
            ),
            Expr::Ref(t2s(w), VarId(2)),
        ]);

        assert_eq!(expected, body_expr_for_str(j).unwrap());
    }

    #[test]
    fn double_typed_var_def() {
        // We should fail to annotate a variable a second time
        let j = "(def [[x : true] : false] 1)";
        let t = "      ^^^^^^^^^^            ";

        let err = Error::new(t2s(t), ErrorKind::ExpectedSymbol);
        assert_eq!(err, body_expr_for_str(j).unwrap_err());
    }

    #[test]
    fn wildcard_def() {
        let j = "(def _ 1)";
        let t = "     ^   ";
        let u = "^^^^^^^^^";
        let v = "       ^ ";

        let destruc = destruc::Destruc::Scalar(
            t2s(t),
            destruc::Scalar::new(None, "_".to_owned(), ty::Decl::Free(ty::FreeTyId::new(1))),
        );

        let expected = Expr::Def(t2s(u), destruc, Box::new(Expr::Lit(Datum::Int(t2s(v), 1))));
        assert_eq!(expected, body_expr_for_str(j).unwrap());
    }

    #[test]
    fn destruc_to_bad_ellipsis_def() {
        let j = "(def ... 1)";
        let t = "     ^^^   ";

        let err = Error::new(
            t2s(t),
            ErrorKind::IllegalArg(
                "ellipsis can only be used to destructure the rest of a list".to_owned(),
            ),
        );
        assert_eq!(err, body_expr_for_str(j).unwrap_err());
    }

    #[test]
    fn list_destruc_def() {
        let j = "(def (x rest ...) '(1)) x";
        let t = "     ^^^^^^^^^^^^        ";
        let u = "      ^                  ";
        let v = "^^^^^^^^^^^^^^^^^^^^^^^  ";
        let w = "                   ^^^   ";
        let x = "                    ^    ";
        let y = "                        ^";

        let destruc = destruc::Destruc::List(
            t2s(t),
            destruc::List::new(
                vec![destruc::Destruc::Scalar(
                    t2s(u),
                    destruc::Scalar::new(
                        Some(VarId(2)),
                        "x".to_owned(),
                        ty::Decl::Free(ty::FreeTyId::new(1)),
                    ),
                )],
                Some(Box::new(destruc::Scalar::new(
                    Some(VarId(3)),
                    "rest".to_owned(),
                    ty::Decl::Free(ty::FreeTyId::new(2)),
                ))),
            ),
        );

        let expected = Expr::Do(vec![
            Expr::Def(
                t2s(v),
                destruc,
                Box::new(Expr::Lit(Datum::List(t2s(w), vec![Datum::Int(t2s(x), 1)]))),
            ),
            Expr::Ref(t2s(y), VarId(2)),
        ]);

        assert_eq!(expected, body_expr_for_str(j).unwrap());
    }

    #[test]
    fn def_of_bad_destruc() {
        let j = "(def 1 1)";
        let t = "     ^   ";

        let err = Error::new(
            t2s(t),
            ErrorKind::IllegalArg(
                "values can only be bound to variables or destructured into lists".to_owned(),
            ),
        );
        assert_eq!(err, body_expr_for_str(j).unwrap_err());
    }

    #[test]
    fn def_of_vec_destruc() {
        let j = "(def [x y] [1 2])";
        let t = "     ^^^^^       ";

        let err = Error::new(t2s(t), ErrorKind::NoVecDestruc);
        assert_eq!(err, body_expr_for_str(j).unwrap_err());
    }

    #[test]
    fn def_in_non_body() {
        let j = "(def x (def y 1))";
        let t = "       ^^^^^^^^^ ";

        let err = Error::new(t2s(t), ErrorKind::DefOutsideBody);
        assert_eq!(err, body_expr_for_str(j).unwrap_err());
    }

    #[test]
    fn reference_prim() {
        let j = "def";
        let t = "^^^";

        let err = Error::new(t2s(t), ErrorKind::PrimRef);
        assert_eq!(err, body_expr_for_str(j).unwrap_err());
    }

    #[test]
    fn reference_unbound() {
        let j = "nopenopenope";
        let t = "^^^^^^^^^^^^";

        let err = Error::new(t2s(t), ErrorKind::UnboundSymbol("nopenopenope".to_owned()));
        assert_eq!(err, body_expr_for_str(j).unwrap_err());
    }

    #[test]
    fn fn_without_param_decl() {
        let j = "(fn)";
        let t = "^^^^";

        let err = Error::new(
            t2s(t),
            ErrorKind::IllegalArg("parameter declaration missing".to_owned()),
        );
        assert_eq!(err, body_expr_for_str(j).unwrap_err());
    }

    #[test]
    fn fn_with_bad_destruc_param() {
        let j = "(fn (1))";
        let t = "     ^  ";

        let err = Error::new(
            t2s(t),
            ErrorKind::IllegalArg(
                "values can only be bound to variables or destructured into lists".to_owned(),
            ),
        );
        assert_eq!(err, body_expr_for_str(j).unwrap_err());
    }

    #[test]
    fn empty_fn() {
        let j = "(fn ())";
        let t = "^^^^^^^";

        let expected = Expr::Fun(
            t2s(t),
            Fun {
                tvar_ids: ty::TVarIds::empty(),
                purity: ty::purity::Decl::Free(ty::purity::FreePurityId::new(0)),
                params: destruc::List::new(vec![], None),
                ret_ty: ty::Decl::Free(ty::FreeTyId::new(1)),
                body_expr: Box::new(Expr::from_vec(vec![])),
            },
        );

        assert_eq!(expected, body_expr_for_str(j).unwrap());
    }

    #[test]
    fn empty_fn_with_ret_ty() {
        let j = "(fn () -> Int 1)";
        let t = "^^^^^^^^^^^^^^^^";
        let u = "              ^ ";

        let expected = Expr::Fun(
            t2s(t),
            Fun {
                tvar_ids: ty::TVarIds::empty(),
                purity: ty::purity::Decl::Fixed(Purity::Pure),
                params: destruc::List::new(vec![], None),
                ret_ty: ty::Ty::Int.into_decl(),
                body_expr: Box::new(Expr::Lit(Datum::Int(t2s(u), 1))),
            },
        );

        assert_eq!(expected, body_expr_for_str(j).unwrap());
    }

    #[test]
    fn identity_fn() {
        let j = "(fn (x) x)";
        let t = "     ^    ";
        let u = "^^^^^^^^^^";
        let v = "        ^ ";

        let param_var_id = VarId::new(2);
        let params = destruc::List::new(
            vec![destruc::Destruc::Scalar(
                t2s(t),
                destruc::Scalar::new(
                    Some(param_var_id),
                    "x".to_owned(),
                    ty::Decl::Free(ty::FreeTyId::new(1)),
                ),
            )],
            None,
        );

        let expected = Expr::Fun(
            t2s(u),
            Fun {
                tvar_ids: ty::TVarIds::empty(),
                purity: ty::purity::Decl::Free(ty::purity::FreePurityId::new(0)),
                params,
                ret_ty: ty::Decl::Free(ty::FreeTyId::new(2)),
                body_expr: Box::new(Expr::Ref(t2s(v), param_var_id)),
            },
        );

        assert_eq!(expected, body_expr_for_str(j).unwrap());
    }

    #[test]
    fn poly_impure_identity_fn() {
        let j = "(fn #{A} ([x : A]) ->! A x)";
        let t = "          ^^^^^^^          ";
        let u = "^^^^^^^^^^^^^^^^^^^^^^^^^^^";
        let v = "                         ^ ";

        let tvar_id = ty::TVarId::new(0);

        let param_var_id = VarId::new(2);
        let params = destruc::List::new(
            vec![destruc::Destruc::Scalar(
                t2s(t),
                destruc::Scalar::new(Some(param_var_id), "x".to_owned(), ty::Decl::Var(tvar_id)),
            )],
            None,
        );

        let expected = Expr::Fun(
            t2s(u),
            Fun {
                tvar_ids: ty::TVarId::new(0)..ty::TVarId::new(1),
                purity: ty::purity::Decl::Fixed(Purity::Impure),
                params,
                ret_ty: ty::Decl::Var(tvar_id),
                body_expr: Box::new(Expr::Ref(t2s(v), param_var_id)),
            },
        );

        assert_eq!(expected, body_expr_for_str(j).unwrap());
    }

    #[test]
    fn capturing_fn() {
        let j = "(def x 1)(fn () x)";
        let t = "     ^            ";
        let u = "^^^^^^^^^         ";
        let v = "       ^          ";
        let w = "         ^^^^^^^^^";
        let x = "                ^ ";

        let outer_var_id = VarId::new(2);
        let outer_destruc = destruc::Destruc::Scalar(
            t2s(t),
            destruc::Scalar::new(
                Some(outer_var_id),
                "x".to_owned(),
                ty::Decl::Free(ty::FreeTyId::new(1)),
            ),
        );

        let expected = Expr::Do(vec![
            Expr::Def(
                t2s(u),
                outer_destruc,
                Box::new(Expr::Lit(Datum::Int(t2s(v), 1))),
            ),
            Expr::Fun(
                t2s(w),
                Fun {
                    tvar_ids: ty::TVarIds::empty(),
                    purity: ty::purity::Decl::Free(ty::purity::FreePurityId::new(0)),
                    params: destruc::List::new(vec![], None),
                    ret_ty: ty::Decl::Free(ty::FreeTyId::new(2)),
                    body_expr: Box::new(Expr::Ref(t2s(x), outer_var_id)),
                },
            ),
        ]);

        assert_eq!(expected, body_expr_for_str(j).unwrap());
    }

    #[test]
    fn shadowing_fn() {
        let j = "(def x 1)(fn (x) x)";
        let t = "     ^             ";
        let u = "       ^           ";
        let v = "              ^    ";
        let w = "^^^^^^^^^          ";
        let x = "         ^^^^^^^^^^";
        let y = "                 ^ ";

        let outer_var_id = VarId::new(2);
        let outer_destruc = destruc::Destruc::Scalar(
            t2s(t),
            destruc::Scalar::new(
                Some(outer_var_id),
                "x".to_owned(),
                ty::Decl::Free(ty::FreeTyId::new(1)),
            ),
        );

        let param_var_id = VarId::new(3);
        let params = destruc::List::new(
            vec![destruc::Destruc::Scalar(
                t2s(v),
                destruc::Scalar::new(
                    Some(param_var_id),
                    "x".to_owned(),
                    ty::Decl::Free(ty::FreeTyId::new(2)),
                ),
            )],
            None,
        );

        let expected = Expr::Do(vec![
            Expr::Def(
                t2s(w),
                outer_destruc,
                Box::new(Expr::Lit(Datum::Int(t2s(u), 1))),
            ),
            Expr::Fun(
                t2s(x),
                Fun {
                    tvar_ids: ty::TVarIds::empty(),
                    purity: ty::purity::Decl::Free(ty::purity::FreePurityId::new(0)),
                    params,
                    ret_ty: ty::Decl::Free(ty::FreeTyId::new(3)),
                    body_expr: Box::new(Expr::Ref(t2s(y), param_var_id)),
                },
            ),
        ]);

        assert_eq!(expected, body_expr_for_str(j).unwrap());
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
            App {
                fun_expr: Box::new(Expr::Lit(Datum::Int(t2s(u), 1))),
                fixed_arg_exprs: vec![
                    Expr::Lit(Datum::Int(t2s(v), 2)),
                    Expr::Lit(Datum::Int(t2s(w), 3)),
                ],
                rest_arg_expr: None,
            },
        );

        assert_eq!(expected, body_expr_for_str(j).unwrap());
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
            App {
                fun_expr: Box::new(Expr::Lit(Datum::Int(t2s(u), 1))),
                fixed_arg_exprs: vec![Expr::Lit(Datum::Int(t2s(v), 2))],
                rest_arg_expr: Some(Box::new(Expr::Lit(Datum::Int(t2s(w), 3)))),
            },
        );

        assert_eq!(expected, body_expr_for_str(j).unwrap());
    }

    #[test]
    fn empty_if() {
        let j = "(if)";
        let t = "^^^^";

        let err = Error::new(t2s(t), ErrorKind::WrongArgCount(3));
        assert_eq!(err, body_expr_for_str(j).unwrap_err());
    }

    #[test]
    fn if_without_test() {
        let j = "(if true)";
        let t = "^^^^^^^^^";

        let err = Error::new(t2s(t), ErrorKind::WrongArgCount(3));
        assert_eq!(err, body_expr_for_str(j).unwrap_err());
    }

    #[test]
    fn if_without_false_branch() {
        let j = "(if true 1)";
        let t = "^^^^^^^^^^^";

        let err = Error::new(t2s(t), ErrorKind::WrongArgCount(3));
        assert_eq!(err, body_expr_for_str(j).unwrap_err());
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
            Cond {
                test_expr: Box::new(Expr::Lit(Datum::Bool(t2s(u), true))),
                true_expr: Box::new(Expr::Lit(Datum::Int(t2s(v), 1))),
                false_expr: Box::new(Expr::Lit(Datum::Int(t2s(w), 2))),
            },
        );

        assert_eq!(expected, body_expr_for_str(j).unwrap());
    }

    #[test]
    fn simple_export() {
        let j = "(def x 1)(export x)";
        let t = "^^^^^^^^^          ";
        let u = "     ^             ";
        let v = "       ^           ";

        let var_id = VarId(1);

        let expected_module_def = ModuleDef::new(
            t2s(t),
            destruc::Destruc::Scalar(
                t2s(u),
                destruc::Scalar::new(
                    Some(var_id),
                    "x".to_owned(),
                    ty::Decl::Free(ty::FreeTyId::new(0)),
                ),
            ),
            Expr::Lit(Datum::Int(t2s(v), 1)),
        );

        let mut expected_exports = HashMap::new();
        expected_exports.insert("x".to_owned(), Binding::Var(var_id));

        let expected = Module::new(vec![expected_module_def], expected_exports);
        assert_eq!(expected, module_for_str(j).unwrap());
    }

    #[test]
    fn export_unbound() {
        let j = "(export x)";
        let t = "        ^ ";

        let err = Error::new(t2s(t), ErrorKind::UnboundSymbol("x".to_owned()));
        assert_eq!(err, module_for_str(j).unwrap_err());
    }

    #[test]
    fn defmacro_of_non_symbol() {
        let j = "(defmacro 1 (macro-rules ${}))";
        let t = "          ^                   ";

        let err = Error::new(t2s(t), ErrorKind::ExpectedSymbol);
        assert_eq!(err, body_expr_for_str(j).unwrap_err());
    }

    #[test]
    fn defmacro_of_non_list() {
        let j = "(defmacro a b)";
        let t = "            ^ ";

        let err = Error::new(
            t2s(t),
            ErrorKind::IllegalArg("macro specification must be a list".to_owned()),
        );
        assert_eq!(err, body_expr_for_str(j).unwrap_err());
    }

    #[test]
    fn defmacro_of_unsupported_type() {
        let j = "(defmacro a (macro-fn #{}))";
        let t = "            ^^^^^^^^^^^^^^ ";

        let err = Error::new(
            t2s(t),
            ErrorKind::IllegalArg("unsupported macro type".to_owned()),
        );
        assert_eq!(err, body_expr_for_str(j).unwrap_err());
    }

    #[test]
    fn defmacro_with_duplicate_vars() {
        let j = "(defmacro a (macro-rules #{} [[(a x x) x]]))";
        let t = "                                  ^         ";
        let u = "                                    ^       ";

        let err = Error::new(t2s(u), ErrorKind::DuplicateMacroVar("x".to_owned(), t2s(t)));
        assert_eq!(err, body_expr_for_str(j).unwrap_err());
    }

    #[test]
    fn defmacro_with_bad_ellipsis() {
        let j = "(defmacro a (macro-rules #{} [[(a ...) false]]))";
        let t = "                                  ^^^           ";

        let err = Error::new(
            t2s(t),
            ErrorKind::IllegalArg(
                "ellipsis can only be used as part of a zero or more match".to_owned(),
            ),
        );
        assert_eq!(err, body_expr_for_str(j).unwrap_err());
    }

    #[test]
    fn expand_macro_without_matching_rule() {
        let j1 = "(defmacro one (macro-rules #{} [[(one) 1]]))";
        let t1 = "                                            ";
        let j2 = "(one extra-arg)";
        let t2 = "^^^^^^^^^^^^^^^";

        let j = &[j1, j2].join("");
        let t = &[t1, t2].join("");

        let err = Error::new(t2s(t), ErrorKind::NoMacroRule);
        assert_eq!(err, body_expr_for_str(j).unwrap_err());
    }

    #[test]
    fn expand_trivial_macro() {
        let j1 = "(defmacro one (macro-rules #{} [[(one) 1]]))";
        let t1 = "                                       ^    ";
        let j2 = "(one)";
        let t2 = "     ";

        let j = &[j1, j2].join("");
        let t = &[t1, t2].join("");

        let expected = Expr::Lit(Datum::Int(t2s(t), 1));
        assert_eq!(expected, body_expr_for_str(j).unwrap());
    }

    #[test]
    fn expand_replacing_macro() {
        let j1 = "(defmacro identity (macro-rules #{} [[(identity x) x]]))";
        let t1 = "                                                        ";
        let j2 = "(identity 1)";
        let t2 = "          ^ ";

        let j = &[j1, j2].join("");
        let t = &[t1, t2].join("");

        let expected = Expr::Lit(Datum::Int(t2s(t), 1));
        assert_eq!(expected, body_expr_for_str(j).unwrap());
    }

    #[test]
    fn expand_two_value_replacement() {
        let j = "(defmacro ret-two (macro-rules #{} [[(ret-two x y) [x y]]])) (ret-two 1 2)";
        let t = "                                                   ^^^^^                  ";
        let u = "                                                                      ^   ";
        let v = "                                                                        ^ ";

        let expected = Expr::Lit(Datum::Vec(
            t2s(t),
            vec![Datum::Int(t2s(u), 1), Datum::Int(t2s(v), 2)],
        ));
        assert_eq!(expected, body_expr_for_str(j).unwrap());
    }

    #[test]
    fn expand_with_matching_literals() {
        let j = "(defmacro for (macro-rules #{in} [[(for x in y) [x y]]])) (for 1 in 2)";
        let t = "                                                ^^^^^                 ";
        let u = "                                                               ^      ";
        let v = "                                                                    ^ ";

        let expected = Expr::Lit(Datum::Vec(
            t2s(t),
            vec![Datum::Int(t2s(u), 1), Datum::Int(t2s(v), 2)],
        ));
        assert_eq!(expected, body_expr_for_str(j).unwrap());
    }

    #[test]
    fn expand_with_escaped_ellipsis() {
        let j1 = "(defmacro m (macro-rules #{} [[(m) '(... ...)]]))";
        let t1 = "                                         ^^^     ";
        let j2 = "(m)";

        let j = &[j1, j2].join("");
        let t = t1;

        let expected = Expr::Lit(Datum::Sym(t2s(t), "...".to_owned()));
        assert_eq!(expected, body_expr_for_str(j).unwrap());
    }

    #[test]
    fn expand_with_literal_underscore() {
        let j1 = "(defmacro m (macro-rules #{_} [[(m _) 1][(m a) a]]))";
        let sp = "                                                    ";
        let j2 = "(m 2)";
        let t2 = "   ^ ";

        let j = &[j1, j2].join("");
        let t = &[sp, t2].join("");

        let expected = Expr::Lit(Datum::Int(t2s(t), 2));
        assert_eq!(expected, body_expr_for_str(j).unwrap());
    }

    #[test]
    fn expand_with_non_matching_literals() {
        let j1 = "(defmacro for (macro-rules #{in} [[(for x in y) [x y]]]))";
        let t1 = "                                                         ";
        let j2 = "(for 1 foo 2)";
        let t2 = "^^^^^^^^^^^^^";

        let j = &[j1, j2].join("");
        let t = &[t1, t2].join("");

        let err = Error::new(t2s(t), ErrorKind::NoMacroRule);
        assert_eq!(err, body_expr_for_str(j).unwrap_err());
    }

    #[test]
    fn expand_with_wildcard() {
        let j1 = "(defmacro third (macro-rules #{} [[(third _ _ x) x]]))";
        let t1 = "                                                      ";
        let j2 = "(third 1 2 3)";
        let t2 = "           ^ ";

        let j = &[j1, j2].join("");
        let t = &[t1, t2].join("");

        let expected = Expr::Lit(Datum::Int(t2s(t), 3));
        assert_eq!(expected, body_expr_for_str(j).unwrap());
    }

    #[test]
    fn expand_recursive() {
        let j1 = "(defmacro rec (macro-rules #{} [[(rec) 7] [(rec _) (rec)]]))";
        let t1 = "                                       ^                    ";
        let j2 = "(rec)";
        let t2 = "     ";

        let j = &[j1, j2].join("");
        let t = &[t1, t2].join("");

        let expected = Expr::Lit(Datum::Int(t2s(t), 7));
        assert_eq!(expected, body_expr_for_str(j).unwrap());
    }

    #[test]
    fn expand_fixed_list_match() {
        let j1 = "(defmacro ret-second (macro-rules #{} [[(ret-second (_ second _)) second]]))";
        let t1 = "                                                                            ";
        let j2 = "(ret-second (1 2 3))";
        let t2 = "               ^    ";

        let j = &[j1, j2].join("");
        let t = &[t1, t2].join("");

        let expected = Expr::Lit(Datum::Int(t2s(t), 2));
        assert_eq!(expected, body_expr_for_str(j).unwrap());
    }

    #[test]
    fn expand_fixed_vector_match() {
        let j1 = "(defmacro ret-third (macro-rules #{} [[(ret-third [_ _ third]) third]]))";
        let t1 = "                                                                        ";
        let j2 = "(ret-third [1 2 3])";
        let t2 = "                ^  ";

        let j = &[j1, j2].join("");
        let t = &[t1, t2].join("");

        let expected = Expr::Lit(Datum::Int(t2s(t), 3));
        assert_eq!(expected, body_expr_for_str(j).unwrap());
    }

    #[test]
    fn expand_empty_set_match() {
        let j1 = "(defmacro empty-set? (macro-rules #{} [[(empty-set? #{}) true]]))";
        let t1 = "                                                         ^^^^    ";
        let j2 = "(empty-set? #{})";

        let j = &[j1, j2].join("");
        let t = t1;

        let expected = Expr::Lit(Datum::Bool(t2s(t), true));
        assert_eq!(expected, body_expr_for_str(j).unwrap());
    }

    #[test]
    fn expand_zero_or_more_set_match() {
        let j1 = "(defmacro set->list (macro-rules #{} [[(set->list #{v ...}) '(v ...)]]))";
        let t1 = "                                                             ^^^^^^^    ";
        let sp = "                                                                        ";
        let j2 = "(set->list #{1 2 3})";
        let u2 = "             ^      ";
        let v2 = "               ^    ";
        let w2 = "                 ^  ";

        let j = &[j1, j2].join("");
        let t = t1;
        let u = &[sp, u2].join("");
        let v = &[sp, v2].join("");
        let w = &[sp, w2].join("");

        let expected = Expr::Lit(Datum::List(
            t2s(t),
            vec![
                Datum::Int(t2s(u), 1),
                Datum::Int(t2s(v), 2),
                Datum::Int(t2s(w), 3),
            ],
        ));
        assert_eq!(expected, body_expr_for_str(j).unwrap());
    }

    #[test]
    fn expand_fixed_set_match() {
        let j = "(defmacro two-set? (macro-rules #{} [[(two-set? #{_ _}) false]]))";
        let t = "                                                ^^^^^^           ";

        let err = Error::new(
            t2s(t),
            ErrorKind::IllegalArg(
                "set patterns must either be empty or a zero or more match".to_owned(),
            ),
        );
        assert_eq!(err, body_expr_for_str(j).unwrap_err());
    }

    #[test]
    fn expand_constant_match() {
        let j1 = "(defmacro alph (macro-rules #{} [[(alph 1) 'a] [(alph 2) 'b] [(alph 3) 'c]]))";
        let t1 = "                                                          ^                  ";
        let j2 = "(alph 2)";
        let t2 = "        ";

        let j = &[j1, j2].join("");
        let t = &[t1, t2].join("");

        let expected = Expr::Lit(Datum::Sym(t2s(t), "b".to_owned()));
        assert_eq!(expected, body_expr_for_str(j).unwrap());
    }

    #[test]
    fn expand_terminal_zero_or_more_match() {
        let j1 =
            "(defmacro return-all (macro-rules #{} [[(return-all values ...) '(values ...)]]))";
        let t1 =
            "                                                                 ^^^^^^^^^^^^    ";
        let sp =
            "                                                                                 ";
        let j2 = "(return-all 1 2 3)";
        let u2 = "            ^     ";
        let v2 = "              ^   ";
        let w2 = "                ^ ";

        let j = &[j1, j2].join("");
        let t = t1;
        let u = &[sp, u2].join("");
        let v = &[sp, v2].join("");
        let w = &[sp, w2].join("");

        let expected = Expr::Lit(Datum::List(
            t2s(t),
            vec![
                Datum::Int(t2s(u), 1),
                Datum::Int(t2s(v), 2),
                Datum::Int(t2s(w), 3),
            ],
        ));
        assert_eq!(expected, body_expr_for_str(j).unwrap());
    }

    #[test]
    fn expand_middle_zero_or_more_match() {
        let j1 = "(defmacro mid (macro-rules #{} [[(mid [_ vals ... _]) [true vals ... false]]]))";
        let t1 = "                                                      ^^^^^^^^^^^^^^^^^^^^^    ";
        let u1 = "                                                       ^^^^                    ";
        let v1 = "                                                                     ^^^^^     ";
        let sp = "                                                                               ";
        let j2 = "(mid [1 2 3 4])";
        let w2 = "        ^      ";
        let x2 = "          ^    ";

        let j = &[j1, j2].join("");
        let t = t1;
        let u = u1;
        let v = v1;
        let w = &[sp, w2].join("");
        let x = &[sp, x2].join("");

        let expected = Expr::Lit(Datum::Vec(
            t2s(t),
            vec![
                Datum::Bool(t2s(u), true),
                Datum::Int(t2s(w), 2),
                Datum::Int(t2s(x), 3),
                Datum::Bool(t2s(v), false),
            ],
        ));
        assert_eq!(expected, body_expr_for_str(j).unwrap());
    }

    #[test]
    fn expand_multiple_zero_or_more() {
        let j1 = "(defmacro vm (macro-rules #{} [[(vm (l ...) (r ...)) [r ... l ...]]]))";
        let t1 = "                                                     ^^^^^^^^^^^^^    ";
        let sp = "                                                                      ";
        let j2 = "(vm (1 2) (3 4))";
        let u2 = "     ^          ";
        let v2 = "       ^        ";
        let w2 = "           ^    ";
        let x2 = "             ^  ";

        let j = &[j1, j2].join("");
        let t = t1;
        let u = &[sp, u2].join("");
        let v = &[sp, v2].join("");
        let w = &[sp, w2].join("");
        let x = &[sp, x2].join("");

        let expected = Expr::Lit(Datum::Vec(
            t2s(t),
            vec![
                Datum::Int(t2s(w), 3),
                Datum::Int(t2s(x), 4),
                Datum::Int(t2s(u), 1),
                Datum::Int(t2s(v), 2),
            ],
        ));
        assert_eq!(expected, body_expr_for_str(j).unwrap());
    }

    #[test]
    fn expand_multiple_zero_or_more_in_same_pattern_seq() {
        let j = "(defmacro vm (macro-rules #{} [[(vm (l ... r ...)) true]]))";
        let t = "                                     ^                     ";
        let u = "                                           ^               ";

        let err = Error::new(t2s(u), ErrorKind::MultipleZeroOrMoreMatch(t2s(t)));
        assert_eq!(err, body_expr_for_str(j).unwrap_err());
    }

    #[test]
    fn expand_subtemplate_without_matching_subpattern() {
        let j1 = "(defmacro m (macro-rules #{} [[(m expr ...) (5 ...)]]))";
        let t1 = "                                            ^^^^^^^    ";
        let j2 = "(m 1 2 3 4)";
        let t2 = "           ";

        let j = &[j1, j2].join("");
        let t = &[t1, t2].join("");

        let err = Error::new(
            t2s(t),
            ErrorKind::IllegalArg("subtemplate does not include any macro variables".to_owned()),
        );
        assert_eq!(err, body_expr_for_str(j).unwrap_err());
    }

    #[test]
    fn expand_subtemplate_matching_multiple_subpatterns() {
        let j =
            "(defmacro m (macro-rules #{} [[(m (list1 ...) (list2 ...)) ([list1 list2] ...)]]))";
        let t =
            "                                                           ^^^^^^^^^^^^^^^^^^^    ";

        let err = Error::new(
            t2s(t),
            ErrorKind::IllegalArg(
                "subtemplate references macro variables from multiple subpatterns".to_owned(),
            ),
        );
        assert_eq!(err, body_expr_for_str(j).unwrap_err());
    }

    #[test]
    fn expand_nested_subpatterns() {
        let j1 = "(defmacro m (macro-rules #{} [[(m (a b rest ...) ...) [(rest ... b a) ...]]]))";
        let t1 = "                                                      ^^^^^^^^^^^^^^^^^^^^    ";
        let u1 = "                                                       ^^^^^^^^^^^^^^         ";
        let sp = "                                                                              ";
        let j2 = "(m (1 2 3 4) (5 6))";
        let v2 = "    ^              ";
        let w2 = "      ^            ";
        let x2 = "        ^          ";
        let y2 = "          ^        ";
        let z2 = "              ^    ";
        let a2 = "                ^  ";

        let j = &[j1, j2].join("");
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
            vec![
                Datum::List(
                    t2s(u),
                    vec![
                        Datum::Int(t2s(x), 3),
                        Datum::Int(t2s(y), 4),
                        Datum::Int(t2s(w), 2),
                        Datum::Int(t2s(v), 1),
                    ],
                ),
                Datum::List(t2s(u), vec![Datum::Int(t2s(a), 6), Datum::Int(t2s(z), 5)]),
            ],
        ));
        assert_eq!(expected, body_expr_for_str(j).unwrap());
    }

    #[test]
    fn expand_body_def() {
        let j1 = "(defmacro def1 (macro-rules #{} [[(def1 name) (def name 1)]]))";
        let u1 = "                                              ^^^^^^^^^^^^    ";
        let v1 = "                                                        ^     ";
        let s1 = "                                                              ";

        let j2 = "(def1 x)";
        let t2 = "      ^ ";
        let s2 = "        ";

        let j3 = "x";
        let w3 = "^";

        let j = &[j1, j2, j3].join("");
        let t = &[s1, t2].join("");
        let u = u1;
        let v = v1;
        let w = &[s1, s2, w3].join("");

        let destruc = destruc::Destruc::Scalar(
            t2s(t),
            destruc::Scalar::new(
                Some(VarId(2)),
                "x".to_owned(),
                ty::Decl::Free(ty::FreeTyId::new(1)),
            ),
        );

        let expected = Expr::Do(vec![
            Expr::Def(t2s(u), destruc, Box::new(Expr::Lit(Datum::Int(t2s(v), 1)))),
            Expr::Ref(t2s(w), VarId(2)),
        ]);
        assert_eq!(expected, body_expr_for_str(j).unwrap());
    }

    #[test]
    fn trivial_deftype() {
        let j1 = "(deftype MyTrue true)";
        let s1 = "                     ";

        let j2 = "(def [x : MyTrue] true)";
        let t2 = "     ^^^^^^^^^^^^      ";
        let u2 = "^^^^^^^^^^^^^^^^^^^^^^^";
        let v2 = "                  ^^^^ ";

        let j = &[j1, j2].join("");
        let t = &[s1, t2].join("");
        let u = &[s1, u2].join("");
        let v = &[s1, v2].join("");

        let destruc = destruc::Destruc::Scalar(
            t2s(t),
            destruc::Scalar::new(
                Some(VarId(2)),
                "x".to_owned(),
                ty::Ty::LitBool(true).into_decl(),
            ),
        );

        let expected = Expr::Def(
            t2s(u),
            destruc,
            Box::new(Expr::Lit(Datum::Bool(t2s(v), true))),
        );

        assert_eq!(expected, body_expr_for_str(j).unwrap());
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
        let j1 = "(def x y)";
        let j2 = "(def y x)";

        let j = &[j1, j2].join("");

        let module = module_for_str(j).unwrap();
        assert_eq!(2, module.into_defs().len());
    }

    #[test]
    fn type_predicate() {
        let j = "(type-predicate true)";
        let t = "^^^^^^^^^^^^^^^^^^^^^";

        let expected = Expr::TyPred(t2s(t), ty::Ty::LitBool(true).into_poly());
        assert_eq!(expected, body_expr_for_str(j).unwrap());
    }

    #[test]
    fn module_user_compile_error() {
        let j = r#"(compile-error "Hello")"#;
        let t = r#"^^^^^^^^^^^^^^^^^^^^^^^"#;

        let err = Error::new(t2s(t), ErrorKind::UserError("Hello".to_owned()));
        assert_eq!(err, module_for_str(j).unwrap_err());
    }

    #[test]
    fn body_expr_user_compile_error() {
        let j = r#"(compile-error "Hello")"#;
        let t = r#"^^^^^^^^^^^^^^^^^^^^^^^"#;

        let err = Error::new(t2s(t), ErrorKind::UserError("Hello".to_owned()));
        assert_eq!(err, body_expr_for_str(j).unwrap_err());
    }
}
