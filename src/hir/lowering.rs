use std::collections::{BTreeMap, HashMap};
use std::io::Read;

use hir::{Cond, Destruc, Expr, Fun, Var, VarId};
use hir::loader::{load_library_data, load_module_data, LibraryName};
use hir::scope::{Binding, MacroId, Scope};
use hir::ns::{Ident, NsDatum, NsId, NsIdAlloc};
use hir::prim::Prim;
use hir::module::{Module, ModuleDef};
use hir::macros::{expand_macro, lower_macro_rules, Macro};
use hir::error::{Error, ErrorKind, Result};
use hir::types::{lower_pty, lower_pvar, TyCons};
use hir::util::{expect_arg_count, expect_ident, pop_vec_front, split_into_fixed_and_rest};
use ty;
use syntax::datum::Datum;
use syntax::span::{Span, EMPTY_SPAN};
use ctx::CompileContext;

pub struct LoweringContext<'ccx> {
    curr_var_id: usize,
    ns_id_alloc: NsIdAlloc,
    loaded_libraries: BTreeMap<LibraryName, Module>,
    macros: Vec<Macro>,
    pvars: Vec<ty::PVar>,
    ccx: &'ccx mut CompileContext,
}

enum DeferredModulePrim {
    Def(Span, Destruc, NsDatum),
    Export(Span, Ident),
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
            ccx,
        }
    }

    fn insert_pvar(&mut self, pvar: ty::PVar) -> ty::PVarId {
        let pvar_id = ty::PVarId::new(self.pvars.len());
        self.pvars.push(pvar);

        pvar_id
    }

    fn alloc_var_id(&mut self) -> VarId {
        self.curr_var_id = self.curr_var_id + 1;
        VarId::new(self.curr_var_id)
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

        let ty = lower_pty(scope, ty_datum)?;

        scope.insert_binding(ident, Binding::Ty(ty));
        Ok(())
    }

    fn lower_destruc(&mut self, scope: &mut Scope, destruc_datum: NsDatum) -> Result<Destruc> {
        match destruc_datum {
            NsDatum::Ident(span, ident) => {
                match scope.get(&ident) {
                    Some(Binding::Prim(Prim::Wildcard)) => {
                        return Ok(Destruc::Wildcard(None));
                    }
                    Some(Binding::Prim(Prim::Ellipsis)) => {
                        return Err(Error::new(
                            span,
                            ErrorKind::IllegalArg(
                                "ellipsis can only be used to destructure the rest of a list"
                                    .to_owned(),
                            ),
                        ));
                    }
                    _ => {}
                }

                let var_id = self.alloc_var_id();
                let source_name = ident.name().clone();

                scope.insert_var(ident, var_id);

                Ok(Destruc::Var(Var {
                    id: var_id,
                    source_name,
                    bound: None,
                }))
            }
            NsDatum::List(_, vs) => {
                let (fixed, rest) = split_into_fixed_and_rest(scope, vs);

                let rest_destruc = match rest {
                    Some(rest) => Some(Box::new(self.lower_destruc(scope, rest)?)),
                    None => None,
                };

                let fixed_destrucs = fixed
                    .into_iter()
                    .map(|v| self.lower_destruc(scope, v))
                    .collect::<Result<Vec<Destruc>>>()?;

                Ok(Destruc::List(fixed_destrucs, rest_destruc))
            }
            NsDatum::Vec(span, mut vs) => {
                if vs.len() != 3 {
                    return Err(Error::new(span, ErrorKind::NoVecDestruc));
                }

                // Make sure the middle element is a type colon
                if scope.get_datum(&vs[1]) != Some(Binding::Prim(Prim::TyColon)) {
                    return Err(Error::new(span, ErrorKind::NoVecDestruc));
                }

                let ty = lower_pty(scope, vs.pop().unwrap())?;

                // Discard the type colon
                vs.pop();

                let inner_destruc_datum = vs.pop().unwrap();
                let inner_destruc_span = inner_destruc_datum.span();
                let inner_destruc = self.lower_destruc(scope, inner_destruc_datum)?;

                match inner_destruc {
                    Destruc::Var(var) => Ok(Destruc::Var(var.with_bound(ty))),
                    _ => Err(Error::new(
                        inner_destruc_span,
                        ErrorKind::IllegalArg(
                            "only variables can have type ascriptions".to_owned(),
                        ),
                    )),
                }
            }
            _ => Err(Error::new(
                destruc_datum.span(),
                ErrorKind::IllegalArg(
                    "values can only be bound to variables or destructured into lists".to_owned(),
                ),
            )),
        }
    }

    fn lower_fun(&mut self, scope: &Scope, span: Span, arg_data: Vec<NsDatum>) -> Result<Expr> {
        if arg_data.is_empty() {
            return Err(Error::new(
                span,
                ErrorKind::IllegalArg("parameter declaration missing".to_owned()),
            ));
        }

        let mut fun_scope = Scope::new_child(scope);

        let (mut next_datum, mut rest_data) = pop_vec_front(arg_data);

        // We can either begin with a set of polymorphic variables or a list of parameters
        let pvar_ids = if let NsDatum::Set(_, vs) = next_datum {
            let pvars = vs.into_iter()
                .map(|pvar_datum| lower_pvar(scope, pvar_datum))
                .collect::<Result<Vec<(Ident, ty::PVar)>>>()?;

            let pvar_ids = pvars
                .into_iter()
                .map(|(ident, pvar)| {
                    let pvar_id = self.insert_pvar(pvar);
                    fun_scope.insert_binding(ident, Binding::Ty(ty::Poly::Var(pvar_id)));
                    pvar_id
                })
                .collect::<Vec<ty::PVarId>>();

            if rest_data.is_empty() {
                return Err(Error::new(
                    span,
                    ErrorKind::IllegalArg(
                        "polymorphic variables should be followed by parameters".to_owned(),
                    ),
                ));
            }

            let (new_next_datum, new_rest_data) = pop_vec_front(rest_data);
            next_datum = new_next_datum;
            rest_data = new_rest_data;

            pvar_ids
        } else {
            vec![]
        };

        // TODO: It would be consistent to also allow assigning the entire argument list to a
        // symbol. This would be the same as using a list with a single rest parameter. It could
        // potentially be confusing/ambiguous to allow both so require a list for now.
        if let NsDatum::List(_, _) = next_datum {
        } else {
            return Err(Error::new(
                next_datum.span(),
                ErrorKind::IllegalArg("parameter declaration should be a list".to_owned()),
            ));
        }

        // Pull out our params
        let params = self.lower_destruc(&mut fun_scope, next_datum)?;

        // Determine if we have a return type after the parameters, eg (param) -> RetTy
        let ret_ty;
        let body_data;
        if rest_data.len() >= 2
            && scope.get_datum(&rest_data[0]) == Some(Binding::TyCons(TyCons::Fun))
        {
            body_data = rest_data.split_off(2);
            ret_ty = Some(lower_pty(&fun_scope, rest_data.pop().unwrap())?)
        } else {
            body_data = rest_data;
            ret_ty = None;
        };

        // Extract the body
        let body_exprs = body_data
            .into_iter()
            .map(|body_datum| self.lower_body_expr(&mut fun_scope, body_datum))
            .collect::<Result<Vec<Expr>>>()?;

        Ok(Expr::Fun(
            span,
            Fun {
                pvar_ids,
                params,
                ret_ty,
                body_expr: Box::new(Expr::from_vec(body_exprs)),
            },
        ))
    }

    fn lower_inner_prim_apply(
        &mut self,
        scope: &mut Scope,
        span: Span,
        fn_prim: &Prim,
        mut arg_data: Vec<NsDatum>,
    ) -> Result<Expr> {
        match fn_prim {
            &Prim::Def | &Prim::DefMacro | &Prim::DefType | &Prim::Import => {
                Err(Error::new(span, ErrorKind::DefOutsideBody))
            }
            &Prim::Export => Err(Error::new(span, ErrorKind::ExportOutsideModule)),
            &Prim::Quote => {
                expect_arg_count(span, &arg_data, 1)?;
                Ok(Expr::Lit(arg_data.pop().unwrap().into_syntax_datum()))
            }
            &Prim::Fun => self.lower_fun(scope, span, arg_data),
            &Prim::If => {
                expect_arg_count(span, &arg_data, 3)?;

                macro_rules! pop_as_boxed_expr {
                    () => {Box::new(self.lower_inner_expr(scope, arg_data.pop().unwrap())?)}
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
            &Prim::TyPred => {
                expect_arg_count(span, &arg_data, 1)?;
                Ok(Expr::TyPred(
                    span,
                    lower_pty(scope, arg_data.pop().unwrap())?,
                ))
            }
            &Prim::Ellipsis | &Prim::Wildcard | &Prim::MacroRules | &Prim::TyColon => {
                Err(Error::new(span, ErrorKind::PrimRef))
            }
        }
    }

    fn load_library(
        &mut self,
        scope: &mut Scope,
        span: Span,
        library_name: LibraryName,
    ) -> Result<&Module> {
        // TODO: This does a lot of hash lookups
        if !self.loaded_libraries.contains_key(&library_name) {
            let library_data = load_library_data(self.ccx, span, &library_name)?;
            let loaded_library = self.lower_module(scope, library_data)?;

            self.loaded_libraries
                .insert(library_name.clone(), loaded_library);
        }

        Ok(self.loaded_libraries.get(&library_name).unwrap())
    }

    fn lower_import_set(&mut self, scope: &mut Scope, import_set_datum: NsDatum) -> Result<()> {
        match import_set_datum {
            NsDatum::Vec(span, vs) => {
                if vs.len() < 1 {
                    return Err(Error::new(
                        span,
                        ErrorKind::IllegalArg(
                            "library name requires a least one element".to_owned(),
                        ),
                    ));
                }

                let mut import_ns_id = NsId::new(0);

                let mut name_components = vs.into_iter()
                    .map(|datum| {
                        match datum {
                            NsDatum::Ident(_, ident) => {
                                // TODO: What happens with mixed namespaces?
                                import_ns_id = ident.ns_id();
                                Ok(ident.name().clone())
                            }
                            other => Err(Error::new(
                                other.span(),
                                ErrorKind::IllegalArg(
                                    "library name component must be a symbol".to_owned(),
                                ),
                            )),
                        }
                    })
                    .collect::<Result<Vec<String>>>()?;

                let terminal_name = name_components.pop().unwrap();
                let library_name = LibraryName::new(name_components, terminal_name);
                let loaded_library = self.load_library(scope, span, library_name)?;

                for (name, binding) in loaded_library.exports() {
                    let imported_ident = Ident::new(import_ns_id, name.clone());
                    scope.insert_binding(imported_ident, binding.clone());
                }

                Ok(())
            }
            other => Err(Error::new(
                other.span(),
                ErrorKind::IllegalArg("import set must be a vector".to_owned()),
            )),
        }
    }

    fn lower_import(&mut self, scope: &mut Scope, arg_data: Vec<NsDatum>) -> Result<()> {
        for arg_datum in arg_data {
            self.lower_import_set(scope, arg_datum)?;
        }

        Ok(())
    }

    fn lower_body_prim_apply(
        &mut self,
        scope: &mut Scope,
        span: Span,
        fn_prim: &Prim,
        mut arg_data: Vec<NsDatum>,
    ) -> Result<Expr> {
        match fn_prim {
            &Prim::Def => {
                expect_arg_count(span, &arg_data, 2)?;

                let value_datum = arg_data.pop().unwrap();
                let destruc_datum = arg_data.pop().unwrap();

                let value_expr = self.lower_inner_expr(scope, value_datum)?;
                let destruc = self.lower_destruc(scope, destruc_datum)?;

                Ok(Expr::Def(span, destruc, Box::new(value_expr)))
            }
            &Prim::DefMacro => self.lower_defmacro(scope, span, arg_data)
                .map(|_| Expr::Do(vec![])),
            &Prim::DefType => self.lower_deftype(scope, span, arg_data)
                .map(|_| Expr::Do(vec![])),
            &Prim::Import => self.lower_import(scope, arg_data).map(|_| Expr::Do(vec![])),
            _ => self.lower_inner_prim_apply(scope, span, fn_prim, arg_data),
        }
    }

    fn lower_expr_apply(
        &mut self,
        scope: &mut Scope,
        span: Span,
        fn_expr: Expr,
        arg_data: Vec<NsDatum>,
    ) -> Result<Expr> {
        let arg_exprs = arg_data
            .into_iter()
            .map(|arg_datum| self.lower_inner_expr(scope, arg_datum))
            .collect::<Result<Vec<Expr>>>()?;

        Ok(Expr::App(span, Box::new(fn_expr), arg_exprs))
    }

    fn generic_lower_expr<F>(
        &mut self,
        scope: &mut Scope,
        datum: NsDatum,
        lower_prim_apply: F,
    ) -> Result<Expr>
    where
        F: Fn(&mut Self, &mut Scope, Span, &Prim, Vec<NsDatum>) -> Result<Expr>,
    {
        match datum {
            NsDatum::Ident(span, ref ident) => match scope.get(ident) {
                Some(Binding::Var(id)) => Ok(Expr::Ref(span, id)),
                Some(Binding::Prim(_)) => Err(Error::new(span, ErrorKind::PrimRef)),
                Some(Binding::Ty(_)) | Some(Binding::TyCons(_)) => {
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
                if vs.len() == 0 {
                    return Ok(Expr::Lit(Datum::List(span, vec![])));
                }

                let arg_data = vs.split_off(1);
                let fn_datum = vs.pop().unwrap();

                match fn_datum {
                    NsDatum::Ident(fn_span, ref ident) => match scope.get(ident) {
                        Some(Binding::Prim(ref fn_prim)) => {
                            lower_prim_apply(self, scope, span, fn_prim, arg_data)
                        }
                        Some(Binding::Macro(macro_id)) => {
                            let expanded_datum = {
                                let mac = &self.macros[macro_id.to_usize()];
                                expand_macro(&mut self.ns_id_alloc, scope, span, mac, arg_data)?
                            };

                            self.generic_lower_expr(scope, expanded_datum, lower_prim_apply)
                                .map_err(|e| e.with_macro_invocation_span(span))
                        }
                        Some(Binding::Var(id)) => {
                            self.lower_expr_apply(scope, span, Expr::Ref(span, id), arg_data)
                        }
                        Some(Binding::Ty(_)) | Some(Binding::TyCons(_)) => {
                            Err(Error::new(span, ErrorKind::TyRef))
                        }
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
        span: Span,
        fn_prim: &Prim,
        mut arg_data: Vec<NsDatum>,
    ) -> Result<Vec<DeferredModulePrim>> {
        match fn_prim {
            &Prim::Export => {
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
            &Prim::Def => {
                expect_arg_count(span, &arg_data, 2)?;

                let value_datum = arg_data.pop().unwrap();

                let destruc_datum = arg_data.pop().unwrap();
                let destruc = self.lower_destruc(scope, destruc_datum)?;

                Ok(vec![DeferredModulePrim::Def(span, destruc, value_datum)])
            }
            &Prim::DefMacro => self.lower_defmacro(scope, span, arg_data).map(|_| vec![]),
            &Prim::DefType => self.lower_deftype(scope, span, arg_data).map(|_| vec![]),
            &Prim::Import => self.lower_import(scope, arg_data).map(|_| vec![]),
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

                match scope.get_datum(&fn_datum) {
                    Some(Binding::Prim(ref fn_prim)) => {
                        return self.lower_module_prim_apply(scope, span, fn_prim, arg_data)
                    }
                    Some(Binding::Macro(macro_id)) => {
                        let expanded_datum = {
                            let mac = &self.macros[macro_id.to_usize()];
                            expand_macro(&mut self.ns_id_alloc, scope, span, mac, arg_data)?
                        };

                        return self.lower_module_def(scope, expanded_datum)
                            .map_err(|e| e.with_macro_invocation_span(span));
                    }
                    _ => {}
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
        for input_datum in data.into_iter() {
            let ns_datum = NsDatum::from_syntax_datum(ns_id, input_datum);
            let mut new_deferred_prims = self.lower_module_def(scope, ns_datum)?;

            deferred_prims.append(&mut new_deferred_prims);
        }

        // Process our variable definitions and exports
        let mut module_defs = Vec::<ModuleDef>::new();
        let mut exports = HashMap::<String, Binding>::new();
        for deferred_prim in deferred_prims.into_iter() {
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

    pub fn lower_program(
        &mut self,
        display_name: String,
        input_reader: &mut Read,
    ) -> Result<Module> {
        let mut root_scope = Scope::new_empty();

        let data = load_module_data(self.ccx, EMPTY_SPAN, display_name, input_reader)?;
        self.lower_module(&mut root_scope, data)
    }
}

////

#[cfg(test)]
mod test {
    use super::*;
    use syntax::span::t2s;
    use syntax::parser::data_from_str;
    use hir::ty;

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
        let t = "^^^^^^^^^  ";
        let u = "       ^   ";
        let v = "          ^";

        let destruc = Destruc::Var(Var {
            id: VarId(2),
            source_name: "x".to_owned(),
            bound: None,
        });

        let expected = Expr::Do(vec![
            Expr::Def(t2s(t), destruc, Box::new(Expr::Lit(Datum::Int(t2s(u), 1)))),
            Expr::Ref(t2s(v), VarId(2)),
        ]);

        assert_eq!(expected, body_expr_for_str(j).unwrap());
    }

    #[test]
    fn basic_typed_var_def() {
        let j = "(def [x : true] true) x";
        let t = "^^^^^^^^^^^^^^^^^^^^^  ";
        let u = "                ^^^^   ";
        let v = "                      ^";

        let destruc = Destruc::Var(Var {
            id: VarId(2),
            source_name: "x".to_owned(),
            bound: Some(ty::NonFun::Bool(true).into()),
        });

        let expected = Expr::Do(vec![
            Expr::Def(
                t2s(t),
                destruc,
                Box::new(Expr::Lit(Datum::Bool(t2s(u), true))),
            ),
            Expr::Ref(t2s(v), VarId(2)),
        ]);

        assert_eq!(expected, body_expr_for_str(j).unwrap());
    }

    #[test]
    fn wildcard_def() {
        let j = "(def _ 1)";
        let t = "^^^^^^^^^";
        let u = "       ^ ";

        let destruc = Destruc::Wildcard(None);

        let expected = Expr::Def(t2s(t), destruc, Box::new(Expr::Lit(Datum::Int(t2s(u), 1))));
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
        let t = "^^^^^^^^^^^^^^^^^^^^^^^  ";
        let u = "                   ^^^   ";
        let v = "                    ^    ";
        let w = "                        ^";

        let destruc = Destruc::List(
            vec![
                Destruc::Var(Var {
                    id: VarId(3),
                    source_name: "x".to_owned(),
                    bound: None,
                }),
            ],
            Some(Box::new(Destruc::Var(Var {
                id: VarId(2),
                source_name: "rest".to_owned(),
                bound: None,
            }))),
        );

        let expected = Expr::Do(vec![
            Expr::Def(
                t2s(t),
                destruc,
                Box::new(Expr::Lit(Datum::List(t2s(u), vec![Datum::Int(t2s(v), 1)]))),
            ),
            Expr::Ref(t2s(w), VarId(3)),
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
    fn fn_with_non_list_param_decl() {
        let j = "(fn [])";
        let t = "    ^^ ";

        let err = Error::new(
            t2s(t),
            ErrorKind::IllegalArg("parameter declaration should be a list".to_owned()),
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
                pvar_ids: vec![],
                params: Destruc::List(vec![], None),
                ret_ty: None,
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
                pvar_ids: vec![],
                params: Destruc::List(vec![], None),
                ret_ty: Some(ty::NonFun::Int.into()),
                body_expr: Box::new(Expr::Lit(Datum::Int(t2s(u), 1))),
            },
        );

        assert_eq!(expected, body_expr_for_str(j).unwrap());
    }

    #[test]
    fn identity_fn() {
        let j = "(fn (x) x)";
        let t = "^^^^^^^^^^";
        let u = "        ^ ";

        let param_var_id = VarId::new(2);
        let params = Destruc::List(
            vec![
                Destruc::Var(Var {
                    id: param_var_id,
                    source_name: "x".to_owned(),
                    bound: None,
                }),
            ],
            None,
        );

        let expected = Expr::Fun(
            t2s(t),
            Fun {
                pvar_ids: vec![],
                params,
                ret_ty: None,
                body_expr: Box::new(Expr::Ref(t2s(u), param_var_id)),
            },
        );

        assert_eq!(expected, body_expr_for_str(j).unwrap());
    }

    #[test]
    fn poly_identity_fn() {
        let j = "(fn #{A} ([x : A]) -> A x)";
        let t = "^^^^^^^^^^^^^^^^^^^^^^^^^^";
        let u = "                        ^ ";

        let pvar_id = ty::PVarId::new(0);

        let param_var_id = VarId::new(2);
        let params = Destruc::List(
            vec![
                Destruc::Var(Var {
                    id: param_var_id,
                    source_name: "x".to_owned(),
                    bound: Some(ty::Poly::Var(pvar_id)),
                }),
            ],
            None,
        );

        let expected = Expr::Fun(
            t2s(t),
            Fun {
                pvar_ids: vec![pvar_id],
                params,
                ret_ty: Some(ty::Poly::Var(pvar_id)),
                body_expr: Box::new(Expr::Ref(t2s(u), param_var_id)),
            },
        );

        assert_eq!(expected, body_expr_for_str(j).unwrap());
    }

    #[test]
    fn capturing_fn() {
        let j = "(def x 1)(fn () x)";
        let t = "^^^^^^^^^         ";
        let u = "       ^          ";
        let v = "         ^^^^^^^^^";
        let w = "                ^ ";

        let outer_var_id = VarId::new(2);
        let outer_destruc = Destruc::Var(Var {
            id: outer_var_id,
            source_name: "x".to_owned(),
            bound: None,
        });

        let expected = Expr::Do(vec![
            Expr::Def(
                t2s(t),
                outer_destruc,
                Box::new(Expr::Lit(Datum::Int(t2s(u), 1))),
            ),
            Expr::Fun(
                t2s(v),
                Fun {
                    pvar_ids: vec![],
                    params: Destruc::List(vec![], None),
                    ret_ty: None,
                    body_expr: Box::new(Expr::Ref(t2s(w), outer_var_id)),
                },
            ),
        ]);

        assert_eq!(expected, body_expr_for_str(j).unwrap());
    }

    #[test]
    fn shadowing_fn() {
        let j = "(def x 1)(fn (x) x)";
        let t = "^^^^^^^^^          ";
        let u = "       ^           ";
        let v = "         ^^^^^^^^^^";
        let w = "                 ^ ";

        let outer_var_id = VarId::new(2);
        let outer_destruc = Destruc::Var(Var {
            id: outer_var_id,
            source_name: "x".to_owned(),
            bound: None,
        });

        let param_var_id = VarId::new(3);
        let params = Destruc::List(
            vec![
                Destruc::Var(Var {
                    id: param_var_id,
                    source_name: "x".to_owned(),
                    bound: None,
                }),
            ],
            None,
        );

        let expected = Expr::Do(vec![
            Expr::Def(
                t2s(t),
                outer_destruc,
                Box::new(Expr::Lit(Datum::Int(t2s(u), 1))),
            ),
            Expr::Fun(
                t2s(v),
                Fun {
                    pvar_ids: vec![],
                    params,
                    ret_ty: None,
                    body_expr: Box::new(Expr::Ref(t2s(w), param_var_id)),
                },
            ),
        ]);

        assert_eq!(expected, body_expr_for_str(j).unwrap());
    }

    #[test]
    fn expr_apply() {
        let j = "(1 2 3)";
        let t = "^^^^^^^";
        let u = " ^     ";
        let v = "   ^   ";
        let w = "     ^ ";

        let expected = Expr::App(
            t2s(t),
            Box::new(Expr::Lit(Datum::Int(t2s(u), 1))),
            vec![
                Expr::Lit(Datum::Int(t2s(v), 2)),
                Expr::Lit(Datum::Int(t2s(w), 3)),
            ],
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
        let u = "       ^           ";

        let var_id = VarId(1);

        let expected_module_def = ModuleDef::new(
            t2s(t),
            Destruc::Var(Var {
                id: var_id,
                source_name: "x".to_owned(),
                bound: None,
            }),
            Expr::Lit(Datum::Int(t2s(u), 1)),
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
        let t1 = "                                              ^^^^^^^^^^^^    ";
        let u1 = "                                                        ^     ";
        let s1 = "                                                              ";
        let j2 = "(def1 x)";
        let s2 = "        ";
        let j3 = "x";
        let v3 = "^";

        let j = &[j1, j2, j3].join("");
        let t = t1;
        let u = u1;
        let v = &[s1, s2, v3].join("");

        let destruc = Destruc::Var(Var {
            id: VarId(2),
            source_name: "x".to_owned(),
            bound: None,
        });

        let expected = Expr::Do(vec![
            Expr::Def(t2s(t), destruc, Box::new(Expr::Lit(Datum::Int(t2s(u), 1)))),
            Expr::Ref(t2s(v), VarId(2)),
        ]);
        assert_eq!(expected, body_expr_for_str(j).unwrap());
    }

    #[test]
    fn trivial_deftype() {
        let j1 = "(deftype MyTrue true)";
        let s1 = "                     ";
        let j2 = "(def [x : MyTrue] true)";
        let t2 = "^^^^^^^^^^^^^^^^^^^^^^^";
        let u2 = "                  ^^^^ ";

        let j = &[j1, j2].join("");
        let t = &[s1, t2].join("");
        let u = &[s1, u2].join("");

        let destruc = Destruc::Var(Var {
            id: VarId(2),
            source_name: "x".to_owned(),
            bound: Some(ty::NonFun::Bool(true).into()),
        });

        let expected = Expr::Def(
            t2s(t),
            destruc,
            Box::new(Expr::Lit(Datum::Bool(t2s(u), true))),
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

        let expected = Expr::TyPred(t2s(t), ty::NonFun::Bool(true).into());
        assert_eq!(expected, body_expr_for_str(j).unwrap());
    }
}
