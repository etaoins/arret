use std::collections::{BTreeMap, HashMap};
use std::io::Read;

use hir::{Cond, Expr, Fun, Var, VarId};
use hir::loader::{load_library_data, load_module_data, LibraryName};
use hir::scope::{Binding, Ident, MacroId, NsId, NsIdAlloc, NsValue, Prim, Scope};
use hir::module::Module;
use hir::macros::{expand_macro, lower_macro_rules, Macro};
use hir::error::{Error, Result};
use syntax::value::Value;
use syntax::span::Span;
use ctx::CompileContext;

pub struct LoweringContext<'ccx> {
    curr_var_id: usize,
    ns_id_alloc: NsIdAlloc,
    loaded_libraries: BTreeMap<LibraryName, Module>,
    macros: Vec<Macro>,
    ccx: &'ccx mut CompileContext,
}

// TODO: Change this to define_lower_expr_impl() and remove $lower_expr?
macro_rules! lower_expr_impl {
    ($self:ident, $scope:ident, $datum:ident, $lower_prim_apply:ident, $lower_expr:ident) => {
        match $datum {
            NsValue::Ident(span, ref ident) => match $scope.get(ident) {
                Some(Binding::Var(id)) => Ok(Expr::Ref(span, id)),
                Some(Binding::Prim(_)) => Err(Error::PrimRef(span)),
                Some(Binding::Macro(_)) => Err(Error::MacroRef(span, ident.name().clone())),
                None => Err(Error::UnboundSymbol(span, ident.name().clone())),
            },
            NsValue::List(span, mut vs) => {
                if vs.len() == 0 {
                    return Ok(Expr::Lit(Value::List(span, vec![])));
                }

                let arg_data = vs.split_off(1);
                let fn_datum = vs.pop().unwrap();

                match fn_datum {
                    NsValue::Ident(fn_span, ref ident) => match $scope.get(ident) {
                        Some(Binding::Prim(ref fn_prim)) => {
                            $self.$lower_prim_apply($scope, span, fn_prim, arg_data)
                        }
                        Some(Binding::Macro(macro_id)) => {
                            let (mut expanded_scope, expanded_datum) = {
                                let mac = &$self.macros[macro_id.to_usize()];
                                expand_macro(&mut $self.ns_id_alloc, $scope, span, mac, arg_data)?
                            };

                            // This will recurse
                            $self.$lower_expr(&mut expanded_scope, expanded_datum)
                        }
                        Some(Binding::Var(id)) => {
                            $self.lower_expr_apply($scope, span, Expr::Ref(span, id), arg_data)
                        }
                        None => Err(Error::UnboundSymbol(fn_span, ident.name().clone())),
                    },
                    _ => {
                        let fn_expr = $self.lower_expr($scope, fn_datum)?;
                        $self.lower_expr_apply($scope, span, fn_expr, arg_data)
                    }
                }
            },
            other => Ok(Expr::Lit(other.into_value())),
        }
    }
}

impl<'ccx> LoweringContext<'ccx> {
    pub fn new(ccx: &'ccx mut CompileContext) -> LoweringContext {
        let mut loaded_libraries = BTreeMap::new();

        // This library is always loaded
        loaded_libraries.insert(
            LibraryName::new(
                vec!["risp".to_owned(), "internal".to_owned()],
                "primitives".to_owned(),
            ),
            Module::prims_module(),
        );

        LoweringContext {
            curr_var_id: 0,
            ns_id_alloc: NsIdAlloc::new(),
            loaded_libraries,
            macros: vec![],
            ccx,
        }
    }

    fn alloc_var_id(&mut self) -> VarId {
        self.curr_var_id = self.curr_var_id + 1;
        VarId::new(self.curr_var_id)
    }

    fn lower_defmacro(
        &mut self,
        scope: &mut Scope,
        span: Span,
        sym_datum: NsValue,
        transformer_spec: NsValue,
    ) -> Result<()> {
        let self_ident = if let NsValue::Ident(_, ident) = sym_datum {
            ident
        } else {
            return Err(Error::ExpectedSymbol(sym_datum.span()));
        };

        let macro_rules_data = if let NsValue::List(span, mut vs) = transformer_spec {
            match vs.first() {
                Some(&NsValue::Ident(_, ref ident))
                    if scope.get(ident) == Some(Binding::Prim(Prim::MacroRules)) => {}
                _ => return Err(Error::IllegalArg(span, "Unsupported macro type".to_owned())),
            }

            vs.remove(0);
            vs
        } else {
            return Err(Error::IllegalArg(
                transformer_spec.span(),
                "Macro specification must be a list".to_owned(),
            ));
        };

        let mac = lower_macro_rules(scope, span, &self_ident, macro_rules_data)?;

        let macro_id = MacroId::new(self.macros.len());
        self.macros.push(mac);
        scope.insert_binding(self_ident, Binding::Macro(macro_id));

        Ok(())
    }

    fn lower_def_var(
        &mut self,
        scope: &mut Scope,
        span: Span,
        sym_datum: NsValue,
        value_datum: NsValue,
    ) -> Result<Expr> {
        let sym_ident = match sym_datum {
            NsValue::Ident(_, ident) => ident.clone(),
            other => {
                return Err(Error::ExpectedSymbol(other.span()));
            }
        };

        let var_id = self.alloc_var_id();
        let sym_name = sym_ident.name().clone();
        let value_expr = self.lower_expr(scope, value_datum)?;

        scope.insert_var(sym_ident, var_id);

        Ok(Expr::Def(
            span,
            Var {
                id: var_id,
                source_name: sym_name,
                bound: None,
            },
            Box::new(value_expr),
        ))
    }

    fn lower_fun(&mut self, scope: &Scope, span: Span, mut arg_data: Vec<NsValue>) -> Result<Expr> {
        if arg_data.len() < 1 {
            return Err(Error::IllegalArg(
                span,
                "parameter declaration missing".to_owned(),
            ));
        }

        // Body starts after the parameter declaration
        let body_data = arg_data.split_off(1);

        let param_data = match arg_data.pop().unwrap() {
            NsValue::Vector(_, vs) => vs,
            other => {
                return Err(Error::IllegalArg(
                    other.span().clone(),
                    "parameter declaration should be a vector".to_owned(),
                ));
            }
        };

        // Pull our our params
        let mut body_scope = Scope::new_child(scope);

        let fixed_params = param_data
            .into_iter()
            .map(|param_datum| {
                let ident = match param_datum {
                    NsValue::Ident(_, ident) => ident,
                    other => {
                        return Err(Error::IllegalArg(
                            other.span().clone(),
                            "unsupported binding type".to_owned(),
                        ));
                    }
                };

                let var_id = self.alloc_var_id();
                let var = Var {
                    id: var_id,
                    source_name: ident.name().clone(),
                    bound: None,
                };

                body_scope.insert_var(ident, var_id);
                Ok(var)
            })
            .collect::<Result<Vec<Var>>>()?;

        let body_exprs = body_data
            .into_iter()
            .map(|body_datum| self.lower_body_expr(&mut body_scope, body_datum))
            .collect::<Result<Vec<Expr>>>()?;

        Ok(Expr::Fun(
            span,
            Fun {
                source_name: None,
                ty: None,
                fixed_params,
                rest_param: None,
                body_expr: Box::new(Expr::from_vec(body_exprs)),
            },
        ))
    }

    fn lower_prim_apply(
        &mut self,
        scope: &Scope,
        span: Span,
        fn_prim: &Prim,
        mut arg_data: Vec<NsValue>,
    ) -> Result<Expr> {
        match fn_prim {
            &Prim::Def | &Prim::DefMacro | &Prim::Import => Err(Error::DefOutsideBody(span)),
            &Prim::Export => Err(Error::ExportOutsideModule(span)),
            &Prim::Quote => match arg_data.len() {
                1 => Ok(Expr::Lit(arg_data[0].clone().into_value())),
                _ => Err(Error::WrongArgCount(span, 1)),
            },
            &Prim::Fun => self.lower_fun(scope, span, arg_data),
            &Prim::If => {
                let arg_count = arg_data.len();

                if arg_count != 3 {
                    return Err(Error::WrongArgCount(span, 3));
                }

                macro_rules! pop_as_boxed_expr {
                    () => {Box::new(self.lower_expr(scope, arg_data.pop().unwrap())?)}
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
            &Prim::Ellipsis | &Prim::Wildcard | &Prim::MacroRules => Err(Error::PrimRef(span)),
        }
    }

    fn load_library(&mut self, span: Span, library_name: LibraryName) -> Result<&Module> {
        // TODO: This does a lot of hash lookups
        if !self.loaded_libraries.contains_key(&library_name) {
            let library_data = load_library_data(self.ccx, span, &library_name)?;
            let loaded_library = self.lower_module(library_data)?;

            self.loaded_libraries
                .insert(library_name.clone(), loaded_library);
        }

        Ok(self.loaded_libraries.get(&library_name).unwrap())
    }

    fn lower_import_set(&mut self, scope: &mut Scope, import_set_datum: NsValue) -> Result<()> {
        match import_set_datum {
            NsValue::Vector(span, vs) => {
                if vs.len() < 1 {
                    return Err(Error::IllegalArg(
                        span,
                        "Library name requires a least one element".to_owned(),
                    ));
                }

                let mut import_ns_id = NsId::new(0);

                let mut name_components = vs.into_iter()
                    .map(|datum| {
                        match datum {
                            NsValue::Ident(_, ident) => {
                                // TODO: What happens with mixed namespaces?
                                import_ns_id = ident.ns_id();
                                Ok(ident.name().clone())
                            }
                            other => Err(Error::IllegalArg(
                                other.span(),
                                "Library name component must be a symbol".to_owned(),
                            )),
                        }
                    })
                    .collect::<Result<Vec<String>>>()?;

                let terminal_name = name_components.pop().unwrap();
                let library_name = LibraryName::new(name_components, terminal_name);
                let loaded_library = self.load_library(span, library_name)?;

                for (name, binding) in loaded_library.exports() {
                    let imported_ident = Ident::new(import_ns_id, name.clone());
                    scope.insert_binding(imported_ident, binding.clone());
                }

                Ok(())
            }
            other => Err(Error::IllegalArg(
                other.span(),
                "import set must be a vector".to_owned(),
            )),
        }
    }

    fn lower_import(&mut self, scope: &mut Scope, arg_data: Vec<NsValue>) -> Result<()> {
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
        mut arg_data: Vec<NsValue>,
    ) -> Result<Expr> {
        match fn_prim {
            &Prim::Def => {
                let arg_count = arg_data.len();

                if arg_count != 2 {
                    return Err(Error::WrongArgCount(span, 2));
                }

                let value_datum = arg_data.pop().unwrap();
                let sym_datum = arg_data.pop().unwrap();

                self.lower_def_var(scope, span, sym_datum, value_datum)
            }
            &Prim::DefMacro => {
                let arg_count = arg_data.len();

                if arg_count != 2 {
                    return Err(Error::WrongArgCount(span, 2));
                }

                let transformer_spec = arg_data.pop().unwrap();
                let sym_datum = arg_data.pop().unwrap();

                self.lower_defmacro(scope, span, sym_datum, transformer_spec)
                    .map(|_| Expr::Do(vec![]))
            }
            &Prim::Import => {
                self.lower_import(scope, arg_data)?;
                Ok(Expr::Do(vec![]))
            }
            _ => self.lower_prim_apply(scope, span, fn_prim, arg_data),
        }
    }

    fn lower_module_prim_apply(
        &mut self,
        scope: &mut Scope,
        span: Span,
        fn_prim: &Prim,
        arg_data: Vec<NsValue>,
    ) -> Result<Expr> {
        match fn_prim {
            &Prim::Export => {
                for arg_datum in arg_data {
                    match arg_datum {
                        NsValue::Ident(span, ident) => {
                            scope.insert_export(span, ident);
                        }
                        other => {
                            return Err(Error::ExpectedSymbol(other.span()));
                        }
                    };
                }

                Ok(Expr::from_vec(vec![]))
            }
            _ => self.lower_body_prim_apply(scope, span, fn_prim, arg_data),
        }
    }

    fn lower_expr_apply(
        &mut self,
        scope: &Scope,
        span: Span,
        fn_expr: Expr,
        arg_data: Vec<NsValue>,
    ) -> Result<Expr> {
        let arg_exprs = arg_data
            .into_iter()
            .map(|arg_datum| self.lower_expr(scope, arg_datum))
            .collect::<Result<Vec<Expr>>>()?;

        Ok(Expr::App(span, Box::new(fn_expr), arg_exprs))
    }

    fn lower_expr(&mut self, scope: &Scope, datum: NsValue) -> Result<Expr> {
        lower_expr_impl!(self, scope, datum, lower_prim_apply, lower_expr)
    }

    fn lower_body_expr(&mut self, scope: &mut Scope, datum: NsValue) -> Result<Expr> {
        lower_expr_impl!(self, scope, datum, lower_body_prim_apply, lower_body_expr)
    }

    fn lower_module_expr(&mut self, scope: &mut Scope, datum: NsValue) -> Result<Expr> {
        lower_expr_impl!(
            self,
            scope,
            datum,
            lower_module_prim_apply,
            lower_module_expr
        )
    }

    fn lower_module(&mut self, data: Vec<Value>) -> Result<Module> {
        let ns_id = self.ns_id_alloc.alloc();
        let mut scope = Scope::new_empty();

        // The default scope only consists of (import)
        scope.insert_binding(
            Ident::new(ns_id, "import".to_owned()),
            Binding::Prim(Prim::Import),
        );

        let exprs = data.into_iter()
            .map(|datum| {
                let ns_datum = NsValue::from_value(datum, ns_id);
                self.lower_module_expr(&mut scope, ns_datum)
            })
            .collect::<Result<Vec<Expr>>>()?;

        let body_expr = Expr::from_vec(exprs);

        let mut exports = HashMap::new();
        for (ident, span) in scope.exports() {
            let binding = scope
                .get(ident)
                .ok_or_else(|| Error::UnboundSymbol(*span, ident.name().clone()))?;

            exports.insert(ident.name().clone(), binding);
        }

        Ok(Module::new(body_expr, exports))
    }

    pub fn lower_program(&mut self, display_name: String, input_reader: &mut Read) -> Result<Expr> {
        let data = load_module_data(self.ccx, display_name, input_reader)?;
        self.lower_module(data)
            .map(|module| module.into_body_expr())
    }
}

////

#[cfg(test)]
use syntax::span::{t2s, EMPTY_SPAN};
#[cfg(test)]
use syntax::parser::data_from_str;

#[cfg(test)]
fn module_for_str(data_str: &str) -> Result<Module> {
    let import_statement = Value::List(
        EMPTY_SPAN,
        vec![
            Value::Symbol(EMPTY_SPAN, "import".to_owned()),
            Value::Vector(
                EMPTY_SPAN,
                vec![
                    Value::Symbol(EMPTY_SPAN, "risp".to_owned()),
                    Value::Symbol(EMPTY_SPAN, "internal".to_owned()),
                    Value::Symbol(EMPTY_SPAN, "primitives".to_owned()),
                ],
            ),
        ],
    );

    let mut test_data = data_from_str(data_str).unwrap();
    test_data.insert(0, import_statement);

    let mut ccx = CompileContext::new();
    let mut lcx = LoweringContext::new(&mut ccx);
    lcx.lower_module(test_data)
}

#[cfg(test)]
fn body_expr_for_str(data_str: &str) -> Result<Expr> {
    module_for_str(data_str).map(|module| module.into_body_expr())
}

#[test]
fn self_quoting_bool() {
    let j = "false";
    let t = "^^^^^";

    let expected = Expr::Lit(Value::Bool(t2s(t), false));
    assert_eq!(expected, body_expr_for_str(j).unwrap());
}

#[test]
fn self_quoting_empty_list() {
    let j = "()";
    let t = "^^";

    let expected = Expr::Lit(Value::List(t2s(t), vec![]));
    assert_eq!(expected, body_expr_for_str(j).unwrap());
}

#[test]
fn quoted_datum_shorthand() {
    let j = "'foo";
    let t = " ^^^";

    let expected = Expr::Lit(Value::Symbol(t2s(t), "foo".to_owned()));
    assert_eq!(expected, body_expr_for_str(j).unwrap());
}

#[test]
fn quoted_datum_explicit() {
    let j = "(quote foo)";
    let t = "       ^^^ ";

    let expected = Expr::Lit(Value::Symbol(t2s(t), "foo".to_owned()));
    assert_eq!(expected, body_expr_for_str(j).unwrap());
}

#[test]
fn quoted_multiple_data() {
    let j = "(quote 1 2 3)";
    let t = "^^^^^^^^^^^^^";

    let err = Error::WrongArgCount(t2s(t), 1);
    assert_eq!(err, body_expr_for_str(j).unwrap_err());
}

#[test]
fn basic_untyped_def() {
    let j = "(def x 1) x";
    let t = "^^^^^^^^^  ";
    let u = "       ^   ";
    let v = "          ^";

    let expected = Expr::Do(vec![
        Expr::Def(
            t2s(t),
            Var {
                id: VarId(1),
                source_name: "x".to_owned(),
                bound: None,
            },
            Box::new(Expr::Lit(Value::Int(t2s(u), 1))),
        ),
        Expr::Ref(t2s(v), VarId(1)),
    ]);

    assert_eq!(expected, body_expr_for_str(j).unwrap());
}

#[test]
fn def_of_non_symbol() {
    let j = "(def 1 1)";
    let t = "     ^   ";

    let err = Error::ExpectedSymbol(t2s(t));
    assert_eq!(err, body_expr_for_str(j).unwrap_err());
}

#[test]
fn def_in_non_body() {
    let j = "(def x (def y 1))";
    let t = "       ^^^^^^^^^ ";

    let err = Error::DefOutsideBody(t2s(t));
    assert_eq!(err, body_expr_for_str(j).unwrap_err());
}

#[test]
fn reference_prim() {
    let j = "def";
    let t = "^^^";

    let err = Error::PrimRef(t2s(t));
    assert_eq!(err, body_expr_for_str(j).unwrap_err());
}

#[test]
fn reference_unbound() {
    let j = "nopenopenope";
    let t = "^^^^^^^^^^^^";

    let err = Error::UnboundSymbol(t2s(t), "nopenopenope".to_owned());
    assert_eq!(err, body_expr_for_str(j).unwrap_err());
}

#[test]
fn fn_without_param_decl() {
    let j = "(fn)";
    let t = "^^^^";

    let err = Error::IllegalArg(t2s(t), "parameter declaration missing".to_owned());
    assert_eq!(err, body_expr_for_str(j).unwrap_err());
}

#[test]
fn fn_with_non_vector_param_decl() {
    let j = "(fn ())";
    let t = "    ^^ ";

    let err = Error::IllegalArg(
        t2s(t),
        "parameter declaration should be a vector".to_owned(),
    );

    assert_eq!(err, body_expr_for_str(j).unwrap_err());
}

#[test]
fn fn_with_non_symbol_param() {
    let j = "(fn [()])";
    let t = "     ^^  ";

    let err = Error::IllegalArg(t2s(t), "unsupported binding type".to_owned());
    assert_eq!(err, body_expr_for_str(j).unwrap_err());
}

#[test]
fn empty_fn() {
    let j = "(fn [])";
    let t = "^^^^^^^";

    let expected = Expr::Fun(
        t2s(t),
        Fun {
            source_name: None,
            ty: None,
            fixed_params: vec![],
            rest_param: None,
            body_expr: Box::new(Expr::from_vec(vec![])),
        },
    );

    assert_eq!(expected, body_expr_for_str(j).unwrap());
}

#[test]
fn identity_fn() {
    let j = "(fn [x] x)";
    let t = "^^^^^^^^^^";
    let u = "        ^ ";

    let param_var_id = VarId::new(1);
    let param_var = Var {
        id: param_var_id,
        source_name: "x".to_owned(),
        bound: None,
    };

    let expected = Expr::Fun(
        t2s(t),
        Fun {
            source_name: None,
            ty: None,
            fixed_params: vec![param_var],
            rest_param: None,
            body_expr: Box::new(Expr::Ref(t2s(u), param_var_id)),
        },
    );

    assert_eq!(expected, body_expr_for_str(j).unwrap());
}

#[test]
fn capturing_fn() {
    let j = "(def x 1)(fn [] x)";
    let t = "^^^^^^^^^         ";
    let u = "       ^          ";
    let v = "         ^^^^^^^^^";
    let w = "                ^ ";

    let outer_var_id = VarId::new(1);
    let outer_var = Var {
        id: outer_var_id,
        source_name: "x".to_owned(),
        bound: None,
    };

    let expected = Expr::Do(vec![
        Expr::Def(
            t2s(t),
            outer_var,
            Box::new(Expr::Lit(Value::Int(t2s(u), 1))),
        ),
        Expr::Fun(
            t2s(v),
            Fun {
                source_name: None,
                ty: None,
                fixed_params: vec![],
                rest_param: None,
                body_expr: Box::new(Expr::Ref(t2s(w), outer_var_id)),
            },
        ),
    ]);

    assert_eq!(expected, body_expr_for_str(j).unwrap());
}

#[test]
fn shadowing_fn() {
    let j = "(def x 1)(fn [x] x)";
    let t = "^^^^^^^^^          ";
    let u = "       ^           ";
    let v = "         ^^^^^^^^^^";
    let w = "                 ^ ";

    let outer_var_id = VarId::new(1);
    let outer_var = Var {
        id: outer_var_id,
        source_name: "x".to_owned(),
        bound: None,
    };

    let param_var_id = VarId::new(2);
    let param_var = Var {
        id: param_var_id,
        source_name: "x".to_owned(),
        bound: None,
    };

    let expected = Expr::Do(vec![
        Expr::Def(
            t2s(t),
            outer_var,
            Box::new(Expr::Lit(Value::Int(t2s(u), 1))),
        ),
        Expr::Fun(
            t2s(v),
            Fun {
                source_name: None,
                ty: None,
                fixed_params: vec![param_var],
                rest_param: None,
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
        Box::new(Expr::Lit(Value::Int(t2s(u), 1))),
        vec![
            Expr::Lit(Value::Int(t2s(v), 2)),
            Expr::Lit(Value::Int(t2s(w), 3)),
        ],
    );

    assert_eq!(expected, body_expr_for_str(j).unwrap());
}

#[test]
fn empty_if() {
    let j = "(if)";
    let t = "^^^^";

    let err = Error::WrongArgCount(t2s(t), 3);
    assert_eq!(err, body_expr_for_str(j).unwrap_err());
}

#[test]
fn if_without_test() {
    let j = "(if true)";
    let t = "^^^^^^^^^";

    let err = Error::WrongArgCount(t2s(t), 3);
    assert_eq!(err, body_expr_for_str(j).unwrap_err());
}

#[test]
fn if_without_false_branch() {
    let j = "(if true 1)";
    let t = "^^^^^^^^^^^";

    let err = Error::WrongArgCount(t2s(t), 3);
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
            test_expr: Box::new(Expr::Lit(Value::Bool(t2s(u), true))),
            true_expr: Box::new(Expr::Lit(Value::Int(t2s(v), 1))),
            false_expr: Box::new(Expr::Lit(Value::Int(t2s(w), 2))),
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

    let expected_body_expr = Expr::Def(
        t2s(t),
        Var {
            id: var_id,
            source_name: "x".to_owned(),
            bound: None,
        },
        Box::new(Expr::Lit(Value::Int(t2s(u), 1))),
    );

    let mut expected_exports = HashMap::new();
    expected_exports.insert("x".to_owned(), Binding::Var(var_id));

    let expected = Module::new(expected_body_expr, expected_exports);
    assert_eq!(expected, module_for_str(j).unwrap());
}

#[test]
fn export_unbound() {
    let j = "(export x)";
    let t = "        ^ ";

    let err = Error::UnboundSymbol(t2s(t), "x".to_owned());
    assert_eq!(err, body_expr_for_str(j).unwrap_err());
}

#[test]
fn defmacro_of_non_symbol() {
    let j = "(defmacro 1 (macro-rules ${}))";
    let t = "          ^                   ";

    let err = Error::ExpectedSymbol(t2s(t));
    assert_eq!(err, body_expr_for_str(j).unwrap_err());
}

#[test]
fn defmacro_of_non_list() {
    let j = "(defmacro a b)";
    let t = "            ^ ";

    let err = Error::IllegalArg(t2s(t), "Macro specification must be a list".to_owned());
    assert_eq!(err, body_expr_for_str(j).unwrap_err());
}

#[test]
fn defmacro_of_unsupported_type() {
    let j = "(defmacro a (macro-fn ${}))";
    let t = "            ^^^^^^^^^^^^^^ ";

    let err = Error::IllegalArg(t2s(t), "Unsupported macro type".to_owned());
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

    let err = Error::NoMacroRule(t2s(t));
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

    let expected = Expr::Lit(Value::Int(t2s(t), 1));
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

    let expected = Expr::Lit(Value::Int(t2s(t), 1));
    assert_eq!(expected, body_expr_for_str(j).unwrap());
}

#[test]
fn expand_two_value_replacement() {
    let j = "(defmacro ret-two (macro-rules #{} [[(ret-two x y) [x y]]])) (ret-two 1 2)";
    let t = "                                                   ^^^^^                  ";
    let u = "                                                                      ^   ";
    let v = "                                                                        ^ ";

    let expected = Expr::Lit(Value::Vector(
        t2s(t),
        vec![Value::Int(t2s(u), 1), Value::Int(t2s(v), 2)],
    ));
    assert_eq!(expected, body_expr_for_str(j).unwrap());
}

#[test]
fn expand_with_matching_literals() {
    let j = "(defmacro for (macro-rules #{in} [[(for x in y) [x y]]])) (for 1 in 2)";
    let t = "                                                ^^^^^                 ";
    let u = "                                                               ^      ";
    let v = "                                                                    ^ ";

    let expected = Expr::Lit(Value::Vector(
        t2s(t),
        vec![Value::Int(t2s(u), 1), Value::Int(t2s(v), 2)],
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

    let err = Error::NoMacroRule(t2s(t));
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

    let expected = Expr::Lit(Value::Int(t2s(t), 3));
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

    let expected = Expr::Lit(Value::Int(t2s(t), 7));
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

    let expected = Expr::Lit(Value::Int(t2s(t), 2));
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

    let expected = Expr::Lit(Value::Int(t2s(t), 3));
    assert_eq!(expected, body_expr_for_str(j).unwrap());
}

#[test]
fn expand_constant_match() {
    let j1 = "(defmacro alph (macro-rules #{} [[(alph 1) 'a] [(alph 2) 'b] [(alph 3) 'c]]))";
    let t1 = "                                                          ^                  ";
    let j2 = "(alph 2)";
    let t2 = "        ";

    let j = &[j1, j2].join("");
    let t = &[t1, t2].join("");

    let expected = Expr::Lit(Value::Symbol(t2s(t), "b".to_owned()));
    assert_eq!(expected, body_expr_for_str(j).unwrap());
}

#[test]
fn expand_terminal_zero_or_more_match() {
    let j1 = "(defmacro return-all (macro-rules #{} [[(return-all values ...) '(values ...)]]))";
    let t1 = "                                                                 ^^^^^^^^^^^^    ";
    let sp = "                                                                                 ";
    let j2 = "(return-all 1 2 3)";
    let u2 = "            ^     ";
    let v2 = "              ^   ";
    let w2 = "                ^ ";

    let j = &[j1, j2].join("");
    let t = t1;
    let u = &[sp, u2].join("");
    let v = &[sp, v2].join("");
    let w = &[sp, w2].join("");

    let expected = Expr::Lit(Value::List(
        t2s(t),
        vec![
            Value::Int(t2s(u), 1),
            Value::Int(t2s(v), 2),
            Value::Int(t2s(w), 3),
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

    let expected = Expr::Lit(Value::Vector(
        t2s(t),
        vec![
            Value::Bool(t2s(u), true),
            Value::Int(t2s(w), 2),
            Value::Int(t2s(x), 3),
            Value::Bool(t2s(v), false),
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

    let expected = Expr::Lit(Value::Vector(
        t2s(t),
        vec![
            Value::Int(t2s(w), 3),
            Value::Int(t2s(x), 4),
            Value::Int(t2s(u), 1),
            Value::Int(t2s(v), 2),
        ],
    ));
    assert_eq!(expected, body_expr_for_str(j).unwrap());
}

#[test]
fn expand_subtemplate_without_matching_subpattern() {
    let j1 = "(defmacro m (macro-rules #{} [[(m expr ...) (5 ...)]]))";
    let t1 = "                                            ^^^^^^^    ";
    let j2 = "(m 1 2 3 4)";
    let t2 = "           ";

    let j = &[j1, j2].join("");
    let t = &[t1, t2].join("");

    let err = Error::IllegalArg(
        t2s(t),
        "Subtemplate does not include any macro variables".to_owned(),
    );
    assert_eq!(err, body_expr_for_str(j).unwrap_err());
}

#[test]
fn expand_subtemplate_matching_multiple_subpatterns() {
    let j = "(defmacro m (macro-rules #{} [[(m (list1 ...) (list2 ...)) ([list1 list2] ...)]]))";
    let t = "                                                           ^^^^^^^^^^^^^^^^^^^    ";

    let err = Error::IllegalArg(
        t2s(t),
        "Subtemplate references macro variables from multiple subpatterns".to_owned(),
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

    let expected = Expr::Lit(Value::Vector(
        t2s(t),
        vec![
            Value::List(
                t2s(u),
                vec![
                    Value::Int(t2s(x), 3),
                    Value::Int(t2s(y), 4),
                    Value::Int(t2s(w), 2),
                    Value::Int(t2s(v), 1),
                ],
            ),
            Value::List(t2s(u), vec![Value::Int(t2s(a), 6), Value::Int(t2s(z), 5)]),
        ],
    ));
    assert_eq!(expected, body_expr_for_str(j).unwrap());
}
