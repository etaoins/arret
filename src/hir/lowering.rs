use std::collections::{BTreeMap, HashMap};
use std::io::Read;

use hir::{Cond, Expr, Fun, Var, VarId};
use hir::loader::{load_library_data, load_module_data, LibraryName};
use hir::scope::{Binding, Ident, NsId, NsValue, Primitive, Scope};
use hir::module::Module;
use hir::macros::{lower_macro_rules, Macro};
use hir::error::{Error, Result};
use syntax::value::Value;
use syntax::span::Span;
use ctx::CompileContext;

pub struct LoweringContext<'ccx> {
    curr_var_id: usize,
    curr_ns_id: usize,
    loaded_libraries: BTreeMap<LibraryName, Module>,
    ccx: &'ccx mut CompileContext,
}

macro_rules! lower_expr_impl {
    ($self:ident, $scope:ident, $datum:ident, $lower_primitive_apply:ident, $lower_macro_apply:ident) => {
        match $datum {
            NsValue::Ident(span, ref ident) => match $scope.get(ident) {
                Some(Binding::Var(id)) => Ok(Expr::Ref(span, id)),
                Some(Binding::Primitive(_)) => Err(Error::PrimitiveRef(span, ident.name().clone())),
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
                        Some(Binding::Primitive(ref fn_prim)) => {
                            $self.$lower_primitive_apply($scope, span, fn_prim, arg_data)
                        }
                        Some(Binding::Macro(ref mac)) => {
                            $self.$lower_macro_apply($scope, span, mac, arg_data)
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
            Module::primitives_module(),
        );

        LoweringContext {
            curr_var_id: 0,
            curr_ns_id: 0,
            loaded_libraries,
            ccx,
        }
    }

    fn alloc_var_id(&mut self) -> usize {
        self.curr_var_id = self.curr_var_id + 1;
        self.curr_var_id
    }

    fn alloc_ns_id(&mut self) -> usize {
        self.curr_ns_id = self.curr_ns_id + 1;
        self.curr_ns_id
    }

    fn lower_defmacro(
        &mut self,
        scope: &mut Scope,
        span: Span,
        sym_datum: NsValue,
        transformer_spec: NsValue,
    ) -> Result<()> {
        let self_ident = match sym_datum {
            NsValue::Ident(_, ident) => ident.clone(),
            other => {
                return Err(Error::ExpectedSymbol(other.span()));
            }
        };

        let macro_rules_data = match transformer_spec {
            NsValue::List(span, ref vs) => match vs.first() {
                Some(&NsValue::Ident(_, ref ident)) if ident.name() == "macro-rules" => {
                    let (_, rest) = vs.split_first().unwrap();
                    rest
                }
                _ => return Err(Error::IllegalArg(span, "Unsupported macro type".to_owned())),
            },
            other => {
                return Err(Error::IllegalArg(
                    other.span(),
                    "Macro specification must be a list".to_owned(),
                ))
            }
        };

        let mac = lower_macro_rules(span, self_ident.clone(), macro_rules_data)?;
        scope.insert_binding(self_ident, Binding::Macro(Box::new(mac)));

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

        let var_id = VarId::new(self.alloc_var_id());
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

                let var_id = VarId::new(self.alloc_var_id());
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

    fn lower_primitive_apply(
        &mut self,
        scope: &Scope,
        span: Span,
        fn_prim: &Primitive,
        mut arg_data: Vec<NsValue>,
    ) -> Result<Expr> {
        match fn_prim {
            &Primitive::Def | &Primitive::DefMacro | &Primitive::Import => {
                Err(Error::DefOutsideBody(span))
            }
            &Primitive::Export => Err(Error::ExportOutsideModule(span)),
            &Primitive::Quote => match arg_data.len() {
                1 => Ok(Expr::Lit(arg_data[0].clone().into_value())),
                _ => Err(Error::WrongArgCount(span, 1)),
            },
            &Primitive::Fun => self.lower_fun(scope, span, arg_data),
            &Primitive::If => {
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

    fn lower_body_primitive_apply(
        &mut self,
        scope: &mut Scope,
        span: Span,
        fn_prim: &Primitive,
        mut arg_data: Vec<NsValue>,
    ) -> Result<Expr> {
        match fn_prim {
            &Primitive::Def => {
                let arg_count = arg_data.len();

                if arg_count != 2 {
                    return Err(Error::WrongArgCount(span, 2));
                }

                let value_datum = arg_data.pop().unwrap();
                let sym_datum = arg_data.pop().unwrap();

                self.lower_def_var(scope, span, sym_datum, value_datum)
            }
            &Primitive::DefMacro => {
                let arg_count = arg_data.len();

                if arg_count != 2 {
                    return Err(Error::WrongArgCount(span, 2));
                }

                let transformer_spec = arg_data.pop().unwrap();
                let sym_datum = arg_data.pop().unwrap();

                self.lower_defmacro(scope, span, sym_datum, transformer_spec)
                    .map(|_| Expr::Do(vec![]))
            }
            &Primitive::Import => {
                self.lower_import(scope, arg_data)?;
                Ok(Expr::Do(vec![]))
            }
            _ => self.lower_primitive_apply(scope, span, fn_prim, arg_data),
        }
    }

    fn lower_module_primitive_apply(
        &mut self,
        scope: &mut Scope,
        span: Span,
        fn_prim: &Primitive,
        arg_data: Vec<NsValue>,
    ) -> Result<Expr> {
        match fn_prim {
            &Primitive::Export => {
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
            _ => self.lower_body_primitive_apply(scope, span, fn_prim, arg_data),
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

    fn lower_macro_apply(
        &mut self,
        scope: &Scope,
        span: Span,
        mac: &Macro,
        arg_data: Vec<NsValue>,
    ) -> Result<Expr> {
        unimplemented!("HERE")
    }

    fn lower_expr(&mut self, scope: &Scope, datum: NsValue) -> Result<Expr> {
        lower_expr_impl!(self, scope, datum, lower_primitive_apply, lower_macro_apply)
    }

    fn lower_body_expr(&mut self, scope: &mut Scope, datum: NsValue) -> Result<Expr> {
        lower_expr_impl!(
            self,
            scope,
            datum,
            lower_body_primitive_apply,
            lower_macro_apply
        )
    }

    fn lower_module_expr(&mut self, scope: &mut Scope, datum: NsValue) -> Result<Expr> {
        lower_expr_impl!(
            self,
            scope,
            datum,
            lower_module_primitive_apply,
            lower_macro_apply
        )
    }

    fn lower_module(&mut self, data: Vec<Value>) -> Result<Module> {
        let ns_id = NsId::new(self.alloc_ns_id());
        let mut scope = Scope::new_empty();

        // The default scope only consists of (import)
        scope.insert_binding(
            Ident::new(ns_id, "import".to_owned()),
            Binding::Primitive(Primitive::Import),
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
use syntax::span::t2s;
#[cfg(test)]
use syntax::parser::data_from_str;
#[cfg(test)]
use syntax::span::EMPTY_SPAN;

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
fn reference_primitive() {
    let j = "def";
    let t = "^^^";

    let err = Error::PrimitiveRef(t2s(t), "def".to_owned());
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
