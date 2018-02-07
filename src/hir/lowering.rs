use hir::{Expr, Var, VarId};
use hir::scope::{insert_primitive_bindings, Binding, NsId, NsValue, Primitive, Scope};
use hir::error::Error;
use syntax::value::Value;
use syntax::span::Span;

pub struct LoweringContext {
    curr_inst_id: usize,
}

impl LoweringContext {
    pub fn new() -> LoweringContext {
        LoweringContext { curr_inst_id: 0 }
    }

    fn alloc_inst_id(&mut self) -> usize {
        self.curr_inst_id = self.curr_inst_id + 1;
        self.curr_inst_id
    }

    fn lower_def(
        &mut self,
        scope: &mut Scope,
        span: Span,
        sym_datum: NsValue,
        value_datum: NsValue,
    ) -> Result<Expr, Error> {
        let sym_ident = match sym_datum {
            NsValue::Symbol(_, ident) => ident.clone(),
            other => {
                return Err(Error::ExpectedDefSymbol(*other.span()));
            }
        };

        let var_id = VarId::new(self.alloc_inst_id());
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

    fn lower_primitive_apply(
        &mut self,
        scope: &mut Scope,
        span: Span,
        fn_prim: &Primitive,
        mut arg_data: Vec<NsValue>,
    ) -> Result<Expr, Error> {
        match fn_prim {
            &Primitive::Quote => match arg_data.len() {
                1 => Ok(Expr::Lit(arg_data[0].clone().into_value())),
                other => Err(Error::WrongArgCount(span, other)),
            },
            &Primitive::Do => {
                let mut arg_exprs: Vec<Expr> = vec![];

                for arg_datum in arg_data {
                    arg_exprs.push(self.lower_expr(scope, arg_datum)?);
                }

                Ok(Expr::Do(span, arg_exprs))
            }
            &Primitive::Def => {
                let arg_count = arg_data.len();

                if arg_count != 2 {
                    return Err(Error::WrongArgCount(span, arg_count));
                }

                let value_datum = arg_data.pop().unwrap();
                let sym_datum = arg_data.pop().unwrap();

                self.lower_def(scope, span, sym_datum, value_datum)
            }
            _ => {
                unimplemented!("foo");
            }
        }
    }

    fn lower_expr_apply(
        &mut self,
        scope: &mut Scope,
        span: Span,
        fn_expr: Expr,
        arg_data: Vec<NsValue>,
    ) -> Result<Expr, Error> {
        let mut arg_exprs: Vec<Expr> = vec![];

        for arg_datum in arg_data {
            arg_exprs.push(self.lower_expr(scope, arg_datum)?);
        }

        Ok(Expr::App(span, Box::new(fn_expr), arg_exprs))
    }

    fn lower_apply(
        &mut self,
        scope: &mut Scope,
        span: Span,
        fn_datum: NsValue,
        arg_data: Vec<NsValue>,
    ) -> Result<Expr, Error> {
        match fn_datum {
            NsValue::Symbol(fn_span, ref ident) => match scope.get(ident) {
                Some(Binding::Primitive(ref fn_prim)) => {
                    self.lower_primitive_apply(scope, span, fn_prim, arg_data)
                }
                Some(Binding::Var(id)) => {
                    self.lower_expr_apply(scope, span, Expr::Ref(span, id), arg_data)
                }
                None => Err(Error::UnboundSymbol(fn_span, ident.name().clone())),
            },
            _ => {
                let fn_expr = self.lower_expr(scope, fn_datum)?;
                self.lower_expr_apply(scope, span, fn_expr, arg_data)
            }
        }
    }

    fn lower_expr(&mut self, scope: &mut Scope, datum: NsValue) -> Result<Expr, Error> {
        match datum {
            NsValue::Symbol(span, ref ident) => match scope.get(ident) {
                Some(Binding::Var(id)) => Ok(Expr::Ref(span, id)),
                Some(Binding::Primitive(_)) => Err(Error::PrimitiveRef(span, ident.name().clone())),
                None => Err(Error::UnboundSymbol(span, ident.name().clone())),
            },
            NsValue::List(span, mut vs) => if vs.len() == 0 {
                Ok(Expr::Lit(Value::List(span, vec![])))
            } else {
                let arg_data = vs.split_off(1);
                self.lower_apply(scope, span, vs.pop().unwrap(), arg_data)
            },
            lit @ NsValue::Bool(_, _) => Ok(Expr::Lit(lit.into_value())),
            lit @ NsValue::Int(_, _) => Ok(Expr::Lit(lit.into_value())),
            lit @ NsValue::String(_, _) => Ok(Expr::Lit(lit.into_value())),
            _ => unimplemented!("Unsupported datum"),
        }
    }

    pub fn lower_module(&mut self, data: Vec<Value>) -> Result<Expr, Error> {
        let ns_id = NsId::new(0);
        let mut scope = Scope::new();
        insert_primitive_bindings(&mut scope, ns_id);

        let mut exprs = Vec::<Expr>::new();

        for datum in data {
            let ns_datum = NsValue::from_value(datum, ns_id);
            exprs.push(self.lower_expr(&mut scope, ns_datum)?);
        }

        Ok(Expr::from_vec(exprs))
    }
}

////

#[cfg(test)]
use syntax::span::{t2s, EMPTY_SPAN};
#[cfg(test)]
use syntax::parser::data_from_str;

#[test]
fn self_quoting_bool() {
    let j = "false";
    let t = "^^^^^";

    let expected = Expr::Lit(Value::Bool(t2s(t), false));
    let data = data_from_str(j).unwrap();

    let mut lcx = LoweringContext::new();
    assert_eq!(expected, lcx.lower_module(data).unwrap());
}

#[test]
fn self_quoting_empty_list() {
    let j = "()";
    let t = "^^";

    let expected = Expr::Lit(Value::List(t2s(t), vec![]));
    let data = data_from_str(j).unwrap();

    let mut lcx = LoweringContext::new();
    assert_eq!(expected, lcx.lower_module(data).unwrap());
}

#[test]
fn quoted_datum_shorthand() {
    let j = "'foo";
    let t = " ^^^";

    let expected = Expr::Lit(Value::Symbol(t2s(t), "foo".to_owned()));
    let data = data_from_str(j).unwrap();

    let mut lcx = LoweringContext::new();
    assert_eq!(expected, lcx.lower_module(data).unwrap());
}

#[test]
fn quoted_datum_explicit() {
    let j = "(quote foo)";
    let t = "       ^^^ ";

    let expected = Expr::Lit(Value::Symbol(t2s(t), "foo".to_owned()));
    let data = data_from_str(j).unwrap();

    let mut lcx = LoweringContext::new();
    assert_eq!(expected, lcx.lower_module(data).unwrap());
}

#[test]
fn quoted_multiple_data() {
    let j = "(quote 1 2 3)";
    let t = "^^^^^^^^^^^^^";

    let err = Error::WrongArgCount(t2s(t), 3);
    let data = data_from_str(j).unwrap();

    let mut lcx = LoweringContext::new();
    assert_eq!(err, lcx.lower_module(data).unwrap_err());
}

#[test]
fn do_expr() {
    let j = "(do 1 2 3)";
    let t = "^^^^^^^^^^";
    let u = "    ^     ";
    let v = "      ^   ";
    let w = "        ^ ";

    let expected = Expr::Do(
        t2s(t),
        vec![
            Expr::Lit(Value::Int(t2s(u), 1)),
            Expr::Lit(Value::Int(t2s(v), 2)),
            Expr::Lit(Value::Int(t2s(w), 3)),
        ],
    );

    let data = data_from_str(j).unwrap();

    let mut lcx = LoweringContext::new();
    assert_eq!(expected, lcx.lower_module(data).unwrap());
}

#[test]
fn basic_untyped_def() {
    let j = "(def x 1) x";
    let t = "^^^^^^^^^  ";
    let u = "       ^   ";
    let v = "          ^";

    let expected = Expr::Do(
        EMPTY_SPAN,
        vec![
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
        ],
    );

    let data = data_from_str(j).unwrap();

    let mut lcx = LoweringContext::new();
    assert_eq!(expected, lcx.lower_module(data).unwrap());
}

#[test]
fn def_of_non_symbol() {
    let j = "(def 1 1)";
    let t = "     ^   ";

    let err = Error::ExpectedDefSymbol(t2s(t));
    let data = data_from_str(j).unwrap();

    let mut lcx = LoweringContext::new();
    assert_eq!(err, lcx.lower_module(data).unwrap_err());
}

#[test]
fn reference_primitive() {
    let j = "def";
    let t = "^^^";

    let err = Error::PrimitiveRef(t2s(t), "def".to_owned());
    let data = data_from_str(j).unwrap();

    let mut lcx = LoweringContext::new();
    assert_eq!(err, lcx.lower_module(data).unwrap_err());
}

#[test]
fn reference_unbound() {
    let j = "nopenopenope";
    let t = "^^^^^^^^^^^^";

    let err = Error::UnboundSymbol(t2s(t), "nopenopenope".to_owned());
    let data = data_from_str(j).unwrap();

    let mut lcx = LoweringContext::new();
    assert_eq!(err, lcx.lower_module(data).unwrap_err());
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

    let data = data_from_str(j).unwrap();

    let mut lcx = LoweringContext::new();
    assert_eq!(expected, lcx.lower_module(data).unwrap());
}
