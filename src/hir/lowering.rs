use hir::{Expr, Var, VarId};
use hir::scope::{Binding, Primitive, SValue, Scope};
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
        span: Span,
        sym_datum: SValue,
        value_datum: SValue,
    ) -> Result<Expr, Error> {
        let sym_name = match sym_datum {
            SValue::Symbol(_, _, sym_name) => sym_name.clone(),
            _ => {
                return Err(Error::ExpectedDefSymbol(span));
            }
        };

        let value_expr = self.lower_expr(value_datum)?;

        Ok(Expr::Def(
            span,
            Var {
                id: VarId(self.alloc_inst_id()),
                source_name: sym_name,
                bound: None,
            },
            Box::new(value_expr),
        ))
    }

    fn lower_primitive_apply(
        &mut self,
        span: Span,
        fn_prim: &Primitive,
        mut arg_data: Vec<SValue>,
    ) -> Result<Expr, Error> {
        match fn_prim {
            &Primitive::Quote => match arg_data.len() {
                1 => Ok(Expr::Lit(arg_data[0].clone().into_value())),
                other => Err(Error::WrongArgCount(span, other)),
            },
            &Primitive::Do => {
                let mut arg_exprs: Vec<Expr> = vec![];

                for arg_datum in arg_data {
                    arg_exprs.push(self.lower_expr(arg_datum)?);
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

                self.lower_def(span, sym_datum, value_datum)
            }
            _ => {
                unimplemented!("foo");
            }
        }
    }

    fn lower_expr_apply(
        &mut self,
        span: Span,
        fn_expr: Expr,
        arg_data: Vec<SValue>,
    ) -> Result<Expr, Error> {
        let mut arg_exprs: Vec<Expr> = vec![];

        for arg_datum in arg_data {
            arg_exprs.push(self.lower_expr(arg_datum)?);
        }

        Ok(Expr::App(span, Box::new(fn_expr), arg_exprs))
    }

    fn lower_apply(
        &mut self,
        span: Span,
        fn_datum: SValue,
        arg_data: Vec<SValue>,
    ) -> Result<Expr, Error> {
        match fn_datum {
            SValue::Symbol(fn_span, scope, ident) => match scope.resolve(ident.as_ref()) {
                Some(&Binding::Primitive(ref fn_prim)) => {
                    self.lower_primitive_apply(span, fn_prim, arg_data)
                }
                Some(&Binding::Var(id)) => {
                    self.lower_expr_apply(span, Expr::Ref(span, id), arg_data)
                }
                None => Err(Error::UnboundSymbol(fn_span, ident.clone())),
            },
            _ => {
                let fn_expr = self.lower_expr(fn_datum)?;
                self.lower_expr_apply(span, fn_expr, arg_data)
            }
        }
    }

    fn lower_expr(&mut self, datum: SValue) -> Result<Expr, Error> {
        match datum {
            SValue::Symbol(span, scope, ident) => match scope.resolve(ident.as_ref()) {
                Some(&Binding::Var(id)) => Ok(Expr::Ref(span, id)),
                Some(&Binding::Primitive(_)) => Err(Error::PrimitiveRef(span, ident.clone())),
                None => Err(Error::UnboundSymbol(span, ident.clone())),
            },
            SValue::List(span, mut vs) => if vs.len() == 0 {
                Ok(Expr::Lit(Value::List(span, vec![])))
            } else {
                let arg_data = vs.split_off(1);
                self.lower_apply(span, vs.pop().unwrap(), arg_data)
            },
            lit @ SValue::Bool(_, _) => Ok(Expr::Lit(lit.into_value())),
            lit @ SValue::Int(_, _) => Ok(Expr::Lit(lit.into_value())),
            lit @ SValue::String(_, _) => Ok(Expr::Lit(lit.into_value())),
            _ => unimplemented!("Unsupported datum"),
        }
    }

    pub fn lower_module(&mut self, data: Vec<Value>) -> Result<Expr, Error> {
        let prim_scope = Scope::new_primitive_scope();
        let mut exprs = Vec::<Expr>::new();

        for datum in data {
            let scoped_datum = SValue::from_value(datum, &prim_scope);
            exprs.push(self.lower_expr(scoped_datum)?);
        }

        Ok(Expr::from_vec(exprs))
    }
}

////

#[cfg(test)]
use syntax::span::t2s;
#[cfg(test)]
use syntax::parser::data_from_str;

#[test]
fn self_quoting_datum() {
    let j = "false";
    let t = "^^^^^";

    let expected = Expr::Lit(Value::Bool(t2s(t), false));
    let data = data_from_str(j).unwrap();

    let mut lcx = LoweringContext::new();
    assert_eq!(expected, lcx.lower_module(data).unwrap());

    let j = "()";
    let t = "^^";

    let expected = Expr::Lit(Value::List(t2s(t), vec![]));
    let data = data_from_str(j).unwrap();

    let mut lcx = LoweringContext::new();
    assert_eq!(expected, lcx.lower_module(data).unwrap());
}

#[test]
fn quoted_datum() {
    let j = "'foo";
    let t = " ^^^";

    let expected = Expr::Lit(Value::Symbol(t2s(t), "foo".to_owned()));
    let data = data_from_str(j).unwrap();

    let mut lcx = LoweringContext::new();
    assert_eq!(expected, lcx.lower_module(data).unwrap());

    let j = "(quote foo)";
    let t = "       ^^^ ";

    let expected = Expr::Lit(Value::Symbol(t2s(t), "foo".to_owned()));
    let data = data_from_str(j).unwrap();

    let mut lcx = LoweringContext::new();
    assert_eq!(expected, lcx.lower_module(data).unwrap());

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
fn untyped_def() {
    let j = "(def x 1)";
    let t = "^^^^^^^^^";
    let u = "       ^ ";

    let expected = Expr::Def(
        t2s(t),
        Var {
            id: VarId(1),
            source_name: "x".to_owned(),
            bound: None,
        },
        Box::new(Expr::Lit(Value::Int(t2s(u), 1))),
    );

    let data = data_from_str(j).unwrap();

    let mut lcx = LoweringContext::new();
    assert_eq!(expected, lcx.lower_module(data).unwrap());
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
