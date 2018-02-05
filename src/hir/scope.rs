use std::collections::HashMap;
use syntax::span::Span;
use syntax::value::Value;
use hir::VarId;

macro_rules! primitives {
    ( $( ($n:expr, $i:ident) ),* ) => {
        pub enum Primitive {
            $($i,)*
        }

        fn primitive_bindings() -> Bindings {
            let mut bindings: Bindings = HashMap::new();
            $(bindings.insert($n.to_owned(), Binding::Primitive(Primitive::$i));)*
            bindings
        }
    }
}

pub enum Binding {
    Var(VarId),
    Primitive(Primitive),
}

pub type Bindings = HashMap<String, Binding>;

primitives!(
    ("def", Def),
    ("fn", Fun),
    ("if", If),
    ("quote", Quote),
    ("do", Do)
);

pub struct Scope<'a> {
    pub parent: Option<&'a Scope<'a>>,
    pub bindings: Bindings,
}

impl<'a> Scope<'a> {
    pub fn new_primitive_scope() -> Scope<'a> {
        Scope {
            parent: None,
            bindings: primitive_bindings(),
        }
    }

    pub fn resolve(&self, ident: &str) -> Option<&Binding> {
        match self.bindings.get(ident) {
            Some(b) => Some(b),
            None => self.parent.and_then(|p| p.resolve(ident)),
        }
    }
}

#[derive(Clone)]
pub enum SValue<'a> {
    Bool(Span, bool),
    Char(Span, char),
    Int(Span, i64),
    Float(Span, f64),
    List(Span, Vec<SValue<'a>>),
    String(Span, String),
    Symbol(Span, &'a Scope<'a>, String),
    Vector(Span, Vec<SValue<'a>>),
    Map(Span, Vec<(SValue<'a>, SValue<'a>)>),
    Set(Span, Vec<SValue<'a>>),
}

impl<'a> SValue<'a> {
    fn map_value_vec(vs: Vec<Value>, scope: &'a Scope) -> Vec<SValue<'a>> {
        vs.into_iter().map(|v| Self::from_value(v, scope)).collect()
    }

    pub fn from_value(value: Value, scope: &'a Scope<'a>) -> SValue<'a> {
        match value {
            Value::Bool(span, v) => SValue::Bool(span, v),
            Value::Char(span, v) => SValue::Char(span, v),
            Value::Int(span, v) => SValue::Int(span, v),
            Value::Float(span, v) => SValue::Float(span, v),
            Value::String(span, v) => SValue::String(span, v),
            Value::Symbol(span, v) => SValue::Symbol(span, scope, v),
            Value::List(span, vs) => SValue::List(span, Self::map_value_vec(vs, scope)),
            Value::Vector(span, vs) => SValue::Vector(span, Self::map_value_vec(vs, scope)),
            Value::Set(span, vs) => SValue::Set(span, Self::map_value_vec(vs, scope)),
            Value::Map(span, vs) => SValue::Map(
                span,
                vs.into_iter()
                    .map(|(k, v)| {
                        (SValue::from_value(k, scope), SValue::from_value(v, scope))
                    })
                    .collect(),
            ),
        }
    }

    fn map_svalue_vec(vs: Vec<SValue<'a>>) -> Vec<Value> {
        vs.into_iter().map(|v| v.into_value()).collect()
    }

    pub fn into_value(self) -> Value {
        match self {
            SValue::Bool(span, v) => Value::Bool(span, v),
            SValue::Char(span, v) => Value::Char(span, v),
            SValue::Int(span, v) => Value::Int(span, v),
            SValue::Float(span, v) => Value::Float(span, v),
            SValue::String(span, v) => Value::String(span, v),
            SValue::Symbol(span, _, v) => Value::Symbol(span, v),
            SValue::List(span, vs) => Value::List(span, Self::map_svalue_vec(vs)),
            SValue::Vector(span, vs) => Value::Vector(span, Self::map_svalue_vec(vs)),
            SValue::Set(span, vs) => Value::Set(span, Self::map_svalue_vec(vs)),
            SValue::Map(span, vs) => Value::Map(
                span,
                vs.into_iter()
                    .map(|(k, v)| (k.into_value(), v.into_value()))
                    .collect(),
            ),
        }
    }
}
