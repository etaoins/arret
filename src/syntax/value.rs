use syntax::span::Span;
use std::collections::{BTreeMap, BTreeSet};

#[derive(PartialEq, Eq, Debug, Hash, Clone, PartialOrd, Ord)]
pub enum Value {
    Bool(bool),
    Char(char),
    Int(i64),
    List(Vec<Value>),
    String(String),
    Symbol(String),
    Vector(Vec<Value>),
    Map(BTreeMap<Value, Value>),
    Set(BTreeSet<Value>),
}

#[derive(PartialEq, Eq, Debug, Hash, Clone, PartialOrd, Ord)]
pub enum SValue {
    Bool(Span, bool),
    Char(Span, char),
    Int(Span, i64),
    List(Span, Vec<SValue>),
    String(Span, String),
    Symbol(Span, String),
    Vector(Span, Vec<SValue>),
    Map(Span, BTreeMap<SValue, SValue>),
    Set(Span, BTreeSet<SValue>),
}

impl SValue {
    pub fn to_unspanned(&self) -> Value {
        fn unspan_vec(vs: &Vec<SValue>) -> Vec<Value> {
            vs.iter().map(|v| v.to_unspanned()).collect()
        }

        match *self {
            SValue::Bool(_, v) => Value::Bool(v),
            SValue::Char(_, v) => Value::Char(v),
            SValue::Int(_, v) => Value::Int(v),
            SValue::List(_, ref vs) => Value::List(unspan_vec(vs)),
            SValue::String(_, ref v) => Value::String(v.clone()),
            SValue::Symbol(_, ref v) => Value::Symbol(v.clone()),
            SValue::Vector(_, ref vs) => Value::Vector(unspan_vec(vs)),
            SValue::Map(_, ref vs) => {
                let uvs = vs.iter().map(|(k, v)| (k.to_unspanned(), v.to_unspanned()));
                Value::Map(uvs.collect())
            }
            SValue::Set(_, ref vs) => Value::Set(vs.iter().map(|v| v.to_unspanned()).collect()),
        }
    }
}
