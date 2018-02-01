use syntax::span::Span;
use std::collections::{BTreeMap, BTreeSet};

#[derive(PartialEq, Eq, Debug, Hash, Clone, PartialOrd, Ord)]
pub enum Value {
    Bool(Span, bool),
    Char(Span, char),
    Int(Span, i64),
    List(Span, Vec<Value>),
    String(Span, String),
    Symbol(Span, String),
    Vector(Span, Vec<Value>),
    Map(Span, BTreeMap<Value, Value>),
    Set(Span, BTreeSet<Value>),
}
