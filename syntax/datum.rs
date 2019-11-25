use std::sync::Arc;

use crate::span::Span;

pub type DataStr = Arc<str>;

#[derive(PartialEq, Debug, Clone)]
pub enum Datum {
    Bool(Span, bool),
    Char(Span, char),
    Int(Span, i64),
    Float(Span, f64),
    List(Span, Box<[Datum]>),
    Str(Span, DataStr),
    Sym(Span, DataStr),
    Vector(Span, Box<[Datum]>),
    Map(Span, Box<[(Datum, Datum)]>),
    Set(Span, Box<[Datum]>),
}

impl Datum {
    pub fn span(&self) -> Span {
        match self {
            Datum::Bool(span, _)
            | Datum::Char(span, _)
            | Datum::Int(span, _)
            | Datum::Float(span, _)
            | Datum::List(span, _)
            | Datum::Str(span, _)
            | Datum::Sym(span, _)
            | Datum::Vector(span, _)
            | Datum::Map(span, _)
            | Datum::Set(span, _) => *span,
        }
    }

    pub fn description(&self) -> &'static str {
        match self {
            Datum::Bool(_, true) => "boolean true",
            Datum::Bool(_, false) => "boolean false",
            Datum::Char(_, _) => "character",
            Datum::Int(_, _) => "integer",
            Datum::Float(_, _) => "floating point number",
            Datum::Str(_, _) => "string",
            Datum::Sym(_, name) => {
                if name.starts_with(':') {
                    "keyword"
                } else {
                    "symbol"
                }
            }
            Datum::List(_, vs) if vs.is_empty() => "empty list",
            Datum::List(_, _) => "list",

            Datum::Vector(_, vs) if vs.is_empty() => "empty vector",
            Datum::Vector(_, _) => "vector",

            Datum::Set(_, vs) if vs.is_empty() => "empty set",
            Datum::Set(_, _) => "set",

            Datum::Map(_, vs) if vs.is_empty() => "empty map",
            Datum::Map(_, _) => "map",
        }
    }
}
