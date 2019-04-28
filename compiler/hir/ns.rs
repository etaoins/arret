use std::vec;

use arret_syntax::datum::{DataStr, Datum};
use arret_syntax::span::Span;

use crate::hir::scope::Scope;

new_counting_id_type!(NsIdCounter, NsId);

#[derive(Clone, PartialEq, Eq, Hash, Debug)]
pub struct Ident {
    ns_id: NsId,
    data_name: DataStr,
}

impl Ident {
    pub fn new(ns_id: NsId, data_name: DataStr) -> Ident {
        Ident { ns_id, data_name }
    }

    pub fn ns_id(&self) -> NsId {
        self.ns_id
    }

    pub fn name(&self) -> &DataStr {
        &self.data_name
    }

    pub fn into_name(self) -> DataStr {
        self.data_name
    }

    pub fn is_underscore(&self) -> bool {
        self.data_name.as_ref() == "_"
    }

    pub fn is_ellipsis(&self) -> bool {
        self.data_name.as_ref() == "..."
    }

    pub fn is_ampersand(&self) -> bool {
        self.data_name.as_ref() == "&"
    }

    pub fn with_ns_id(&self, new_ns_id: NsId) -> Ident {
        Ident {
            ns_id: new_ns_id,
            data_name: self.data_name.clone(),
        }
    }
}

#[derive(Clone, PartialEq, Debug)]
pub enum NsDatum {
    Bool(Span, bool),
    Char(Span, char),
    Int(Span, i64),
    Float(Span, f64),
    List(Span, Box<[NsDatum]>),
    Str(Span, DataStr),
    Keyword(Span, DataStr),
    Ident(Span, Ident),
    Vector(Span, Box<[NsDatum]>),
    Map(Span, Box<[(NsDatum, NsDatum)]>),
    Set(Span, Box<[NsDatum]>),
}

impl NsDatum {
    fn map_syntax_data(vs: &[Datum]) -> Box<[NsDatum]> {
        vs.iter().map(Self::from_syntax_datum).collect()
    }

    pub fn from_syntax_datum(value: &Datum) -> NsDatum {
        match value {
            Datum::Bool(span, v) => NsDatum::Bool(*span, *v),
            Datum::Char(span, v) => NsDatum::Char(*span, *v),
            Datum::Int(span, v) => NsDatum::Int(*span, *v),
            Datum::Float(span, v) => NsDatum::Float(*span, *v),
            Datum::Str(span, v) => NsDatum::Str(*span, v.clone()),
            Datum::Sym(span, v) => {
                if v.starts_with(':') {
                    NsDatum::Keyword(*span, v.clone())
                } else {
                    NsDatum::Ident(*span, Ident::new(Scope::root_ns_id(), v.clone()))
                }
            }
            Datum::List(span, vs) => NsDatum::List(*span, Self::map_syntax_data(vs)),
            Datum::Vector(span, vs) => NsDatum::Vector(*span, Self::map_syntax_data(vs)),
            Datum::Set(span, vs) => NsDatum::Set(*span, Self::map_syntax_data(vs)),
            Datum::Map(span, vs) => NsDatum::Map(
                *span,
                vs.iter()
                    .map(|(k, v)| (NsDatum::from_syntax_datum(k), NsDatum::from_syntax_datum(v)))
                    .collect(),
            ),
        }
    }

    fn map_nsdata(vs: Box<[NsDatum]>) -> Box<[Datum]> {
        vs.into_vec()
            .into_iter()
            .map(NsDatum::into_syntax_datum)
            .collect()
    }

    pub fn into_syntax_datum(self) -> Datum {
        match self {
            NsDatum::Bool(span, v) => Datum::Bool(span, v),
            NsDatum::Char(span, v) => Datum::Char(span, v),
            NsDatum::Int(span, v) => Datum::Int(span, v),
            NsDatum::Float(span, v) => Datum::Float(span, v),
            NsDatum::Str(span, v) => Datum::Str(span, v),
            NsDatum::Keyword(span, v) => Datum::Sym(span, v),
            NsDatum::Ident(span, v) => Datum::Sym(span, v.into_name()),
            NsDatum::List(span, vs) => Datum::List(span, Self::map_nsdata(vs)),
            NsDatum::Vector(span, vs) => Datum::Vector(span, Self::map_nsdata(vs)),
            NsDatum::Set(span, vs) => Datum::Set(span, Self::map_nsdata(vs)),
            NsDatum::Map(span, vs) => Datum::Map(
                span,
                vs.into_vec()
                    .into_iter()
                    .map(|(k, v)| (k.into_syntax_datum(), v.into_syntax_datum()))
                    .collect(),
            ),
        }
    }

    pub fn span(&self) -> Span {
        match self {
            NsDatum::Bool(span, _)
            | NsDatum::Char(span, _)
            | NsDatum::Int(span, _)
            | NsDatum::Float(span, _)
            | NsDatum::Str(span, _)
            | NsDatum::Keyword(span, _)
            | NsDatum::Ident(span, _)
            | NsDatum::List(span, _)
            | NsDatum::Vector(span, _)
            | NsDatum::Set(span, _)
            | NsDatum::Map(span, _) => *span,
        }
    }
}

/// Iterator for NsDatum used inside the HIR
///
/// This is a specific type as we use .as_slice() to peek at data in certain places for
/// context-sensitive parsing.
pub type NsDataIter = vec::IntoIter<NsDatum>;
