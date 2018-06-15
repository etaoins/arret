use std::vec;
use syntax::datum::Datum;
use syntax::span::Span;

new_counting_id_type!(NsIdCounter, NsId, u32);

#[derive(Clone, PartialEq, Eq, Hash, Debug)]
pub struct Ident {
    ns_id: NsId,
    name: Box<str>,
}

impl Ident {
    pub fn new(ns_id: NsId, name: Box<str>) -> Ident {
        Ident { ns_id, name }
    }

    pub fn ns_id(&self) -> NsId {
        self.ns_id
    }

    pub fn name(&self) -> &str {
        &self.name
    }

    pub fn into_name(self) -> Box<str> {
        self.name
    }

    pub fn with_ns_id(&self, new_ns_id: NsId) -> Ident {
        Ident {
            ns_id: new_ns_id,
            name: self.name.clone(),
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
    Str(Span, Box<str>),
    Ident(Span, Ident),
    Vec(Span, Box<[NsDatum]>),
    Map(Span, Box<[(NsDatum, NsDatum)]>),
    Set(Span, Box<[NsDatum]>),
}

impl NsDatum {
    fn map_syntax_data(ns_id: NsId, vs: Box<[Datum]>) -> Box<[NsDatum]> {
        vs.into_vec()
            .into_iter()
            .map(|v| Self::from_syntax_datum(ns_id, v))
            .collect::<Vec<NsDatum>>()
            .into_boxed_slice()
    }

    pub fn from_syntax_datum(ns_id: NsId, value: Datum) -> NsDatum {
        match value {
            Datum::Bool(span, v) => NsDatum::Bool(span, v),
            Datum::Char(span, v) => NsDatum::Char(span, v),
            Datum::Int(span, v) => NsDatum::Int(span, v),
            Datum::Float(span, v) => NsDatum::Float(span, v),
            Datum::Str(span, v) => NsDatum::Str(span, v),
            Datum::Sym(span, v) => NsDatum::Ident(span, Ident::new(ns_id, v)),
            Datum::List(span, vs) => NsDatum::List(span, Self::map_syntax_data(ns_id, vs)),
            Datum::Vec(span, vs) => NsDatum::Vec(span, Self::map_syntax_data(ns_id, vs)),
            Datum::Set(span, vs) => NsDatum::Set(span, Self::map_syntax_data(ns_id, vs)),
            Datum::Map(span, vs) => NsDatum::Map(
                span,
                vs.into_vec()
                    .into_iter()
                    .map(|(k, v)| {
                        (
                            NsDatum::from_syntax_datum(ns_id, k),
                            NsDatum::from_syntax_datum(ns_id, v),
                        )
                    })
                    .collect::<Vec<(NsDatum, NsDatum)>>()
                    .into_boxed_slice(),
            ),
        }
    }

    fn map_nsdata(vs: Box<[NsDatum]>) -> Box<[Datum]> {
        vs.into_vec()
            .into_iter()
            .map(|v| v.into_syntax_datum())
            .collect::<Vec<Datum>>()
            .into_boxed_slice()
    }

    pub fn into_syntax_datum(self) -> Datum {
        match self {
            NsDatum::Bool(span, v) => Datum::Bool(span, v),
            NsDatum::Char(span, v) => Datum::Char(span, v),
            NsDatum::Int(span, v) => Datum::Int(span, v),
            NsDatum::Float(span, v) => Datum::Float(span, v),
            NsDatum::Str(span, v) => Datum::Str(span, v),
            NsDatum::Ident(span, v) => Datum::Sym(span, v.into_name()),
            NsDatum::List(span, vs) => Datum::List(span, Self::map_nsdata(vs)),
            NsDatum::Vec(span, vs) => Datum::Vec(span, Self::map_nsdata(vs)),
            NsDatum::Set(span, vs) => Datum::Set(span, Self::map_nsdata(vs)),
            NsDatum::Map(span, vs) => Datum::Map(
                span,
                vs.into_vec()
                    .into_iter()
                    .map(|(k, v)| (k.into_syntax_datum(), v.into_syntax_datum()))
                    .collect::<Vec<(Datum, Datum)>>()
                    .into_boxed_slice(),
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
            | NsDatum::Ident(span, _)
            | NsDatum::List(span, _)
            | NsDatum::Vec(span, _)
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
