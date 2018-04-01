use syntax::span::Span;
use syntax::datum::Datum;

#[derive(Clone, Copy, PartialEq, Eq, Hash, Debug)]
pub struct NsId(usize);

impl NsId {
    pub fn new(id: usize) -> NsId {
        NsId(id)
    }
}

#[derive(Clone, PartialEq, Eq, Hash, Debug)]
pub struct Ident {
    ns_id: NsId,
    name: String,
}

impl Ident {
    pub fn new(ns_id: NsId, name: String) -> Ident {
        Ident { ns_id, name }
    }

    pub fn ns_id(&self) -> NsId {
        self.ns_id
    }

    pub fn name(&self) -> &String {
        &self.name
    }

    pub fn into_name(self) -> String {
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
    List(Span, Vec<NsDatum>),
    Str(Span, String),
    Ident(Span, Ident),
    Vec(Span, Vec<NsDatum>),
    Map(Span, Vec<(NsDatum, NsDatum)>),
    Set(Span, Vec<NsDatum>),
}

impl NsDatum {
    fn map_syntax_datum_vec(ns_id: NsId, vs: Vec<Datum>) -> Vec<NsDatum> {
        vs.into_iter()
            .map(|v| Self::from_syntax_datum(ns_id, v))
            .collect()
    }

    pub fn from_syntax_datum(ns_id: NsId, value: Datum) -> NsDatum {
        match value {
            Datum::Bool(span, v) => NsDatum::Bool(span, v),
            Datum::Char(span, v) => NsDatum::Char(span, v),
            Datum::Int(span, v) => NsDatum::Int(span, v),
            Datum::Float(span, v) => NsDatum::Float(span, v),
            Datum::Str(span, v) => NsDatum::Str(span, v),
            Datum::Sym(span, v) => NsDatum::Ident(span, Ident::new(ns_id, v)),
            Datum::List(span, vs) => NsDatum::List(span, Self::map_syntax_datum_vec(ns_id, vs)),
            Datum::Vec(span, vs) => NsDatum::Vec(span, Self::map_syntax_datum_vec(ns_id, vs)),
            Datum::Set(span, vs) => NsDatum::Set(span, Self::map_syntax_datum_vec(ns_id, vs)),
            Datum::Map(span, vs) => NsDatum::Map(
                span,
                vs.into_iter()
                    .map(|(k, v)| {
                        (
                            NsDatum::from_syntax_datum(ns_id, k),
                            NsDatum::from_syntax_datum(ns_id, v),
                        )
                    })
                    .collect(),
            ),
        }
    }

    fn map_nsdatum_vec(vs: Vec<NsDatum>) -> Vec<Datum> {
        vs.into_iter().map(|v| v.into_syntax_datum()).collect()
    }

    pub fn into_syntax_datum(self) -> Datum {
        match self {
            NsDatum::Bool(span, v) => Datum::Bool(span, v),
            NsDatum::Char(span, v) => Datum::Char(span, v),
            NsDatum::Int(span, v) => Datum::Int(span, v),
            NsDatum::Float(span, v) => Datum::Float(span, v),
            NsDatum::Str(span, v) => Datum::Str(span, v),
            NsDatum::Ident(span, v) => Datum::Sym(span, v.into_name()),
            NsDatum::List(span, vs) => Datum::List(span, Self::map_nsdatum_vec(vs)),
            NsDatum::Vec(span, vs) => Datum::Vec(span, Self::map_nsdatum_vec(vs)),
            NsDatum::Set(span, vs) => Datum::Set(span, Self::map_nsdatum_vec(vs)),
            NsDatum::Map(span, vs) => Datum::Map(
                span,
                vs.into_iter()
                    .map(|(k, v)| (k.into_syntax_datum(), v.into_syntax_datum()))
                    .collect(),
            ),
        }
    }

    pub fn span(&self) -> Span {
        match *self {
            NsDatum::Bool(span, _)
            | NsDatum::Char(span, _)
            | NsDatum::Int(span, _)
            | NsDatum::Float(span, _)
            | NsDatum::Str(span, _)
            | NsDatum::Ident(span, _)
            | NsDatum::List(span, _)
            | NsDatum::Vec(span, _)
            | NsDatum::Set(span, _)
            | NsDatum::Map(span, _) => span,
        }
    }
}

pub struct NsIdAlloc {
    curr_ns_id: usize,
}

impl NsIdAlloc {
    pub fn new() -> NsIdAlloc {
        NsIdAlloc { curr_ns_id: 0 }
    }

    pub fn alloc(&mut self) -> NsId {
        self.curr_ns_id += 1;
        NsId::new(self.curr_ns_id)
    }
}
