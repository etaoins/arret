use syntax::span::Span;

#[derive(PartialEq, Debug, Clone)]
pub enum Datum {
    Bool(Span, bool),
    Char(Span, char),
    Int(Span, i64),
    Float(Span, f64),
    List(Span, Vec<Datum>),
    Str(Span, String),
    Sym(Span, String),
    Vec(Span, Vec<Datum>),
    Map(Span, Vec<(Datum, Datum)>),
    Set(Span, Vec<Datum>),
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
            | Datum::Vec(span, _)
            | Datum::Map(span, _)
            | Datum::Set(span, _) => *span,
        }
    }
}
