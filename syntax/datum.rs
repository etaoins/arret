use span::Span;

#[derive(PartialEq, Debug, Clone)]
pub enum Datum {
    Bool(Span, bool),
    Char(Span, char),
    Int(Span, i64),
    Float(Span, f64),
    List(Span, Box<[Datum]>),
    Str(Span, Box<str>),
    Sym(Span, Box<str>),
    Vec(Span, Box<[Datum]>),
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
            | Datum::Vec(span, _)
            | Datum::Map(span, _)
            | Datum::Set(span, _) => *span,
        }
    }
}
