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
