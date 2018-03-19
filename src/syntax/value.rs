use syntax::span::Span;

#[derive(PartialEq, Debug, Clone)]
pub enum Value {
    Bool(Span, bool),
    Char(Span, char),
    Int(Span, i64),
    Float(Span, f64),
    List(Span, Vec<Value>),
    Str(Span, String),
    Sym(Span, String),
    Vec(Span, Vec<Value>),
    Map(Span, Vec<(Value, Value)>),
    Set(Span, Vec<Value>),
}
