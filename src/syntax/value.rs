use syntax::span::Span;

#[derive(PartialEq, Eq, Debug, Clone)]
pub enum Value {
    Bool(Span, bool),
    Char(Span, char),
    Int(Span, i64),
    List(Span, Vec<Value>),
    String(Span, String),
    Symbol(Span, String),
    Vector(Span, Vec<Value>),
    Map(Span, Vec<(Value, Value)>),
    Set(Span, Vec<Value>),
}
