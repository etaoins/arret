use syntax::span::Span;

#[derive(PartialEq, Debug, Hash, Clone)]
pub enum Value {
    Bool(bool),
    Char(char),
    Int(i64),
    List(Vec<Value>),
    String(String),
    Symbol(String),
    Vector(Vec<Value>),
}

#[derive(PartialEq, Debug, Hash, Clone)]
pub enum SValue {
    Bool(Span, bool),
    Char(Span, char),
    Int(Span, i64),
    List(Span, Vec<SValue>),
    String(Span, String),
    Symbol(Span, String),
    Vector(Span, Vec<SValue>),
}

impl Value {
    pub fn to_spanned(&self, span: Span) -> SValue {
        match *self {
            Value::Bool(v) => SValue::Bool(span, v),
            Value::Char(v) => SValue::Char(span, v),
            Value::Int(v) => SValue::Int(span, v),
            Value::String(ref v) => SValue::String(span, v.clone()),
            Value::Symbol(ref v) => SValue::Symbol(span, v.clone()),
            _ => {
                panic!("Cannot span nested values");
            }
        }
    }
}

impl SValue {
    pub fn to_unspanned(&self) -> Value {
        fn unspan_vec(vs: &Vec<SValue>) -> Vec<Value> {
            vs.iter().map(|v| v.to_unspanned()).collect()
        }

        match *self {
            SValue::Bool(_, v) => Value::Bool(v),
            SValue::Char(_, v) => Value::Char(v),
            SValue::Int(_, v) => Value::Int(v),
            SValue::List(_, ref vs) => Value::List(unspan_vec(vs)),
            SValue::String(_, ref v) => Value::String(v.clone()),
            SValue::Symbol(_, ref v) => Value::Symbol(v.clone()),
            SValue::Vector(_, ref vs) => Value::Vector(unspan_vec(vs)),
        }
    }
}
