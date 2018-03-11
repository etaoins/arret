use std::collections::HashMap;

use syntax::span::Span;
use syntax::value::Value;
use hir::VarId;

#[derive(Debug, PartialEq, Eq, Clone, Copy, Hash)]
pub struct MacroId(usize);

impl MacroId {
    pub fn new(id: usize) -> MacroId {
        MacroId(id)
    }

    pub fn to_usize(&self) -> usize {
        self.0
    }
}

#[derive(Clone, PartialEq, Eq, Debug, Hash)]
pub enum Binding {
    Var(VarId),
    Primitive(Primitive),
    Macro(MacroId),
}

pub struct Scope {
    bindings: HashMap<Ident, Binding>,
    exports: HashMap<Ident, Span>,
}

impl Scope {
    pub fn new_empty() -> Scope {
        Scope {
            bindings: HashMap::new(),
            exports: HashMap::new(),
        }
    }

    pub fn new_child(parent: &Scope) -> Scope {
        // TODO: Can we use a pointer to our parent without entering borrow checker hell?
        Scope {
            bindings: parent.bindings.clone(),
            exports: HashMap::new(),
        }
    }

    pub fn get(&self, ident: &Ident) -> Option<Binding> {
        match self.bindings.get(ident) {
            Some(b) => Some(b.clone()),
            None => None,
        }
    }

    pub fn insert_export(&mut self, span: Span, ident: Ident) {
        self.exports.insert(ident, span);
    }

    pub fn insert_binding(&mut self, ident: Ident, binding: Binding) {
        self.bindings.insert(ident, binding);
    }

    pub fn insert_var(&mut self, ident: Ident, var_id: VarId) {
        self.insert_binding(ident, Binding::Var(var_id));
    }

    // This is used to rebind variables to fresh locations when expanding macros
    pub fn rebind(&mut self, old_ident: &Ident, new_ident: &Ident) {
        let new_binding = if let Some(old_binding) = self.bindings.get(old_ident) {
            old_binding.clone()
        } else {
            return;
        };

        self.bindings.insert(new_ident.clone(), new_binding);
    }

    pub fn exports(&self) -> &HashMap<Ident, Span> {
        &self.exports
    }
}

macro_rules! primitives {
    ( $( ($n:expr, $i:ident) ),* ) => {
        #[derive(Clone, Copy, PartialEq, Eq, Debug, Hash)]
        pub enum Primitive {
            $($i,)*
        }

        pub fn insert_primitive_exports(exports: &mut HashMap<String, Binding>) {
            $(
                exports.insert($n.to_owned(), Binding::Primitive(Primitive::$i));
            )*
        }
    }
}

primitives!(
    ("def", Def),
    ("fn", Fun),
    ("if", If),
    ("quote", Quote),
    ("import", Import),
    ("export", Export),
    ("defmacro", DefMacro),
    ("...", Ellipsis),
    ("_", Wildcard),
    ("macro-rules", MacroRules)
);

#[derive(Clone, Copy, PartialEq, Eq, Hash, Debug)]
pub struct NsId(usize);

impl NsId {
    pub fn new(id: usize) -> NsId {
        NsId(id)
    }
}

#[derive(Clone, PartialEq, Eq, Hash, Debug)]
pub struct Ident(NsId, String);

impl Ident {
    pub fn new(ns_id: NsId, name: String) -> Ident {
        Ident(ns_id, name)
    }

    pub fn ns_id(&self) -> NsId {
        self.0
    }

    pub fn name(&self) -> &String {
        &self.1
    }

    pub fn with_ns_id(&self, new_ns_id: NsId) -> Ident {
        Ident(new_ns_id, self.1.clone())
    }
}

#[derive(Clone, PartialEq, Debug)]
pub enum NsValue {
    Bool(Span, bool),
    Char(Span, char),
    Int(Span, i64),
    Float(Span, f64),
    List(Span, Vec<NsValue>),
    String(Span, String),
    Ident(Span, Ident),
    Vector(Span, Vec<NsValue>),
    Map(Span, Vec<(NsValue, NsValue)>),
    Set(Span, Vec<NsValue>),
}

impl NsValue {
    fn map_value_vec(vs: Vec<Value>, ns_id: NsId) -> Vec<NsValue> {
        vs.into_iter().map(|v| Self::from_value(v, ns_id)).collect()
    }

    pub fn from_value(value: Value, ns_id: NsId) -> NsValue {
        match value {
            Value::Bool(span, v) => NsValue::Bool(span, v),
            Value::Char(span, v) => NsValue::Char(span, v),
            Value::Int(span, v) => NsValue::Int(span, v),
            Value::Float(span, v) => NsValue::Float(span, v),
            Value::String(span, v) => NsValue::String(span, v),
            Value::Symbol(span, v) => NsValue::Ident(span, Ident(ns_id, v)),
            Value::List(span, vs) => NsValue::List(span, Self::map_value_vec(vs, ns_id)),
            Value::Vector(span, vs) => NsValue::Vector(span, Self::map_value_vec(vs, ns_id)),
            Value::Set(span, vs) => NsValue::Set(span, Self::map_value_vec(vs, ns_id)),
            Value::Map(span, vs) => NsValue::Map(
                span,
                vs.into_iter()
                    .map(|(k, v)| (NsValue::from_value(k, ns_id), NsValue::from_value(v, ns_id)))
                    .collect(),
            ),
        }
    }

    fn map_nsvalue_vec(vs: Vec<NsValue>) -> Vec<Value> {
        vs.into_iter().map(|v| v.into_value()).collect()
    }

    pub fn into_value(self) -> Value {
        match self {
            NsValue::Bool(span, v) => Value::Bool(span, v),
            NsValue::Char(span, v) => Value::Char(span, v),
            NsValue::Int(span, v) => Value::Int(span, v),
            NsValue::Float(span, v) => Value::Float(span, v),
            NsValue::String(span, v) => Value::String(span, v),
            NsValue::Ident(span, v) => Value::Symbol(span, v.1),
            NsValue::List(span, vs) => Value::List(span, Self::map_nsvalue_vec(vs)),
            NsValue::Vector(span, vs) => Value::Vector(span, Self::map_nsvalue_vec(vs)),
            NsValue::Set(span, vs) => Value::Set(span, Self::map_nsvalue_vec(vs)),
            NsValue::Map(span, vs) => Value::Map(
                span,
                vs.into_iter()
                    .map(|(k, v)| (k.into_value(), v.into_value()))
                    .collect(),
            ),
        }
    }

    pub fn span(&self) -> Span {
        match *self {
            NsValue::Bool(span, _) => span,
            NsValue::Char(span, _) => span,
            NsValue::Int(span, _) => span,
            NsValue::Float(span, _) => span,
            NsValue::String(span, _) => span,
            NsValue::Ident(span, _) => span,
            NsValue::List(span, _) => span,
            NsValue::Vector(span, _) => span,
            NsValue::Set(span, _) => span,
            NsValue::Map(span, _) => span,
        }
    }
}

pub struct NsIdAllocator {
    curr_ns_id: usize,
}

impl NsIdAllocator {
    pub fn new() -> NsIdAllocator {
        NsIdAllocator { curr_ns_id: 0 }
    }

    pub fn alloc(&mut self) -> NsId {
        self.curr_ns_id = self.curr_ns_id + 1;
        NsId::new(self.curr_ns_id)
    }
}
