use std::collections::HashMap;

use syntax::span::Span;
use syntax::datum::Datum;
use hir::{types, VarId};
use ty;

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
    Prim(Prim),
    Macro(MacroId),
    Ty(ty::Poly),
    TyCons(types::TyCons),
}

pub struct Scope {
    bindings: HashMap<Ident, Binding>,
}

impl Scope {
    pub fn new_empty() -> Scope {
        Scope {
            bindings: HashMap::new(),
        }
    }

    pub fn new_child(parent: &Scope) -> Scope {
        // TODO: Can we use a pointer to our parent without entering borrow checker hell?
        Scope {
            bindings: parent.bindings.clone(),
        }
    }

    /// Returns the binding for a given datum if it exists
    ///
    /// Only idents can have bindings; other data will return None.
    pub fn get_datum(&self, datum: &NsDatum) -> Option<Binding> {
        if let &NsDatum::Ident(_, ref ident) = datum {
            self.get(ident)
        } else {
            None
        }
    }

    /// Returns the binding for a given ident if it exists
    pub fn get(&self, ident: &Ident) -> Option<Binding> {
        match self.bindings.get(ident) {
            Some(b) => Some(b.clone()),
            None => None,
        }
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
}

macro_rules! prims {
    ( $( ($n:expr, $i:ident) ),* ) => {
        #[derive(Clone, Copy, PartialEq, Eq, Debug, Hash)]
        pub enum Prim {
            $($i,)*
        }

        pub fn insert_prim_exports(exports: &mut HashMap<String, Binding>) {
            $(
                exports.insert($n.to_owned(), Binding::Prim(Prim::$i));
            )*
        }
    }
}

prims!(
    ("def", Def),
    ("fn", Fun),
    ("if", If),
    ("quote", Quote),
    ("import", Import),
    ("export", Export),
    ("defmacro", DefMacro),
    ("...", Ellipsis),
    ("_", Wildcard),
    ("macro-rules", MacroRules),
    (":", TyColon),
    ("deftype", DefType)
);

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
            NsDatum::Bool(span, _) => span,
            NsDatum::Char(span, _) => span,
            NsDatum::Int(span, _) => span,
            NsDatum::Float(span, _) => span,
            NsDatum::Str(span, _) => span,
            NsDatum::Ident(span, _) => span,
            NsDatum::List(span, _) => span,
            NsDatum::Vec(span, _) => span,
            NsDatum::Set(span, _) => span,
            NsDatum::Map(span, _) => span,
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
        self.curr_ns_id = self.curr_ns_id + 1;
        NsId::new(self.curr_ns_id)
    }
}
