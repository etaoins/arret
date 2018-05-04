use std::collections::HashMap;

use hir::ns::{Ident, NsDatum};
use hir::prim::Prim;
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
    Purity(ty::purity::Purity),
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
        if let NsDatum::Ident(_, ref ident) = *datum {
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
