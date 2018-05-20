use std::collections::HashMap;
use std::rc::Rc;

use hir::ns::{Ident, NsDatum, NsId};
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
    Purity(ty::purity::Poly),
}

pub struct ScopeData {
    bindings: HashMap<Ident, Binding>,
    parent: Option<Rc<ScopeData>>,
    curr_ns_id: u32,
}

impl ScopeData {
    fn get(&self, ident: &Ident) -> Option<Binding> {
        match self.bindings.get(ident) {
            Some(b) => Some(b.clone()),
            None => if let Some(ref parent) = self.parent {
                parent.get(ident)
            } else {
                None
            },
        }
    }
}

// TODO: We shouldn't need to use Rc here but the borrow checker has broken me
pub struct Scope(Rc<ScopeData>);

impl Scope {
    pub fn new_empty() -> Scope {
        Scope(Rc::new(ScopeData {
            bindings: HashMap::new(),
            curr_ns_id: 0,
            parent: None,
        }))
    }

    pub fn new_child(parent: &Scope) -> Scope {
        Scope(Rc::new(ScopeData {
            bindings: HashMap::new(),
            curr_ns_id: parent.data().curr_ns_id,
            parent: Some(parent.0.clone()),
        }))
    }

    /// Returns the binding for a given datum if it exists
    ///
    /// Only idents can have bindings; other data will return None.
    pub fn get_datum(&self, datum: &NsDatum) -> Option<Binding> {
        if let NsDatum::Ident(_, ident) = datum {
            self.get(ident)
        } else {
            None
        }
    }

    /// Returns the binding for a given ident if it exists
    pub fn get(&self, ident: &Ident) -> Option<Binding> {
        self.data().get(ident)
    }

    pub fn insert_binding(&mut self, ident: Ident, binding: Binding) {
        self.mut_data().bindings.insert(ident, binding);
    }

    pub fn insert_var(&mut self, ident: Ident, var_id: VarId) {
        self.insert_binding(ident, Binding::Var(var_id));
    }

    // This is used to rebind variables to fresh locations when expanding macros
    pub fn rebind(&mut self, old_ident: &Ident, new_ident: &Ident) {
        let new_binding = if let Some(old_binding) = self.get(old_ident) {
            old_binding.clone()
        } else {
            return;
        };

        self.mut_data()
            .bindings
            .insert(new_ident.clone(), new_binding);
    }

    /// Allocates a new ns_id
    ///
    /// This is not globally unique; it will only be unique in the current scope chain
    pub fn alloc_ns_id(&mut self) -> NsId {
        self.mut_data().curr_ns_id += 1;
        NsId::new(self.data().curr_ns_id)
    }

    fn data(&self) -> &ScopeData {
        &self.0
    }

    fn mut_data(&mut self) -> &mut ScopeData {
        // This is also important to keep our `ns_alloc_id` invariant
        Rc::get_mut(&mut self.0).expect("Cannot mutate non-leaf scope")
    }
}
