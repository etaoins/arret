use std::collections::HashMap;
use std::rc::Rc;

use hir::error::{Error, ErrorKind};
use hir::ns::{Ident, NsDatum, NsId, NsIdCounter};
use hir::prim::Prim;
use hir::{types, VarId};
use syntax::span::Span;
use ty;

new_indexing_id_type!(MacroId, u32);

#[derive(Clone, PartialEq, Eq, Debug, Hash)]
pub enum Binding {
    Var(VarId),
    Prim(Prim),
    Macro(MacroId),
    Ty(ty::Poly),
    TyCons(types::TyCons),
    Purity(ty::purity::Poly),
}

struct ScopeEntry {
    span: Span,
    binding: Binding,
}

struct ScopeData {
    entries: HashMap<Ident, ScopeEntry>,
    parent: Option<Rc<ScopeData>>,
    ns_id_counter: NsIdCounter,
}

impl ScopeData {
    fn get(&self, ident: &Ident) -> Option<Binding> {
        match self.entries.get(ident) {
            Some(e) => Some(e.binding.clone()),
            None => {
                if let Some(ref parent) = self.parent {
                    parent.get(ident)
                } else {
                    None
                }
            }
        }
    }
}

// TODO: We shouldn't need to use Rc here but the borrow checker has broken me
pub struct Scope(Rc<ScopeData>);

impl Scope {
    pub fn new_empty() -> Scope {
        Scope(Rc::new(ScopeData {
            entries: HashMap::new(),
            ns_id_counter: NsIdCounter::new(),
            parent: None,
        }))
    }

    pub fn new_child(parent: &Scope) -> Scope {
        Scope(Rc::new(ScopeData {
            entries: HashMap::new(),
            ns_id_counter: parent.data().ns_id_counter.clone(),
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

    pub fn insert_binding(
        &mut self,
        span: Span,
        ident: Ident,
        binding: Binding,
    ) -> Result<(), Error> {
        use std::collections::hash_map::Entry;

        match self.mut_data().entries.entry(ident) {
            Entry::Occupied(occupied) => Err(Error::new(
                span,
                ErrorKind::DuplicateDef(occupied.get().span),
            )),
            Entry::Vacant(vacant) => {
                vacant.insert(ScopeEntry { span, binding });
                Ok(())
            }
        }
    }

    pub fn insert_var(&mut self, span: Span, ident: Ident, var_id: VarId) -> Result<(), Error> {
        self.insert_binding(span, ident, Binding::Var(var_id))
    }

    // This is used to rebind variables to fresh locations when expanding macros
    pub fn rebind(
        &mut self,
        span: Span,
        old_ident: &Ident,
        new_ident: &Ident,
    ) -> Result<(), Error> {
        if let Some(old_binding) = self.get(old_ident) {
            let new_binding = old_binding.clone();
            self.insert_binding(span, new_ident.clone(), new_binding)
        } else {
            Ok(())
        }
    }

    /// Allocates a new ns_id
    ///
    /// This is not globally unique; it will only be unique in the current scope chain
    pub fn alloc_ns_id(&mut self) -> NsId {
        self.mut_data().ns_id_counter.alloc()
    }

    fn data(&self) -> &ScopeData {
        &self.0
    }

    fn mut_data(&mut self) -> &mut ScopeData {
        // This is also important to keep our `ns_alloc_id` invariant
        Rc::get_mut(&mut self.0).expect("Cannot mutate non-leaf scope")
    }
}
