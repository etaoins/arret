use std::collections::HashMap;
use std::rc::Rc;

use hir::error::{Error, ErrorKind};
use hir::ns::{Ident, NsDatum, NsId, NsIdCounter};
use hir::prim::Prim;
use hir::{types, VarId};
use syntax::span::{Span, EMPTY_SPAN};
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
    /// Creates an empty root scope
    pub fn empty() -> Scope {
        Scope(Rc::new(ScopeData {
            entries: HashMap::new(),
            ns_id_counter: NsIdCounter::new(),
            parent: None,
        }))
    }

    /// Creates a new root scope containing all primitives and types
    ///
    /// The bindings will be in `NsId(0)` with an empty span
    pub fn new_with_primitives() -> Scope {
        use hir::prim::PRIM_EXPORTS;
        use hir::types::TY_EXPORTS;

        let entries = PRIM_EXPORTS
            .iter()
            .chain(TY_EXPORTS.iter())
            .map(|(name, binding)| ((*name).into(), binding.clone()));

        Self::new_with_entries(entries)
    }

    /// Creates a new root scope with entries
    ///
    /// The bindings will be in `NsId(0)` with an empty span
    pub fn new_with_entries<I>(entries: I) -> Scope
    where
        I: Iterator<Item = (Box<str>, Binding)>,
    {
        let mut ns_id_counter = NsIdCounter::new();
        let ns_id = ns_id_counter.alloc();

        let entries = entries
            .map(|(name, binding)| {
                (
                    Ident::new(ns_id, (*name).into()),
                    ScopeEntry {
                        span: EMPTY_SPAN,
                        binding: binding.clone(),
                    },
                )
            }).collect::<HashMap<Ident, ScopeEntry>>();

        Scope(Rc::new(ScopeData {
            entries,
            ns_id_counter,
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

    /// Returns the binding for a given ident if it exists, otherwise returns an error
    pub fn get_or_err(&self, span: Span, ident: &Ident) -> Result<Binding, Error> {
        self.data()
            .get(ident)
            .ok_or_else(|| Error::new(span, ErrorKind::UnboundSym(ident.name().into())))
    }

    pub fn insert_binding(
        &mut self,
        span: Span,
        ident: Ident,
        binding: Binding,
    ) -> Result<(), Error> {
        use std::iter;
        self.insert_bindings(span, iter::once((ident, binding)))
    }

    pub fn insert_bindings<I>(&mut self, span: Span, bindings: I) -> Result<(), Error>
    where
        I: Iterator<Item = (Ident, Binding)>,
    {
        use std::collections::hash_map::Entry;
        let mut_data = self.mut_data();

        mut_data.entries.reserve(bindings.size_hint().0);

        for (ident, binding) in bindings {
            match mut_data.entries.entry(ident) {
                Entry::Occupied(occupied) => {
                    return Err(Error::new(
                        span,
                        ErrorKind::DuplicateDef(occupied.get().span),
                    ))
                }
                Entry::Vacant(vacant) => {
                    vacant.insert(ScopeEntry { span, binding });
                }
            }
        }
        Ok(())
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
