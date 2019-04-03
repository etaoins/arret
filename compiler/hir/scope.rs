use std::collections::HashMap;

use syntax::span::{Span, EMPTY_SPAN};

use crate::hir::error::{Error, ErrorKind};
use crate::hir::macros::MacroId;
use crate::hir::ns::{Ident, NsDatum, NsId, NsIdCounter};
use crate::hir::prim::Prim;
use crate::hir::{types, VarId};
use crate::ty;
use crate::ty::purity;

#[derive(Clone, PartialEq, Debug)]
pub enum Binding {
    Var(VarId),
    Prim(Prim),
    Macro(MacroId),
    Ty(ty::Ref<ty::Poly>),
    TyCons(types::TyCons),
    TyPred(ty::pred::TestTy),
    EqPred,
    Purity(purity::Ref),
}

struct SpannedBinding {
    span: Span,
    binding: Binding,
}

pub struct Scope<'parent> {
    ns_id_counter: NsIdCounter,

    entries: HashMap<Ident, SpannedBinding>,
    parent: Option<&'parent Scope<'parent>>,

    /// Allow redefinition of bindings
    ///
    /// This is only set for the root scope inside a REPL
    allow_redef: bool,
}

impl<'parent> Scope<'parent> {
    pub fn root_ns_id() -> NsId {
        NsId::new(0)
    }

    /// Creates a new root scope with entries
    pub fn new_with_entries<I>(entries: I) -> Scope<'static>
    where
        I: Iterator<Item = (&'static str, Binding)>,
    {
        let entries = entries
            .map(|(name, binding)| {
                (
                    Ident::new(Self::root_ns_id(), (*name).into()),
                    SpannedBinding {
                        span: EMPTY_SPAN,
                        binding: binding.clone(),
                    },
                )
            })
            .collect::<HashMap<Ident, SpannedBinding>>();

        Scope {
            ns_id_counter: NsIdCounter::new(),
            entries,
            parent: None,
            allow_redef: false,
        }
    }

    /// Creates a new REPL scope containing `import`
    ///
    /// This scope is special as it allows redefinitions at the root level.
    pub fn new_repl() -> Scope<'static> {
        use std::iter;

        let entries = iter::once(("import", Binding::Prim(Prim::Import)));
        let mut scope = Self::new_with_entries(entries);
        scope.allow_redef = true;

        scope
    }

    /// Creates a new root scope containing all primitives and types
    pub fn new_with_primitives() -> Scope<'static> {
        use crate::hir::prim::PRIM_EXPORTS;
        use crate::hir::types::TY_EXPORTS;

        let entries = PRIM_EXPORTS
            .iter()
            .chain(TY_EXPORTS.iter())
            .map(|(name, binding)| (*name, binding.clone()));

        Self::new_with_entries(entries)
    }

    pub fn new_child(parent: &'parent Scope<'parent>) -> Scope<'parent> {
        Scope {
            ns_id_counter: parent.ns_id_counter.clone(),
            entries: HashMap::new(),
            parent: Some(parent),
            allow_redef: false,
        }
    }

    /// Returns the binding for a given datum if it exists
    ///
    /// Only idents can have bindings; other data will return None.
    pub fn get_datum<'a>(&'a self, datum: &NsDatum) -> Option<&'a Binding> {
        if let NsDatum::Ident(_, ident) = datum {
            self.get(ident)
        } else {
            None
        }
    }

    /// Returns the binding for a given ident if it exists
    pub fn get<'a>(&'a self, ident: &Ident) -> Option<&'a Binding> {
        self.entries.get(ident).map(|e| &e.binding).or_else(|| {
            if let Some(ref parent) = self.parent {
                parent.get(ident)
            } else {
                None
            }
        })
    }

    /// Returns the binding for a given ident if it exists, otherwise returns an error
    pub fn get_or_err<'a>(&'a self, span: Span, ident: &Ident) -> Result<&'a Binding, Error> {
        self.get(ident)
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

    pub fn insert_bindings<I>(&mut self, span: Span, new_bindings: I) -> Result<(), Error>
    where
        I: Iterator<Item = (Ident, Binding)>,
    {
        use std::collections::hash_map::Entry;

        self.entries.reserve(new_bindings.size_hint().0);

        for (ident, binding) in new_bindings {
            let entry = SpannedBinding { span, binding };

            match self.entries.entry(ident) {
                Entry::Occupied(mut occupied) => {
                    if self.allow_redef {
                        occupied.insert(entry);
                    } else {
                        return Err(Error::new(
                            span,
                            ErrorKind::DuplicateDef(
                                occupied.get().span,
                                occupied.key().name().into(),
                            ),
                        ));
                    }
                }
                Entry::Vacant(vacant) => {
                    vacant.insert(entry);
                }
            }
        }
        Ok(())
    }

    pub fn insert_var(&mut self, span: Span, ident: Ident, var_id: VarId) -> Result<(), Error> {
        self.insert_binding(span, ident, Binding::Var(var_id))
    }

    /// Returns all bound idents
    pub fn bound_idents(&self) -> impl Iterator<Item = &Ident> {
        self.entries.iter().map(|(ident, _)| ident)
    }

    /// Allocates a new NsId
    ///
    /// This is not globally unique; it will only be unique in the current scope chain
    pub fn alloc_ns_id(&mut self) -> NsId {
        self.ns_id_counter.alloc()
    }
}
