use std::collections::HashMap;
use std::sync::Arc;

use arret_syntax::span::Span;

use crate::context::ModuleId;
use crate::hir::error::{Error, ErrorKind};
use crate::hir::macros::Macro;
use crate::hir::ns::{Ident, NsDatum, NsId, NsIdCounter};
use crate::hir::prim::Prim;
use crate::hir::{types, LocalId};
use crate::ty;
use crate::ty::purity;
use crate::ty::record;

#[derive(Clone, Debug)]
pub enum Binding {
    Var(Option<ModuleId>, LocalId),
    Prim(Prim),
    Macro(Option<ModuleId>, Arc<Macro>),
    Ty(ty::Ref<ty::Poly>),
    TyCons(types::TyCons),
    TyPred(ty::pred::TestTy),
    EqPred,
    RecordValueCons(record::ConsId),
    RecordTyCons(record::ConsId),
    FieldAccessor(record::ConsId, usize),
    Purity(purity::Ref),
}

impl Binding {
    pub fn description(&self) -> &'static str {
        match self {
            Binding::Var(_, _) | Binding::TyPred(_) | Binding::EqPred => "value",
            Binding::Prim(_) => "primitive",
            Binding::Macro(_, _) => "macro",
            Binding::Ty(_) => "type",
            Binding::TyCons(_) => "type constructor",
            Binding::RecordValueCons(_) => "record value constructor",
            Binding::RecordTyCons(_) => "record type constructor",
            Binding::FieldAccessor(_, _) => "record field accessor",
            Binding::Purity(_) => "purity",
        }
    }

    pub fn import_from(&self, module_id: ModuleId) -> Binding {
        match self {
            Binding::Var(None, local_id) => Binding::Var(Some(module_id), *local_id),
            Binding::Macro(None, macro_id) => Binding::Macro(Some(module_id), macro_id.clone()),
            other => other.clone(),
        }
    }
}

pub struct SpannedBinding {
    span: Option<Span>,
    binding: Binding,
}

pub struct Scope<'parent> {
    ns_id_counter: NsIdCounter,

    entries: HashMap<Ident, SpannedBinding>,
    parent: Option<&'parent Scope<'parent>>,
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
                        span: None,
                        binding,
                    },
                )
            })
            .collect::<HashMap<Ident, SpannedBinding>>();

        Scope {
            ns_id_counter: NsIdCounter::new(),
            entries,
            parent: None,
        }
    }

    /// Creates a root scope containing `import`
    pub fn root() -> Scope<'static> {
        // The default root scope only consists of a placeholder for (import)
        let entries = std::iter::once(("import", Binding::Prim(Prim::ImportPlaceholder)));
        Self::new_with_entries(entries)
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

    pub fn child(&'parent self) -> Scope<'parent> {
        Scope {
            ns_id_counter: self.ns_id_counter.clone(),
            entries: HashMap::new(),
            parent: Some(self),
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
            if let Some(parent) = self.parent {
                parent.get(ident)
            } else {
                None
            }
        })
    }

    /// Returns the binding for a given ident if it exists, otherwise returns an error
    pub fn get_or_err<'a>(&'a self, span: Span, ident: &Ident) -> Result<&'a Binding, Error> {
        self.get(ident)
            .ok_or_else(|| Error::new(span, ErrorKind::UnboundIdent(ident.name().clone())))
    }

    /// Inserts a new binding if it doesn't exist or redefinition is allowed
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

        for (ident, binding) in new_bindings.filter(|b| !b.0.is_underscore()) {
            let entry = SpannedBinding {
                span: Some(span),
                binding,
            };

            match self.entries.entry(ident) {
                Entry::Occupied(occupied) => {
                    return Err(Error::new(
                        span,
                        ErrorKind::DuplicateDef(occupied.get().span, occupied.key().name().clone()),
                    ));
                }
                Entry::Vacant(vacant) => {
                    vacant.insert(entry);
                }
            }
        }
        Ok(())
    }

    /// Unconditionally replaces a binding
    pub fn replace_binding(&mut self, span: Span, ident: Ident, binding: Binding) {
        self.entries.insert(
            ident,
            SpannedBinding {
                span: Some(span),
                binding,
            },
        );
    }

    pub fn insert_local(
        &mut self,
        span: Span,
        ident: Ident,
        local_id: LocalId,
    ) -> Result<(), Error> {
        self.insert_binding(span, ident, Binding::Var(None, local_id))
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

    /// Exports our entire contents as bindings suitable to be imported in to another scope
    ///
    /// This is used to fold child REPL scopes back in to their parent
    pub fn into_exported_bindings(self) -> HashMap<Ident, SpannedBinding> {
        self.entries
    }

    pub fn import_bindings(
        &mut self,
        exported_bindings: impl IntoIterator<Item = (Ident, SpannedBinding)>,
        module_id: ModuleId,
    ) {
        self.entries.extend(exported_bindings.into_iter().map(
            |(ident, SpannedBinding { span, binding })| {
                (
                    ident,
                    SpannedBinding {
                        span,
                        binding: binding.import_from(module_id),
                    },
                )
            },
        ));
    }
}
