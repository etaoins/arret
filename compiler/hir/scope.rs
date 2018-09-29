use std::collections::HashMap;
use std::rc::Rc;

use syntax::span::{Span, EMPTY_SPAN};

use crate::hir::error::{Error, ErrorKind};
use crate::hir::macros::Macro;
use crate::hir::ns::{Ident, NsDatum, NsId, NsIdCounter};
use crate::hir::prim::Prim;
use crate::hir::{types, VarId};
use crate::ty;
use crate::ty::purity;

#[derive(Clone, PartialEq, Eq, Debug, Hash)]
pub enum Binding {
    Var(VarId),
    Prim(Prim),
    Macro(Rc<Macro>),
    Ty(ty::Poly),
    TyCons(types::TyCons),
    Purity(purity::Poly),
}

struct SpannedBinding {
    span: Span,
    binding: Binding,
}

struct Bindings {
    entries: HashMap<Ident, SpannedBinding>,
    parent: Option<Rc<Bindings>>,

    /// Allow redefinition of bindings
    ///
    /// This is only set for the root scope inside a REPL
    allow_redef: bool,
}

impl Bindings {
    fn get(&self, ident: &Ident) -> Option<&Binding> {
        match self.entries.get(ident) {
            Some(e) => Some(&e.binding),
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
pub struct Scope {
    bindings: Rc<Bindings>,
    tvars: ty::TVars,
    ns_id_counter: NsIdCounter,
}

impl Scope {
    /// Creates an empty root scope
    pub fn empty() -> Scope {
        Scope {
            bindings: Rc::new(Bindings {
                entries: HashMap::new(),
                parent: None,
                allow_redef: false,
            }),
            tvars: ty::TVars::new(),
            ns_id_counter: NsIdCounter::new(),
        }
    }

    pub fn root_ns_id() -> NsId {
        NsId::new(0)
    }

    /// Creates a new REPL scope containing `import`
    ///
    /// This scope is special as it allows redefinitions at the root level.
    pub fn new_repl() -> Scope {
        use std::iter;

        let entries = iter::once(("import".into(), Binding::Prim(Prim::Import)));
        let mut scope = Self::new_with_entries(entries);
        scope.bindings_mut().allow_redef = true;

        scope
    }

    /// Creates a new root scope containing all primitives and types
    pub fn new_with_primitives() -> Scope {
        use crate::hir::prim::PRIM_EXPORTS;
        use crate::hir::types::TY_EXPORTS;

        let entries = PRIM_EXPORTS
            .iter()
            .chain(TY_EXPORTS.iter())
            .map(|(name, binding)| ((*name).into(), binding.clone()));

        Self::new_with_entries(entries)
    }

    /// Creates a new root scope with entries
    pub fn new_with_entries<I>(entries: I) -> Scope
    where
        I: Iterator<Item = (Box<str>, Binding)>,
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
            bindings: Rc::new(Bindings {
                entries,
                parent: None,
                allow_redef: false,
            }),
            tvars: ty::TVars::new(),
            ns_id_counter: NsIdCounter::new(),
        }
    }

    pub fn new_child(parent: &Scope) -> Scope {
        Scope {
            bindings: Rc::new(Bindings {
                entries: HashMap::new(),
                parent: Some(parent.bindings.clone()),
                allow_redef: false,
            }),
            tvars: parent.tvars.clone(),
            ns_id_counter: parent.ns_id_counter.clone(),
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
        self.bindings().get(ident)
    }

    /// Returns the binding for a given ident if it exists, otherwise returns an error
    pub fn get_or_err<'a>(&'a self, span: Span, ident: &Ident) -> Result<&'a Binding, Error> {
        self.bindings()
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

    pub fn insert_bindings<I>(&mut self, span: Span, new_bindings: I) -> Result<(), Error>
    where
        I: Iterator<Item = (Ident, Binding)>,
    {
        use std::collections::hash_map::Entry;
        let bindings = self.bindings_mut();

        bindings.entries.reserve(new_bindings.size_hint().0);

        for (ident, binding) in new_bindings {
            let entry = SpannedBinding { span, binding };

            match bindings.entries.entry(ident) {
                Entry::Occupied(mut occupied) => {
                    if bindings.allow_redef {
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

    /// Returns all bound idents
    pub fn bound_idents(&self) -> impl Iterator<Item = &Ident> {
        self.bindings().entries.iter().map(|(ident, _)| ident)
    }

    pub fn tvars(&self) -> &ty::TVars {
        &self.tvars
    }

    pub fn tvars_mut(&mut self) -> &mut ty::TVars {
        &mut self.tvars
    }

    /// Allocates a new ns_id
    ///
    /// This is not globally unique; it will only be unique in the current scope chain
    pub fn alloc_ns_id(&mut self) -> NsId {
        self.ns_id_counter.alloc()
    }

    fn bindings(&self) -> &Bindings {
        &self.bindings
    }

    fn bindings_mut(&mut self) -> &mut Bindings {
        // This is also important to keep our `ns_alloc_id` invariant
        Rc::get_mut(&mut self.bindings).expect("Cannot mutate non-leaf scope")
    }
}
