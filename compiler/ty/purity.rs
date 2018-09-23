use std::collections::BTreeMap;

#[derive(Copy, Clone, PartialEq, Eq, Hash, Debug)]
pub enum Purity {
    Pure,
    Impure,
}

impl Purity {
    pub fn into_poly(self) -> Poly {
        Poly::Fixed(self)
    }

    #[cfg(test)]
    pub fn into_decl(self) -> Decl {
        Decl::Known(Poly::Fixed(self))
    }
}

new_global_id_type!(PVarId);

#[derive(PartialEq, Eq, Debug, Hash, Clone)]
pub struct PVar {
    source_name: Box<str>,
}

pub type PVars = BTreeMap<PVarId, PVar>;

impl PVar {
    pub fn new(source_name: Box<str>) -> PVar {
        PVar { source_name }
    }

    pub fn source_name(&self) -> &str {
        &self.source_name
    }
}

pub fn merge_pvars(outer: &PVars, inner: &PVars) -> PVars {
    outer
        .iter()
        .map(|(pvar_id, pvar)| (*pvar_id, pvar.clone()))
        .chain(inner.iter().map(|(pvar_id, pvar)| (*pvar_id, pvar.clone())))
        .collect()
}

#[derive(Debug, PartialEq, Eq, Clone, Hash)]
pub enum Poly {
    Fixed(Purity),
    Var(PVarId),
}

impl Poly {
    pub fn into_decl(self) -> Decl {
        Decl::Known(self)
    }
}

/// Decl is a purity declared by a user
///
/// The `Known` variant indicates the purity is specified while `Free` indicates it must be
/// inferred.
#[derive(PartialEq, Eq, Debug, Clone)]
pub enum Decl {
    Known(Poly),
    Free,
}
