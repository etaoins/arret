use std::ops::Range;

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

new_indexing_id_type!(PVarId, u32);
pub type PVarIds = Range<PVarId>;

impl PVarId {
    pub fn monomorphic() -> PVarIds {
        PVarId::new(0)..PVarId::new(0)
    }
}

#[derive(PartialEq, Eq, Debug, Hash, Clone)]
pub struct PVar {
    source_name: Box<str>,
}

impl PVar {
    pub fn new(source_name: Box<str>) -> PVar {
        PVar { source_name }
    }

    pub fn source_name(&self) -> &str {
        &self.source_name
    }
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
