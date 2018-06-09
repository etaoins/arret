use std;
use std::ops::Range;

#[derive(Copy, Clone, PartialEq, Eq, Hash, Debug)]
pub enum Purity {
    Pure,
    Impure,
}

impl Purity {
    pub fn into_poly(self) -> Poly {
        Poly::from_purity(self)
    }

    #[cfg(test)]
    pub fn into_decl(self) -> Decl {
        Decl::Known(Poly::from_purity(self))
    }
}

pub trait PRef: PartialEq + Eq + Clone + std::fmt::Debug + std::hash::Hash + Sized {
    fn from_purity(purity: Purity) -> Self;
}

impl PRef for Purity {
    fn from_purity(purity: Purity) -> Purity {
        purity
    }
}

new_indexing_id_type!(PVarId, u32);

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

impl PRef for Poly {
    fn from_purity(purity: Purity) -> Poly {
        Poly::Fixed(purity)
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

pub trait PVarIds: PartialEq + Eq + Clone + std::fmt::Debug + std::hash::Hash + Sized {
    fn monomorphic() -> Self;
    fn is_monomorphic(&self) -> bool;
}

impl PVarIds for Range<PVarId> {
    fn monomorphic() -> Range<PVarId> {
        PVarId::new(0)..PVarId::new(0)
    }

    fn is_monomorphic(&self) -> bool {
        self.start >= self.end
    }
}

#[derive(PartialEq, Eq, Debug, Hash, Clone)]
pub struct EmptyPVarIds();

impl PVarIds for EmptyPVarIds {
    fn monomorphic() -> EmptyPVarIds {
        EmptyPVarIds()
    }

    fn is_monomorphic(&self) -> bool {
        true
    }
}
