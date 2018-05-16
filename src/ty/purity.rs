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
}

pub trait PRef: PartialEq + Eq + Clone + std::fmt::Debug + std::hash::Hash + Sized {
    fn from_purity(purity: Purity) -> Self;
}

impl PRef for Purity {
    fn from_purity(purity: Purity) -> Purity {
        purity
    }
}

#[derive(Debug, PartialEq, Eq, Clone, Copy, Hash, PartialOrd)]
pub struct PVarId(u32);

impl PVarId {
    pub fn new(id: usize) -> PVarId {
        PVarId(id as u32)
    }

    pub fn to_usize(self) -> usize {
        self.0 as usize
    }
}

#[derive(PartialEq, Eq, Debug, Hash, Clone)]
pub struct PVar {
    source_name: String,
}

impl PVar {
    pub fn new(source_name: String) -> PVar {
        PVar { source_name }
    }

    pub fn source_name(&self) -> &String {
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
        match self {
            Poly::Fixed(fixed) => Decl::Fixed(fixed),
            Poly::Var(var) => Decl::Var(var),
        }
    }
}

impl PRef for Poly {
    fn from_purity(purity: Purity) -> Poly {
        Poly::Fixed(purity)
    }
}

/// Decl is a purity declared by a user
///
/// By analogy with `ty::Decl` this allows an additional `Free` type where the user has not
/// specified a purity.
#[derive(PartialEq, Eq, Debug, Clone)]
pub enum Decl {
    Fixed(Purity),
    Var(PVarId),
    Free(FreePurityId),
}

impl Decl {
    pub fn try_to_poly(&self) -> Option<Poly> {
        match self {
            Decl::Fixed(fixed) => Some(Poly::Fixed(*fixed)),
            Decl::Var(pvar_id) => Some(Poly::Var(*pvar_id)),
            Decl::Free(_) => None,
        }
    }
}

#[derive(Debug, PartialEq, Eq, Clone, Copy, Hash)]
pub struct FreePurityId(u32);

impl FreePurityId {
    pub fn new(id: usize) -> FreePurityId {
        FreePurityId(id as u32)
    }
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
