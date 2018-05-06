use std;

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

#[derive(Debug, PartialEq, Eq, Clone, Copy, Hash)]
pub struct PVarId(u32);

impl PVarId {
    pub fn new(id: usize) -> PVarId {
        PVarId(id as u32)
    }
}

#[derive(Debug, PartialEq, Eq, Clone, Copy, Hash)]
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

#[derive(Debug, PartialEq, Eq, Clone, Copy, Hash)]
pub struct FreePurityId(u32);

impl FreePurityId {
    pub fn new(id: usize) -> FreePurityId {
        FreePurityId(id as u32)
    }
}
