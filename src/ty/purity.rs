use std;

#[derive(Copy, Clone, PartialEq, Eq, Hash, Debug)]
pub enum Purity {
    Pure,
    Impure,
}

pub trait PRef: PartialEq + Eq + Clone + std::fmt::Debug + std::hash::Hash + Sized {
    fn from_purity(purity: Purity) -> Self;
}

impl PRef for Purity {
    fn from_purity(purity: Purity) -> Purity {
        purity
    }
}

/// Decl is a purity declared by a user
///
/// By analogy with `ty::Decl` this allows an additional `Free` type where the user has not
/// specified a purity.
#[derive(PartialEq, Eq, Debug, Clone)]
pub enum Decl {
    Fixed(Purity),
    Free(FreePurityId),
}

#[derive(Debug, PartialEq, Eq, Clone, Copy, Hash)]
pub struct FreePurityId(u32);

impl FreePurityId {
    pub fn new(id: usize) -> FreePurityId {
        FreePurityId(id as u32)
    }
}
