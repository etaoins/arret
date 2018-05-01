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
