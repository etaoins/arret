use crate::id_type::RcId;

#[derive(Copy, Clone, PartialEq, Eq, Hash, Debug)]
pub enum Purity {
    Pure,
    Impure,
}

#[derive(PartialEq, Eq, Debug, Hash, Clone)]
pub struct PVar {
    source_name: Box<str>,
}

pub type PVarId = RcId<PVar>;
pub type PVarIds = Vec<PVarId>;

impl PVar {
    pub fn new(source_name: Box<str>) -> PVar {
        PVar { source_name }
    }

    pub fn source_name(&self) -> &str {
        &self.source_name
    }
}

#[derive(Debug, PartialEq, Eq, Clone, Hash)]
pub enum Ref {
    Fixed(Purity),
    Var(PVarId),
}

impl From<Purity> for Ref {
    fn from(purity: Purity) -> Self {
        Ref::Fixed(purity)
    }
}
