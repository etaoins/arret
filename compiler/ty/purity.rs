use std::collections::BTreeMap;

#[derive(Copy, Clone, PartialEq, Eq, Hash, Debug)]
pub enum Purity {
    Pure,
    Impure,
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
pub enum Ref {
    Fixed(Purity),
    Var(PVarId),
}

impl From<Purity> for Ref {
    fn from(purity: Purity) -> Self {
        Ref::Fixed(purity)
    }
}
