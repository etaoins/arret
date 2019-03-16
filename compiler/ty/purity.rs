use syntax::span::Span;

use crate::id_type::RcId;

#[derive(Copy, Clone, PartialEq, Eq, Hash, Debug)]
pub enum Purity {
    Pure,
    Impure,
}

#[derive(PartialEq, Eq, Debug, Hash, Clone)]
pub struct PVar {
    span: Span,
    source_name: Box<str>,
}

pub type PVarId = RcId<PVar>;
pub type PVarIds = Vec<PVarId>;

impl PVar {
    pub fn new(span: Span, source_name: Box<str>) -> PVar {
        PVar { span, source_name }
    }

    pub fn span(&self) -> Span {
        self.span
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
