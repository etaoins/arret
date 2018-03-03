use syntax::span::Span;
use hir::scope::{Ident, Macro, NsValue};
use hir::error::Result;

pub fn lower_macro_rules(
    span: Span,
    self_ident: Ident,
    macro_rules_data: &[NsValue],
) -> Result<Macro> {
    unimplemented!("Not implemented!")
}
