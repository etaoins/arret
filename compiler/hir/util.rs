use hir::error::{Error, ErrorKind, Result};
use hir::ns::{Ident, NsDatum};
use hir::prim::Prim;
use hir::scope::{Binding, Scope};
use syntax::span::Span;

/// Splits data in to fixed arguments and an optional rest argument
///
/// The rest argument is denoted by using `...` at the end of the data
pub fn split_into_fixed_and_rest(
    scope: &Scope,
    mut vs: Vec<NsDatum>,
) -> (Vec<NsDatum>, Option<NsDatum>) {
    let has_rest = if vs.len() >= 2 {
        scope.get_datum(&vs[vs.len() - 1]) == Some(Binding::Prim(Prim::Ellipsis))
    } else {
        false
    };

    let rest_datum = if has_rest {
        // Remove the ellipsis completely
        vs.pop();
        Some(vs.pop().unwrap())
    } else {
        None
    };

    (vs, rest_datum)
}

pub fn pop_vec_front<T>(mut vs: Vec<T>) -> (T, Vec<T>) {
    let rest_vs = vs.split_off(1);
    (vs.pop().unwrap(), rest_vs)
}

pub fn expect_arg_count(span: Span, vs: &[NsDatum], expected_arg_count: usize) -> Result<()> {
    if vs.len() != expected_arg_count {
        Err(Error::new(
            span,
            ErrorKind::WrongArgCount(expected_arg_count),
        ))
    } else {
        Ok(())
    }
}

pub fn expect_ident_and_span(datum: NsDatum) -> Result<(Ident, Span)> {
    if let NsDatum::Ident(span, ident) = datum {
        Ok((ident, span))
    } else {
        Err(Error::new(datum.span(), ErrorKind::ExpectedSym))
    }
}

pub fn expect_ident(datum: NsDatum) -> Result<Ident> {
    expect_ident_and_span(datum).map(|(ident, _)| ident)
}
