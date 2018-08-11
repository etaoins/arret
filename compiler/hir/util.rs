use crate::hir::error::{Error, ErrorKind, Result};
use crate::hir::ns::{Ident, NsDataIter, NsDatum};
use crate::hir::prim::Prim;
use crate::hir::scope::{Binding, Scope};
use syntax::span::Span;

/// Removes the rest argument from the passed iterator and returns it
///
/// The rest argument is denoted by using `...` at the end of the data
pub fn try_take_rest_arg(scope: &Scope, data_iter: &mut NsDataIter) -> Option<NsDatum> {
    let data_len = data_iter.len();

    // This is gross becase we need to "peek" at the end of the iterator
    let has_rest = data_len >= 2
        && scope.get_datum(&data_iter.as_slice()[data_len - 1])
            == Some(Binding::Prim(Prim::Ellipsis));

    if has_rest {
        // Remove the ellipsis completely
        data_iter.next_back();
        Some(data_iter.next_back().unwrap())
    } else {
        None
    }
}

pub fn expect_arg_count(
    span: Span,
    expected_arg_count: usize,
    actual_arg_count: usize,
) -> Result<()> {
    if actual_arg_count != expected_arg_count {
        Err(Error::new(
            span,
            ErrorKind::WrongArgCount(expected_arg_count),
        ))
    } else {
        Ok(())
    }
}

pub fn expect_one_arg(span: Span, mut iter: NsDataIter) -> Result<NsDatum> {
    expect_arg_count(span, 1, iter.len())?;
    Ok(iter.next().unwrap())
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
