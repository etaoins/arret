use crate::hir::error::{Error, ErrorKind, Result};
use crate::hir::ns::{Ident, NsDataIter, NsDatum};
use syntax::span::Span;

/// Removes the rest argument from the passed iterator and returns it
///
/// The rest argument is denoted by using `&` before a final datum
pub fn try_take_rest_arg(data_iter: &mut NsDataIter) -> Option<NsDatum> {
    let data_len = data_iter.len();
    if data_len < 2 {
        return None;
    }

    // This is gross because we need to "peek" at the end of the iterator
    if let NsDatum::Ident(_, ident) = &data_iter.as_slice()[data_len - 2] {
        if ident.is_ampersand() {
            let rest = data_iter.next_back();
            // Remove the & completely
            data_iter.next_back();
            return rest;
        }
    }

    None
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
