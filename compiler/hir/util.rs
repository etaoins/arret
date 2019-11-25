use arret_syntax::datum::{DataStr, Datum};
use arret_syntax::span::Span;

use crate::hir::error::{Error, ErrorKind, ExpectedSym, Result};
use crate::hir::ns::{Ident, NsDataIter, NsDatum};

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

pub fn expect_spanned_ns_ident(datum: NsDatum, usage: &'static str) -> Result<(Span, Ident)> {
    if let NsDatum::Ident(span, ident) = datum {
        Ok((span, ident))
    } else {
        Err(Error::new(
            datum.span(),
            ErrorKind::ExpectedSym(
                ExpectedSym {
                    found: datum.description(),
                    usage,
                }
                .into(),
            ),
        ))
    }
}

pub fn expect_ns_ident(datum: NsDatum, usage: &'static str) -> Result<Ident> {
    expect_spanned_ns_ident(datum, usage).map(|(_, ident)| ident)
}

pub fn expect_spanned_ident<'a>(
    datum: &'a Datum,
    usage: &'static str,
) -> Result<(Span, &'a DataStr)> {
    if let Datum::Sym(span, name) = datum {
        if !name.starts_with(':') {
            return Ok((*span, name));
        }
    }

    Err(Error::new(
        datum.span(),
        ErrorKind::ExpectedSym(
            ExpectedSym {
                found: datum.description(),
                usage,
            }
            .into(),
        ),
    ))
}

pub fn expect_ident<'a>(datum: &'a Datum, usage: &'static str) -> Result<&'a DataStr> {
    expect_spanned_ident(datum, usage).map(|(_, ident)| ident)
}
