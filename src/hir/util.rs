use hir::ns::{Ident, NsDatum};
use hir::scope::{Binding, Scope};
use hir::prim::Prim;
use hir::error::{Error, ErrorKind, Result};
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

/// Splits data in an optional start argument and fixed arguments
///
/// The start argument is denoted by using `...` after the first element
pub fn split_into_start_and_fixed(
    scope: &Scope,
    mut vs: Vec<NsDatum>,
) -> (Option<NsDatum>, Vec<NsDatum>) {
    let has_start = if vs.len() >= 2 {
        scope.get_datum(&vs[1]) == Some(Binding::Prim(Prim::Ellipsis))
    } else {
        false
    };

    if has_start {
        let fixed_data = vs.split_off(2);
        // Remove the ellipsis completely
        vs.pop();

        (Some(vs.pop().unwrap()), fixed_data)
    } else {
        (None, vs)
    }
}

pub fn pop_vec_front<T>(mut vs: Vec<T>) -> (T, Vec<T>) {
    let rest_vs = vs.split_off(1);
    (vs.pop().unwrap(), rest_vs)
}

pub fn expect_arg_count(span: Span, vs: &Vec<NsDatum>, expected_arg_count: usize) -> Result<()> {
    if vs.len() != expected_arg_count {
        Err(Error::new(
            span,
            ErrorKind::WrongArgCount(expected_arg_count),
        ))
    } else {
        Ok(())
    }
}

pub fn expect_ident(datum: NsDatum) -> Result<Ident> {
    if let NsDatum::Ident(_, ident) = datum {
        Ok(ident)
    } else {
        Err(Error::new(datum.span(), ErrorKind::ExpectedSymbol))
    }
}
