use std::collections::HashMap;

use hir::scope::{Binding, Ident, NsDatum, Prim, Scope};
use ty;
use hir::error::{Error, ErrorKind, Result};
use syntax::span::Span;

fn lower_literal_pty(datum: NsDatum) -> Result<ty::PTy> {
    match datum {
        NsDatum::Bool(_, v) => Ok(ty::NonFun::Bool(v).into()),
        NsDatum::Ident(_, ident) => Ok(ty::NonFun::Sym(ident.name().clone()).into()),
        _ => Err(Error::new(
            datum.span(),
            ErrorKind::IllegalArg("only boolean and symbol literals are supported".to_owned()),
        )),
    }
}

fn lower_ident_pty(scope: &Scope, span: Span, ident: Ident) -> Result<ty::PTy> {
    match scope.get(&ident) {
        Some(Binding::Ty(ref ty)) => Ok(ty.clone()),
        Some(_) => Err(Error::new(span, ErrorKind::ValueAsTy)),
        None => Err(Error::new(
            span,
            ErrorKind::UnboundSymbol(ident.name().clone()),
        )),
    }
}

pub fn lower_pty(scope: &Scope, datum: NsDatum) -> Result<ty::PTy> {
    match datum {
        NsDatum::List(span, mut vs) => {
            if vs.len() == 0 {
                return Ok(ty::NonFun::List(vec![], None).into());
            }

            let mut arg_data = vs.split_off(1);
            let fn_datum = vs.pop().unwrap();

            if let NsDatum::Ident(_, ref ident) = fn_datum {
                if scope.get(ident) == Some(Binding::Prim(Prim::Quote)) {
                    if arg_data.len() != 1 {
                        return Err(Error::new(span, ErrorKind::WrongArgCount(1)));
                    }

                    return lower_literal_pty(arg_data.pop().unwrap());
                }
            }

            Err(Error::new(
                fn_datum.span(),
                ErrorKind::IllegalArg("type constructor expected".to_owned()),
            ))
        }
        NsDatum::Ident(span, ident) => lower_ident_pty(scope, span, ident),
        _ => lower_literal_pty(datum),
    }
}

pub fn insert_ty_exports(exports: &mut HashMap<String, Binding>) {
    exports.insert(
        "Bool".to_owned(),
        Binding::Ty(union![ty::NonFun::Bool(false), ty::NonFun::Bool(true)]),
    );
}

#[cfg(test)]
use syntax::parser::datum_from_str;
#[cfg(test)]
use hir::scope::{insert_prim_exports, NsId};
#[cfg(test)]
use syntax::span::t2s;

#[cfg(test)]
fn pty_for_str(datum_str: &str) -> Result<ty::PTy> {
    let test_ns_id = NsId::new(1);

    // Capture our exports
    let mut exports = HashMap::<String, Binding>::new();
    insert_prim_exports(&mut exports);
    insert_ty_exports(&mut exports);

    // Place them on our scope
    let mut scope = Scope::new_empty();
    for (name, binding) in exports.into_iter() {
        scope.insert_binding(Ident::new(test_ns_id, name), binding);
    }

    let test_datum = datum_from_str(datum_str).unwrap();
    lower_pty(&scope, NsDatum::from_value(test_datum, test_ns_id))
}

#[cfg(test)]
fn assert_ty_for_str(expected: ty::PTy, datum_str: &str) {
    assert_eq!(expected, pty_for_str(datum_str).unwrap());
}

#[cfg(test)]
fn assert_err_for_str(err: Error, datum_str: &str) {
    assert_eq!(err, pty_for_str(datum_str).unwrap_err());
}

#[test]
fn true_literal() {
    let j = "true";

    let expected = ty::NonFun::Bool(true);
    assert_ty_for_str(expected.into(), j);
}

#[test]
fn false_literal() {
    let j = "false";

    let expected = ty::NonFun::Bool(false);
    assert_ty_for_str(expected.into(), j);
}

#[test]
fn sym_literal() {
    let j = "'foo";

    let expected = ty::NonFun::Sym("foo".to_owned());
    assert_ty_for_str(expected.into(), j);
}

#[test]
fn empty_list_literal() {
    let j = "()";

    let expected = ty::NonFun::List(vec![], None);
    assert_ty_for_str(expected.into(), j);
}

#[test]
fn ty_ref() {
    let j = "Bool";

    let expected = union![ty::NonFun::Bool(false), ty::NonFun::Bool(true)];
    assert_ty_for_str(expected.into(), j);
}

#[test]
fn unbound_symbol() {
    let j = "notbound";
    let t = "^^^^^^^^";

    let err = Error::new(t2s(t), ErrorKind::UnboundSymbol("notbound".to_owned()));
    assert_err_for_str(err, j);
}

#[test]
fn unsupported_int_literal() {
    let j = "1";
    let t = "^";

    let err = Error::new(
        t2s(t),
        ErrorKind::IllegalArg("only boolean and symbol literals are supported".to_owned()),
    );
    assert_err_for_str(err, j);
}

#[test]
fn non_literal_value_ref() {
    let j = "quote";
    let t = "^^^^^";

    let err = Error::new(t2s(t), ErrorKind::ValueAsTy);
    assert_err_for_str(err, j);
}
