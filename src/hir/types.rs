use std::collections::HashMap;

use hir::scope::{Binding, Ident, NsDatum, Prim, Scope};
use ty;
use hir::error::{Error, ErrorKind, Result};
use hir::util::split_into_fixed_and_rest;
use syntax::span::Span;

#[derive(Debug, PartialEq, Eq, Hash, Clone)]
pub enum TyCons {
    List,
    Listof,
}

fn lower_ty_cons_apply(
    scope: &Scope,
    span: Span,
    ty_cons: TyCons,
    mut arg_data: Vec<NsDatum>,
) -> Result<ty::PTy> {
    match ty_cons {
        TyCons::List => {
            let (fixed, rest) = split_into_fixed_and_rest(scope, arg_data);

            let fixed_tys = fixed
                .into_iter()
                .map(|arg_datum| lower_pty(scope, arg_datum))
                .collect::<Result<Vec<ty::PTy>>>()?;

            let rest_ty = match rest {
                Some(rest) => Some(lower_pty(scope, rest)?),
                None => None,
            };

            Ok(ty::NonFun::List(fixed_tys, rest_ty).into())
        }
        TyCons::Listof => {
            if arg_data.len() != 1 {
                return Err(Error::new(span, ErrorKind::WrongArgCount(1)));
            }

            let rest_ty = lower_pty(scope, arg_data.pop().unwrap())?;
            Ok(ty::NonFun::List(vec![], Some(rest_ty)).into())
        }
    }
}

fn lower_literal(datum: NsDatum) -> Result<ty::PTy> {
    match datum {
        NsDatum::Bool(_, v) => Ok(ty::NonFun::Bool(v).into()),
        NsDatum::Ident(_, ident) => Ok(ty::NonFun::Sym(ident.name().clone()).into()),
        _ => Err(Error::new(
            datum.span(),
            ErrorKind::IllegalArg("only boolean and symbol literals are supported".to_owned()),
        )),
    }
}

fn lower_ident(scope: &Scope, span: Span, ident: Ident) -> Result<ty::PTy> {
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

            if let NsDatum::Ident(ident_span, ref ident) = fn_datum {
                match scope.get(ident) {
                    Some(Binding::Prim(Prim::Quote)) => {
                        if arg_data.len() != 1 {
                            return Err(Error::new(span, ErrorKind::WrongArgCount(1)));
                        }

                        return lower_literal(arg_data.pop().unwrap());
                    }
                    Some(Binding::TyCons(ty_cons)) => {
                        return lower_ty_cons_apply(scope, span, ty_cons, arg_data);
                    }
                    None => {
                        return Err(Error::new(
                            ident_span,
                            ErrorKind::UnboundSymbol(ident.name().clone()),
                        ));
                    }
                    Some(_) => {}
                }
            }

            Err(Error::new(
                fn_datum.span(),
                ErrorKind::IllegalArg("type constructor expected".to_owned()),
            ))
        }
        NsDatum::Ident(span, ident) => lower_ident(scope, span, ident),
        _ => lower_literal(datum),
    }
}

pub fn insert_ty_exports(exports: &mut HashMap<String, Binding>) {
    macro_rules! export_ty {
        ( $name:expr, $type:expr) => {
            exports.insert($name.to_owned(), Binding::Ty($type));
        }
    }

    macro_rules! export_ty_cons {
        ( $name:expr, $ty_cons:expr) => {
            exports.insert($name.to_owned(), Binding::TyCons($ty_cons));
        }
    }

    export_ty!(
        "Bool",
        union![ty::NonFun::Bool(false), ty::NonFun::Bool(true)]
    );

    export_ty_cons!("List", TyCons::List);
    export_ty_cons!("Listof", TyCons::Listof);
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

#[test]
fn unbound_cons() {
    let j = "(notbound)";
    let t = " ^^^^^^^^ ";

    let err = Error::new(t2s(t), ErrorKind::UnboundSymbol("notbound".to_owned()));
    assert_err_for_str(err, j);
}

#[test]
fn listof_cons() {
    let j = "(Listof true)";

    let inner_ty = ty::NonFun::Bool(true);
    let expected = ty::NonFun::List(vec![], Some(inner_ty.into()));

    assert_ty_for_str(expected.into(), j);
}

#[test]
fn fixed_list_cons() {
    let j = "(List true false)";

    let expected = ty::NonFun::List(
        vec![
            ty::NonFun::Bool(true).into(),
            ty::NonFun::Bool(false).into(),
        ],
        None,
    );

    assert_ty_for_str(expected.into(), j);
}

#[test]
fn rest_list_cons() {
    let j = "(List true false ...)";

    let expected = ty::NonFun::List(
        vec![ty::NonFun::Bool(true).into()],
        Some(ty::NonFun::Bool(false).into()),
    );

    assert_ty_for_str(expected.into(), j);
}
