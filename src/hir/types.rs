use std::collections::HashMap;

use hir::scope::{Binding, Ident, NsDatum, Prim, Scope};
use ty;
use hir::error::{Error, ErrorKind, Result};
use hir::util::{expect_arg_count, split_into_fixed_and_rest, split_into_start_and_fixed};
use syntax::span::Span;

#[derive(Debug, PartialEq, Eq, Hash, Clone)]
pub enum TyCons {
    List,
    Listof,
    Vector,
    Vectorof,
    Fun,
    ImpureFun,
    Set,
    Hash,
}

fn lower_list_cons(scope: &Scope, arg_data: Vec<NsDatum>) -> Result<ty::Poly> {
    let (fixed, rest) = split_into_fixed_and_rest(scope, arg_data);

    let fixed_tys = fixed
        .into_iter()
        .map(|arg_datum| lower_pty(scope, arg_datum))
        .collect::<Result<Vec<ty::Poly>>>()?;

    let rest_ty = match rest {
        Some(rest) => Some(lower_pty(scope, rest)?),
        None => None,
    };

    Ok(ty::NonFun::List(fixed_tys, rest_ty).into())
}

fn lower_vec_cons(scope: &Scope, arg_data: Vec<NsDatum>) -> Result<ty::Poly> {
    let (start, fixed) = split_into_start_and_fixed(scope, arg_data);

    let fixed_tys = fixed
        .into_iter()
        .map(|arg_datum| lower_pty(scope, arg_datum))
        .collect::<Result<Vec<ty::Poly>>>()?;

    let start_ty = match start {
        Some(start) => Some(lower_pty(scope, start)?),
        None => None,
    };

    Ok(ty::NonFun::Vec(start_ty, fixed_tys).into())
}

fn lower_fun_cons(
    scope: &Scope,
    span: Span,
    ty_cons_name: &'static str,
    impure: bool,
    mut arg_data: Vec<NsDatum>,
) -> Result<ty::Poly> {
    if arg_data.is_empty() {
        return Err(Error::new(
            span,
            ErrorKind::IllegalArg(format!("{} requires at least one argument", ty_cons_name)),
        ));
    }

    let ret_ty = lower_pty(scope, arg_data.pop().unwrap())?;
    let params_ty = lower_list_cons(scope, arg_data)?;

    Ok(ty::Fun::new(impure, params_ty, ret_ty).into())
}

fn lower_infix_fun_cons(
    scope: &Scope,
    impure: bool,
    mut arg_data: Vec<NsDatum>,
) -> Result<ty::Poly> {
    let ret_ty = lower_pty(scope, arg_data.pop().unwrap())?;

    // Discard the constructor
    arg_data.pop();

    let params_ty = lower_list_cons(scope, arg_data)?;

    Ok(ty::Fun::new(impure, params_ty, ret_ty).into())
}

fn lower_ty_cons_apply(
    scope: &Scope,
    span: Span,
    ty_cons: TyCons,
    mut arg_data: Vec<NsDatum>,
) -> Result<ty::Poly> {
    match ty_cons {
        TyCons::List => lower_list_cons(scope, arg_data),
        TyCons::Listof => {
            expect_arg_count(span, &arg_data, 1)?;
            let rest_ty = lower_pty(scope, arg_data.pop().unwrap())?;
            Ok(ty::NonFun::List(vec![], Some(rest_ty)).into())
        }
        TyCons::Vector => lower_vec_cons(scope, arg_data),
        TyCons::Vectorof => {
            expect_arg_count(span, &arg_data, 1)?;
            let start_ty = lower_pty(scope, arg_data.pop().unwrap())?;
            Ok(ty::NonFun::Vec(Some(start_ty), vec![]).into())
        }
        TyCons::Fun => lower_fun_cons(scope, span, "->", false, arg_data),
        TyCons::ImpureFun => lower_fun_cons(scope, span, "->!", true, arg_data),
        TyCons::Set => {
            expect_arg_count(span, &arg_data, 1)?;
            let member_ty = lower_pty(scope, arg_data.pop().unwrap())?;
            Ok(ty::NonFun::Set(member_ty).into())
        }
        TyCons::Hash => {
            expect_arg_count(span, &arg_data, 2)?;
            let value_ty = lower_pty(scope, arg_data.pop().unwrap())?;
            let key_ty = lower_pty(scope, arg_data.pop().unwrap())?;
            Ok(ty::NonFun::Hash(key_ty, value_ty).into())
        }
    }
}

fn lower_literal(datum: NsDatum) -> Result<ty::Poly> {
    match datum {
        NsDatum::Bool(_, v) => Ok(ty::NonFun::Bool(v).into()),
        NsDatum::Ident(_, ident) => Ok(ty::NonFun::Sym(ident.name().clone()).into()),
        _ => Err(Error::new(
            datum.span(),
            ErrorKind::IllegalArg("only boolean and symbol literals are supported".to_owned()),
        )),
    }
}

fn lower_ident(scope: &Scope, span: Span, ident: Ident) -> Result<ty::Poly> {
    match scope.get(&ident) {
        Some(Binding::Ty(ref ty)) => Ok(ty.clone()),
        Some(_) => Err(Error::new(span, ErrorKind::ValueAsTy)),
        None => Err(Error::new(
            span,
            ErrorKind::UnboundSymbol(ident.name().clone()),
        )),
    }
}

pub fn lower_pty(scope: &Scope, datum: NsDatum) -> Result<ty::Poly> {
    match datum {
        NsDatum::List(span, mut vs) => {
            if vs.len() == 0 {
                return Ok(ty::NonFun::List(vec![], None).into());
            }

            if vs.len() >= 3 {
                match scope.get_datum(&vs[vs.len() - 2]) {
                    Some(Binding::TyCons(TyCons::Fun)) => {
                        return lower_infix_fun_cons(scope, false, vs);
                    }
                    Some(Binding::TyCons(TyCons::ImpureFun)) => {
                        return lower_infix_fun_cons(scope, true, vs);
                    }
                    _ => {}
                };
            }

            let mut arg_data = vs.split_off(1);
            let fn_datum = vs.pop().unwrap();

            if let NsDatum::Ident(ident_span, ref ident) = fn_datum {
                match scope.get(ident) {
                    Some(Binding::Prim(Prim::Quote)) => {
                        expect_arg_count(span, &arg_data, 1)?;
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
        union_ty![ty::NonFun::Bool(false), ty::NonFun::Bool(true)]
    );
    export_ty!("Symbol", ty::NonFun::AnySym.into());
    export_ty!("String", ty::NonFun::Str.into());
    export_ty!("Int", ty::NonFun::Int.into());
    export_ty!("Float", ty::NonFun::Float.into());
    export_ty!("Char", ty::NonFun::Char.into());

    export_ty_cons!("List", TyCons::List);
    export_ty_cons!("Listof", TyCons::Listof);
    export_ty_cons!("Vector", TyCons::Vector);
    export_ty_cons!("Vectorof", TyCons::Vectorof);
    export_ty_cons!("->", TyCons::Fun);
    export_ty_cons!("->!", TyCons::ImpureFun);
    export_ty_cons!("Setof", TyCons::Set);
    export_ty_cons!("Hash", TyCons::Hash);
}

#[cfg(test)]
use syntax::parser::datum_from_str;
#[cfg(test)]
use hir::scope::{insert_prim_exports, NsId};
#[cfg(test)]
use syntax::span::t2s;

#[cfg(test)]
fn pty_for_str(datum_str: &str) -> Result<ty::Poly> {
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
fn assert_ty_for_str(expected: ty::Poly, datum_str: &str) {
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
    let j = "Symbol";

    let expected = ty::NonFun::AnySym;
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

#[test]
fn vectorof_cons() {
    let j = "(Vectorof true)";

    let inner_ty = ty::NonFun::Bool(true);
    let expected = ty::NonFun::Vec(Some(inner_ty.into()), vec![]);

    assert_ty_for_str(expected.into(), j);
}

#[test]
fn fixed_vector_cons() {
    let j = "(Vector true false)";

    let expected = ty::NonFun::Vec(
        None,
        vec![
            ty::NonFun::Bool(true).into(),
            ty::NonFun::Bool(false).into(),
        ],
    );

    assert_ty_for_str(expected.into(), j);
}

#[test]
fn rest_vector_cons() {
    let j = "(Vector false ... true)";

    let expected = ty::NonFun::Vec(
        Some(ty::NonFun::Bool(false).into()),
        vec![ty::NonFun::Bool(true).into()],
    );

    assert_ty_for_str(expected.into(), j);
}

#[test]
fn empty_fun() {
    let j = "(->)";
    let t = "^^^^";

    let err = Error::new(
        t2s(t),
        ErrorKind::IllegalArg("-> requires at least one argument".to_owned()),
    );
    assert_err_for_str(err, j);
}

#[test]
fn pure_fun() {
    let j = "(-> true)";

    let expected = ty::Fun::new(
        false,
        ty::NonFun::List(vec![], None).into(),
        ty::NonFun::Bool(true).into(),
    );

    assert_ty_for_str(expected.into(), j);
}

#[test]
fn impure_fun() {
    let j = "(->! true)";

    let expected = ty::Fun::new(
        true,
        ty::NonFun::List(vec![], None).into(),
        ty::NonFun::Bool(true).into(),
    );

    assert_ty_for_str(expected.into(), j);
}

#[test]
fn fixed_fun() {
    let j = "(-> false true)";

    let expected = ty::Fun::new(
        false,
        ty::NonFun::List(vec![ty::NonFun::Bool(false).into()], None).into(),
        ty::NonFun::Bool(true).into(),
    );

    assert_ty_for_str(expected.into(), j);
}

#[test]
fn rest_fun() {
    let j = "(-> Symbol ... true)";

    let expected = ty::Fun::new(
        false,
        ty::NonFun::List(vec![], Some(ty::NonFun::AnySym.into())).into(),
        ty::NonFun::Bool(true).into(),
    );

    assert_ty_for_str(expected.into(), j);
}

#[test]
fn fixed_infix_fun() {
    let j = "(false -> true)";

    let expected = ty::Fun::new(
        false,
        ty::NonFun::List(vec![ty::NonFun::Bool(false).into()], None).into(),
        ty::NonFun::Bool(true).into(),
    );

    assert_ty_for_str(expected.into(), j);
}

#[test]
fn rest_infix_impure_fun() {
    let j = "(String Symbol ... ->! true)";

    let expected = ty::Fun::new(
        true,
        ty::NonFun::List(
            vec![ty::NonFun::Str.into()],
            Some(ty::NonFun::AnySym.into()),
        ).into(),
        ty::NonFun::Bool(true).into(),
    );

    assert_ty_for_str(expected.into(), j);
}

#[test]
fn set_cons() {
    let j = "(Setof true)";

    let inner_ty = ty::NonFun::Bool(true);
    let expected = ty::NonFun::Set(inner_ty.into());

    assert_ty_for_str(expected.into(), j);
}

#[test]
fn hash_cons() {
    let j = "(Hash true false)";

    let key_ty = ty::NonFun::Bool(true);
    let value_ty = ty::NonFun::Bool(false);
    let expected = ty::NonFun::Hash(key_ty.into(), value_ty.into());

    assert_ty_for_str(expected.into(), j);
}
