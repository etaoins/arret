use std::collections::HashMap;

use hir::scope::{Binding, Scope};
use hir::ns::{Ident, NsDatum};
use hir::prim::Prim;
use ty;
use hir::error::{Error, ErrorKind, Result};
use hir::util::{expect_arg_count, expect_ident, split_into_fixed_and_rest,
                split_into_start_and_fixed};
use syntax::span::Span;

#[derive(Debug, PartialEq, Eq, Hash, Clone, Copy)]
pub enum TyCons {
    List,
    Listof,
    Vector,
    Vectorof,
    Fun,
    ImpureFun,
    Set,
    Hash,
    Union,
    #[cfg(test)]
    RawU,
}

pub fn lower_pvar(scope: &Scope, pvar_datum: NsDatum) -> Result<(Ident, ty::PVar)> {
    let span = pvar_datum.span();

    match pvar_datum {
        NsDatum::Ident(_, ident) => {
            let source_name = ident.name().clone();
            return Ok((ident, ty::PVar::new(source_name, ty::Ty::Any.into_poly())));
        }
        NsDatum::Vec(_, mut arg_data) => {
            if arg_data.len() == 3
                && scope.get_datum(&arg_data[1]) == Some(Binding::Prim(Prim::TyColon))
            {
                let bound_datum = arg_data.pop().unwrap();
                let bound_ty = lower_ty(scope, bound_datum)?;

                // Discard the : completely
                arg_data.pop();

                let pvar_ident = expect_ident(arg_data.pop().unwrap())?;

                let source_name = pvar_ident.name().clone();
                return Ok((pvar_ident, ty::PVar::new(source_name, bound_ty)));
            }
        }
        _ => {}
    }

    Err(Error::new(
        span,
        ErrorKind::IllegalArg(
            "polymorphic variables must be either an identifier or [identifier : Type]".to_owned(),
        ),
    ))
}

fn lower_list_cons(scope: &Scope, arg_data: Vec<NsDatum>) -> Result<ty::Poly> {
    let (fixed, rest) = split_into_fixed_and_rest(scope, arg_data);

    let fixed_tys = fixed
        .into_iter()
        .map(|arg_datum| lower_ty(scope, arg_datum))
        .collect::<Result<Vec<ty::Poly>>>()?;

    let rest_ty = match rest {
        Some(rest) => Some(Box::new(lower_ty(scope, rest)?)),
        None => None,
    };

    Ok(ty::Ty::List(fixed_tys, rest_ty).into_poly())
}

fn lower_vec_cons(scope: &Scope, arg_data: Vec<NsDatum>) -> Result<ty::Poly> {
    let (start, fixed) = split_into_start_and_fixed(scope, arg_data);

    let fixed_tys = fixed
        .into_iter()
        .map(|arg_datum| lower_ty(scope, arg_datum))
        .collect::<Result<Vec<ty::Poly>>>()?;

    let start_ty = match start {
        Some(start) => Some(Box::new(lower_ty(scope, start)?)),
        None => None,
    };

    Ok(ty::Ty::Vec(start_ty, fixed_tys).into_poly())
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

    let ret_ty = lower_ty(scope, arg_data.pop().unwrap())?;
    let params_ty = lower_list_cons(scope, arg_data)?;

    Ok(ty::Ty::new_fun(impure, params_ty, ret_ty).into_poly())
}

fn lower_infix_fun_cons(
    scope: &Scope,
    impure: bool,
    mut arg_data: Vec<NsDatum>,
) -> Result<ty::Poly> {
    let ret_ty = lower_ty(scope, arg_data.pop().unwrap())?;

    // Discard the constructor
    arg_data.pop();

    let params_ty = lower_list_cons(scope, arg_data)?;

    Ok(ty::Ty::new_fun(impure, params_ty, ret_ty).into_poly())
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
            let rest_ty = lower_ty(scope, arg_data.pop().unwrap())?;
            Ok(ty::Ty::List(vec![], Some(Box::new(rest_ty))).into_poly())
        }
        TyCons::Vector => lower_vec_cons(scope, arg_data),
        TyCons::Vectorof => {
            expect_arg_count(span, &arg_data, 1)?;
            let start_ty = lower_ty(scope, arg_data.pop().unwrap())?;
            Ok(ty::Ty::Vec(Some(Box::new(start_ty)), vec![]).into_poly())
        }
        TyCons::Fun => lower_fun_cons(scope, span, "->", false, arg_data),
        TyCons::ImpureFun => lower_fun_cons(scope, span, "->!", true, arg_data),
        TyCons::Set => {
            expect_arg_count(span, &arg_data, 1)?;
            let member_ty = lower_ty(scope, arg_data.pop().unwrap())?;
            Ok(ty::Ty::Set(Box::new(member_ty)).into_poly())
        }
        TyCons::Hash => {
            expect_arg_count(span, &arg_data, 2)?;
            let value_ty = lower_ty(scope, arg_data.pop().unwrap())?;
            let key_ty = lower_ty(scope, arg_data.pop().unwrap())?;
            Ok(ty::Ty::Hash(Box::new(key_ty), Box::new(value_ty)).into_poly())
        }
        TyCons::Union => {
            let member_tys = arg_data
                .into_iter()
                .map(|arg_datum| lower_ty(scope, arg_datum))
                .collect::<Result<Vec<ty::Poly>>>()?;

            ty::unify::poly_unify_iter(&[], member_tys.iter()).map_err(|err| match err {
                ty::unify::Error::Erased(left, right) => {
                    Error::new(span, ErrorKind::TypeErased(left, right))
                }
                ty::unify::Error::Unnatural(_, _) => {
                    panic!("Unnatural types should be allowed");
                }
            })
        }
        #[cfg(test)]
        TyCons::RawU => {
            // This performs a union *without* unifying the types. This is used when testing the
            // union code itself
            let member_tys = arg_data
                .into_iter()
                .map(|arg_datum| lower_ty(scope, arg_datum))
                .collect::<Result<Vec<ty::Poly>>>()?;

            Ok(ty::Ty::Union(member_tys).into_poly())
        }
    }
}

fn lower_literal(datum: NsDatum) -> Result<ty::Poly> {
    match datum {
        NsDatum::Bool(_, v) => Ok(ty::Ty::LitBool(v).into_poly()),
        NsDatum::Ident(_, ident) => Ok(ty::Ty::LitSym(ident.name().clone()).into_poly()),
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
            ErrorKind::UnboundSymbol(ident.into_name()),
        )),
    }
}

pub fn lower_ty(scope: &Scope, datum: NsDatum) -> Result<ty::Poly> {
    match datum {
        NsDatum::List(span, mut vs) => {
            if vs.is_empty() {
                return Ok(ty::Ty::List(vec![], None).into_poly());
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

    export_ty!("Any", ty::Ty::Any.into_poly());

    export_ty!("Bool", ty::Ty::Bool.into_poly());
    export_ty!("Symbol", ty::Ty::Sym.into_poly());
    export_ty!("String", ty::Ty::Str.into_poly());
    export_ty!("Int", ty::Ty::Int.into_poly());
    export_ty!("Float", ty::Ty::Float.into_poly());
    export_ty!("Char", ty::Ty::Char.into_poly());

    export_ty_cons!("List", TyCons::List);
    export_ty_cons!("Listof", TyCons::Listof);
    export_ty_cons!("Vector", TyCons::Vector);
    export_ty_cons!("Vectorof", TyCons::Vectorof);
    export_ty_cons!("->", TyCons::Fun);
    export_ty_cons!("->!", TyCons::ImpureFun);
    export_ty_cons!("Setof", TyCons::Set);
    export_ty_cons!("Hash", TyCons::Hash);
    export_ty_cons!("U", TyCons::Union);

    #[cfg(test)]
    export_ty_cons!("RawU", TyCons::RawU);
}

#[cfg(test)]
pub fn ty_for_str(datum_str: &str) -> Result<ty::Poly> {
    use hir::ns::NsId;
    use syntax::parser::datum_from_str;
    use hir::prim::insert_prim_exports;

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

    lower_ty(&scope, NsDatum::from_syntax_datum(test_ns_id, test_datum))
}

#[cfg(test)]
mod test {
    use super::*;
    use syntax::span::t2s;

    fn assert_ty_for_str(expected: ty::Poly, datum_str: &str) {
        assert_eq!(expected, ty_for_str(datum_str).unwrap());
    }

    fn assert_err_for_str(err: Error, datum_str: &str) {
        assert_eq!(err, ty_for_str(datum_str).unwrap_err());
    }

    #[test]
    fn true_literal() {
        let j = "true";

        let expected = ty::Ty::LitBool(true).into_poly();
        assert_ty_for_str(expected, j);
    }

    #[test]
    fn false_literal() {
        let j = "false";

        let expected = ty::Ty::LitBool(false).into_poly();
        assert_ty_for_str(expected, j);
    }

    #[test]
    fn sym_literal() {
        let j = "'foo";

        let expected = ty::Ty::LitSym("foo".to_owned()).into_poly();
        assert_ty_for_str(expected, j);
    }

    #[test]
    fn empty_list_literal() {
        let j = "()";

        let expected = ty::Ty::List(vec![], None).into_poly();
        assert_ty_for_str(expected, j);
    }

    #[test]
    fn ty_ref() {
        let j = "Symbol";

        let expected = ty::Ty::Sym.into_poly();
        assert_ty_for_str(expected, j);
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

        let inner_ty = ty::Ty::LitBool(true).into_poly();
        let expected = ty::Ty::List(vec![], Some(Box::new(inner_ty))).into_poly();

        assert_ty_for_str(expected, j);
    }

    #[test]
    fn fixed_list_cons() {
        let j = "(List true false)";

        let expected = ty::Ty::List(
            vec![
                ty::Ty::LitBool(true).into_poly(),
                ty::Ty::LitBool(false).into_poly(),
            ],
            None,
        ).into_poly();

        assert_ty_for_str(expected, j);
    }

    #[test]
    fn rest_list_cons() {
        let j = "(List true false ...)";

        let expected = ty::Ty::List(
            vec![ty::Ty::LitBool(true).into_poly()],
            Some(Box::new(ty::Ty::LitBool(false).into_poly())),
        ).into_poly();

        assert_ty_for_str(expected, j);
    }

    #[test]
    fn vectorof_cons() {
        let j = "(Vectorof true)";

        let inner_ty = ty::Ty::LitBool(true).into_poly();
        let expected = ty::Ty::Vec(Some(Box::new(inner_ty)), vec![]).into_poly();

        assert_ty_for_str(expected, j);
    }

    #[test]
    fn fixed_vector_cons() {
        let j = "(Vector true false)";

        let expected = ty::Ty::Vec(
            None,
            vec![
                ty::Ty::LitBool(true).into_poly(),
                ty::Ty::LitBool(false).into_poly(),
            ],
        ).into_poly();

        assert_ty_for_str(expected, j);
    }

    #[test]
    fn rest_vector_cons() {
        let j = "(Vector false ... true)";

        let expected = ty::Ty::Vec(
            Some(Box::new(ty::Ty::LitBool(false).into_poly())),
            vec![ty::Ty::LitBool(true).into_poly()],
        ).into_poly();

        assert_ty_for_str(expected, j);
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

        let expected = ty::Ty::new_fun(
            false,
            ty::Ty::List(vec![], None).into_poly(),
            ty::Ty::LitBool(true).into_poly(),
        ).into_poly();

        assert_ty_for_str(expected, j);
    }

    #[test]
    fn impure_fun() {
        let j = "(->! true)";

        let expected = ty::Ty::new_fun(
            true,
            ty::Ty::List(vec![], None).into_poly(),
            ty::Ty::LitBool(true).into_poly(),
        ).into_poly();

        assert_ty_for_str(expected, j);
    }

    #[test]
    fn fixed_fun() {
        let j = "(-> false true)";

        let expected = ty::Ty::new_fun(
            false,
            ty::Ty::List(vec![ty::Ty::LitBool(false).into_poly()], None).into_poly(),
            ty::Ty::LitBool(true).into_poly(),
        ).into_poly();

        assert_ty_for_str(expected, j);
    }

    #[test]
    fn rest_fun() {
        let j = "(-> Symbol ... true)";

        let expected = ty::Ty::new_fun(
            false,
            ty::Ty::List(vec![], Some(Box::new(ty::Ty::Sym.into_poly()))).into_poly(),
            ty::Ty::LitBool(true).into_poly(),
        ).into_poly();

        assert_ty_for_str(expected, j);
    }

    #[test]
    fn fixed_infix_fun() {
        let j = "(false -> true)";

        let expected = ty::Ty::new_fun(
            false,
            ty::Ty::List(vec![ty::Ty::LitBool(false).into_poly()], None).into_poly(),
            ty::Ty::LitBool(true).into_poly(),
        ).into_poly();

        assert_ty_for_str(expected, j);
    }

    #[test]
    fn rest_infix_impure_fun() {
        let j = "(String Symbol ... ->! true)";

        let expected = ty::Ty::new_fun(
            true,
            ty::Ty::List(
                vec![ty::Ty::Str.into_poly()],
                Some(Box::new(ty::Ty::Sym.into_poly())),
            ).into_poly(),
            ty::Ty::LitBool(true).into_poly(),
        ).into_poly();

        assert_ty_for_str(expected, j);
    }

    #[test]
    fn set_cons() {
        let j = "(Setof true)";

        let inner_ty = ty::Ty::LitBool(true).into_poly();
        let expected = ty::Ty::Set(Box::new(inner_ty)).into_poly();

        assert_ty_for_str(expected, j);
    }

    #[test]
    fn hash_cons() {
        let j = "(Hash true false)";

        let key_ty = ty::Ty::LitBool(true);
        let value_ty = ty::Ty::LitBool(false);
        let expected =
            ty::Ty::Hash(Box::new(key_ty.into_poly()), Box::new(value_ty.into_poly())).into_poly();

        assert_ty_for_str(expected, j);
    }

    #[test]
    fn union_cons() {
        let j = "(U true false)";
        let expected = ty::Ty::Bool.into_poly();

        assert_ty_for_str(expected, j);
    }

    #[test]
    fn erased_union_cons() {
        let j = "(U (-> Symbol) (-> String))";
        let t = "^^^^^^^^^^^^^^^^^^^^^^^^^^^";

        let left = ty::Ty::new_fun(
            false,
            ty::Ty::List(vec![], None).into_poly(),
            ty::Ty::Sym.into_poly(),
        ).into_poly();

        let right = ty::Ty::new_fun(
            false,
            ty::Ty::List(vec![], None).into_poly(),
            ty::Ty::Str.into_poly(),
        ).into_poly();

        let err = Error::new(t2s(t), ErrorKind::TypeErased(left, right));
        assert_err_for_str(err, j);
    }
}
