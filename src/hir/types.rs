use std::collections::HashMap;

use hir::scope::{Binding, Scope};
use hir::ns::{Ident, NsDatum};
use hir::prim::Prim;
use ty;
use hir::error::{Error, ErrorKind, Result};
use hir::util::{expect_arg_count, expect_ident, split_into_fixed_and_rest};
use syntax::span::Span;

#[derive(Debug, PartialEq, Eq, Hash, Clone, Copy)]
pub enum TyCons {
    List,
    Listof,
    Vector,
    Vectorof,
    PureArrow,
    ImpureArrow,
    PureFun,
    ImpureFun,
    Set,
    Map,
    Union,
    Cons,
    #[cfg(test)]
    RawU,
}

struct LowerTyContext<'a> {
    pvars: &'a [ty::PVar],
    scope: &'a Scope,
}

impl<'a> LowerTyContext<'a> {
    fn lower_pvar(&self, pvar_datum: NsDatum) -> Result<(Ident, ty::PVar)> {
        let span = pvar_datum.span();

        match pvar_datum {
            NsDatum::Ident(_, ident) => {
                let source_name = ident.name().clone();
                return Ok((ident, ty::PVar::new(source_name, ty::Ty::Any.into_poly())));
            }
            NsDatum::Vec(_, mut arg_data) => {
                if arg_data.len() == 3
                    && self.scope.get_datum(&arg_data[1]) == Some(Binding::Prim(Prim::TyColon))
                {
                    let bound_datum = arg_data.pop().unwrap();
                    let bound_ty = self.lower_poly(bound_datum)?;

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
                "polymorphic variables must be either an identifier or [identifier : Type]"
                    .to_owned(),
            ),
        ))
    }
    fn lower_list_cons(&self, arg_data: Vec<NsDatum>) -> Result<ty::Poly> {
        let (fixed, rest) = split_into_fixed_and_rest(self.scope, arg_data);

        let mut tail_poly = match rest {
            Some(rest) => ty::Ty::Listof(Box::new(self.lower_poly(rest)?)).into_poly(),
            None => ty::Ty::Nil.into_poly(),
        };

        for fixed_datum in fixed.into_iter().rev() {
            let fixed_poly = self.lower_poly(fixed_datum)?;
            tail_poly = ty::Ty::Cons(Box::new(fixed_poly), Box::new(tail_poly)).into_poly();
        }

        Ok(tail_poly)
    }

    fn lower_infix_fun_cons(&self, impure: bool, mut arg_data: Vec<NsDatum>) -> Result<ty::Poly> {
        let ret_ty = self.lower_poly(arg_data.pop().unwrap())?;

        // Discard the constructor
        arg_data.pop();

        let params_ty = self.lower_list_cons(arg_data)?;

        Ok(ty::Ty::new_fun(impure, params_ty, ret_ty).into_poly())
    }

    fn lower_complex_fun_cons(
        &self,
        span: Span,
        impure: bool,
        mut arg_data: Vec<NsDatum>,
    ) -> Result<ty::Poly> {
        expect_arg_count(span, &arg_data, 2)?;

        let ret_ty = self.lower_poly(arg_data.pop().unwrap())?;
        let params_ty = self.lower_poly(arg_data.pop().unwrap())?;

        Ok(ty::Ty::new_fun(impure, params_ty, ret_ty).into_poly())
    }

    fn lower_ty_cons_apply(
        &self,
        span: Span,
        ty_cons: TyCons,
        mut arg_data: Vec<NsDatum>,
    ) -> Result<ty::Poly> {
        match ty_cons {
            TyCons::List => self.lower_list_cons(arg_data),
            TyCons::Listof => {
                expect_arg_count(span, &arg_data, 1)?;
                let rest_ty = self.lower_poly(arg_data.pop().unwrap())?;
                Ok(ty::Ty::Listof(Box::new(rest_ty)).into_poly())
            }
            TyCons::Vector => {
                let member_tys = arg_data
                    .into_iter()
                    .map(|arg_datum| self.lower_poly(arg_datum))
                    .collect::<Result<Vec<ty::Poly>>>()?;

                Ok(ty::Ty::Vec(member_tys).into_poly())
            }
            TyCons::Vectorof => {
                expect_arg_count(span, &arg_data, 1)?;
                let start_ty = self.lower_poly(arg_data.pop().unwrap())?;
                Ok(ty::Ty::Vecof(Box::new(start_ty)).into_poly())
            }
            TyCons::PureFun => self.lower_complex_fun_cons(span, false, arg_data),
            TyCons::ImpureFun => self.lower_complex_fun_cons(span, true, arg_data),
            TyCons::PureArrow | TyCons::ImpureArrow => Err(Error::new(
                span,
                ErrorKind::IllegalArg("functions must return exactly one value".to_owned()),
            )),
            TyCons::Set => {
                expect_arg_count(span, &arg_data, 1)?;
                let member_ty = self.lower_poly(arg_data.pop().unwrap())?;
                Ok(ty::Ty::Set(Box::new(member_ty)).into_poly())
            }
            TyCons::Map => {
                expect_arg_count(span, &arg_data, 2)?;
                let value_ty = self.lower_poly(arg_data.pop().unwrap())?;
                let key_ty = self.lower_poly(arg_data.pop().unwrap())?;
                Ok(ty::Ty::Map(Box::new(key_ty), Box::new(value_ty)).into_poly())
            }
            TyCons::Cons => {
                expect_arg_count(span, &arg_data, 2)?;
                let cdr_ty = self.lower_poly(arg_data.pop().unwrap())?;
                let car_ty = self.lower_poly(arg_data.pop().unwrap())?;
                Ok(ty::Ty::Cons(Box::new(car_ty), Box::new(cdr_ty)).into_poly())
            }
            TyCons::Union => {
                let member_tys = arg_data
                    .into_iter()
                    .map(|arg_datum| self.lower_poly(arg_datum))
                    .collect::<Result<Vec<ty::Poly>>>()?;

                ty::unify::poly_unify_iter(self.pvars, member_tys.into_iter()).map_err(|err| {
                    match err {
                        ty::unify::PolyError::PolyConflict(left, right) => {
                            let left_str = str_for_poly(self.pvars, &left);
                            let right_str = str_for_poly(self.pvars, &right);

                            Error::new(span, ErrorKind::PolyUnionConflict(left_str, right_str))
                        }
                    }
                })
            }
            #[cfg(test)]
            TyCons::RawU => {
                // This performs a union *without* unifying the types. This is used when testing the
                // union code itself
                let member_tys = arg_data
                    .into_iter()
                    .map(|arg_datum| self.lower_poly(arg_datum))
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

    fn lower_ident(&self, span: Span, ident: Ident) -> Result<ty::Poly> {
        match self.scope.get(&ident) {
            Some(Binding::Ty(ref ty)) => Ok(ty.clone()),
            Some(_) => Err(Error::new(span, ErrorKind::ValueAsTy)),
            None => Err(Error::new(
                span,
                ErrorKind::UnboundSymbol(ident.into_name()),
            )),
        }
    }

    fn lower_poly(&self, datum: NsDatum) -> Result<ty::Poly> {
        match datum {
            NsDatum::List(span, mut vs) => {
                if vs.is_empty() {
                    // This is by analogy with () being self-evaluating in expressions
                    return Ok(ty::Ty::Nil.into_poly());
                }

                if vs.len() >= 2 {
                    match self.scope.get_datum(&vs[vs.len() - 2]) {
                        Some(Binding::TyCons(TyCons::PureArrow)) => {
                            return self.lower_infix_fun_cons(false, vs);
                        }
                        Some(Binding::TyCons(TyCons::ImpureArrow)) => {
                            return self.lower_infix_fun_cons(true, vs);
                        }
                        _ => {}
                    };
                }

                let mut arg_data = vs.split_off(1);
                let fn_datum = vs.pop().unwrap();

                if let NsDatum::Ident(ident_span, ref ident) = fn_datum {
                    match self.scope.get(ident) {
                        Some(Binding::Prim(Prim::Quote)) => {
                            expect_arg_count(span, &arg_data, 1)?;
                            return Self::lower_literal(arg_data.pop().unwrap());
                        }
                        Some(Binding::TyCons(ty_cons)) => {
                            return self.lower_ty_cons_apply(span, ty_cons, arg_data);
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
            NsDatum::Ident(span, ident) => self.lower_ident(span, ident),
            _ => Self::lower_literal(datum),
        }
    }
}

pub fn lower_pvar(
    pvars: &[ty::PVar],
    scope: &Scope,
    pvar_datum: NsDatum,
) -> Result<(Ident, ty::PVar)> {
    let ctx = LowerTyContext { pvars, scope };
    ctx.lower_pvar(pvar_datum)
}

pub fn lower_poly(pvars: &[ty::PVar], scope: &Scope, datum: NsDatum) -> Result<ty::Poly> {
    let ctx = LowerTyContext { pvars, scope };
    ctx.lower_poly(datum)
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
    export_ty_cons!("->", TyCons::PureArrow);
    export_ty_cons!("->!", TyCons::ImpureArrow);
    export_ty_cons!("Fn", TyCons::PureFun);
    export_ty_cons!("Fn!", TyCons::ImpureFun);
    export_ty_cons!("Setof", TyCons::Set);
    export_ty_cons!("Map", TyCons::Map);
    export_ty_cons!("Cons", TyCons::Cons);
    export_ty_cons!("U", TyCons::Union);

    #[cfg(test)]
    export_ty_cons!("RawU", TyCons::RawU);
}

/// Tries to construct the args for a `(List)` or `->` from a poly type
///
/// This will return None if the poly type cannot be constructed using `(List)` or `->` shorthand.
/// This can occur because not all "list-like" types can be expressed using shorthand. A fallback
/// to `(Cons)`/`(Fn)` is required for those cases.
fn str_for_simple_list_poly_ty(
    pvars: &[ty::PVar],
    mut poly_ty: &ty::Ty<ty::Poly>,
) -> Option<String> {
    let mut list_parts: Vec<String> = vec![];

    loop {
        match *poly_ty {
            ty::Ty::Nil => {
                break;
            }
            ty::Ty::Listof(ref rest) => {
                list_parts.push(str_for_poly(pvars, rest));
                list_parts.push("...".to_owned());
                break;
            }
            ty::Ty::Cons(ref car_poly, ref cdr_poly) => {
                list_parts.push(str_for_poly(pvars, car_poly));
                match *cdr_poly.as_ref() {
                    ty::Poly::Fixed(ref tail) => {
                        poly_ty = tail;
                    }
                    _ => {
                        return None;
                    }
                };
            }
            _ => {
                return None;
            }
        }
    }

    Some(list_parts.join(" "))
}

fn str_for_simple_list_poly(pvars: &[ty::PVar], poly: &ty::Poly) -> Option<String> {
    match *poly {
        ty::Poly::Fixed(ref poly_ty) => str_for_simple_list_poly_ty(pvars, poly_ty),
        _ => None,
    }
}

fn str_for_poly_ty(pvars: &[ty::PVar], poly_ty: &ty::Ty<ty::Poly>) -> String {
    match *poly_ty {
        ty::Ty::Any => "Any".to_owned(),
        ty::Ty::Bool => "Bool".to_owned(),
        ty::Ty::Char => "Char".to_owned(),
        ty::Ty::Int => "Int".to_owned(),
        ty::Ty::Sym => "Symbol".to_owned(),
        ty::Ty::Str => "String".to_owned(),
        ty::Ty::Float => "Float".to_owned(),
        ty::Ty::LitBool(false) => "false".to_owned(),
        ty::Ty::LitBool(true) => "true".to_owned(),
        ty::Ty::LitSym(ref name) => format!("'{}", name),
        ty::Ty::Map(ref key, ref value) => format!(
            "(Map {} {})",
            str_for_poly(pvars, key),
            str_for_poly(pvars, value)
        ),
        ty::Ty::Set(ref member) => format!("(Setof {})", str_for_poly(pvars, member)),
        ty::Ty::Vec(ref members) => {
            let mut result_parts: Vec<String> = members
                .iter()
                .map(|member| format!(" {}", str_for_poly(pvars, member)))
                .collect();

            format!("(Vector{})", result_parts.join(""))
        }
        ty::Ty::Vecof(ref member) => format!("(Vectorof {})", str_for_poly(pvars, member)),
        ty::Ty::Fun(ref fun) => {
            if let Some(simple_params_str) = str_for_simple_list_poly(pvars, fun.params()) {
                format!(
                    "({} {} {})",
                    simple_params_str,
                    if fun.impure() { "->!" } else { "->" },
                    str_for_poly(pvars, fun.ret())
                )
            } else {
                format!(
                    "({} {} {})",
                    if fun.impure() { "Fn!" } else { "Fn" },
                    str_for_poly(pvars, fun.params()),
                    str_for_poly(pvars, fun.ret())
                )
            }
        }
        ty::Ty::Union(ref members) => {
            let member_strs: Vec<String> = members
                .iter()
                .map(|m| format!(" {}", str_for_poly(pvars, m)))
                .collect();

            format!("(U{})", member_strs.join(""))
        }
        ty::Ty::Listof(ref member) => format!("(Listof {})", str_for_poly(pvars, member)),
        ty::Ty::Cons(ref car, ref cdr) => str_for_simple_list_poly_ty(pvars, poly_ty)
            .map(|cons_args| format!("(List {})", cons_args))
            .unwrap_or_else(|| {
                // Fall back to rendering the type using (Cons)
                format!(
                    "(Cons {} {})",
                    str_for_poly(pvars, car),
                    str_for_poly(pvars, cdr)
                )
            }),
        ty::Ty::Nil => {
            // This has many representations: `(List)`, `()` and `'()`. ()` has been chosen as idiomatic as it's the shortest.
            // TODO: Is the lexographic correspondence with Rust's unit type a good thing?
            "()".to_owned()
        }
    }
}

pub fn str_for_poly(pvars: &[ty::PVar], poly: &ty::Poly) -> String {
    match *poly {
        ty::Poly::Var(pvar_id) => {
            // TODO: It's possible to have pvars with overlapping source names
            pvars[pvar_id.to_usize()].source_name().clone()
        }
        ty::Poly::Fixed(ref poly_ty) => str_for_poly_ty(pvars, poly_ty),
    }
}

#[cfg(test)]
pub fn poly_for_str(datum_str: &str) -> Result<ty::Poly> {
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
        if name == "U" {
            // Using `U` in tests is very dubious as it invokes a lot of type system logic. It's
            // easy to write tautological tests due to `U` creating a simplified type. Rename to
            // `UnifyingU` so it's clear what's happening.
            scope.insert_binding(Ident::new(test_ns_id, "UnifyingU".to_owned()), binding);
        } else {
            scope.insert_binding(Ident::new(test_ns_id, name), binding);
        }
    }

    let test_datum = datum_from_str(datum_str).unwrap();

    lower_poly(
        &[],
        &scope,
        NsDatum::from_syntax_datum(test_ns_id, test_datum),
    )
}

#[cfg(test)]
mod test {
    use super::*;
    use syntax::span::t2s;

    fn assert_poly_for_str(expected: ty::Poly, datum_str: &str) {
        assert_eq!(expected, poly_for_str(datum_str).unwrap());

        // Try to round trip this to make sure str_for_poly works
        let recovered_str = str_for_poly(&[], &expected);
        assert_eq!(expected, poly_for_str(&recovered_str).unwrap());
    }

    fn assert_err_for_str(err: Error, datum_str: &str) {
        assert_eq!(err, poly_for_str(datum_str).unwrap_err());
    }

    fn simple_list_type(fixed: Vec<ty::Ty<ty::Poly>>, rest: Option<ty::Ty<ty::Poly>>) -> ty::Poly {
        let tail_poly = rest.map(|t| ty::Ty::Listof(Box::new(t.into_poly())).into_poly())
            .unwrap_or_else(|| ty::Ty::Nil.into_poly());

        fixed
            .into_iter()
            .rev()
            .fold(tail_poly, |tail_poly, fixed_poly| {
                ty::Ty::Cons(Box::new(fixed_poly.into_poly()), Box::new(tail_poly)).into_poly()
            })
    }

    /// This asserts that a type uses an exact string in str_for_poly
    ///
    /// This is to make sure we use e.g. (List Int) instead of (Cons (Cons Int ()))
    fn assert_exact_str_repr(datum_str: &str) {
        assert_eq!(
            datum_str,
            str_for_poly(&[], &poly_for_str(datum_str).unwrap())
        );
    }

    #[test]
    fn true_literal() {
        let j = "true";

        let expected = ty::Ty::LitBool(true).into_poly();
        assert_poly_for_str(expected, j);
    }

    #[test]
    fn false_literal() {
        let j = "false";

        let expected = ty::Ty::LitBool(false).into_poly();
        assert_poly_for_str(expected, j);
    }

    #[test]
    fn sym_literal() {
        let j = "'foo";

        let expected = ty::Ty::LitSym("foo".to_owned()).into_poly();
        assert_poly_for_str(expected, j);
    }

    #[test]
    fn empty_list_literal() {
        let j = "()";

        let expected = ty::Ty::Nil.into_poly();
        assert_poly_for_str(expected, j);
    }

    #[test]
    fn ty_ref() {
        let j = "Symbol";

        let expected = ty::Ty::Sym.into_poly();
        assert_poly_for_str(expected, j);
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
    fn cons_cons() {
        let j = "(Cons true false)";

        let expected = ty::Ty::Cons(
            Box::new(ty::Ty::LitBool(true).into_poly()),
            Box::new(ty::Ty::LitBool(false).into_poly()),
        ).into_poly();
        assert_poly_for_str(expected, j);
    }

    #[test]
    fn listof_cons() {
        let j = "(Listof true)";

        let inner_ty = ty::Ty::LitBool(true);
        let expected = simple_list_type(vec![], Some(inner_ty));

        assert_poly_for_str(expected, j);
    }

    #[test]
    fn fixed_list_cons() {
        let j = "(List true false)";

        let expected = simple_list_type(vec![ty::Ty::LitBool(true), ty::Ty::LitBool(false)], None);

        assert_poly_for_str(expected, j);
    }

    #[test]
    fn rest_list_cons() {
        let j = "(List true false ...)";

        let expected = simple_list_type(vec![ty::Ty::LitBool(true)], Some(ty::Ty::LitBool(false)));

        assert_poly_for_str(expected, j);
    }

    #[test]
    fn vectorof_cons() {
        let j = "(Vectorof true)";

        let inner_ty = ty::Ty::LitBool(true).into_poly();
        let expected = ty::Ty::Vecof(Box::new(inner_ty)).into_poly();

        assert_poly_for_str(expected, j);
    }

    #[test]
    fn vector_cons() {
        let j = "(Vector true false)";

        let expected = ty::Ty::Vec(vec![
            ty::Ty::LitBool(true).into_poly(),
            ty::Ty::LitBool(false).into_poly(),
        ]).into_poly();

        assert_poly_for_str(expected, j);
    }

    #[test]
    fn empty_fun() {
        let j = "(->)";
        let t = "^^^^";

        let err = Error::new(
            t2s(t),
            ErrorKind::IllegalArg("functions must return exactly one value".to_owned()),
        );
        assert_err_for_str(err, j);
    }

    #[test]
    fn pure_infix_fun() {
        let j = "(-> true)";

        let expected = ty::Ty::new_fun(
            false,
            simple_list_type(vec![], None),
            ty::Ty::LitBool(true).into_poly(),
        ).into_poly();

        assert_poly_for_str(expected, j);
    }

    #[test]
    fn impure_infix_fun() {
        let j = "(->! true)";

        let expected = ty::Ty::new_fun(
            true,
            simple_list_type(vec![], None),
            ty::Ty::LitBool(true).into_poly(),
        ).into_poly();

        assert_poly_for_str(expected, j);
    }

    #[test]
    fn fixed_infix_fun() {
        let j = "(false -> true)";

        let expected = ty::Ty::new_fun(
            false,
            simple_list_type(vec![ty::Ty::LitBool(false)], None),
            ty::Ty::LitBool(true).into_poly(),
        ).into_poly();

        assert_poly_for_str(expected, j);
    }

    #[test]
    fn rest_infix_impure_fun() {
        let j = "(String Symbol ... ->! true)";

        let expected = ty::Ty::new_fun(
            true,
            simple_list_type(vec![ty::Ty::Str], Some(ty::Ty::Sym)),
            ty::Ty::LitBool(true).into_poly(),
        ).into_poly();

        assert_poly_for_str(expected, j);
    }

    #[test]
    fn fixed_complex_fun() {
        let j = "(Fn (List false) true)";

        let expected = ty::Ty::new_fun(
            false,
            simple_list_type(vec![ty::Ty::LitBool(false)], None),
            ty::Ty::LitBool(true).into_poly(),
        ).into_poly();

        assert_poly_for_str(expected, j);
    }

    #[test]
    fn rest_complex_fun() {
        let j = "(Fn (Listof Symbol) true)";

        let expected = ty::Ty::new_fun(
            false,
            ty::Ty::Listof(Box::new(ty::Ty::Sym.into_poly())).into_poly(),
            ty::Ty::LitBool(true).into_poly(),
        ).into_poly();

        assert_poly_for_str(expected, j);
    }
    #[test]
    fn set_cons() {
        let j = "(Setof true)";

        let inner_ty = ty::Ty::LitBool(true).into_poly();
        let expected = ty::Ty::Set(Box::new(inner_ty)).into_poly();

        assert_poly_for_str(expected, j);
    }

    #[test]
    fn map_cons() {
        let j = "(Map true false)";

        let key_ty = ty::Ty::LitBool(true);
        let value_ty = ty::Ty::LitBool(false);
        let expected =
            ty::Ty::Map(Box::new(key_ty.into_poly()), Box::new(value_ty.into_poly())).into_poly();

        assert_poly_for_str(expected, j);
    }

    #[test]
    fn merged_union_cons() {
        let j = "(UnifyingU true false)";
        let expected = ty::Ty::Bool.into_poly();

        assert_poly_for_str(expected, j);
    }

    #[test]
    fn simpifying_str_for_poly() {
        assert_exact_str_repr("(List Int Float)");
        assert_exact_str_repr("(List Int Float ...)");
        assert_exact_str_repr("(Listof Float)");
        assert_exact_str_repr("(Float Int ... -> Symbol)");
    }
}
