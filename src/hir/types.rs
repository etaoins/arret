use std::collections::HashMap;

use hir::error::{Error, ErrorKind, Result};
use hir::ns::{Ident, NsDatum};
use hir::prim::Prim;
use hir::scope::{Binding, Scope};
use hir::util::{expect_arg_count, expect_ident, split_into_fixed_and_rest};
use syntax::span::Span;
use ty;
use ty::purity::Purity;

#[derive(Debug, PartialEq, Eq, Hash, Clone, Copy)]
pub enum TyCons {
    List,
    Listof,
    Vector,
    Vectorof,
    TyPred,
    Set,
    Map,
    Union,
    Cons,
    #[cfg(test)]
    RawU,
}

pub enum PolymorphicVar {
    TVar(ty::TVar),
    PVar(ty::purity::PVar),
    Pure,
}

struct LowerTyContext<'a> {
    pvars: &'a [ty::purity::PVar],
    tvars: &'a [ty::TVar],
    scope: &'a Scope,
}

impl<'a> LowerTyContext<'a> {
    fn lower_polymorphic_var(&self, tvar_datum: NsDatum) -> Result<(Ident, PolymorphicVar)> {
        let span = tvar_datum.span();

        match tvar_datum {
            NsDatum::Ident(_, ident) => {
                let source_name = ident.name().clone();
                return Ok((
                    ident,
                    PolymorphicVar::TVar(ty::TVar::new(source_name, ty::Ty::Any.into_poly())),
                ));
            }
            NsDatum::Vec(span, mut arg_data) => {
                if arg_data.len() == 3
                    && self.scope.get_datum(&arg_data[1]) == Some(Binding::Prim(Prim::TyColon))
                {
                    let bound_datum = arg_data.pop().unwrap();
                    arg_data.pop(); // Discard the : completely
                    let ident = expect_ident(arg_data.pop().unwrap())?;

                    let source_name = ident.name().clone();

                    match try_lower_purity(self.scope, &bound_datum) {
                        Some(ty::purity::Poly::Fixed(Purity::Impure)) => {
                            return Ok((
                                ident,
                                PolymorphicVar::PVar(ty::purity::PVar::new(source_name)),
                            ));
                        }
                        Some(ty::purity::Poly::Fixed(Purity::Pure)) => {
                            // Emulate bounding to pure in case the purity comes from e.g. a macro
                            // expansion
                            return Ok((ident, PolymorphicVar::Pure));
                        }
                        Some(_) => {
                            return Err(Error::new(
                                span,
                                ErrorKind::IllegalArg(
                                    "Purity variables do not support variable bounds".to_owned(),
                                ),
                            ))
                        }
                        None => {
                            let bound_ty = self.lower_poly(bound_datum)?;
                            return Ok((
                                ident,
                                PolymorphicVar::TVar(ty::TVar::new(source_name, bound_ty)),
                            ));
                        }
                    }
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

    fn lower_fun_cons(
        &self,
        purity: ty::purity::Poly,
        mut arg_data: Vec<NsDatum>,
    ) -> Result<ty::Poly> {
        let ret_ty = self.lower_poly(arg_data.pop().unwrap())?;

        // Discard the constructor
        arg_data.pop();

        let top_fun = ty::TopFun::new(purity, ret_ty);

        if arg_data.len() == 1
            && self.scope.get_datum(&arg_data[0]) == Some(Binding::Prim(Prim::Ellipsis))
        {
            // Top function type in the form `(... -> ReturnType)`
            Ok(top_fun.into_ref())
        } else {
            let (fixed, rest) = split_into_fixed_and_rest(self.scope, arg_data);

            let fixed_polys = fixed
                .into_iter()
                .map(|fixed_datum| self.lower_poly(fixed_datum))
                .collect::<Result<Vec<ty::Poly>>>()?;

            let rest_poly = match rest {
                Some(rest) => Some(self.lower_poly(rest)?),
                None => None,
            };

            let params = ty::Params::new(fixed_polys, rest_poly);
            Ok(ty::Fun::new(
                ty::purity::PVarIds::empty(),
                ty::TVarIds::empty(),
                top_fun,
                params,
            ).into_ref())
        }
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
            TyCons::TyPred => {
                expect_arg_count(span, &arg_data, 1)?;
                let test_ty = self.lower_poly(arg_data.pop().unwrap())?;
                Ok(ty::Ty::TyPred(Box::new(test_ty)).into_poly())
            }
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

                ty::unify::poly_unify_iter(self.tvars, member_tys.into_iter()).map_err(|err| {
                    match err {
                        ty::unify::PolyError::PolyConflict(left, right) => {
                            let left_str = str_for_poly(self.pvars, self.tvars, &left);
                            let right_str = str_for_poly(self.pvars, self.tvars, &right);

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
            Some(Binding::Ty(ty)) => Ok(ty),
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
                    if let Some(purity) = try_lower_purity(self.scope, &vs[vs.len() - 2]) {
                        // This is a function type
                        return self.lower_fun_cons(purity, vs);
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

pub fn lower_polymorphic_var(
    pvars: &[ty::purity::PVar],
    tvars: &[ty::TVar],
    scope: &Scope,
    tvar_datum: NsDatum,
) -> Result<(Ident, PolymorphicVar)> {
    let ctx = LowerTyContext {
        pvars,
        tvars,
        scope,
    };
    ctx.lower_polymorphic_var(tvar_datum)
}

pub fn lower_poly(
    pvars: &[ty::purity::PVar],
    tvars: &[ty::TVar],
    scope: &Scope,
    datum: NsDatum,
) -> Result<ty::Poly> {
    let ctx = LowerTyContext {
        pvars,
        tvars,
        scope,
    };
    ctx.lower_poly(datum)
}

pub fn try_lower_purity(scope: &Scope, datum: &NsDatum) -> Option<ty::purity::Poly> {
    scope.get_datum(datum).and_then(|binding| match binding {
        Binding::Purity(purity) => Some(purity),
        _ => None,
    })
}

pub fn insert_ty_exports(exports: &mut HashMap<String, Binding>) {
    macro_rules! export_ty {
        ($name:expr, $type:expr) => {
            exports.insert($name.to_owned(), Binding::Ty($type));
        };
    }

    macro_rules! export_ty_cons {
        ($name:expr, $ty_cons:expr) => {
            exports.insert($name.to_owned(), Binding::TyCons($ty_cons));
        };
    }

    macro_rules! export_purity {
        ($name:expr, $purity:expr) => {
            exports.insert($name.to_owned(), Binding::Purity($purity));
        };
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
    export_ty_cons!("Setof", TyCons::Set);
    export_ty_cons!("Map", TyCons::Map);
    export_ty_cons!("Cons", TyCons::Cons);
    export_ty_cons!("U", TyCons::Union);

    export_ty_cons!("Type?", TyCons::TyPred);

    export_purity!("->", Purity::Pure.into_poly());
    export_purity!("->!", Purity::Impure.into_poly());

    #[cfg(test)]
    export_ty_cons!("RawU", TyCons::RawU);
}

struct StrForPolyContext<'a> {
    pvars: &'a [ty::purity::PVar],
    tvars: &'a [ty::TVar],
}

impl<'a> StrForPolyContext<'a> {
    /// Tries to construct the args for a `(List)` or `->` from a poly type
    ///
    /// This will return None if the poly type cannot be constructed using `(List)` or `->`
    /// shorthand. This can occur because not all "list-like" types can be expressed using
    /// shorthand.
    fn str_for_simple_list(&self, mut poly_ty: &ty::Ty<ty::Poly>) -> Option<String> {
        let mut list_parts: Vec<String> = vec![];

        loop {
            match poly_ty {
                ty::Ty::Nil => {
                    break;
                }
                ty::Ty::Listof(rest) => {
                    list_parts.push(self.str_for_poly(rest));
                    list_parts.push("...".to_owned());
                    break;
                }
                ty::Ty::Cons(car_poly, cdr_poly) => {
                    list_parts.push(self.str_for_poly(car_poly));
                    match cdr_poly.as_ref() {
                        ty::Poly::Fixed(tail) => {
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

    fn str_for_purity(&self, purity: &ty::purity::Poly) -> String {
        match purity {
            ty::purity::Poly::Fixed(Purity::Pure) => "->".to_owned(),
            ty::purity::Poly::Fixed(Purity::Impure) => "->!".to_owned(),
            ty::purity::Poly::Var(pvar_id) => self.pvars[pvar_id.to_usize()].source_name().clone(),
        }
    }

    fn str_for_poly_ty(&self, poly_ty: &ty::Ty<ty::Poly>) -> String {
        match poly_ty {
            ty::Ty::Any => "Any".to_owned(),
            ty::Ty::Bool => "Bool".to_owned(),
            ty::Ty::Char => "Char".to_owned(),
            ty::Ty::Int => "Int".to_owned(),
            ty::Ty::Sym => "Symbol".to_owned(),
            ty::Ty::Str => "String".to_owned(),
            ty::Ty::Float => "Float".to_owned(),
            ty::Ty::LitBool(false) => "false".to_owned(),
            ty::Ty::LitBool(true) => "true".to_owned(),
            ty::Ty::LitSym(name) => format!("'{}", name),
            ty::Ty::Map(key, value) => format!(
                "(Map {} {})",
                self.str_for_poly(key),
                self.str_for_poly(value)
            ),
            ty::Ty::Set(member) => format!("(Setof {})", self.str_for_poly(member)),
            ty::Ty::Vec(members) => {
                let mut result_parts: Vec<String> = members
                    .iter()
                    .map(|member| format!(" {}", self.str_for_poly(member)))
                    .collect();

                format!("(Vector{})", result_parts.join(""))
            }
            ty::Ty::Vecof(member) => format!("(Vectorof {})", self.str_for_poly(member)),
            ty::Ty::TopFun(top_fun) => format!(
                "(... {} {})",
                self.str_for_purity(top_fun.purity()),
                self.str_for_poly(top_fun.ret())
            ),
            ty::Ty::Fun(fun) => {
                let mut fun_parts = Vec::with_capacity(2);

                for fixed in fun.params().fixed() {
                    fun_parts.push(self.str_for_poly(fixed));
                }
                for rest in fun.params().rest() {
                    fun_parts.push(self.str_for_poly(rest));
                    fun_parts.push("...".to_owned());
                }
                fun_parts.push(self.str_for_purity(fun.purity()));
                fun_parts.push(self.str_for_poly(fun.ret()));

                format!("({})", fun_parts.join(" "),)
            }
            ty::Ty::TyPred(test) => format!("(Type? {})", self.str_for_poly(test)),
            ty::Ty::Union(members) => {
                let member_strs: Vec<String> = members
                    .iter()
                    .map(|m| format!(" {}", self.str_for_poly(m)))
                    .collect();

                format!("(U{})", member_strs.join(""))
            }
            ty::Ty::Listof(member) => format!("(Listof {})", self.str_for_poly(member)),
            ty::Ty::Cons(car, cdr) => self.str_for_simple_list(poly_ty)
                .map(|cons_args| format!("(List {})", cons_args))
                .unwrap_or_else(|| {
                    // Fall back to rendering the type using (Cons)
                    format!(
                        "(Cons {} {})",
                        self.str_for_poly(car),
                        self.str_for_poly(cdr)
                    )
                }),
            ty::Ty::Nil => {
                // This has many representations: `(List)`, `()` and `'()`. ()` has been chosen as idiomatic as it's the shortest.
                // TODO: Is the lexographic correspondence with Rust's unit type a good thing?
                "()".to_owned()
            }
        }
    }

    fn str_for_poly(&self, poly: &ty::Poly) -> String {
        match poly {
            ty::Poly::Var(tvar_id) => {
                // TODO: It's possible to have tvars with overlapping source names
                self.tvars[tvar_id.to_usize()].source_name().clone()
            }
            ty::Poly::Fixed(poly_ty) => self.str_for_poly_ty(poly_ty),
        }
    }
}

pub fn str_for_poly(pvars: &[ty::purity::PVar], tvars: &[ty::TVar], poly: &ty::Poly) -> String {
    let ctx = StrForPolyContext { pvars, tvars };
    ctx.str_for_poly(poly)
}

#[cfg(test)]
pub fn poly_for_str(datum_str: &str) -> Result<ty::Poly> {
    use hir::ns::NsId;
    use hir::prim::insert_prim_exports;
    use syntax::parser::datum_from_str;

    let test_ns_id = NsId::new(1);

    // Capture our exports
    let mut exports = HashMap::<String, Binding>::new();
    insert_prim_exports(&mut exports);
    insert_ty_exports(&mut exports);

    // Place them on our scope
    let mut scope = Scope::new_empty();
    for (name, binding) in exports {
        if name == "U" {
            // Using `U` in tests is very dubious as it invokes a lot of type system logic. It's
            // easy to write tautological tests due to `U` creating a simplified type. Rename to
            // `UnifyingU` so it's clear what's happening.
            scope.insert_binding(Ident::new(test_ns_id, "UnifyingU".to_owned()), binding);
        } else {
            scope.insert_binding(Ident::new(test_ns_id, name), binding);
        }
    }

    // Add some test polymorphic variables
    for var_idx in 0..26 {
        let var_name = (b'A' + var_idx) as char;
        let tvar_id = ty::TVarId::new(var_idx as usize);
        let poly = ty::Poly::Var(tvar_id);

        scope.insert_binding(
            Ident::new(test_ns_id, var_name.to_string()),
            Binding::Ty(poly),
        );
    }

    for var_idx in 0..26 {
        let var_name = format!("->{}", (b'A' + var_idx) as char);
        let pvar_id = ty::purity::PVarId::new(var_idx as usize);
        let poly = ty::purity::Poly::Var(pvar_id);

        scope.insert_binding(
            Ident::new(test_ns_id, var_name.to_string()),
            Binding::Purity(poly),
        );
    }

    let test_datum = datum_from_str(datum_str).unwrap();

    lower_poly(
        &[],
        &[],
        &scope,
        NsDatum::from_syntax_datum(test_ns_id, test_datum),
    )
}

#[cfg(test)]
mod test {
    use super::*;
    use syntax::span::t2s;

    fn assert_poly_for_str(expected: &ty::Poly, datum_str: &str) {
        assert_eq!(*expected, poly_for_str(datum_str).unwrap());

        // Try to round trip this to make sure str_for_poly works
        let recovered_str = str_for_poly(&[], &[], &expected);
        assert_eq!(*expected, poly_for_str(&recovered_str).unwrap());
    }

    fn assert_err_for_str(err: &Error, datum_str: &str) {
        assert_eq!(*err, poly_for_str(datum_str).unwrap_err());
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
            str_for_poly(&[], &[], &poly_for_str(datum_str).unwrap())
        );
    }

    #[test]
    fn true_literal() {
        let j = "true";

        let expected = ty::Ty::LitBool(true).into_poly();
        assert_poly_for_str(&expected, j);
    }

    #[test]
    fn false_literal() {
        let j = "false";

        let expected = ty::Ty::LitBool(false).into_poly();
        assert_poly_for_str(&expected, j);
    }

    #[test]
    fn sym_literal() {
        let j = "'foo";

        let expected = ty::Ty::LitSym("foo".to_owned()).into_poly();
        assert_poly_for_str(&expected, j);
    }

    #[test]
    fn empty_list_literal() {
        let j = "()";

        let expected = ty::Ty::Nil.into_poly();
        assert_poly_for_str(&expected, j);
    }

    #[test]
    fn ty_ref() {
        let j = "Symbol";

        let expected = ty::Ty::Sym.into_poly();
        assert_poly_for_str(&expected, j);
    }

    #[test]
    fn unbound_symbol() {
        let j = "notbound";
        let t = "^^^^^^^^";

        let err = Error::new(t2s(t), ErrorKind::UnboundSymbol("notbound".to_owned()));
        assert_err_for_str(&err, j);
    }

    #[test]
    fn unsupported_int_literal() {
        let j = "1";
        let t = "^";

        let err = Error::new(
            t2s(t),
            ErrorKind::IllegalArg("only boolean and symbol literals are supported".to_owned()),
        );
        assert_err_for_str(&err, j);
    }

    #[test]
    fn non_literal_value_ref() {
        let j = "quote";
        let t = "^^^^^";

        let err = Error::new(t2s(t), ErrorKind::ValueAsTy);
        assert_err_for_str(&err, j);
    }

    #[test]
    fn unbound_cons() {
        let j = "(notbound)";
        let t = " ^^^^^^^^ ";

        let err = Error::new(t2s(t), ErrorKind::UnboundSymbol("notbound".to_owned()));
        assert_err_for_str(&err, j);
    }

    #[test]
    fn cons_cons() {
        let j = "(Cons true false)";

        let expected = ty::Ty::Cons(
            Box::new(ty::Ty::LitBool(true).into_poly()),
            Box::new(ty::Ty::LitBool(false).into_poly()),
        ).into_poly();
        assert_poly_for_str(&expected, j);
    }

    #[test]
    fn listof_cons() {
        let j = "(Listof true)";

        let inner_ty = ty::Ty::LitBool(true);
        let expected = simple_list_type(vec![], Some(inner_ty));

        assert_poly_for_str(&expected, j);
    }

    #[test]
    fn fixed_list_cons() {
        let j = "(List true false)";

        let expected = simple_list_type(vec![ty::Ty::LitBool(true), ty::Ty::LitBool(false)], None);

        assert_poly_for_str(&expected, j);
    }

    #[test]
    fn rest_list_cons() {
        let j = "(List true false ...)";

        let expected = simple_list_type(vec![ty::Ty::LitBool(true)], Some(ty::Ty::LitBool(false)));

        assert_poly_for_str(&expected, j);
    }

    #[test]
    fn vectorof_cons() {
        let j = "(Vectorof true)";

        let inner_ty = ty::Ty::LitBool(true).into_poly();
        let expected = ty::Ty::Vecof(Box::new(inner_ty)).into_poly();

        assert_poly_for_str(&expected, j);
    }

    #[test]
    fn vector_cons() {
        let j = "(Vector true false)";

        let expected = ty::Ty::Vec(vec![
            ty::Ty::LitBool(true).into_poly(),
            ty::Ty::LitBool(false).into_poly(),
        ]).into_poly();

        assert_poly_for_str(&expected, j);
    }

    #[test]
    fn empty_fun() {
        let j = "(->)";
        let t = " ^^ ";

        let err = Error::new(
            t2s(t),
            ErrorKind::IllegalArg("type constructor expected".to_owned()),
        );
        assert_err_for_str(&err, j);
    }

    #[test]
    fn pure_fun() {
        let j = "(-> true)";

        let expected = ty::Fun::new(
            ty::purity::PVarIds::empty(),
            ty::TVarIds::empty(),
            ty::TopFun::new(Purity::Pure.into_poly(), ty::Ty::LitBool(true).into_poly()),
            ty::Params::new(vec![], None),
        ).into_ref();

        assert_poly_for_str(&expected, j);
    }

    #[test]
    fn impure_fun() {
        let j = "(->! true)";

        let expected = ty::Fun::new(
            ty::purity::PVarIds::empty(),
            ty::TVarIds::empty(),
            ty::TopFun::new(
                Purity::Impure.into_poly(),
                ty::Ty::LitBool(true).into_poly(),
            ),
            ty::Params::new(vec![], None),
        ).into_ref();

        assert_poly_for_str(&expected, j);
    }

    #[test]
    fn fixed_fun() {
        let j = "(false -> true)";

        let expected = ty::Fun::new(
            ty::purity::PVarIds::empty(),
            ty::TVarIds::empty(),
            ty::TopFun::new(Purity::Pure.into_poly(), ty::Ty::LitBool(true).into_poly()),
            ty::Params::new(vec![ty::Ty::LitBool(false).into_poly()], None),
        ).into_ref();

        assert_poly_for_str(&expected, j);
    }

    #[test]
    fn rest_impure_fun() {
        let j = "(String Symbol ... ->! true)";

        let expected = ty::Fun::new(
            ty::purity::PVarIds::empty(),
            ty::TVarIds::empty(),
            ty::TopFun::new(
                Purity::Impure.into_poly(),
                ty::Ty::LitBool(true).into_poly(),
            ),
            ty::Params::new(vec![ty::Ty::Str.into_poly()], Some(ty::Ty::Sym.into_poly())),
        ).into_ref();

        assert_poly_for_str(&expected, j);
    }

    #[test]
    fn top_impure_fun() {
        let j = "(... ->! true)";

        let expected = ty::Ty::TopFun(Box::new(ty::TopFun::new(
            Purity::Impure.into_poly(),
            ty::Ty::LitBool(true).into_poly(),
        ))).into_poly();

        assert_poly_for_str(&expected, j);
    }

    #[test]
    fn ty_predicate() {
        let j = "(Type? String)";

        let expected = ty::Ty::TyPred(Box::new(ty::Ty::Str.into_poly())).into_poly();
        assert_poly_for_str(&expected, j);
    }

    #[test]
    fn set_cons() {
        let j = "(Setof true)";

        let inner_ty = ty::Ty::LitBool(true).into_poly();
        let expected = ty::Ty::Set(Box::new(inner_ty)).into_poly();

        assert_poly_for_str(&expected, j);
    }

    #[test]
    fn map_cons() {
        let j = "(Map true false)";

        let key_ty = ty::Ty::LitBool(true);
        let value_ty = ty::Ty::LitBool(false);
        let expected =
            ty::Ty::Map(Box::new(key_ty.into_poly()), Box::new(value_ty.into_poly())).into_poly();

        assert_poly_for_str(&expected, j);
    }

    #[test]
    fn merged_union_cons() {
        let j = "(UnifyingU true false)";
        let expected = ty::Ty::Bool.into_poly();

        assert_poly_for_str(&expected, j);
    }

    #[test]
    fn simpifying_str_for_poly() {
        assert_exact_str_repr("(List Int Float)");
        assert_exact_str_repr("(List Int Float ...)");
        assert_exact_str_repr("(Listof Float)");
        assert_exact_str_repr("(Float Int ... -> Symbol)");
    }
}
