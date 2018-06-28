use std::collections::HashMap;
use std::ops::Range;

use hir::error::{Error, ErrorKind, Result};
use hir::ns::{Ident, NsDataIter, NsDatum};
use hir::prim::Prim;
use hir::scope::{Binding, Scope};
use hir::util::{expect_arg_count, expect_ident_and_span, expect_one_arg, try_take_rest_arg};
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
    #[cfg(test)]
    RawU,
}

pub enum PolymorphicVarKind {
    TVar(ty::TVar),
    PVar(ty::purity::PVar),
    Pure,
}

pub struct PolymorphicVar {
    pub span: Span,
    pub ident: Ident,
    pub kind: PolymorphicVarKind,
}

struct LowerTyContext<'tvars, 'scope> {
    tvars: &'tvars [ty::TVar],
    scope: &'scope Scope,
}

impl<'tvars, 'scope> LowerTyContext<'tvars, 'scope> {
    fn lower_polymorphic_var(&self, tvar_datum: NsDatum) -> Result<PolymorphicVar> {
        let span = tvar_datum.span();

        match tvar_datum {
            NsDatum::Ident(span, ident) => {
                let source_name = ident.name().into();
                return Ok(PolymorphicVar {
                    span,
                    ident,
                    kind: PolymorphicVarKind::TVar(ty::TVar::new(
                        source_name,
                        ty::Ty::Any.into_poly(),
                    )),
                });
            }
            NsDatum::Vec(vector_span, vs) => {
                let mut arg_data = vs.into_vec();

                if arg_data.len() == 3
                    && self.scope.get_datum(&arg_data[1]) == Some(Binding::Prim(Prim::TyColon))
                {
                    let bound_datum = arg_data.pop().unwrap();
                    arg_data.pop(); // Discard the : completely
                    let (ident, span) = expect_ident_and_span(arg_data.pop().unwrap())?;

                    let source_name = ident.name().into();

                    match try_lower_purity(self.scope, &bound_datum) {
                        Some(ty::purity::Poly::Fixed(Purity::Impure)) => {
                            return Ok(PolymorphicVar {
                                span,
                                ident,
                                kind: PolymorphicVarKind::PVar(ty::purity::PVar::new(source_name)),
                            });
                        }
                        Some(ty::purity::Poly::Fixed(Purity::Pure)) => {
                            // Emulate bounding to pure in case the purity comes from e.g. a macro
                            // expansion
                            return Ok(PolymorphicVar {
                                span,
                                ident,
                                kind: PolymorphicVarKind::Pure,
                            });
                        }
                        Some(_) => {
                            return Err(Error::new(
                                vector_span,
                                ErrorKind::IllegalArg(
                                    "Purity variables do not support variable bounds",
                                ),
                            ))
                        }
                        None => {
                            let bound_ty = self.lower_poly(bound_datum)?;
                            return Ok(PolymorphicVar {
                                span,
                                ident,
                                kind: PolymorphicVarKind::TVar(ty::TVar::new(
                                    source_name,
                                    bound_ty,
                                )),
                            });
                        }
                    }
                }
            }
            _ => {}
        }

        Err(Error::new(
            span,
            ErrorKind::IllegalArg(
                "polymorphic variables must be either an identifier or [identifier : Type]",
            ),
        ))
    }

    fn lower_list_cons(&self, mut arg_iter: NsDataIter) -> Result<ty::List<ty::Poly>> {
        let rest = try_take_rest_arg(self.scope, &mut arg_iter);

        let fixed_polys = arg_iter
            .map(|fixed_datum| self.lower_poly(fixed_datum))
            .collect::<Result<Vec<ty::Poly>>>()?
            .into_boxed_slice();

        let rest_poly = match rest {
            Some(rest_datum) => Some(self.lower_poly(rest_datum)?),
            None => None,
        };

        Ok(ty::List::new(fixed_polys, rest_poly))
    }

    fn lower_fun_cons(
        &self,
        purity: ty::purity::Poly,
        mut arg_iter: NsDataIter,
    ) -> Result<ty::Poly> {
        let ret_ty = self.lower_poly(arg_iter.next_back().unwrap())?;

        // Discard the purity
        arg_iter.next_back();

        let top_fun = ty::TopFun::new(purity, ret_ty);

        if arg_iter.len() == 1
            && self.scope.get_datum(&arg_iter.as_slice()[0]) == Some(Binding::Prim(Prim::Ellipsis))
        {
            // Top function type in the form `(... -> ReturnType)`
            Ok(top_fun.into_ty_ref())
        } else {
            let params = self.lower_list_cons(arg_iter)?;

            Ok(ty::Fun::new(
                ty::purity::PVarIds::monomorphic(),
                ty::TVarIds::monomorphic(),
                top_fun,
                params,
            ).into_ty_ref())
        }
    }

    fn lower_ty_cons_apply(
        &self,
        span: Span,
        ty_cons: TyCons,
        mut arg_iter: NsDataIter,
    ) -> Result<ty::Poly> {
        match ty_cons {
            TyCons::List => Ok(ty::Ty::List(self.lower_list_cons(arg_iter)?).into_poly()),
            TyCons::Listof => {
                let rest_datum = expect_one_arg(span, arg_iter)?;
                let rest_poly = self.lower_poly(rest_datum)?;
                let list_poly = ty::List::new(Box::new([]), Some(rest_poly));

                Ok(ty::Ty::List(list_poly).into_poly())
            }
            TyCons::Vector => {
                let member_tys = arg_iter
                    .map(|arg_datum| self.lower_poly(arg_datum))
                    .collect::<Result<Vec<ty::Poly>>>()?
                    .into_boxed_slice();

                Ok(ty::Ty::Vec(member_tys).into_poly())
            }
            TyCons::Vectorof => {
                let start_datum = expect_one_arg(span, arg_iter)?;
                let start_ty = self.lower_poly(start_datum)?;
                Ok(ty::Ty::Vecof(Box::new(start_ty)).into_poly())
            }
            TyCons::TyPred => {
                let test_datum = expect_one_arg(span, arg_iter)?;
                let test_ty = self.lower_poly(test_datum)?;
                Ok(ty::Ty::TyPred(Box::new(test_ty)).into_poly())
            }
            TyCons::Set => {
                let member_datum = expect_one_arg(span, arg_iter)?;
                let member_ty = self.lower_poly(member_datum)?;
                Ok(ty::Ty::Set(Box::new(member_ty)).into_poly())
            }
            TyCons::Map => {
                expect_arg_count(span, 2, arg_iter.len())?;
                let key_ty = self.lower_poly(arg_iter.next().unwrap())?;
                let value_ty = self.lower_poly(arg_iter.next().unwrap())?;
                Ok(ty::Ty::Map(Box::new(ty::Map::new(key_ty, value_ty))).into_poly())
            }
            TyCons::Union => {
                let member_tys = arg_iter
                    .map(|arg_datum| self.lower_poly(arg_datum))
                    .collect::<Result<Vec<ty::Poly>>>()?;

                Ok(ty::unify::poly_unify_iter(
                    self.tvars,
                    member_tys.into_iter(),
                ))
            }
            #[cfg(test)]
            TyCons::RawU => {
                // This performs a union *without* unifying the types. This is used when testing the
                // union code itself
                let member_tys = arg_iter
                    .map(|arg_datum| self.lower_poly(arg_datum))
                    .collect::<Result<Vec<ty::Poly>>>()?;

                Ok(ty::Ty::Union(member_tys.into_boxed_slice()).into_poly())
            }
        }
    }

    fn lower_literal_vec(literal_data: Vec<NsDatum>) -> Result<Vec<ty::Poly>> {
        literal_data.into_iter().map(Self::lower_literal).collect()
    }

    fn lower_literal(datum: NsDatum) -> Result<ty::Poly> {
        match datum {
            NsDatum::Bool(_, v) => Ok(ty::Ty::LitBool(v).into_poly()),
            NsDatum::Ident(_, ident) => Ok(ty::Ty::LitSym(ident.name().into()).into_poly()),
            NsDatum::List(_, vs) => {
                let fixed_literals = Self::lower_literal_vec(vs.into_vec())?;
                Ok(
                    ty::Ty::List(ty::List::new(fixed_literals.into_boxed_slice(), None))
                        .into_poly(),
                )
            }
            NsDatum::Vec(_, vs) => {
                let fixed_literals = Self::lower_literal_vec(vs.into_vec())?;
                Ok(ty::Ty::Vec(fixed_literals.into_boxed_slice()).into_poly())
            }
            _ => Err(Error::new(
                datum.span(),
                ErrorKind::IllegalArg("only boolean and symbol literals are supported"),
            )),
        }
    }

    fn lower_ident(&self, span: Span, ident: Ident) -> Result<ty::Poly> {
        match self.scope.get(&ident) {
            Some(Binding::Ty(ty)) => Ok(ty),
            Some(_) => Err(Error::new(span, ErrorKind::ValueAsTy)),
            None => Err(Error::new(span, ErrorKind::UnboundSym(ident.into_name()))),
        }
    }

    fn lower_poly(&self, datum: NsDatum) -> Result<ty::Poly> {
        match datum {
            NsDatum::List(span, vs) => {
                let mut data_iter = vs.into_vec().into_iter();
                let data_len = data_iter.len();

                if data_len == 0 {
                    // This is by analogy with () being self-evaluating in expressions
                    return Ok(ty::Ty::List(ty::List::new(Box::new([]), None)).into_poly());
                }

                if data_len >= 2 {
                    if let Some(purity) =
                        try_lower_purity(self.scope, &data_iter.as_slice()[data_len - 2])
                    {
                        // This is a function type
                        return self.lower_fun_cons(purity, data_iter);
                    };
                }

                let fn_datum = data_iter.next().unwrap();

                if let NsDatum::Ident(ident_span, ref ident) = fn_datum {
                    match self.scope.get(ident) {
                        Some(Binding::Prim(Prim::Quote)) => {
                            let literal_datum = expect_one_arg(span, data_iter)?;
                            return Self::lower_literal(literal_datum);
                        }
                        Some(Binding::TyCons(ty_cons)) => {
                            return self.lower_ty_cons_apply(span, ty_cons, data_iter);
                        }
                        None => {
                            return Err(Error::new(
                                ident_span,
                                ErrorKind::UnboundSym(ident.name().into()),
                            ));
                        }
                        Some(_) => {}
                    }
                }

                Err(Error::new(
                    fn_datum.span(),
                    ErrorKind::IllegalArg("type constructor expected"),
                ))
            }
            NsDatum::Ident(span, ident) => self.lower_ident(span, ident),
            _ => Self::lower_literal(datum),
        }
    }
}

pub fn lower_polymorphic_var(
    tvars: &[ty::TVar],
    scope: &Scope,
    tvar_datum: NsDatum,
) -> Result<PolymorphicVar> {
    let ctx = LowerTyContext { tvars, scope };
    ctx.lower_polymorphic_var(tvar_datum)
}

pub fn lower_poly(tvars: &[ty::TVar], scope: &Scope, datum: NsDatum) -> Result<ty::Poly> {
    let ctx = LowerTyContext { tvars, scope };
    ctx.lower_poly(datum)
}

pub fn try_lower_purity(scope: &Scope, datum: &NsDatum) -> Option<ty::purity::Poly> {
    scope.get_datum(datum).and_then(|binding| match binding {
        Binding::Purity(purity) => Some(purity),
        _ => None,
    })
}

pub fn insert_ty_exports(exports: &mut HashMap<Box<str>, Binding>) {
    macro_rules! export_ty {
        ($name:expr, $type:expr) => {
            exports.insert($name.into(), Binding::Ty($type));
        };
    }

    macro_rules! export_ty_cons {
        ($name:expr, $ty_cons:expr) => {
            exports.insert($name.into(), Binding::TyCons($ty_cons));
        };
    }

    macro_rules! export_purity {
        ($name:expr, $purity:expr) => {
            exports.insert($name.into(), Binding::Purity($purity));
        };
    }

    export_ty!("Any", ty::Ty::Any.into_poly());

    export_ty!("Bool", ty::Ty::Bool.into_poly());
    export_ty!("Sym", ty::Ty::Sym.into_poly());
    export_ty!("Str", ty::Ty::Str.into_poly());
    export_ty!("Int", ty::Ty::Int.into_poly());
    export_ty!("Float", ty::Ty::Float.into_poly());
    export_ty!("Char", ty::Ty::Char.into_poly());

    export_ty_cons!("List", TyCons::List);
    export_ty_cons!("Listof", TyCons::Listof);
    export_ty_cons!("Vector", TyCons::Vector);
    export_ty_cons!("Vectorof", TyCons::Vectorof);
    export_ty_cons!("Setof", TyCons::Set);
    export_ty_cons!("Map", TyCons::Map);
    export_ty_cons!("U", TyCons::Union);

    export_ty_cons!("Type?", TyCons::TyPred);

    export_purity!("->", Purity::Pure.into_poly());
    export_purity!("->!", Purity::Impure.into_poly());

    #[cfg(test)]
    export_ty_cons!("RawU", TyCons::RawU);
}

struct StrForPolyContext<'vars> {
    pvars: &'vars [ty::purity::PVar],
    tvars: &'vars [ty::TVar],
}

impl<'vars> StrForPolyContext<'vars> {
    /// Pushes the arguments for a list constructor on to the passed Vec
    ///
    /// This is used to share code between list and function types
    fn push_list_parts(&self, list_parts: &mut Vec<String>, list_poly: &ty::List<ty::Poly>) {
        for fixed in list_poly.fixed() {
            list_parts.push(self.str_for_poly(fixed));
        }
        if let Some(rest) = list_poly.rest() {
            list_parts.push(self.str_for_poly(rest));
            list_parts.push("...".to_owned());
        }
    }

    fn str_for_purity(&self, purity: &ty::purity::Poly) -> String {
        match purity {
            ty::purity::Poly::Fixed(Purity::Pure) => "->".to_owned(),
            ty::purity::Poly::Fixed(Purity::Impure) => "->!".to_owned(),
            ty::purity::Poly::Var(pvar_id) => self.pvars[pvar_id.to_usize()].source_name().into(),
        }
    }

    fn str_for_bounds(
        &self,
        pvar_ids: &Range<ty::purity::PVarId>,
        tvar_ids: &Range<ty::TVarId>,
    ) -> String {
        let pvar_ids_usize = pvar_ids.start.to_usize()..pvar_ids.end.to_usize();
        let tvar_ids_usize = tvar_ids.start.to_usize()..tvar_ids.end.to_usize();

        let pvar_parts = pvar_ids_usize
            .into_iter()
            .map(|pvar_id_usize| format!("[{} : ->!]", self.pvars[pvar_id_usize].source_name()));

        let tvar_parts = tvar_ids_usize.into_iter().map(|tvar_id_usize| {
            let tvar = &self.tvars[tvar_id_usize];

            if tvar.bound() == &ty::Ty::Any.into_poly() {
                return tvar.source_name().into();
            }

            format!(
                "[{} : {}]",
                tvar.source_name(),
                self.str_for_poly(tvar.bound())
            )
        });

        let all_parts = pvar_parts.chain(tvar_parts).collect::<Vec<String>>();
        format!("#{{{}}}", all_parts.join(" "))
    }

    fn str_for_poly_ty(&self, poly_ty: &ty::Ty<ty::Poly>) -> String {
        match poly_ty {
            ty::Ty::Any => "Any".to_owned(),
            ty::Ty::Bool => "Bool".to_owned(),
            ty::Ty::Char => "Char".to_owned(),
            ty::Ty::Int => "Int".to_owned(),
            ty::Ty::Sym => "Sym".to_owned(),
            ty::Ty::Str => "Str".to_owned(),
            ty::Ty::Float => "Float".to_owned(),
            ty::Ty::LitBool(false) => "false".to_owned(),
            ty::Ty::LitBool(true) => "true".to_owned(),
            ty::Ty::LitSym(name) => format!("'{}", name),
            ty::Ty::Map(map) => format!(
                "(Map {} {})",
                self.str_for_poly(map.key()),
                self.str_for_poly(map.value())
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

                self.push_list_parts(&mut fun_parts, fun.params());
                fun_parts.push(self.str_for_purity(fun.purity()));
                fun_parts.push(self.str_for_poly(fun.ret()));

                if fun.is_monomorphic() {
                    format!("({})", fun_parts.join(" "))
                } else {
                    format!(
                        "{} ({})",
                        self.str_for_bounds(fun.pvar_ids(), fun.tvar_ids()),
                        fun_parts.join(" ")
                    )
                }
            }
            ty::Ty::TyPred(test) => format!("(Type? {})", self.str_for_poly(test)),
            ty::Ty::Union(members) => {
                let member_strs: Vec<String> = members
                    .iter()
                    .map(|m| format!(" {}", self.str_for_poly(m)))
                    .collect();

                format!("(U{})", member_strs.join(""))
            }
            ty::Ty::List(list) => {
                // While all list types can be expressed using `(List)` we try to find the shortest
                // representation
                if list.fixed().is_empty() {
                    match list.rest() {
                        Some(rest) => format!("(Listof {})", self.str_for_poly(rest)),
                        None => "()".to_owned(),
                    }
                } else {
                    let mut list_parts = Vec::with_capacity(2);

                    list_parts.push("List".to_owned());
                    self.push_list_parts(&mut list_parts, list);

                    format!("({})", list_parts.join(" "),)
                }
            }
        }
    }

    fn str_for_poly(&self, poly: &ty::Poly) -> String {
        match poly {
            ty::Poly::Var(tvar_id) => {
                // TODO: It's possible to have tvars with overlapping source names
                self.tvars[tvar_id.to_usize()].source_name().to_owned()
            }
            ty::Poly::Fixed(poly_ty) => self.str_for_poly_ty(poly_ty),
        }
    }
}

pub fn str_for_poly(pvars: &[ty::purity::PVar], tvars: &[ty::TVar], poly: &ty::Poly) -> Box<str> {
    let ctx = StrForPolyContext { pvars, tvars };
    ctx.str_for_poly(poly).into_boxed_str()
}

pub fn str_for_purity(pvars: &[ty::purity::PVar], purity: &ty::purity::Poly) -> Box<str> {
    let ctx = StrForPolyContext { pvars, tvars: &[] };
    ctx.str_for_purity(purity).into_boxed_str()
}

#[cfg(test)]
pub fn poly_for_str(datum_str: &str) -> ty::Poly {
    use hir::ns::NsId;
    use hir::prim::insert_prim_exports;
    use syntax::parser::datum_from_str;
    use syntax::span::EMPTY_SPAN;

    let test_ns_id = NsId::new(1);

    // Capture our exports
    let mut exports = HashMap::<Box<str>, Binding>::new();
    insert_prim_exports(&mut exports);
    insert_ty_exports(&mut exports);

    // Place them on our scope
    let mut scope = Scope::new_empty();
    for (name, binding) in exports {
        if *name == *"U" {
            // Using `U` in tests is very dubious as it invokes a lot of type system logic. It's
            // easy to write tautological tests due to `U` creating a simplified type. Rename to
            // `UnifyingU` so it's clear what's happening.
            scope
                .insert_binding(
                    EMPTY_SPAN,
                    Ident::new(test_ns_id, "UnifyingU".into()),
                    binding,
                )
                .unwrap();
        } else {
            scope
                .insert_binding(EMPTY_SPAN, Ident::new(test_ns_id, name), binding)
                .unwrap();
        }
    }

    // Add some test polymorphic variables
    for var_idx in 0..26 {
        let var_name = (b'A' + var_idx) as char;
        let tvar_id = ty::TVarId::new(var_idx as usize);
        let poly = ty::Poly::Var(tvar_id);

        scope
            .insert_binding(
                EMPTY_SPAN,
                Ident::new(test_ns_id, var_name.to_string().into_boxed_str()),
                Binding::Ty(poly),
            )
            .unwrap();
    }

    for var_idx in 0..26 {
        let var_name = format!("->{}", (b'A' + var_idx) as char);
        let pvar_id = ty::purity::PVarId::new(var_idx as usize);
        let poly = ty::purity::Poly::Var(pvar_id);

        scope
            .insert_binding(
                EMPTY_SPAN,
                Ident::new(test_ns_id, var_name.to_string().into_boxed_str()),
                Binding::Purity(poly),
            )
            .unwrap();
    }

    let test_datum = datum_from_str(datum_str).unwrap();

    lower_poly(
        &[],
        &scope,
        NsDatum::from_syntax_datum(test_ns_id, test_datum),
    ).unwrap()
}

#[cfg(test)]
mod test {
    use super::*;

    fn assert_poly_for_str(expected: &ty::Poly, datum_str: &str) {
        assert_eq!(*expected, poly_for_str(datum_str));

        // Try to round trip this to make sure str_for_poly works
        let recovered_str = str_for_poly(&[], &[], &expected);
        assert_eq!(*expected, poly_for_str(&recovered_str));
    }

    /// This asserts that a type uses an exact string in str_for_poly
    ///
    /// This is to make sure we use e.g. `(Listof Int)` instead of `(List Int ...)`
    fn assert_exact_str_repr(datum_str: &str) {
        assert_eq!(
            datum_str,
            str_for_poly(&[], &[], &poly_for_str(datum_str)).as_ref()
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

        let expected = ty::Ty::LitSym("foo".into()).into_poly();
        assert_poly_for_str(&expected, j);
    }

    #[test]
    fn empty_list_literal() {
        let j = "()";

        let expected = ty::Ty::List(ty::List::new(Box::new([]), None)).into_poly();
        assert_poly_for_str(&expected, j);
    }

    #[test]
    fn quoted_list_literal() {
        let j = "'(true false)";

        let expected = ty::Ty::List(ty::List::new(
            Box::new([
                ty::Ty::LitBool(true).into_poly(),
                ty::Ty::LitBool(false).into_poly(),
            ]),
            None,
        )).into_poly();
        assert_poly_for_str(&expected, j);
    }

    #[test]
    fn empty_vector_literal() {
        let j = "[]";

        let expected = ty::Ty::Vec(Box::new([])).into_poly();
        assert_poly_for_str(&expected, j);
    }

    #[test]
    fn vector_literal() {
        let j = "[true false]";

        let expected = ty::Ty::Vec(Box::new([
            ty::Ty::LitBool(true).into_poly(),
            ty::Ty::LitBool(false).into_poly(),
        ])).into_poly();
        assert_poly_for_str(&expected, j);
    }

    #[test]
    fn ty_ref() {
        let j = "Sym";

        let expected = ty::Ty::Sym.into_poly();
        assert_poly_for_str(&expected, j);
    }

    #[test]
    fn listof_cons() {
        let j = "(Listof true)";

        let inner_ty = ty::Ty::LitBool(true).into_poly();
        let expected = ty::Ty::List(ty::List::new(Box::new([]), Some(inner_ty))).into_poly();

        assert_poly_for_str(&expected, j);
    }

    #[test]
    fn fixed_list_cons() {
        let j = "(List true false)";

        let expected = ty::Ty::List(ty::List::new(
            Box::new([
                ty::Ty::LitBool(true).into_poly(),
                ty::Ty::LitBool(false).into_poly(),
            ]),
            None,
        )).into_poly();

        assert_poly_for_str(&expected, j);
    }

    #[test]
    fn rest_list_cons() {
        let j = "(List true false ...)";

        let expected = ty::Ty::List(ty::List::new(
            Box::new([ty::Ty::LitBool(true).into_poly()]),
            Some(ty::Ty::LitBool(false).into_poly()),
        )).into_poly();

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

        let expected = ty::Ty::Vec(Box::new([
            ty::Ty::LitBool(true).into_poly(),
            ty::Ty::LitBool(false).into_poly(),
        ])).into_poly();

        assert_poly_for_str(&expected, j);
    }

    #[test]
    fn pure_fun() {
        let j = "(-> true)";

        let expected = ty::Fun::new(
            ty::purity::PVarIds::monomorphic(),
            ty::TVarIds::monomorphic(),
            ty::TopFun::new(Purity::Pure.into_poly(), ty::Ty::LitBool(true).into_poly()),
            ty::List::new(Box::new([]), None),
        ).into_ty_ref();

        assert_poly_for_str(&expected, j);
    }

    #[test]
    fn impure_fun() {
        let j = "(->! true)";

        let expected = ty::Fun::new(
            ty::purity::PVarIds::monomorphic(),
            ty::TVarIds::monomorphic(),
            ty::TopFun::new(
                Purity::Impure.into_poly(),
                ty::Ty::LitBool(true).into_poly(),
            ),
            ty::List::new(Box::new([]), None),
        ).into_ty_ref();

        assert_poly_for_str(&expected, j);
    }

    #[test]
    fn fixed_fun() {
        let j = "(false -> true)";

        let expected = ty::Fun::new(
            ty::purity::PVarIds::monomorphic(),
            ty::TVarIds::monomorphic(),
            ty::TopFun::new(Purity::Pure.into_poly(), ty::Ty::LitBool(true).into_poly()),
            ty::List::new(Box::new([ty::Ty::LitBool(false).into_poly()]), None),
        ).into_ty_ref();

        assert_poly_for_str(&expected, j);
    }

    #[test]
    fn rest_impure_fun() {
        let j = "(Str Sym ... ->! true)";

        let expected = ty::Fun::new(
            ty::purity::PVarIds::monomorphic(),
            ty::TVarIds::monomorphic(),
            ty::TopFun::new(
                Purity::Impure.into_poly(),
                ty::Ty::LitBool(true).into_poly(),
            ),
            ty::List::new(
                Box::new([ty::Ty::Str.into_poly()]),
                Some(ty::Ty::Sym.into_poly()),
            ),
        ).into_ty_ref();

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
        let j = "(Type? Str)";

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
        let expected = ty::Ty::Map(Box::new(ty::Map::new(
            key_ty.into_poly(),
            value_ty.into_poly(),
        ))).into_poly();

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
        assert_exact_str_repr("(Float Int ... -> Sym)");
    }

    #[test]
    fn polymorphic_fun_str() {
        // These have no constructors so they can't be round-tripped
        let pfun = ty::Fun::new(
            ty::purity::PVarId::new(0)..ty::purity::PVarId::new(1),
            ty::TVarId::new(0)..ty::TVarId::new(2),
            ty::TopFun::new(
                ty::purity::Poly::Var(ty::purity::PVarId::new(0)),
                ty::Poly::Var(ty::TVarId::new(0)),
            ),
            ty::List::new(
                Box::new([
                    ty::Poly::Var(ty::TVarId::new(1)),
                    ty::Poly::Var(ty::TVarId::new(2)),
                ]),
                None,
            ),
        );

        let pvars = &[ty::purity::PVar::new("->?".into())];
        let tvars = &[
            ty::TVar::new("A".into(), ty::Ty::Any.into_poly()),
            ty::TVar::new("B".into(), ty::Ty::Int.into_poly()),
            ty::TVar::new("C".into(), ty::Ty::Float.into_poly()),
        ];

        let actual_str = str_for_poly(pvars, tvars, &pfun.into_ty_ref());
        assert_eq!(
            "#{[->? : ->!] A [B : Int]} (B C ->? A)",
            actual_str.as_ref()
        );
    }
}
