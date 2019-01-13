use syntax::span::Span;

use crate::hir::error::{Error, ErrorKind, Result};
use crate::hir::ns::{Ident, NsDataIter, NsDatum};
use crate::hir::prim::Prim;
use crate::hir::scope::{Binding, Scope};
use crate::hir::util::{
    expect_arg_count, expect_ident_and_span, expect_one_arg, try_take_rest_arg,
};
use crate::ty;
use crate::ty::purity;
use crate::ty::purity::Purity;

#[derive(Debug, PartialEq, Eq, Hash, Clone, Copy)]
pub enum TyCons {
    List,
    Listof,
    Vector,
    Vectorof,
    Set,
    Map,
    Union,
    #[cfg(test)]
    RawU,
}

pub enum PolymorphicVarKind {
    TVar(ty::TVar),
    TFixed(ty::Poly),
    PVar(purity::PVar),
    Pure,
}

pub struct PolymorphicVar {
    pub span: Span,
    pub ident: Ident,
    pub kind: PolymorphicVarKind,
}

fn lower_polymorphic_var(scope: &Scope, tvar_datum: NsDatum) -> Result<PolymorphicVar> {
    let span = tvar_datum.span();

    match tvar_datum {
        NsDatum::Ident(span, ident) => {
            let source_name = ident.name().into();
            return Ok(PolymorphicVar {
                span,
                ident,
                kind: PolymorphicVarKind::TVar(ty::TVar::new(source_name, ty::Ty::Any.into_poly())),
            });
        }
        NsDatum::Vector(vector_span, vs) => {
            let mut arg_data = vs.into_vec();

            if arg_data.len() == 2 {
                let bound_datum = arg_data.pop().unwrap();
                let (ident, span) = expect_ident_and_span(arg_data.pop().unwrap())?;

                let source_name = ident.name().into();

                match try_lower_purity(scope, &bound_datum) {
                    Some(purity::Poly::Fixed(Purity::Impure)) => {
                        return Ok(PolymorphicVar {
                            span,
                            ident,
                            kind: PolymorphicVarKind::PVar(purity::PVar::new(source_name)),
                        });
                    }
                    Some(purity::Poly::Fixed(Purity::Pure)) => {
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
                        let bound_ty = lower_poly(scope, bound_datum)?;

                        let kind = if ty::props::has_subtypes(&bound_ty) {
                            PolymorphicVarKind::TVar(ty::TVar::new(source_name, bound_ty))
                        } else {
                            PolymorphicVarKind::TFixed(bound_ty)
                        };

                        return Ok(PolymorphicVar { span, ident, kind });
                    }
                }
            }
        }
        _ => {}
    }

    Err(Error::new(
        span,
        ErrorKind::IllegalArg(
            "polymorphic variables must be either an identifier or [identifier Type]",
        ),
    ))
}

fn lower_list_cons(scope: &Scope, mut arg_iter: NsDataIter) -> Result<ty::List<ty::Poly>> {
    let rest = try_take_rest_arg(scope, &mut arg_iter);

    let fixed_polys = arg_iter
        .map(|fixed_datum| lower_poly(scope, fixed_datum))
        .collect::<Result<Vec<ty::Poly>>>()?
        .into_boxed_slice();

    let rest_poly = match rest {
        Some(rest_datum) => Some(lower_poly(scope, rest_datum)?),
        None => None,
    };

    Ok(ty::List::new(fixed_polys, rest_poly))
}

fn lower_fun_cons(
    scope: &Scope,
    purity: purity::Poly,
    mut arg_iter: NsDataIter,
) -> Result<ty::Poly> {
    let ret_ty = lower_poly(scope, arg_iter.next_back().unwrap())?;

    // Discard the purity
    arg_iter.next_back();

    let top_fun = ty::TopFun::new(purity, ret_ty);

    if arg_iter.len() == 1
        && scope.get_datum(&arg_iter.as_slice()[0]) == Some(&Binding::Prim(Prim::Ellipsis))
    {
        // Top function type in the form `(... -> ReturnType)`
        Ok(top_fun.into_ty_ref())
    } else {
        let params = lower_list_cons(scope, arg_iter)?;
        Ok(ty::Fun::new(purity::PVars::new(), ty::TVars::new(), top_fun, params).into_ty_ref())
    }
}

fn lower_ty_cons_apply(
    scope: &Scope,
    span: Span,
    ty_cons: TyCons,
    mut arg_iter: NsDataIter,
) -> Result<ty::Poly> {
    match ty_cons {
        TyCons::List => Ok(ty::Ty::List(lower_list_cons(scope, arg_iter)?).into_poly()),
        TyCons::Listof => {
            let rest_datum = expect_one_arg(span, arg_iter)?;
            let rest_poly = lower_poly(scope, rest_datum)?;
            let list_poly = ty::List::new(Box::new([]), Some(rest_poly));

            Ok(ty::Ty::List(list_poly).into_poly())
        }
        TyCons::Vector => {
            let member_tys = arg_iter
                .map(|arg_datum| lower_poly(scope, arg_datum))
                .collect::<Result<Vec<ty::Poly>>>()?
                .into_boxed_slice();

            Ok(ty::Ty::Vector(member_tys).into_poly())
        }
        TyCons::Vectorof => {
            let start_datum = expect_one_arg(span, arg_iter)?;
            let start_ty = lower_poly(scope, start_datum)?;
            Ok(ty::Ty::Vectorof(Box::new(start_ty)).into_poly())
        }
        TyCons::Set => {
            let member_datum = expect_one_arg(span, arg_iter)?;
            let member_ty = lower_poly(scope, member_datum)?;
            Ok(ty::Ty::Set(Box::new(member_ty)).into_poly())
        }
        TyCons::Map => {
            expect_arg_count(span, 2, arg_iter.len())?;
            let key_ty = lower_poly(scope, arg_iter.next().unwrap())?;
            let value_ty = lower_poly(scope, arg_iter.next().unwrap())?;
            Ok(ty::Ty::Map(Box::new(ty::Map::new(key_ty, value_ty))).into_poly())
        }
        TyCons::Union => {
            let member_tys = arg_iter
                .map(|arg_datum| lower_poly(scope, arg_datum))
                .collect::<Result<Vec<ty::Poly>>>()?;

            Ok(ty::unify::unify_ty_ref_iter(
                scope.tvars(),
                member_tys.into_iter(),
            ))
        }
        #[cfg(test)]
        TyCons::RawU => {
            // This performs a union *without* unifying the types. This is used when testing the
            // union code itself
            let member_tys = arg_iter
                .map(|arg_datum| lower_poly(scope, arg_datum))
                .collect::<Result<Vec<ty::Poly>>>()?;

            Ok(ty::Ty::Union(member_tys.into_boxed_slice()).into_poly())
        }
    }
}

fn lower_literal_vec(literal_data: Vec<NsDatum>) -> Result<Vec<ty::Poly>> {
    literal_data.into_iter().map(lower_literal).collect()
}

fn lower_literal(datum: NsDatum) -> Result<ty::Poly> {
    match datum {
        NsDatum::Bool(_, v) => Ok(ty::Ty::LitBool(v).into_poly()),
        NsDatum::Ident(_, ident) => Ok(ty::Ty::LitSym(ident.name().into()).into_poly()),
        NsDatum::List(_, vs) => {
            let fixed_literals = lower_literal_vec(vs.into_vec())?;
            Ok(ty::Ty::List(ty::List::new(fixed_literals.into_boxed_slice(), None)).into_poly())
        }
        NsDatum::Vector(_, vs) => {
            let fixed_literals = lower_literal_vec(vs.into_vec())?;
            Ok(ty::Ty::Vector(fixed_literals.into_boxed_slice()).into_poly())
        }
        _ => Err(Error::new(
            datum.span(),
            ErrorKind::IllegalArg("only boolean and symbol type literal atoms are supported"),
        )),
    }
}

fn lower_ident(scope: &Scope, span: Span, ident: &Ident) -> Result<ty::Poly> {
    if ident.is_keyword() {
        // Keywords are self-evaluating
        return Ok(ty::Ty::LitSym(ident.name().into()).into_poly());
    }

    match scope.get_or_err(span, ident)? {
        Binding::Ty(ty) => Ok(ty.clone()),
        Binding::TyPred(test_ty) => Ok(ty::Ty::TyPred(*test_ty).into_poly()),
        Binding::EqPred => Ok(ty::Ty::EqPred.into_poly()),
        _ => Err(Error::new(span, ErrorKind::ValueAsTy)),
    }
}

fn lower_polymorphic_poly(
    scope: &Scope,
    span: Span,
    mut data_iter: NsDataIter,
) -> Result<ty::Poly> {
    let polymorphic_vars_datum = data_iter.next().unwrap();
    let polymorphic_var_data = if let NsDatum::Set(_, vs) = polymorphic_vars_datum {
        vs
    } else {
        return Err(Error::new(
            polymorphic_vars_datum.span(),
            ErrorKind::IllegalArg("polymorphic variable set expected"),
        ));
    };

    let mut inner_scope = Scope::new_child(scope);
    let (pvars, tvars) = lower_polymorphic_vars(
        polymorphic_var_data.into_vec().into_iter(),
        scope,
        &mut inner_scope,
    )?;

    let inner_poly = lower_poly_data_iter(&inner_scope, span, data_iter)?;

    if let ty::Poly::Fixed(ty::Ty::Fun(fun)) = inner_poly {
        Ok(ty::Ty::Fun(Box::new(fun.with_polymorphic_vars(pvars, tvars))).into_poly())
    } else {
        return Err(Error::new(
            span,
            ErrorKind::IllegalArg("polymorphism only supported by function type"),
        ));
    }
}

pub fn lower_poly_data_iter(
    scope: &Scope,
    span: Span,
    mut data_iter: NsDataIter,
) -> Result<ty::Poly> {
    let data_len = data_iter.len();

    if data_len == 0 {
        // This is by analogy with () being self-evaluating in expressions
        return Ok(ty::Ty::List(ty::List::empty()).into_poly());
    }

    if scope.get_datum(&data_iter.as_slice()[0]) == Some(&Binding::Prim(Prim::All)) {
        // Discard the `All`
        data_iter.next();

        return lower_polymorphic_poly(scope, span, data_iter);
    }

    if data_len >= 2 {
        if let Some(purity) = try_lower_purity(scope, &data_iter.as_slice()[data_len - 2]) {
            // This is a function type
            return lower_fun_cons(scope, purity, data_iter);
        };
    }

    let fn_datum = data_iter.next().unwrap();

    if let NsDatum::Ident(ident_span, ref ident) = fn_datum {
        match scope.get_or_err(ident_span, ident)? {
            Binding::Prim(Prim::Quote) => {
                let literal_datum = expect_one_arg(span, data_iter)?;
                return lower_literal(literal_datum);
            }
            Binding::TyCons(ty_cons) => {
                return lower_ty_cons_apply(scope, span, *ty_cons, data_iter);
            }
            _ => {}
        }
    }
    Err(Error::new(
        fn_datum.span(),
        ErrorKind::IllegalArg("type constructor expected"),
    ))
}

pub fn lower_poly(scope: &Scope, datum: NsDatum) -> Result<ty::Poly> {
    match datum {
        NsDatum::List(span, vs) => lower_poly_data_iter(scope, span, vs.into_vec().into_iter()),
        NsDatum::Ident(span, ident) => lower_ident(scope, span, &ident),
        _ => lower_literal(datum),
    }
}

/// Lowers a set of polymorphic variables defined in `outer_scope` and places them in `inner_scope`
pub fn lower_polymorphic_vars(
    polymorphic_var_data: NsDataIter,
    outer_scope: &Scope,
    inner_scope: &mut Scope,
) -> Result<(purity::PVars, ty::TVars)> {
    let mut pvars = purity::PVars::new();
    let mut tvars = ty::TVars::new();

    for var_datum in polymorphic_var_data {
        let PolymorphicVar { span, ident, kind } = lower_polymorphic_var(outer_scope, var_datum)?;

        let binding = match kind {
            PolymorphicVarKind::PVar(pvar) => {
                let pvar_id = purity::PVarId::alloc();
                pvars.insert(pvar_id, pvar);

                Binding::Purity(purity::Poly::Var(pvar_id))
            }
            PolymorphicVarKind::TVar(tvar) => {
                let tvar_id = ty::TVarId::alloc();
                tvars.insert(tvar_id, tvar.clone());
                inner_scope.tvars_mut().insert(tvar_id, tvar);

                Binding::Ty(ty::Poly::Var(tvar_id))
            }
            PolymorphicVarKind::TFixed(poly) => Binding::Ty(poly),
            PolymorphicVarKind::Pure => Binding::Purity(Purity::Pure.into_poly()),
        };

        inner_scope.insert_binding(span, ident, binding)?;
    }

    Ok((pvars, tvars))
}

pub fn try_lower_purity(scope: &Scope, datum: &NsDatum) -> Option<purity::Poly> {
    scope.get_datum(datum).and_then(|binding| match binding {
        Binding::Purity(purity) => Some(purity.clone()),
        _ => None,
    })
}

macro_rules! export_ty {
    ($name:expr, $type:expr) => {
        ($name, Binding::Ty(ty::Poly::Fixed($type)))
    };
}

macro_rules! export_ty_cons {
    ($name:expr, $ty_cons:expr) => {
        ($name, Binding::TyCons($ty_cons))
    };
}

macro_rules! export_purity {
    ($name:expr, $purity:expr) => {
        ($name, Binding::Purity(purity::Poly::Fixed($purity)))
    };
}

macro_rules! export_ty_pred {
    ($name:expr, $test_ty:expr) => {
        ($name, Binding::TyPred($test_ty))
    };
}

pub const TY_EXPORTS: &[(&str, Binding)] = &[
    export_ty!("Any", ty::Ty::Any),
    export_ty!("Bool", ty::Ty::Bool),
    export_ty!("Sym", ty::Ty::Sym),
    export_ty!("Str", ty::Ty::Str),
    export_ty!("Int", ty::Ty::Int),
    export_ty!("Float", ty::Ty::Float),
    export_ty!("Num", ty::Ty::Num),
    export_ty!("Char", ty::Ty::Char),
    export_ty_cons!("List", TyCons::List),
    export_ty_cons!("Listof", TyCons::Listof),
    export_ty_cons!("Vector", TyCons::Vector),
    export_ty_cons!("Vectorof", TyCons::Vectorof),
    export_ty_cons!("Setof", TyCons::Set),
    export_ty_cons!("Map", TyCons::Map),
    export_ty_cons!("U", TyCons::Union),
    export_purity!("->", Purity::Pure),
    export_purity!("->!", Purity::Impure),
    export_ty_pred!("str?", ty::pred::TestTy::Str),
    export_ty_pred!("sym?", ty::pred::TestTy::Sym),
    export_ty_pred!("bool?", ty::pred::TestTy::Bool),
    export_ty_pred!("int?", ty::pred::TestTy::Int),
    export_ty_pred!("float?", ty::pred::TestTy::Float),
    export_ty_pred!("char?", ty::pred::TestTy::Char),
    export_ty_pred!("list?", ty::pred::TestTy::List),
    export_ty_pred!("vector?", ty::pred::TestTy::Vector),
    export_ty_pred!("set?", ty::pred::TestTy::Set),
    export_ty_pred!("map?", ty::pred::TestTy::Map),
    export_ty_pred!("fn?", ty::pred::TestTy::Fun),
    export_ty_pred!("nil?", ty::pred::TestTy::Nil),
    #[cfg(test)]
    export_ty_cons!("RawU", TyCons::RawU),
];

/// Pushes the arguments for a list constructor on to the passed Vec
///
/// This is used to share code between list and function types
fn push_list_parts(
    pvars: &purity::PVars,
    tvars: &ty::TVars,
    list_parts: &mut Vec<String>,
    list_poly: &ty::List<ty::Poly>,
) {
    for fixed in list_poly.fixed() {
        list_parts.push(str_for_poly(pvars, tvars, fixed));
    }
    if let Some(rest) = list_poly.rest() {
        list_parts.push(str_for_poly(pvars, tvars, rest));
        list_parts.push("...".to_owned());
    }
}

fn str_for_bounds(
    all_pvars: &purity::PVars,
    all_tvars: &ty::TVars,
    bound_pvars: &purity::PVars,
    bound_tvars: &ty::TVars,
) -> String {
    let pvar_parts = bound_pvars
        .values()
        .map(|pvar| format!("[{} ->!]", pvar.source_name()));

    let tvar_parts = bound_tvars.values().map(|tvar| {
        if tvar.bound() == &ty::Ty::Any.into_poly() {
            return tvar.source_name().into();
        }

        format!(
            "[{} {}]",
            tvar.source_name(),
            str_for_poly(all_pvars, all_tvars, tvar.bound())
        )
    });

    let all_parts = pvar_parts.chain(tvar_parts).collect::<Vec<String>>();
    format!("#{{{}}}", all_parts.join(" "))
}

fn str_for_poly_ty(pvars: &purity::PVars, tvars: &ty::TVars, poly_ty: &ty::Ty<ty::Poly>) -> String {
    match poly_ty {
        ty::Ty::Any => "Any".to_owned(),
        ty::Ty::Bool => "Bool".to_owned(),
        ty::Ty::Char => "Char".to_owned(),
        ty::Ty::Int => "Int".to_owned(),
        ty::Ty::Sym => "Sym".to_owned(),
        ty::Ty::Str => "Str".to_owned(),
        ty::Ty::Float => "Float".to_owned(),
        ty::Ty::Num => "Num".to_owned(),
        ty::Ty::LitBool(false) => "false".to_owned(),
        ty::Ty::LitBool(true) => "true".to_owned(),
        ty::Ty::LitSym(name) => format!("'{}", name),
        ty::Ty::Map(map) => format!(
            "(Map {} {})",
            str_for_poly(pvars, tvars, map.key()),
            str_for_poly(pvars, tvars, map.value())
        ),
        ty::Ty::Set(member) => format!("(Setof {})", str_for_poly(pvars, tvars, member)),
        ty::Ty::Vector(members) => {
            let result_parts: Vec<String> = members
                .iter()
                .map(|member| format!(" {}", str_for_poly(pvars, tvars, member)))
                .collect();

            format!("(Vector{})", result_parts.join(""))
        }
        ty::Ty::Vectorof(member) => format!("(Vectorof {})", str_for_poly(pvars, tvars, member)),
        ty::Ty::TopFun(top_fun) => format!(
            "(... {} {})",
            str_for_purity(pvars, top_fun.purity()),
            str_for_poly(pvars, tvars, top_fun.ret())
        ),
        ty::Ty::Fun(fun) => {
            let mut fun_parts = Vec::with_capacity(2);

            let inner_pvars = purity::merge_pvars(pvars, fun.pvars());
            let inner_tvars = ty::merge_tvars(tvars, fun.tvars());

            push_list_parts(&inner_pvars, &inner_tvars, &mut fun_parts, fun.params());
            fun_parts.push(str_for_purity(&inner_pvars, fun.purity()));
            fun_parts.push(str_for_poly(&inner_pvars, &inner_tvars, fun.ret()));

            if fun.has_polymorphic_vars() {
                format!(
                    "(All {} {})",
                    str_for_bounds(&inner_pvars, &inner_tvars, fun.pvars(), fun.tvars()),
                    fun_parts.join(" ")
                )
            } else {
                format!("({})", fun_parts.join(" "))
            }
        }
        ty::Ty::TyPred(test_ty) => test_ty.to_str().to_owned(),
        ty::Ty::EqPred => "=".to_owned(),
        ty::Ty::Union(members) => {
            let member_strs: Vec<String> = members
                .iter()
                .map(|m| format!(" {}", str_for_poly(pvars, tvars, m)))
                .collect();

            format!("(U{})", member_strs.join(""))
        }
        ty::Ty::List(list) => {
            // While all list types can be expressed using `(List)` we try to find the shortest
            // representation
            if list.fixed().is_empty() {
                match list.rest() {
                    Some(rest) => format!("(Listof {})", str_for_poly(pvars, tvars, rest)),
                    None => "()".to_owned(),
                }
            } else {
                let mut list_parts = Vec::with_capacity(2);

                list_parts.push("List".to_owned());
                push_list_parts(pvars, tvars, &mut list_parts, list);

                format!("({})", list_parts.join(" "),)
            }
        }
    }
}

pub fn str_for_poly(pvars: &purity::PVars, tvars: &ty::TVars, poly: &ty::Poly) -> String {
    match poly {
        ty::Poly::Var(tvar_id) => tvars[tvar_id].source_name().to_owned(),
        ty::Poly::Fixed(poly_ty) => str_for_poly_ty(pvars, tvars, poly_ty),
    }
}

pub fn str_for_purity(pvars: &purity::PVars, purity: &purity::Poly) -> String {
    match purity {
        purity::Poly::Fixed(Purity::Pure) => "->".to_owned(),
        purity::Poly::Fixed(Purity::Impure) => "->!".to_owned(),
        purity::Poly::Var(pvar_id) => pvars[pvar_id].source_name().into(),
    }
}

#[cfg(test)]
pub fn poly_for_str(datum_str: &str) -> ty::Poly {
    use crate::hir::prim::PRIM_EXPORTS;
    use syntax::parser::datum_from_str;

    let prim_entries = PRIM_EXPORTS
        .iter()
        .chain(TY_EXPORTS.iter())
        .map(|(name, binding)| {
            if *name == "U" {
                // Using `U` in tests is very dubious as it invokes a lot of type system logic. It's
                // easy to write tautological tests due to `U` creating a simplified type. Rename to
                // `UnifyingU` so it's clear what's happening.
                ("UnifyingU".into(), binding.clone())
            } else {
                ((*name).into(), binding.clone())
            }
        });

    let test_ns_id = Scope::root_ns_id();
    let scope = Scope::new_with_entries(prim_entries);

    let test_datum = datum_from_str(datum_str).unwrap();

    lower_poly(&scope, NsDatum::from_syntax_datum(test_ns_id, test_datum)).unwrap()
}

#[cfg(test)]
mod test {
    use super::*;

    fn assert_poly_for_str(expected: &ty::Poly, datum_str: &str) {
        assert_eq!(*expected, poly_for_str(datum_str));

        // Try to round trip this to make sure str_for_poly works
        let recovered_str = str_for_poly(&purity::PVars::new(), &ty::TVars::new(), &expected);
        assert_eq!(*expected, poly_for_str(&recovered_str));
    }

    /// This asserts that a type uses an exact string in str_for_poly
    ///
    /// This is to make sure we use e.g. `(Listof Int)` instead of `(List Int ...)`
    fn assert_exact_str_repr(datum_str: &str) {
        assert_eq!(
            datum_str,
            str_for_poly(
                &purity::PVars::new(),
                &ty::TVars::new(),
                &poly_for_str(datum_str)
            )
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
    fn keyword_literal() {
        let j = ":foo";

        let expected = ty::Ty::LitSym(":foo".into()).into_poly();
        assert_poly_for_str(&expected, j);
    }

    #[test]
    fn empty_list_literal() {
        let j = "()";

        let expected = ty::Ty::List(ty::List::empty()).into_poly();
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
        ))
        .into_poly();
        assert_poly_for_str(&expected, j);
    }

    #[test]
    fn empty_vector_literal() {
        let j = "[]";

        let expected = ty::Ty::Vector(Box::new([])).into_poly();
        assert_poly_for_str(&expected, j);
    }

    #[test]
    fn vector_literal() {
        let j = "[true false]";

        let expected = ty::Ty::Vector(Box::new([
            ty::Ty::LitBool(true).into_poly(),
            ty::Ty::LitBool(false).into_poly(),
        ]))
        .into_poly();
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
        ))
        .into_poly();

        assert_poly_for_str(&expected, j);
    }

    #[test]
    fn rest_list_cons() {
        let j = "(List true false ...)";

        let expected = ty::Ty::List(ty::List::new(
            Box::new([ty::Ty::LitBool(true).into_poly()]),
            Some(ty::Ty::LitBool(false).into_poly()),
        ))
        .into_poly();

        assert_poly_for_str(&expected, j);
    }

    #[test]
    fn vectorof_cons() {
        let j = "(Vectorof true)";

        let inner_ty = ty::Ty::LitBool(true).into_poly();
        let expected = ty::Ty::Vectorof(Box::new(inner_ty)).into_poly();

        assert_poly_for_str(&expected, j);
    }

    #[test]
    fn vector_cons() {
        let j = "(Vector true false)";

        let expected = ty::Ty::Vector(Box::new([
            ty::Ty::LitBool(true).into_poly(),
            ty::Ty::LitBool(false).into_poly(),
        ]))
        .into_poly();

        assert_poly_for_str(&expected, j);
    }

    #[test]
    fn pure_fun() {
        let j = "(-> true)";

        let expected = ty::Fun::new(
            purity::PVars::new(),
            ty::TVars::new(),
            ty::TopFun::new(Purity::Pure.into_poly(), ty::Ty::LitBool(true).into_poly()),
            ty::List::empty(),
        )
        .into_ty_ref();

        assert_poly_for_str(&expected, j);
    }

    #[test]
    fn impure_fun() {
        let j = "(->! true)";

        let expected = ty::Fun::new(
            purity::PVars::new(),
            ty::TVars::new(),
            ty::TopFun::new(
                Purity::Impure.into_poly(),
                ty::Ty::LitBool(true).into_poly(),
            ),
            ty::List::empty(),
        )
        .into_ty_ref();

        assert_poly_for_str(&expected, j);
    }

    #[test]
    fn fixed_fun() {
        let j = "(false -> true)";

        let expected = ty::Fun::new(
            purity::PVars::new(),
            ty::TVars::new(),
            ty::TopFun::new(Purity::Pure.into_poly(), ty::Ty::LitBool(true).into_poly()),
            ty::List::new(Box::new([ty::Ty::LitBool(false).into_poly()]), None),
        )
        .into_ty_ref();

        assert_poly_for_str(&expected, j);
    }

    #[test]
    fn rest_impure_fun() {
        let j = "(Str Sym ... ->! true)";

        let expected = ty::Fun::new(
            purity::PVars::new(),
            ty::TVars::new(),
            ty::TopFun::new(
                Purity::Impure.into_poly(),
                ty::Ty::LitBool(true).into_poly(),
            ),
            ty::List::new(
                Box::new([ty::Ty::Str.into_poly()]),
                Some(ty::Ty::Sym.into_poly()),
            ),
        )
        .into_ty_ref();

        assert_poly_for_str(&expected, j);
    }

    #[test]
    fn top_impure_fun() {
        let j = "(... ->! true)";

        let expected = ty::Ty::TopFun(Box::new(ty::TopFun::new(
            Purity::Impure.into_poly(),
            ty::Ty::LitBool(true).into_poly(),
        )))
        .into_poly();

        assert_poly_for_str(&expected, j);
    }

    #[test]
    fn type_predicate() {
        let j = "str?";

        let expected = ty::Ty::TyPred(ty::pred::TestTy::Str).into_poly();
        assert_poly_for_str(&expected, j);
    }

    #[test]
    fn equality_predicate() {
        let j = "=";

        let expected = ty::Ty::EqPred.into_poly();
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
        )))
        .into_poly();

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
        assert_exact_str_repr("(All #{[->? ->!] A [B Bool] C} B C ->? A)");
    }
}
