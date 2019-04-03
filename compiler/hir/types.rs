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
    PVar(purity::PVar),
    TFixed(Span, ty::Ref<ty::Poly>),
    Pure(Span),
}

pub struct PolymorphicVar {
    pub ident: Ident,
    pub kind: PolymorphicVarKind,
}

fn lower_polymorphic_var(scope: &Scope<'_>, tvar_datum: NsDatum) -> Result<PolymorphicVar> {
    let span = tvar_datum.span();

    match tvar_datum {
        NsDatum::Ident(span, ident) => {
            let source_name = ident.name().clone();
            return Ok(PolymorphicVar {
                ident,
                kind: PolymorphicVarKind::TVar(ty::TVar::new(
                    span,
                    source_name,
                    ty::Ty::Any.into(),
                )),
            });
        }
        NsDatum::Vector(vector_span, vs) => {
            let mut arg_data = vs.into_vec();

            if arg_data.len() == 2 {
                let bound_datum = arg_data.pop().unwrap();
                let (ident, span) = expect_ident_and_span(arg_data.pop().unwrap())?;

                let source_name = ident.name().clone();

                match try_lower_purity(scope, &bound_datum) {
                    Some(purity::Ref::Fixed(Purity::Impure)) => {
                        return Ok(PolymorphicVar {
                            ident,
                            kind: PolymorphicVarKind::PVar(purity::PVar::new(span, source_name)),
                        });
                    }
                    Some(purity::Ref::Fixed(Purity::Pure)) => {
                        // Emulate bounding to pure in case the purity comes from e.g. a macro
                        // expansion
                        return Ok(PolymorphicVar {
                            ident,
                            kind: PolymorphicVarKind::Pure(span),
                        });
                    }
                    Some(_) => {
                        return Err(Error::new(
                            vector_span,
                            ErrorKind::IllegalArg(
                                "Purity variables do not support variable bounds",
                            ),
                        ));
                    }
                    None => {
                        let bound_ty = lower_poly(scope, bound_datum)?;

                        let kind = if ty::props::has_subtypes(&bound_ty) {
                            PolymorphicVarKind::TVar(ty::TVar::new(span, source_name, bound_ty))
                        } else {
                            PolymorphicVarKind::TFixed(span, bound_ty)
                        };

                        return Ok(PolymorphicVar { ident, kind });
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

fn lower_list_cons(scope: &Scope<'_>, mut arg_iter: NsDataIter) -> Result<ty::List<ty::Poly>> {
    let rest = try_take_rest_arg(&mut arg_iter);

    let fixed_polys = arg_iter
        .map(|fixed_datum| lower_poly(scope, fixed_datum))
        .collect::<Result<Box<[ty::Ref<ty::Poly>]>>>()?;

    let rest_poly = match rest {
        Some(rest_datum) => lower_poly(scope, rest_datum)?,
        None => ty::Ty::never().into(),
    };

    Ok(ty::List::new(fixed_polys, rest_poly))
}

fn lower_fun_cons(
    scope: &Scope<'_>,
    purity: purity::Ref,
    mut arg_iter: NsDataIter,
) -> Result<ty::Ref<ty::Poly>> {
    let ret_ty = lower_poly(scope, arg_iter.next_back().unwrap())?;

    // Discard the purity
    arg_iter.next_back();

    let top_fun = ty::TopFun::new(purity, ret_ty);

    if arg_iter.len() == 1 {
        if let NsDatum::Ident(_, ident) = &arg_iter.as_slice()[0] {
            if ident.is_ellipsis() {
                // Top function type in the form `(... -> ReturnType)`
                return Ok(top_fun.into());
            }
        }
    }

    let params = lower_list_cons(scope, arg_iter)?;
    Ok(ty::Fun::new(purity::PVarIds::new(), ty::TVarIds::new(), top_fun, params).into())
}

fn lower_ty_cons_apply(
    scope: &Scope<'_>,
    span: Span,
    ty_cons: TyCons,
    mut arg_iter: NsDataIter,
) -> Result<ty::Ref<ty::Poly>> {
    Ok(match ty_cons {
        TyCons::List => lower_list_cons(scope, arg_iter)?.into(),
        TyCons::Vector => {
            let member_tys = arg_iter
                .map(|arg_datum| lower_poly(scope, arg_datum))
                .collect::<Result<Box<[ty::Ref<ty::Poly>]>>>()?;

            ty::Ty::Vector(member_tys).into()
        }
        TyCons::Vectorof => {
            let start_datum = expect_one_arg(span, arg_iter)?;
            let start_ty = lower_poly(scope, start_datum)?;
            ty::Ty::Vectorof(Box::new(start_ty)).into()
        }
        TyCons::Set => {
            let member_datum = expect_one_arg(span, arg_iter)?;
            let member_ty = lower_poly(scope, member_datum)?;
            ty::Ty::Set(Box::new(member_ty)).into()
        }
        TyCons::Map => {
            expect_arg_count(span, 2, arg_iter.len())?;
            let key_ty = lower_poly(scope, arg_iter.next().unwrap())?;
            let value_ty = lower_poly(scope, arg_iter.next().unwrap())?;
            ty::Map::new(key_ty, value_ty).into()
        }
        TyCons::Union => {
            let member_tys = arg_iter
                .map(|arg_datum| lower_poly(scope, arg_datum))
                .collect::<Result<Vec<ty::Ref<ty::Poly>>>>()?;

            ty::unify::unify_ty_ref_iter(member_tys.into_iter())
        }
        #[cfg(test)]
        TyCons::RawU => {
            // This performs a union *without* unifying the types. This is used when testing the
            // union code itself
            let member_tys = arg_iter
                .map(|arg_datum| lower_poly(scope, arg_datum))
                .collect::<Result<Box<[ty::Ref<ty::Poly>]>>>()?;

            ty::Ty::Union(member_tys).into()
        }
    })
}

fn lower_literal_vec(literal_data: Vec<NsDatum>) -> Result<Vec<ty::Ref<ty::Poly>>> {
    literal_data.into_iter().map(lower_literal).collect()
}

fn lower_literal(datum: NsDatum) -> Result<ty::Ref<ty::Poly>> {
    match datum {
        NsDatum::Bool(_, v) => Ok(ty::Ty::LitBool(v).into()),
        NsDatum::Keyword(_, name) => Ok(ty::Ty::LitSym(name).into()),
        NsDatum::Ident(_, ident) => Ok(ty::Ty::LitSym(ident.into_name()).into()),
        NsDatum::List(_, vs) => {
            let fixed_literals = lower_literal_vec(vs.into_vec())?;
            Ok(ty::List::new(fixed_literals.into_boxed_slice(), ty::Ty::never().into()).into())
        }
        NsDatum::Vector(_, vs) => {
            let fixed_literals = lower_literal_vec(vs.into_vec())?;
            Ok(ty::Ty::Vector(fixed_literals.into_boxed_slice()).into())
        }
        _ => Err(Error::new(
            datum.span(),
            ErrorKind::IllegalArg("only boolean and symbol type literal atoms are supported"),
        )),
    }
}

fn lower_ident(scope: &Scope<'_>, span: Span, ident: &Ident) -> Result<ty::Ref<ty::Poly>> {
    match scope.get_or_err(span, ident)? {
        Binding::Ty(ty) => Ok(ty.clone()),
        Binding::TyPred(test_ty) => Ok(ty::Ty::TyPred(*test_ty).into()),
        Binding::EqPred => Ok(ty::Ty::EqPred.into()),
        _ => Err(Error::new(span, ErrorKind::ValueAsTy)),
    }
}

fn lower_polymorphic_poly(
    scope: &Scope<'_>,
    span: Span,
    mut data_iter: NsDataIter,
) -> Result<ty::Ref<ty::Poly>> {
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

    if let ty::Ref::Fixed(ty::Ty::Fun(fun)) = inner_poly {
        Ok(ty::Ty::Fun(Box::new(fun.with_polymorphic_vars(pvars, tvars))).into())
    } else {
        return Err(Error::new(
            span,
            ErrorKind::IllegalArg("polymorphism only supported by function type"),
        ));
    }
}

pub fn lower_poly_data_iter(
    scope: &Scope<'_>,
    span: Span,
    mut data_iter: NsDataIter,
) -> Result<ty::Ref<ty::Poly>> {
    let data_len = data_iter.len();

    if data_len == 0 {
        // This is by analogy with () being self-evaluating in expressions
        return Ok(ty::List::empty().into());
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

pub fn lower_poly(scope: &Scope<'_>, datum: NsDatum) -> Result<ty::Ref<ty::Poly>> {
    match datum {
        NsDatum::List(span, vs) => lower_poly_data_iter(scope, span, vs.into_vec().into_iter()),
        NsDatum::Ident(span, ident) => lower_ident(scope, span, &ident),
        _ => lower_literal(datum),
    }
}

/// Lowers a set of polymorphic variables defined in `outer_scope` and places them in `inner_scope`
pub fn lower_polymorphic_vars(
    polymorphic_var_data: NsDataIter,
    outer_scope: &Scope<'_>,
    inner_scope: &mut Scope<'_>,
) -> Result<(purity::PVarIds, ty::TVarIds)> {
    let mut pvar_ids = purity::PVarIds::new();
    let mut tvar_ids = ty::TVarIds::new();

    for var_datum in polymorphic_var_data {
        let PolymorphicVar { ident, kind } = lower_polymorphic_var(outer_scope, var_datum)?;

        let span;
        let binding;
        match kind {
            PolymorphicVarKind::PVar(pvar) => {
                let pvar_id = purity::PVarId::new(pvar);
                pvar_ids.push(pvar_id.clone());

                span = pvar_id.span();
                binding = Binding::Purity(purity::Ref::Var(pvar_id))
            }
            PolymorphicVarKind::TVar(tvar) => {
                let tvar_id = ty::TVarId::new(tvar);
                tvar_ids.push(tvar_id.clone());

                span = tvar_id.span();
                binding = Binding::Ty(ty::Ref::Var(tvar_id, ty::Poly {}))
            }
            PolymorphicVarKind::TFixed(fixed_span, poly) => {
                span = fixed_span;
                binding = Binding::Ty(poly);
            }
            PolymorphicVarKind::Pure(pure_span) => {
                span = pure_span;
                binding = Binding::Purity(Purity::Pure.into());
            }
        };

        inner_scope.insert_binding(span, ident, binding)?;
    }

    Ok((pvar_ids, tvar_ids))
}

pub fn try_lower_purity(scope: &Scope<'_>, datum: &NsDatum) -> Option<purity::Ref> {
    scope.get_datum(datum).and_then(|binding| match binding {
        Binding::Purity(purity) => Some(purity.clone()),
        _ => None,
    })
}

macro_rules! export_ty {
    ($name:expr, $type:expr) => {
        ($name, Binding::Ty(ty::Ref::Fixed($type)))
    };
}

macro_rules! export_ty_cons {
    ($name:expr, $ty_cons:expr) => {
        ($name, Binding::TyCons($ty_cons))
    };
}

macro_rules! export_purity {
    ($name:expr, $purity:expr) => {
        ($name, Binding::Purity(purity::Ref::Fixed($purity)))
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
    export_ty_pred!("num?", ty::pred::TestTy::Num),
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

/// Pushes the arguments for a list constructor on to the passed `Vec`
///
/// This is used to share code between list and function types
fn push_list_parts<M: ty::PM>(list_parts: &mut Vec<String>, list_ref: &ty::List<M>) {
    for fixed in list_ref.fixed() {
        list_parts.push(str_for_ty_ref(fixed));
    }

    let rest = list_ref.rest();
    if !rest.is_never() {
        list_parts.push("&".to_owned());
        list_parts.push(str_for_ty_ref(rest));
    }
}

fn str_for_bounds(bound_pvar_ids: &[purity::PVarId], bound_tvar_ids: &[ty::TVarId]) -> String {
    let pvar_parts = bound_pvar_ids
        .iter()
        .map(|pvar_id| format!("[{} ->!]", pvar_id.source_name()));

    let tvar_parts = bound_tvar_ids.iter().map(|tvar_id| {
        if tvar_id.bound() == &ty::Ty::Any.into() {
            return tvar_id.source_name().into();
        }

        format!(
            "[{} {}]",
            tvar_id.source_name(),
            str_for_ty_ref(tvar_id.bound())
        )
    });

    let all_parts = pvar_parts.chain(tvar_parts).collect::<Vec<String>>();
    format!("#{{{}}}", all_parts.join(" "))
}

fn str_for_ty<M: ty::PM>(ty: &ty::Ty<M>) -> String {
    match ty {
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
            str_for_ty_ref(map.key()),
            str_for_ty_ref(map.value())
        ),
        ty::Ty::Set(member) => format!("(Setof {})", str_for_ty_ref(member)),
        ty::Ty::Vector(members) => {
            let result_parts: Vec<String> = members
                .iter()
                .map(|member| format!(" {}", str_for_ty_ref(member)))
                .collect();

            format!("(Vector{})", result_parts.join(""))
        }
        ty::Ty::Vectorof(member) => format!("(Vectorof {})", str_for_ty_ref(member)),
        ty::Ty::TopFun(top_fun) => format!(
            "(... {} {})",
            str_for_purity(top_fun.purity()),
            str_for_ty_ref(top_fun.ret())
        ),
        ty::Ty::Fun(fun) => {
            let mut fun_parts = Vec::with_capacity(2);

            push_list_parts(&mut fun_parts, fun.params());
            fun_parts.push(str_for_purity(fun.purity()));
            fun_parts.push(str_for_ty_ref(fun.ret()));

            if fun.has_polymorphic_vars() {
                format!(
                    "(All {} {})",
                    str_for_bounds(fun.pvar_ids(), fun.tvar_ids()),
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
                .map(|m| format!(" {}", str_for_ty_ref(m)))
                .collect();

            format!("(U{})", member_strs.join(""))
        }
        ty::Ty::Intersect(members) => {
            let member_strs: Vec<String> = members
                .iter()
                .map(|m| format!(" {}", str_for_ty_ref(m)))
                .collect();

            format!("(∩{})", member_strs.join(""))
        }
        ty::Ty::List(list) => {
            // While all list types can be expressed using `(List)` we try to find the shortest
            // representation
            if list.fixed().is_empty() && list.rest().is_never() {
                "()".to_owned()
            } else {
                let mut list_parts = Vec::with_capacity(2);

                list_parts.push("List".to_owned());
                push_list_parts(&mut list_parts, list);

                format!("({})", list_parts.join(" "),)
            }
        }
    }
}

pub fn str_for_ty_ref<M: ty::PM>(ty_ref: &ty::Ref<M>) -> String {
    match ty_ref {
        ty::Ref::Var(tvar_id, _) => tvar_id.source_name().to_owned(),
        ty::Ref::Fixed(ty) => str_for_ty(ty),
    }
}

pub fn str_for_purity(purity: &purity::Ref) -> String {
    match purity {
        purity::Ref::Fixed(Purity::Pure) => "->".to_owned(),
        purity::Ref::Fixed(Purity::Impure) => "->!".to_owned(),
        purity::Ref::Var(pvar_id) => pvar_id.source_name().into(),
    }
}

#[cfg(test)]
pub fn poly_for_str(datum_str: &str) -> ty::Ref<ty::Poly> {
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
                ("UnifyingU", binding.clone())
            } else {
                (*name, binding.clone())
            }
        });

    let scope = Scope::new_with_entries(prim_entries);

    let test_datum = datum_from_str(datum_str).unwrap();
    lower_poly(&scope, NsDatum::from_syntax_datum(&test_datum)).unwrap()
}

#[cfg(test)]
pub fn tvar_bounded_by(bound: ty::Ref<ty::Poly>) -> ty::Ref<ty::Poly> {
    use syntax::span::EMPTY_SPAN;

    let tvar_id = ty::TVarId::new(ty::TVar::new(EMPTY_SPAN, "TVar".into(), bound));
    ty::Ref::Var(tvar_id, ty::Poly {})
}

#[cfg(test)]
mod test {
    use super::*;

    fn assert_ty_for_str(expected: ty::Ty<ty::Poly>, datum_str: &str) {
        let expected_poly = expected.into();
        assert_eq!(expected_poly, poly_for_str(datum_str));

        // Try to round trip this to make sure str_for_ty_ref works
        let recovered_str = str_for_ty_ref(&expected_poly);
        assert_eq!(expected_poly, poly_for_str(&recovered_str));
    }

    /// This asserts that a type uses an exact string in str_for_ty_ref
    fn assert_exact_str_repr(datum_str: &str) {
        assert_eq!(datum_str, str_for_ty_ref(&poly_for_str(datum_str)));
    }

    #[test]
    fn true_literal() {
        let j = "true";

        let expected = ty::Ty::LitBool(true);
        assert_ty_for_str(expected, j);
    }

    #[test]
    fn false_literal() {
        let j = "false";

        let expected = ty::Ty::LitBool(false);
        assert_ty_for_str(expected, j);
    }

    #[test]
    fn sym_literal() {
        let j = "'foo";

        let expected = ty::Ty::LitSym("foo".into());
        assert_ty_for_str(expected, j);
    }

    #[test]
    fn keyword_literal() {
        let j = ":foo";

        let expected = ty::Ty::LitSym(":foo".into());
        assert_ty_for_str(expected, j);
    }

    #[test]
    fn empty_list_literal() {
        let j = "()";

        let expected = ty::List::empty().into();
        assert_ty_for_str(expected, j);
    }

    #[test]
    fn quoted_list_literal() {
        let j = "'(true false)";

        let expected = ty::List::new(
            Box::new([ty::Ty::LitBool(true).into(), ty::Ty::LitBool(false).into()]),
            ty::Ty::never().into(),
        )
        .into();

        assert_ty_for_str(expected, j);
    }

    #[test]
    fn empty_vector_literal() {
        let j = "[]";

        let expected = ty::Ty::Vector(Box::new([]));
        assert_ty_for_str(expected, j);
    }

    #[test]
    fn vector_literal() {
        let j = "[true false]";

        let expected = ty::Ty::Vector(Box::new([
            ty::Ty::LitBool(true).into(),
            ty::Ty::LitBool(false).into(),
        ]));

        assert_ty_for_str(expected, j);
    }

    #[test]
    fn ty_ref() {
        let j = "Sym";

        let expected = ty::Ty::Sym;
        assert_ty_for_str(expected, j);
    }

    #[test]
    fn fixed_list_cons() {
        let j = "(List true false)";

        let expected = ty::List::new(
            Box::new([ty::Ty::LitBool(true).into(), ty::Ty::LitBool(false).into()]),
            ty::Ty::never().into(),
        )
        .into();

        assert_ty_for_str(expected, j);
    }

    #[test]
    fn rest_list_cons() {
        let j = "(List true & false)";

        let expected = ty::List::new(
            Box::new([ty::Ty::LitBool(true).into()]),
            ty::Ty::LitBool(false).into(),
        )
        .into();

        assert_ty_for_str(expected, j);
    }

    #[test]
    fn vectorof_cons() {
        let j = "(Vectorof true)";

        let inner_poly = ty::Ty::LitBool(true).into();
        let expected = ty::Ty::Vectorof(Box::new(inner_poly));

        assert_ty_for_str(expected, j);
    }

    #[test]
    fn vector_cons() {
        let j = "(Vector true false)";

        let expected = ty::Ty::Vector(Box::new([
            ty::Ty::LitBool(true).into(),
            ty::Ty::LitBool(false).into(),
        ]));

        assert_ty_for_str(expected, j);
    }

    #[test]
    fn pure_fun() {
        let j = "(-> true)";

        let expected = ty::Fun::new(
            purity::PVarIds::new(),
            ty::TVarIds::new(),
            ty::TopFun::new(Purity::Pure.into(), ty::Ty::LitBool(true).into()),
            ty::List::empty(),
        )
        .into();

        assert_ty_for_str(expected, j);
    }

    #[test]
    fn impure_fun() {
        let j = "(->! true)";

        let expected = ty::Fun::new(
            purity::PVarIds::new(),
            ty::TVarIds::new(),
            ty::TopFun::new(Purity::Impure.into(), ty::Ty::LitBool(true).into()),
            ty::List::empty(),
        )
        .into();

        assert_ty_for_str(expected, j);
    }

    #[test]
    fn fixed_fun() {
        let j = "(false -> true)";

        let expected = ty::Fun::new(
            purity::PVarIds::new(),
            ty::TVarIds::new(),
            ty::TopFun::new(Purity::Pure.into(), ty::Ty::LitBool(true).into()),
            ty::List::new(
                Box::new([ty::Ty::LitBool(false).into()]),
                ty::Ty::never().into(),
            ),
        )
        .into();

        assert_ty_for_str(expected, j);
    }

    #[test]
    fn rest_impure_fun() {
        let j = "(Str & Sym ->! true)";

        let expected = ty::Fun::new(
            purity::PVarIds::new(),
            ty::TVarIds::new(),
            ty::TopFun::new(Purity::Impure.into(), ty::Ty::LitBool(true).into()),
            ty::List::new(Box::new([ty::Ty::Str.into()]), ty::Ty::Sym.into()),
        )
        .into();

        assert_ty_for_str(expected, j);
    }

    #[test]
    fn top_impure_fun() {
        let j = "(... ->! true)";

        let expected = ty::TopFun::new(Purity::Impure.into(), ty::Ty::LitBool(true).into()).into();

        assert_ty_for_str(expected, j);
    }

    #[test]
    fn type_predicate() {
        let j = "str?";

        let expected = ty::Ty::TyPred(ty::pred::TestTy::Str);
        assert_ty_for_str(expected, j);
    }

    #[test]
    fn equality_predicate() {
        let j = "=";

        let expected = ty::Ty::EqPred;
        assert_ty_for_str(expected, j);
    }

    #[test]
    fn set_cons() {
        let j = "(Setof true)";

        let inner_poly = ty::Ty::LitBool(true).into();
        let expected = ty::Ty::Set(Box::new(inner_poly));

        assert_ty_for_str(expected, j);
    }

    #[test]
    fn map_cons() {
        let j = "(Map true false)";

        let key_ty = ty::Ty::LitBool(true);
        let value_ty = ty::Ty::LitBool(false);
        let expected = ty::Map::new(key_ty.into(), value_ty.into()).into();

        assert_ty_for_str(expected, j);
    }

    #[test]
    fn merged_union_cons() {
        let j = "(UnifyingU true false)";
        let expected = ty::Ty::Bool;

        assert_ty_for_str(expected, j);
    }

    #[test]
    fn simpifying_str_for_ty_ref() {
        assert_exact_str_repr("(List Int Float)");
        assert_exact_str_repr("(List Int & Float)");
        assert_exact_str_repr("(List & Float)");
        assert_exact_str_repr("(Float & Int -> Sym)");
    }

    #[test]
    fn polymorphic_fun_str() {
        assert_exact_str_repr("(All #{[->? ->!] A [B Bool] C} B C ->? A)");
    }
}
