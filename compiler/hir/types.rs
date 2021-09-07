use arret_syntax::span::Span;

use crate::hir::error::{
    Error, ErrorKind, ExpectedPolyPurityArg, PolyArgIsNotPure, PolyArgIsNotTy, Result,
};
use crate::hir::ns::{Ident, NsDataIter, NsDatum};
use crate::hir::prim::Prim;
use crate::hir::scope::{Binding, Scope};
use crate::hir::util::{
    expect_arg_count, expect_one_arg, expect_spanned_ns_ident, try_take_rest_arg,
};
use crate::ty;
use crate::ty::purity;
use crate::ty::purity::Purity;
use crate::ty::record;
use crate::ty::ty_args::TyArgs;
use crate::ty::Ty;

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

#[derive(Clone)]
pub enum PolymorphicVar {
    TVar(ty::TVarId),
    PVar(purity::PVarId),
    TFixed(Span, ty::Ref<ty::Poly>),
    Pure(Span),
}

struct LoweredPolymorphicVar {
    ident: Ident,
    polymorphic_var: PolymorphicVar,
}

fn lower_polymorphic_var(scope: &Scope<'_>, tvar_datum: NsDatum) -> Result<LoweredPolymorphicVar> {
    let span = tvar_datum.span();

    match tvar_datum {
        NsDatum::Ident(span, ident) => {
            if ident.is_underscore() {
                return Err(Error::new(span, ErrorKind::AnonymousPolymorphicParam));
            }

            let source_name = ident.name().clone();
            return Ok(LoweredPolymorphicVar {
                ident,
                polymorphic_var: PolymorphicVar::TVar(ty::TVar::new(
                    span,
                    source_name,
                    Ty::Any.into(),
                )),
            });
        }
        NsDatum::Vector(vector_span, vs) => {
            let mut arg_data = vs.into_vec();

            if arg_data.len() == 2 {
                let bound_datum = arg_data.pop().unwrap();
                let (ident_span, ident) = expect_spanned_ns_ident(
                    arg_data.pop().unwrap(),
                    "new polymorphic parameter name",
                )?;

                if ident.is_underscore() {
                    return Err(Error::new(ident_span, ErrorKind::AnonymousPolymorphicParam));
                }

                let source_name = ident.name().clone();
                match try_lower_purity(scope, &bound_datum) {
                    Some(purity::Ref::Fixed(Purity::Impure)) => {
                        return Ok(LoweredPolymorphicVar {
                            ident,
                            polymorphic_var: PolymorphicVar::PVar(purity::PVar::new(
                                vector_span,
                                source_name,
                            )),
                        });
                    }
                    Some(purity::Ref::Fixed(Purity::Pure)) => {
                        // Emulate bounding to pure in case the purity comes from e.g. a macro
                        // expansion
                        return Ok(LoweredPolymorphicVar {
                            ident,
                            polymorphic_var: PolymorphicVar::Pure(vector_span),
                        });
                    }
                    Some(_) => {
                        return Err(Error::new(bound_datum.span(), ErrorKind::VarPurityBound));
                    }
                    None => {
                        let bound_ty = lower_poly(scope, bound_datum)?;

                        let polymorphic_var = if ty::props::has_subtypes(&bound_ty) {
                            PolymorphicVar::TVar(ty::TVar::new(vector_span, source_name, bound_ty))
                        } else {
                            PolymorphicVar::TFixed(vector_span, bound_ty)
                        };

                        return Ok(LoweredPolymorphicVar {
                            ident,
                            polymorphic_var,
                        });
                    }
                }
            }
        }
        _ => {}
    }

    Err(Error::new(span, ErrorKind::BadPolyVarDecl))
}

fn lower_list_cons(scope: &Scope<'_>, mut arg_iter: NsDataIter) -> Result<ty::List<ty::Poly>> {
    let rest = try_take_rest_arg(&mut arg_iter);

    let fixed_polys = arg_iter
        .map(|fixed_datum| lower_poly(scope, fixed_datum))
        .collect::<Result<Box<[ty::Ref<ty::Poly>]>>>()?;

    let rest_poly = match rest {
        Some(rest_datum) => lower_poly(scope, rest_datum)?,
        None => Ty::never().into(),
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
    Ok(ty::Fun::new(purity::PVars::new(), ty::TVars::new(), top_fun, params).into())
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

            Ty::Vector(member_tys).into()
        }
        TyCons::Vectorof => {
            let start_datum = expect_one_arg(span, arg_iter)?;
            let start_ty = lower_poly(scope, start_datum)?;
            Ty::Vectorof(Box::new(start_ty)).into()
        }
        TyCons::Set => {
            let member_datum = expect_one_arg(span, arg_iter)?;
            let member_ty = lower_poly(scope, member_datum)?;
            Ty::Set(Box::new(member_ty)).into()
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

            Ty::Union(member_tys).into()
        }
    })
}

fn lower_record_ty_cons_purity_arg(
    scope: &Scope<'_>,
    param_span: Span,
    arg_datum: &NsDatum,
) -> Result<purity::Ref> {
    (match arg_datum {
        NsDatum::Ident(span, ident) => match scope.get_or_err(*span, ident)? {
            Binding::Purity(purity) => Ok(purity.clone()),
            other => Err(other.description()),
        },
        other => Err(other.description()),
    })
    .map_err(|found| {
        let details = Box::new(ExpectedPolyPurityArg { found, param_span });
        Error::new(arg_datum.span(), ErrorKind::ExpectedPolyPurityArg(details))
    })
}

fn lower_record_ty_cons_apply(
    scope: &Scope<'_>,
    span: Span,
    record_cons: &record::ConsId,
    arg_iter: NsDataIter,
) -> Result<ty::Ref<ty::Poly>> {
    use crate::ty::is_a::{ty_ref_is_a, ty_refs_equivalent};
    use std::collections::HashMap;

    expect_arg_count(span, record_cons.poly_params().len(), arg_iter.len())?;

    let mut pvar_purities = HashMap::new();
    let mut tvar_types = HashMap::new();

    for (poly_param, arg_datum) in record_cons.poly_params().iter().zip(arg_iter) {
        let arg_span = arg_datum.span();

        match poly_param {
            record::PolyParam::PVar(_, pvar) => {
                let purity_ref = lower_record_ty_cons_purity_arg(scope, pvar.span(), &arg_datum)?;
                pvar_purities.insert(pvar.clone(), purity_ref);
            }
            record::PolyParam::Pure(span) => {
                let purity_ref = lower_record_ty_cons_purity_arg(scope, *span, &arg_datum)?;

                if purity_ref != Purity::Pure.into() {
                    let details = Box::new(PolyArgIsNotPure {
                        arg_purity: purity_ref,
                        param_span: *span,
                    });

                    return Err(Error::new(arg_span, ErrorKind::PolyArgIsNotPure(details)));
                }
            }
            record::PolyParam::TVar(_, tvar) => {
                let arg_type = lower_poly(scope, arg_datum)?;
                if !ty_ref_is_a(&arg_type, tvar.bound()) {
                    let details = Box::new(PolyArgIsNotTy {
                        arg_type,
                        param_bound: tvar.bound().clone(),
                        param_span: tvar.span(),
                    });

                    return Err(Error::new(arg_span, ErrorKind::PolyArgIsNotTy(details)));
                }

                tvar_types.insert(tvar.clone(), arg_type);
            }
            record::PolyParam::TFixed(span, fixed_poly) => {
                let arg_type = lower_poly(scope, arg_datum)?;
                if !ty_refs_equivalent(&arg_type, fixed_poly) {
                    let details = Box::new(PolyArgIsNotTy {
                        arg_type,
                        param_bound: fixed_poly.clone(),
                        param_span: *span,
                    });

                    return Err(Error::new(arg_span, ErrorKind::PolyArgIsNotTy(details)));
                }
            }
        }
    }

    Ok(record::Instance::new(record_cons.clone(), TyArgs::new(pvar_purities, tvar_types)).into())
}

fn lower_literal_vec(literal_data: Vec<NsDatum>) -> Result<Vec<ty::Ref<ty::Poly>>> {
    literal_data.into_iter().map(lower_literal).collect()
}

fn lower_literal(datum: NsDatum) -> Result<ty::Ref<ty::Poly>> {
    match datum {
        NsDatum::Bool(_, v) => Ok(Ty::LitBool(v).into()),
        NsDatum::Keyword(_, name) => Ok(Ty::LitSym(name).into()),
        NsDatum::Ident(_, ident) => Ok(Ty::LitSym(ident.into_name()).into()),
        NsDatum::List(_, vs) => {
            let fixed_literals = lower_literal_vec(vs.into_vec())?;
            Ok(ty::List::new_tuple(fixed_literals.into_boxed_slice()).into())
        }
        NsDatum::Vector(_, vs) => {
            let fixed_literals = lower_literal_vec(vs.into_vec())?;
            Ok(Ty::Vector(fixed_literals.into_boxed_slice()).into())
        }
        _ => Err(Error::new(datum.span(), ErrorKind::UnsupportedLiteralType)),
    }
}

fn lower_ident(scope: &Scope<'_>, span: Span, ident: &Ident) -> Result<ty::Ref<ty::Poly>> {
    match scope.get_or_err(span, ident)? {
        Binding::Ty(ty) => Ok(ty.clone()),
        Binding::TyPred(test_ty) => Ok(Ty::TyPred(test_ty.clone()).into()),
        Binding::EqPred => Ok(Ty::EqPred.into()),
        other => Err(Error::new(span, ErrorKind::ExpectedTy(other.description()))),
    }
}

fn lower_polymorphic_poly(
    scope: &Scope<'_>,
    span: Span,
    mut data_iter: NsDataIter,
) -> Result<ty::Ref<ty::Poly>> {
    let polymorphic_vars_datum = if let Some(datum) = data_iter.next() {
        datum
    } else {
        return Err(Error::new(span, ErrorKind::NoPolyVarsDecl));
    };

    let polymorphic_var_data = if let NsDatum::Set(_, vs) = polymorphic_vars_datum {
        vs
    } else {
        return Err(Error::new(
            polymorphic_vars_datum.span(),
            ErrorKind::ExpectedPolyVarsDecl(polymorphic_vars_datum.description()),
        ));
    };

    let mut inner_scope = scope.child();
    let (pvars, tvars) = lower_polymorphic_var_set(
        scope,
        &mut inner_scope,
        polymorphic_var_data.into_vec().into_iter(),
    )?;

    let inner_poly = lower_poly_data_iter(&inner_scope, span, data_iter)?;

    if let ty::Ref::Fixed(Ty::Fun(fun)) = inner_poly {
        Ok(Ty::Fun(Box::new(fun.with_polymorphic_vars(pvars, tvars))).into())
    } else {
        Err(Error::new(span, ErrorKind::NonFunPolyTy))
    }
}

fn lower_poly_data_iter(
    scope: &Scope<'_>,
    span: Span,
    mut data_iter: NsDataIter,
) -> Result<ty::Ref<ty::Poly>> {
    let data_len = data_iter.len();

    if data_len == 0 {
        // This is by analogy with () being self-evaluating in expressions
        return Ok(ty::List::empty().into());
    }

    if let Some(Binding::Prim(Prim::All)) = scope.get_datum(&data_iter.as_slice()[0]) {
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
    let (ident_span, ident) = expect_spanned_ns_ident(fn_datum, "type constructor name")?;

    match scope.get_or_err(ident_span, &ident)? {
        Binding::Prim(Prim::Quote) => {
            let literal_datum = expect_one_arg(span, data_iter)?;
            lower_literal(literal_datum)
        }
        Binding::TyCons(ty_cons) => lower_ty_cons_apply(scope, span, *ty_cons, data_iter),
        Binding::RecordTyCons(record_cons) => {
            lower_record_ty_cons_apply(scope, span, record_cons, data_iter)
        }
        other => Err(Error::new(
            ident_span,
            ErrorKind::ExpectedTyCons(other.description()),
        )),
    }
}

pub fn lower_poly(scope: &Scope<'_>, datum: NsDatum) -> Result<ty::Ref<ty::Poly>> {
    match datum {
        NsDatum::List(span, vs) => lower_poly_data_iter(scope, span, vs.into_vec().into_iter()),
        NsDatum::Ident(span, ident) => lower_ident(scope, span, &ident),
        _ => lower_literal(datum),
    }
}

fn bind_polymorphic_vars(
    scope: &mut Scope<'_>,
    lowered_poly_vars: Vec<LoweredPolymorphicVar>,
) -> Result<()> {
    for LoweredPolymorphicVar {
        ident,
        polymorphic_var,
    } in lowered_poly_vars
    {
        let (span, binding) = match polymorphic_var {
            PolymorphicVar::PVar(pvar) => (pvar.span(), Binding::Purity(pvar.into())),
            PolymorphicVar::TVar(tvar) => (tvar.span(), Binding::Ty(tvar.into())),
            PolymorphicVar::TFixed(fixed_span, poly) => (fixed_span, Binding::Ty(poly)),
            PolymorphicVar::Pure(pure_span) => (pure_span, Binding::Purity(Purity::Pure.into())),
        };

        scope.insert_binding(span, ident, binding)?;
    }

    Ok(())
}

/// Lowers a set of polymorphic variables defined in `outer_scope` and places them in `inner_scope`
///
/// This is used for functions and function types
pub fn lower_polymorphic_var_set(
    outer_scope: &Scope<'_>,
    inner_scope: &mut Scope<'_>,
    polymorphic_var_data: NsDataIter,
) -> Result<(purity::PVars, ty::TVars)> {
    let mut pvars = purity::PVars::new();
    let mut tvars = ty::TVars::new();

    let lowered_poly_vars = polymorphic_var_data
        .map(|var_datum| lower_polymorphic_var(outer_scope, var_datum))
        .collect::<Result<Vec<LoweredPolymorphicVar>>>()?;

    for lowered_poly_var in lowered_poly_vars.iter() {
        match &lowered_poly_var.polymorphic_var {
            PolymorphicVar::PVar(pvar) => {
                pvars.push(pvar.clone());
            }
            PolymorphicVar::TVar(tvar) => {
                tvars.push(tvar.clone());
            }
            PolymorphicVar::Pure(_) | PolymorphicVar::TFixed(_, _) => {}
        }
    }

    bind_polymorphic_vars(inner_scope, lowered_poly_vars)?;
    Ok((pvars, tvars))
}

/// Lowers a list of polymorphic variables defined in `outer_scope` and places them in `inner_scope`
///
/// This is used for record types
pub fn lower_polymorphic_var_list(
    outer_scope: &Scope<'_>,
    inner_scope: &mut Scope<'_>,
    param_data: NsDataIter,
) -> Result<Box<[PolymorphicVar]>> {
    let lowered_poly_vars = param_data
        .map(|var_datum| lower_polymorphic_var(outer_scope, var_datum))
        .collect::<Result<Vec<LoweredPolymorphicVar>>>()?;

    let poly_vars = lowered_poly_vars
        .iter()
        .map(|lpv| lpv.polymorphic_var.clone())
        .collect();

    bind_polymorphic_vars(inner_scope, lowered_poly_vars)?;
    Ok(poly_vars)
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
    export_ty!("Any", Ty::Any),
    export_ty!("Bool", Ty::Bool),
    export_ty!("Sym", Ty::Sym),
    export_ty!("Str", Ty::Str),
    export_ty!("Int", Ty::Int),
    export_ty!("Float", Ty::Float),
    export_ty!("Num", Ty::Num),
    export_ty!("Char", Ty::Char),
    export_ty!("Record", Ty::TopRecord),
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
    export_ty_pred!("record?", ty::pred::TestTy::TopRecord),
    #[cfg(test)]
    export_ty_cons!("RawU", TyCons::RawU),
];

/// Pushes the arguments for a list constructor on to the passed `Vec`
///
/// This is used to share code between list and function types
fn push_list_parts<M: ty::Pm>(list_parts: &mut Vec<String>, list_ref: &ty::List<M>) {
    for fixed in list_ref.fixed() {
        list_parts.push(str_for_ty_ref(fixed));
    }

    let rest = list_ref.rest();
    if !rest.is_never() {
        list_parts.push("&".to_owned());
        list_parts.push(str_for_ty_ref(rest));
    }
}

fn str_for_bounds(bound_pvars: &[purity::PVarId], bound_tvars: &[ty::TVarId]) -> String {
    let pvar_parts = bound_pvars
        .iter()
        .map(|pvar| format!("[{} ->!]", pvar.source_name()));

    let tvar_parts = bound_tvars.iter().map(|tvar| {
        if tvar.bound() == &Ty::Any.into() {
            return tvar.source_name().into();
        }

        format!("[{} {}]", tvar.source_name(), str_for_ty_ref(tvar.bound()))
    });

    let all_parts = pvar_parts.chain(tvar_parts).collect::<Vec<String>>();
    format!("#{{{}}}", all_parts.join(" "))
}

fn str_for_record_poly_arg<M: ty::Pm>(
    instance: &record::Instance<M>,
    poly_param: &record::PolyParam,
) -> String {
    let ty_args = instance.ty_args();

    match poly_param {
        record::PolyParam::PVar(_, pvar) => str_for_purity(&ty_args.pvar_purities()[pvar]),
        record::PolyParam::Pure(_) => str_for_purity(&Purity::Pure.into()),
        record::PolyParam::TVar(_, tvar) => str_for_ty_ref(&ty_args.tvar_types()[tvar]),
        record::PolyParam::TFixed(_, fixed_poly) => str_for_ty_ref(fixed_poly),
    }
}

fn str_for_ty<M: ty::Pm>(ty: &Ty<M>) -> String {
    match ty {
        Ty::Any => "Any".to_owned(),
        Ty::Bool => "Bool".to_owned(),
        Ty::Char => "Char".to_owned(),
        Ty::Int => "Int".to_owned(),
        Ty::Sym => "Sym".to_owned(),
        Ty::Str => "Str".to_owned(),
        Ty::Float => "Float".to_owned(),
        Ty::Num => "Num".to_owned(),
        Ty::LitBool(false) => "false".to_owned(),
        Ty::LitBool(true) => "true".to_owned(),
        Ty::LitSym(name) => {
            if name.starts_with(':') {
                name.to_string()
            } else {
                format!("'{}", name)
            }
        }
        Ty::Map(map) => format!(
            "(Map {} {})",
            str_for_ty_ref(map.key()),
            str_for_ty_ref(map.value())
        ),
        Ty::Set(member) => format!("(Setof {})", str_for_ty_ref(member)),
        Ty::Vector(members) => {
            let result_parts: Vec<String> = members
                .iter()
                .map(|member| format!(" {}", str_for_ty_ref(member)))
                .collect();

            format!("(Vector{})", result_parts.join(""))
        }
        Ty::Vectorof(member) => format!("(Vectorof {})", str_for_ty_ref(member)),
        Ty::TopFun(top_fun) => format!(
            "(... {} {})",
            str_for_purity(top_fun.purity()),
            str_for_ty_ref(top_fun.ret())
        ),
        Ty::Fun(fun) => {
            let mut fun_parts = Vec::with_capacity(2);

            push_list_parts(&mut fun_parts, fun.params());
            fun_parts.push(str_for_purity(fun.purity()));
            fun_parts.push(str_for_ty_ref(fun.ret()));

            if fun.has_polymorphic_vars() {
                format!(
                    "(All {} {})",
                    str_for_bounds(fun.pvars(), fun.tvars()),
                    fun_parts.join(" ")
                )
            } else {
                format!("({})", fun_parts.join(" "))
            }
        }
        Ty::TyPred(test_ty) => test_ty.to_string(),
        Ty::EqPred => "=".to_owned(),
        Ty::Union(members) => {
            let member_strs: Vec<String> = members
                .iter()
                .map(|m| format!(" {}", str_for_ty_ref(m)))
                .collect();

            format!("(U{})", member_strs.join(""))
        }
        Ty::Intersect(members) => {
            let member_strs: Vec<String> = members
                .iter()
                .map(|m| format!(" {}", str_for_ty_ref(m)))
                .collect();

            format!("(∩{})", member_strs.join(""))
        }
        Ty::List(list) => {
            // While all list types can be expressed using `(List)` we try to find the shortest
            // representation
            if list.is_empty() {
                "()".to_owned()
            } else {
                let mut list_parts = Vec::with_capacity(2);

                list_parts.push("List".to_owned());
                push_list_parts(&mut list_parts, list);

                format!("({})", list_parts.join(" "))
            }
        }

        Ty::TopRecord => "Record".to_owned(),
        Ty::RecordClass(record_cons) => format!("({} ...)", record_cons.ty_cons_name()),
        Ty::Record(instance) => {
            let record_cons = instance.cons();

            if record_cons.is_singleton() {
                // This is bound as its name
                return record_cons.ty_cons_name().to_string();
            }

            let record_parts: Vec<String> = std::iter::once(record_cons.ty_cons_name().to_string())
                .chain(
                    record_cons
                        .poly_params()
                        .iter()
                        .map(|poly_param| str_for_record_poly_arg(instance, poly_param)),
                )
                .collect();

            format!("({})", record_parts.join(" "))
        }
    }
}

pub fn str_for_ty_ref<M: ty::Pm>(ty_ref: &ty::Ref<M>) -> String {
    match ty_ref {
        ty::Ref::Var(tvar, _) => tvar.source_name().to_owned(),
        ty::Ref::Fixed(ty) => str_for_ty(ty),
    }
}

pub fn str_for_purity(purity: &purity::Ref) -> String {
    match purity {
        purity::Ref::Fixed(Purity::Pure) => "->".to_owned(),
        purity::Ref::Fixed(Purity::Impure) => "->!".to_owned(),
        purity::Ref::Var(pvar) => pvar.source_name().into(),
    }
}

#[cfg(test)]
pub fn poly_for_str(datum_str: &str) -> ty::Ref<ty::Poly> {
    use crate::hir::prim::PRIM_EXPORTS;
    use arret_syntax::parser::datum_from_str;

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

    let test_datum = datum_from_str(None, datum_str).unwrap();
    lower_poly(&scope, NsDatum::from_syntax_datum(&test_datum)).unwrap()
}

#[cfg(test)]
pub fn tvar_bounded_by(bound: ty::Ref<ty::Poly>) -> ty::Ref<ty::Poly> {
    ty::TVar::new(crate::source::EMPTY_SPAN, "TVar".into(), bound).into()
}

#[cfg(test)]
mod test {
    use super::*;

    use std::collections::HashMap;

    use crate::source::EMPTY_SPAN;
    use crate::ty::var_usage::Variance;

    fn assert_ty_for_str(expected: Ty<ty::Poly>, datum_str: &str) {
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

        let expected = Ty::LitBool(true);
        assert_ty_for_str(expected, j);
    }

    #[test]
    fn false_literal() {
        let j = "false";

        let expected = Ty::LitBool(false);
        assert_ty_for_str(expected, j);
    }

    #[test]
    fn sym_literal() {
        let j = "'foo";

        let expected = Ty::LitSym("foo".into());
        assert_ty_for_str(expected, j);
    }

    #[test]
    fn keyword_literal() {
        let j = ":foo";

        let expected = Ty::LitSym(":foo".into());
        assert_ty_for_str(expected, j);

        // Make sure we don't quote this needlessly
        assert_exact_str_repr(j);
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

        let expected = ty::List::new_tuple(Box::new([
            Ty::LitBool(true).into(),
            Ty::LitBool(false).into(),
        ]))
        .into();

        assert_ty_for_str(expected, j);
    }

    #[test]
    fn empty_vector_literal() {
        let j = "[]";

        let expected = Ty::Vector(Box::new([]));
        assert_ty_for_str(expected, j);
    }

    #[test]
    fn vector_literal() {
        let j = "[true false]";

        let expected = Ty::Vector(Box::new([
            Ty::LitBool(true).into(),
            Ty::LitBool(false).into(),
        ]));

        assert_ty_for_str(expected, j);
    }

    #[test]
    fn ty_ref() {
        let j = "Sym";

        let expected = Ty::Sym;
        assert_ty_for_str(expected, j);
    }

    #[test]
    fn fixed_list_cons() {
        let j = "(List true false)";

        let expected = ty::List::new_tuple(Box::new([
            Ty::LitBool(true).into(),
            Ty::LitBool(false).into(),
        ]))
        .into();

        assert_ty_for_str(expected, j);
    }

    #[test]
    fn rest_list_cons() {
        let j = "(List true & false)";

        let expected = ty::List::new(
            Box::new([Ty::LitBool(true).into()]),
            Ty::LitBool(false).into(),
        )
        .into();

        assert_ty_for_str(expected, j);
    }

    #[test]
    fn vectorof_cons() {
        let j = "(Vectorof true)";

        let inner_poly = Ty::LitBool(true).into();
        let expected = Ty::Vectorof(Box::new(inner_poly));

        assert_ty_for_str(expected, j);
    }

    #[test]
    fn vector_cons() {
        let j = "(Vector true false)";

        let expected = Ty::Vector(Box::new([
            Ty::LitBool(true).into(),
            Ty::LitBool(false).into(),
        ]));

        assert_ty_for_str(expected, j);
    }

    #[test]
    fn pure_fun() {
        let j = "(-> true)";

        let expected = ty::Fun::new_mono(
            ty::List::empty(),
            Purity::Pure.into(),
            Ty::LitBool(true).into(),
        )
        .into();

        assert_ty_for_str(expected, j);
    }

    #[test]
    fn impure_fun() {
        let j = "(->! true)";

        let expected = ty::Fun::new_mono(
            ty::List::empty(),
            Purity::Impure.into(),
            Ty::LitBool(true).into(),
        )
        .into();

        assert_ty_for_str(expected, j);
    }

    #[test]
    fn fixed_fun() {
        let j = "(false -> true)";

        let expected = ty::Fun::new_mono(
            ty::List::new_tuple(Box::new([Ty::LitBool(false).into()])),
            Purity::Pure.into(),
            Ty::LitBool(true).into(),
        )
        .into();

        assert_ty_for_str(expected, j);
    }

    #[test]
    fn rest_impure_fun() {
        let j = "(Str & Sym ->! true)";

        let expected = ty::Fun::new_mono(
            ty::List::new(Box::new([Ty::Str.into()]), Ty::Sym.into()),
            Purity::Impure.into(),
            Ty::LitBool(true).into(),
        )
        .into();

        assert_ty_for_str(expected, j);
    }

    #[test]
    fn top_impure_fun() {
        let j = "(... ->! true)";

        let expected = ty::TopFun::new(Purity::Impure.into(), Ty::LitBool(true).into()).into();

        assert_ty_for_str(expected, j);
    }

    #[test]
    fn type_predicate() {
        let j = "str?";

        let expected = Ty::TyPred(ty::pred::TestTy::Str);
        assert_ty_for_str(expected, j);
    }

    #[test]
    fn equality_predicate() {
        let j = "=";

        let expected = Ty::EqPred;
        assert_ty_for_str(expected, j);
    }

    #[test]
    fn set_cons() {
        let j = "(Setof true)";

        let inner_poly = Ty::LitBool(true).into();
        let expected = Ty::Set(Box::new(inner_poly));

        assert_ty_for_str(expected, j);
    }

    #[test]
    fn map_cons() {
        let j = "(Map true false)";

        let key_ty = Ty::LitBool(true);
        let value_ty = Ty::LitBool(false);
        let expected = ty::Map::new(key_ty.into(), value_ty.into()).into();

        assert_ty_for_str(expected, j);
    }

    #[test]
    fn merged_union_cons() {
        let j = "(UnifyingU true false)";
        let expected = Ty::Bool;

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

    #[test]
    fn singleton_record_type() {
        let mono_record_cons = record::Cons::new(
            EMPTY_SPAN,
            "MonoCons".into(),
            "mono-cons?".into(),
            None,
            Box::new([record::Field::new(EMPTY_SPAN, "num".into(), Ty::Num.into())]),
        );

        let record_class_ref: ty::Ref<ty::Poly> = mono_record_cons.clone().into();
        assert_eq!("(MonoCons ...)", str_for_ty_ref(&record_class_ref));

        let int_record_instance_ref: ty::Ref<ty::Poly> =
            record::Instance::new(mono_record_cons, TyArgs::empty()).into();
        assert_eq!("MonoCons", str_for_ty_ref(&int_record_instance_ref));
    }

    #[test]
    fn poly_record_type() {
        let tvar = ty::TVar::new(EMPTY_SPAN, "tvar".into(), Ty::Any.into());

        let poly_record_cons = record::Cons::new(
            EMPTY_SPAN,
            "PolyCons".into(),
            "poly-cons?".into(),
            Some(Box::new([
                record::PolyParam::Pure(EMPTY_SPAN),
                record::PolyParam::TVar(Variance::Covariant, tvar.clone()),
            ])),
            Box::new([record::Field::new(
                EMPTY_SPAN,
                "num".into(),
                tvar.clone().into(),
            )]),
        );

        // Record class type
        let record_class_ref: ty::Ref<ty::Poly> = poly_record_cons.clone().into();
        assert_eq!("(PolyCons ...)", str_for_ty_ref(&record_class_ref));

        // Instance parameterised with an `Int`
        let mut int_tvars = HashMap::new();
        int_tvars.insert(tvar, Ty::Int.into());
        let int_ty_args = TyArgs::new(HashMap::new(), int_tvars);

        let poly_record_instance_ref: ty::Ref<ty::Poly> =
            record::Instance::new(poly_record_cons, int_ty_args).into();

        assert_eq!(
            "(PolyCons -> Int)",
            str_for_ty_ref(&poly_record_instance_ref)
        );
    }
}
