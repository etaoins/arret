use std::collections::HashMap;

use crate::ty;
use crate::ty::purity;
use crate::ty::purity::Purity;

fn subst_ty_ref_slice<S>(stx: &S, inputs: &[S::Input]) -> Box<[S::Output]>
where
    S: Substitution,
{
    inputs
        .iter()
        .map(|i| stx.subst_ty_ref(i))
        .collect::<Vec<S::Output>>()
        .into_boxed_slice()
}

fn subst_list<S>(stx: &S, list: &ty::List<S::Input>) -> ty::List<S::Output>
where
    S: Substitution,
{
    ty::List::new(
        subst_ty_ref_slice(stx, list.fixed()),
        list.rest().map(|r| stx.subst_ty_ref(r)),
    )
}

fn subst_top_fun<S>(stx: &S, top_fun: &ty::TopFun) -> ty::TopFun
where
    S: Substitution,
{
    let poly_stx = stx.as_poly_subst();

    ty::TopFun::new(
        poly_stx.subst_purity_ref(top_fun.purity()),
        poly_stx.subst_ty_ref(top_fun.ret()),
    )
}

fn subst_fun<S>(stx: &S, fun: &ty::Fun) -> ty::Fun
where
    S: Substitution,
{
    let poly_stx = stx.as_poly_subst();

    // TODO: This doesn't seem right
    ty::Fun::new(
        purity::PVars::new(),
        ty::TVars::new(),
        subst_top_fun(stx, fun.top_fun()),
        subst_list(poly_stx, fun.params()),
    )
}

fn subst_ty<S>(stx: &S, ty: &ty::Ty<S::Input>) -> ty::Ty<S::Output>
where
    S: Substitution,
{
    match ty {
        ty::Ty::Any => ty::Ty::Any,
        ty::Ty::Bool => ty::Ty::Bool,
        ty::Ty::Char => ty::Ty::Char,
        ty::Ty::Float => ty::Ty::Float,
        ty::Ty::Int => ty::Ty::Int,
        ty::Ty::Str => ty::Ty::Str,
        ty::Ty::Sym => ty::Ty::Sym,
        ty::Ty::TopFun(top_fun) => subst_top_fun(stx, top_fun).into_ty(),
        ty::Ty::Fun(fun) => subst_fun(stx, fun).into_ty(),
        ty::Ty::TyPred(test_ty) => ty::Ty::TyPred(Box::new(stx.subst_ty_ref(test_ty))),
        ty::Ty::Map(map) => ty::Ty::Map(Box::new(ty::Map::new(
            stx.subst_ty_ref(map.key()),
            stx.subst_ty_ref(map.value()),
        ))),
        ty::Ty::LitBool(val) => ty::Ty::LitBool(*val),
        ty::Ty::LitSym(val) => ty::Ty::LitSym(val.clone()),
        ty::Ty::Set(member) => ty::Ty::Set(Box::new(stx.subst_ty_ref(&member))),
        ty::Ty::Union(members) => ty::Ty::Union(subst_ty_ref_slice(stx, members)),
        ty::Ty::Vector(members) => ty::Ty::Vector(subst_ty_ref_slice(stx, members)),
        ty::Ty::Vectorof(member) => ty::Ty::Vectorof(Box::new(stx.subst_ty_ref(member))),
        ty::Ty::List(list) => ty::Ty::List(subst_list(stx, list)),
    }
}

trait Substitution {
    type Input: ty::TyRef;
    type Output: ty::TyRef;
    type AsPolySubst: Substitution<Input = ty::Poly, Output = ty::Poly>;

    fn subst_purity_ref(&self, poly: &purity::Poly) -> purity::Poly;
    fn subst_ty_ref(&self, input: &Self::Input) -> Self::Output;
    fn as_poly_subst(&self) -> &Self::AsPolySubst;
}

impl<'tvars> Substitution for ty::select::SelectCtx<'tvars> {
    type Input = ty::Poly;
    type Output = ty::Poly;
    type AsPolySubst = Self;

    fn subst_purity_ref(&self, poly: &purity::Poly) -> purity::Poly {
        match poly {
            purity::Poly::Fixed(_) => poly.clone(),
            purity::Poly::Var(pvar_id) => {
                if let Some(selected) = self.pvar_purity(*pvar_id) {
                    selected
                } else {
                    poly.clone()
                }
            }
        }
    }

    fn subst_ty_ref(&self, poly: &ty::Poly) -> ty::Poly {
        match poly {
            ty::Poly::Fixed(fixed) => subst_ty(self, fixed).into_poly(),
            ty::Poly::Var(tvar_id) => {
                if let Some(selected) = self.tvar_type(*tvar_id) {
                    selected
                } else {
                    poly.clone()
                }
            }
        }
    }

    fn as_poly_subst(&self) -> &Self {
        self
    }
}

struct PolyIdentity {}

impl Substitution for PolyIdentity {
    type Input = ty::Poly;
    type Output = ty::Poly;
    type AsPolySubst = Self;

    fn subst_purity_ref(&self, poly: &purity::Poly) -> purity::Poly {
        poly.clone()
    }

    fn subst_ty_ref(&self, poly: &ty::Poly) -> ty::Poly {
        poly.clone()
    }

    fn as_poly_subst(&self) -> &Self {
        self
    }
}

struct MonoToPoly {
    poly_identity: PolyIdentity,
}

impl MonoToPoly {
    fn new() -> MonoToPoly {
        MonoToPoly {
            poly_identity: PolyIdentity {},
        }
    }
}

impl Substitution for MonoToPoly {
    type Input = ty::Mono;
    type Output = ty::Poly;
    type AsPolySubst = PolyIdentity;

    fn subst_purity_ref(&self, poly: &purity::Poly) -> purity::Poly {
        poly.clone()
    }

    fn subst_ty_ref(&self, mono: &ty::Mono) -> ty::Poly {
        subst_ty(self, mono.as_ty()).into_poly()
    }

    fn as_poly_subst(&self) -> &PolyIdentity {
        &self.poly_identity
    }
}

struct Monomorphise<'vars> {
    pvar_purities: &'vars HashMap<purity::PVarId, Purity>,
    tvar_types: &'vars HashMap<ty::TVarId, ty::Ty<ty::Mono>>,
    partial: PartialMonomorphise<'vars>,
}

impl<'vars> Monomorphise<'vars> {
    fn new(
        pvar_purities: &'vars HashMap<purity::PVarId, Purity>,
        tvar_types: &'vars HashMap<ty::TVarId, ty::Ty<ty::Mono>>,
    ) -> Monomorphise<'vars> {
        Monomorphise {
            pvar_purities,
            tvar_types,
            partial: PartialMonomorphise {
                pvar_purities,
                tvar_types,
            },
        }
    }
}

impl<'vars> Substitution for Monomorphise<'vars> {
    type Input = ty::Poly;
    type Output = ty::Mono;
    type AsPolySubst = PartialMonomorphise<'vars>;

    fn subst_purity_ref(&self, poly: &purity::Poly) -> purity::Poly {
        match poly {
            purity::Poly::Fixed(_) => poly.clone(),
            purity::Poly::Var(pvar_id) => self.pvar_purities[pvar_id].into_poly(),
        }
    }

    fn subst_ty_ref(&self, poly: &ty::Poly) -> ty::Mono {
        match poly {
            ty::Poly::Fixed(fixed) => subst_ty(self, fixed).clone().into_mono(),
            ty::Poly::Var(tvar_id) => self.tvar_types[tvar_id].clone().into_mono(),
        }
    }

    fn as_poly_subst(&self) -> &PartialMonomorphise<'vars> {
        &self.partial
    }
}

struct PartialMonomorphise<'vars> {
    pvar_purities: &'vars HashMap<purity::PVarId, Purity>,
    tvar_types: &'vars HashMap<ty::TVarId, ty::Ty<ty::Mono>>,
}

impl<'vars> Substitution for PartialMonomorphise<'vars> {
    type Input = ty::Poly;
    type Output = ty::Poly;
    type AsPolySubst = Self;

    fn subst_purity_ref(&self, poly: &purity::Poly) -> purity::Poly {
        match poly {
            purity::Poly::Fixed(_) => poly.clone(),
            purity::Poly::Var(pvar_id) => {
                if let Some(purity) = self.pvar_purities.get(pvar_id) {
                    purity.clone().into_poly()
                } else {
                    poly.clone()
                }
            }
        }
    }

    fn subst_ty_ref(&self, poly: &ty::Poly) -> ty::Poly {
        match poly {
            ty::Poly::Fixed(fixed) => subst_ty(self, fixed).into_poly(),
            ty::Poly::Var(tvar_id) => {
                if let Some(mono_ty) = self.tvar_types.get(tvar_id) {
                    subst_ty(&MonoToPoly::new(), mono_ty).into_poly()
                } else {
                    poly.clone()
                }
            }
        }
    }

    fn as_poly_subst(&self) -> &Self {
        self
    }
}

pub fn inst_ty_selection(stx: &ty::select::SelectCtx<'_>, poly: &ty::Poly) -> ty::Poly {
    stx.subst_ty_ref(poly)
}

pub fn inst_fun_selection(stx: &ty::select::SelectCtx<'_>, fun: &ty::Fun) -> ty::Fun {
    subst_fun(stx, fun)
}

pub fn inst_purity_selection(
    stx: &ty::select::SelectCtx<'_>,
    purity: &purity::Poly,
) -> purity::Poly {
    stx.subst_purity_ref(purity)
}

pub fn monomorphise(
    pvar_purities: &HashMap<purity::PVarId, Purity>,
    tvar_types: &HashMap<ty::TVarId, ty::Ty<ty::Mono>>,
    poly: &ty::Poly,
) -> ty::Mono {
    let stx = Monomorphise::new(pvar_purities, tvar_types);
    stx.subst_ty_ref(poly)
}
