use crate::ty;
use crate::ty::purity;
use crate::ty::ty_args::{MonoTyArgs, PolyTyArgs};

fn subst_ty_ref_slice<S>(stx: &S, inputs: &[S::Input]) -> Box<[S::Output]>
where
    S: Substitution,
{
    inputs.iter().map(|i| stx.subst_ty_ref(i)).collect()
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
        ty::Ty::Num => ty::Ty::Num,
        ty::Ty::Str => ty::Ty::Str,
        ty::Ty::Sym => ty::Ty::Sym,
        ty::Ty::EqPred => ty::Ty::EqPred,
        ty::Ty::TyPred(test_ty) => ty::Ty::TyPred(*test_ty),
        ty::Ty::TopFun(top_fun) => subst_top_fun(stx, top_fun).into_ty(),
        ty::Ty::Fun(fun) => subst_fun(stx, fun).into_ty(),
        ty::Ty::Map(map) => ty::Ty::Map(Box::new(ty::Map::new(
            stx.subst_ty_ref(map.key()),
            stx.subst_ty_ref(map.value()),
        ))),
        ty::Ty::LitBool(val) => ty::Ty::LitBool(*val),
        ty::Ty::LitSym(val) => ty::Ty::LitSym(val.clone()),
        ty::Ty::Set(member) => ty::Ty::Set(Box::new(stx.subst_ty_ref(&member))),
        ty::Ty::Union(members) => ty::Ty::Union(subst_ty_ref_slice(stx, members)),
        ty::Ty::Intersect(members) => ty::Ty::Intersect(subst_ty_ref_slice(stx, members)),
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

impl<'tvars> Substitution for PolyTyArgs {
    type Input = ty::Poly;
    type Output = ty::Poly;
    type AsPolySubst = Self;

    fn subst_purity_ref(&self, poly: &purity::Poly) -> purity::Poly {
        match poly {
            purity::Poly::Fixed(_) => poly.clone(),
            purity::Poly::Var(pvar_id) => {
                if let Some(selected) = self.pvar_purities().get(pvar_id) {
                    selected.clone()
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
                if let Some(selected) = self.tvar_types().get(tvar_id) {
                    selected.clone()
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

struct Monomorphise<'tyargs> {
    mono_ty_args: &'tyargs MonoTyArgs,
    partial: PartialMonomorphise<'tyargs>,
}

impl<'tyargs> Monomorphise<'tyargs> {
    fn new(mta: &'tyargs MonoTyArgs) -> Monomorphise<'tyargs> {
        Monomorphise {
            mono_ty_args: mta,
            partial: PartialMonomorphise { mono_ty_args: mta },
        }
    }
}

impl<'tyargs> Substitution for Monomorphise<'tyargs> {
    type Input = ty::Poly;
    type Output = ty::Mono;
    type AsPolySubst = PartialMonomorphise<'tyargs>;

    fn subst_purity_ref(&self, poly: &purity::Poly) -> purity::Poly {
        match poly {
            purity::Poly::Fixed(_) => poly.clone(),
            purity::Poly::Var(pvar_id) => self
                .mono_ty_args
                .pvar_purities()
                .get(pvar_id)
                .expect("Unable to find purity argument during monomorphisation")
                .clone(),
        }
    }

    fn subst_ty_ref(&self, poly: &ty::Poly) -> ty::Mono {
        match poly {
            ty::Poly::Fixed(fixed) => subst_ty(self, fixed).clone().into_mono(),
            ty::Poly::Var(tvar_id) => self
                .mono_ty_args
                .tvar_types()
                .get(tvar_id)
                .expect("Unable to find type argument during monomorphisation")
                .clone()
                .into_mono(),
        }
    }

    fn as_poly_subst(&self) -> &PartialMonomorphise<'tyargs> {
        &self.partial
    }
}

struct PartialMonomorphise<'tyargs> {
    mono_ty_args: &'tyargs MonoTyArgs,
}

impl<'tyargs> Substitution for PartialMonomorphise<'tyargs> {
    type Input = ty::Poly;
    type Output = ty::Poly;
    type AsPolySubst = Self;

    fn subst_purity_ref(&self, poly: &purity::Poly) -> purity::Poly {
        match poly {
            purity::Poly::Fixed(_) => poly.clone(),
            purity::Poly::Var(pvar_id) => {
                if let Some(purity) = self.mono_ty_args.pvar_purities().get(pvar_id) {
                    purity.clone()
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
                if let Some(mono_ty) = self.mono_ty_args.tvar_types().get(tvar_id) {
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

pub fn subst_poly(pta: &PolyTyArgs, poly: &ty::Poly) -> ty::Poly {
    pta.subst_ty_ref(poly)
}

pub fn subst_poly_fun(pta: &PolyTyArgs, fun: &ty::Fun) -> ty::Fun {
    subst_fun(pta, fun)
}

pub fn subst_purity(pta: &PolyTyArgs, purity: &purity::Poly) -> purity::Poly {
    pta.subst_purity_ref(purity)
}

pub fn monomorphise(mta: &MonoTyArgs, poly: &ty::Poly) -> ty::Mono {
    let stx = Monomorphise::new(mta);
    stx.subst_ty_ref(poly)
}
