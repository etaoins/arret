use crate::ty;
use crate::ty::purity;
use crate::ty::record;
use crate::ty::ty_args::TyArgs;
use crate::ty::Ty;

fn subst_ty_ref_slice<S>(stx: &S, inputs: &[ty::Ref<S::InputPM>]) -> Box<[ty::Ref<S::OutputPM>]>
where
    S: Substitution,
{
    inputs.iter().map(|i| stx.subst_ty_ref(i)).collect()
}

fn subst_list<S>(stx: &S, list: &ty::List<S::InputPM>) -> ty::List<S::OutputPM>
where
    S: Substitution,
{
    ty::List::new(
        subst_ty_ref_slice(stx, list.fixed()),
        stx.subst_ty_ref(list.rest()),
    )
}

fn subst_record_instance<S>(
    stx: &S,
    instance: &record::Instance<S::InputPM>,
) -> record::Instance<S::OutputPM>
where
    S: Substitution,
{
    let subst_pvar_purities = instance
        .ty_args()
        .pvar_purities()
        .iter()
        .map(|(pvar, purity_ref)| (pvar.clone(), stx.subst_purity_ref(purity_ref)))
        .collect();

    let subst_tvar_types = instance
        .ty_args()
        .tvar_types()
        .iter()
        .map(|(tvar, ty_ref)| (tvar.clone(), stx.subst_ty_ref(ty_ref)))
        .collect();

    record::Instance::new(
        instance.cons().clone(),
        TyArgs::new(subst_pvar_purities, subst_tvar_types),
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

fn subst_ty<S>(stx: &S, ty: &Ty<S::InputPM>) -> Ty<S::OutputPM>
where
    S: Substitution,
{
    match ty {
        Ty::Any => Ty::Any,
        Ty::Bool => Ty::Bool,
        Ty::Char => Ty::Char,
        Ty::Float => Ty::Float,
        Ty::Int => Ty::Int,
        Ty::Num => Ty::Num,
        Ty::Str => Ty::Str,
        Ty::Sym => Ty::Sym,
        Ty::EqPred => Ty::EqPred,
        Ty::TopRecord => Ty::TopRecord,

        Ty::TyPred(test_ty) => Ty::TyPred(test_ty.clone()),
        Ty::TopFun(top_fun) => subst_top_fun(stx, top_fun).into(),
        Ty::Fun(fun) => subst_fun(stx, fun).into(),
        Ty::Map(map) => {
            ty::Map::new(stx.subst_ty_ref(map.key()), stx.subst_ty_ref(map.value())).into()
        }
        Ty::LitBool(val) => Ty::LitBool(*val),
        Ty::LitSym(val) => Ty::LitSym(val.clone()),
        Ty::Set(member) => Ty::Set(Box::new(stx.subst_ty_ref(member))),
        Ty::Union(members) => Ty::Union(subst_ty_ref_slice(stx, members)),
        Ty::Intersect(members) => Ty::Intersect(subst_ty_ref_slice(stx, members)),
        Ty::Vector(members) => Ty::Vector(subst_ty_ref_slice(stx, members)),
        Ty::Vectorof(member) => Ty::Vectorof(Box::new(stx.subst_ty_ref(member))),
        Ty::List(list) => subst_list(stx, list).into(),
        Ty::RecordClass(cons) => Ty::RecordClass(cons.clone()),
        Ty::Record(instance) => Ty::Record(Box::new(subst_record_instance(stx, instance))),
    }
}

trait Substitution {
    type InputPM: ty::Pm;
    type OutputPM: ty::Pm;
    type AsPolySubst: Substitution<InputPM = ty::Poly, OutputPM = ty::Poly>;

    fn subst_purity_ref(&self, poly: &purity::Ref) -> purity::Ref;
    fn subst_ty_ref(&self, input: &ty::Ref<Self::InputPM>) -> ty::Ref<Self::OutputPM>;
    fn as_poly_subst(&self) -> &Self::AsPolySubst;
}

impl<'tvars> Substitution for TyArgs<ty::Poly> {
    type InputPM = ty::Poly;
    type OutputPM = ty::Poly;
    type AsPolySubst = Self;

    fn subst_purity_ref(&self, poly: &purity::Ref) -> purity::Ref {
        match poly {
            purity::Ref::Fixed(_) => poly.clone(),
            purity::Ref::Var(pvar) => {
                if let Some(selected) = self.pvar_purities().get(pvar) {
                    selected.clone()
                } else {
                    poly.clone()
                }
            }
        }
    }

    fn subst_ty_ref(&self, poly: &ty::Ref<ty::Poly>) -> ty::Ref<ty::Poly> {
        match poly {
            ty::Ref::Fixed(fixed) => subst_ty(self, fixed).into(),
            ty::Ref::Var(tvar, _) => {
                if let Some(selected) = self.tvar_types().get(tvar) {
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
    type InputPM = ty::Poly;
    type OutputPM = ty::Poly;
    type AsPolySubst = Self;

    fn subst_purity_ref(&self, poly: &purity::Ref) -> purity::Ref {
        poly.clone()
    }

    fn subst_ty_ref(&self, poly: &ty::Ref<ty::Poly>) -> ty::Ref<ty::Poly> {
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
    type InputPM = ty::Mono;
    type OutputPM = ty::Poly;
    type AsPolySubst = PolyIdentity;

    fn subst_purity_ref(&self, poly: &purity::Ref) -> purity::Ref {
        poly.clone()
    }

    fn subst_ty_ref(&self, mono: &ty::Ref<ty::Mono>) -> ty::Ref<ty::Poly> {
        subst_ty(self, mono.as_ty()).into()
    }

    fn as_poly_subst(&self) -> &PolyIdentity {
        &self.poly_identity
    }
}

struct Monomorphise<'tyargs> {
    mono_ty_args: &'tyargs TyArgs<ty::Mono>,
    partial: PartialMonomorphise<'tyargs>,
}

impl<'tyargs> Monomorphise<'tyargs> {
    fn new(mta: &'tyargs TyArgs<ty::Mono>) -> Monomorphise<'tyargs> {
        Monomorphise {
            mono_ty_args: mta,
            partial: PartialMonomorphise { mono_ty_args: mta },
        }
    }
}

impl<'tyargs> Substitution for Monomorphise<'tyargs> {
    type InputPM = ty::Poly;
    type OutputPM = ty::Mono;
    type AsPolySubst = PartialMonomorphise<'tyargs>;

    fn subst_purity_ref(&self, poly: &purity::Ref) -> purity::Ref {
        match poly {
            purity::Ref::Fixed(_) => poly.clone(),
            purity::Ref::Var(pvar) => self
                .mono_ty_args
                .pvar_purities()
                .get(pvar)
                .unwrap_or_else(|| {
                    panic!(
                        "unable to find purity argument `{}` during monomorphisation",
                        pvar.source_name()
                    )
                })
                .clone(),
        }
    }

    fn subst_ty_ref(&self, poly: &ty::Ref<ty::Poly>) -> ty::Ref<ty::Mono> {
        match poly {
            ty::Ref::Fixed(fixed) => subst_ty(self, fixed).into(),
            ty::Ref::Var(tvar, _) => self
                .mono_ty_args
                .tvar_types()
                .get(tvar)
                .unwrap_or_else(|| {
                    panic!(
                        "unable to find type argument `{}` during monomorphisation",
                        tvar.source_name()
                    )
                })
                .clone(),
        }
    }

    fn as_poly_subst(&self) -> &PartialMonomorphise<'tyargs> {
        &self.partial
    }
}

struct PartialMonomorphise<'tyargs> {
    mono_ty_args: &'tyargs TyArgs<ty::Mono>,
}

impl<'tyargs> Substitution for PartialMonomorphise<'tyargs> {
    type InputPM = ty::Poly;
    type OutputPM = ty::Poly;
    type AsPolySubst = Self;

    fn subst_purity_ref(&self, poly: &purity::Ref) -> purity::Ref {
        match poly {
            purity::Ref::Fixed(_) => poly.clone(),
            purity::Ref::Var(pvar) => {
                if let Some(purity) = self.mono_ty_args.pvar_purities().get(pvar) {
                    purity.clone()
                } else {
                    poly.clone()
                }
            }
        }
    }

    fn subst_ty_ref(&self, poly: &ty::Ref<ty::Poly>) -> ty::Ref<ty::Poly> {
        match poly {
            ty::Ref::Fixed(fixed) => subst_ty(self, fixed).into(),
            ty::Ref::Var(tvar, _) => {
                if let Some(mono) = self.mono_ty_args.tvar_types().get(tvar) {
                    subst_ty(&MonoToPoly::new(), mono.as_ty()).into()
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

pub fn subst_poly(pta: &TyArgs<ty::Poly>, poly: &ty::Ref<ty::Poly>) -> ty::Ref<ty::Poly> {
    pta.subst_ty_ref(poly)
}

pub fn subst_poly_fun(pta: &TyArgs<ty::Poly>, fun: &ty::Fun) -> ty::Fun {
    subst_fun(pta, fun)
}

pub fn subst_purity(pta: &TyArgs<ty::Poly>, purity: &purity::Ref) -> purity::Ref {
    pta.subst_purity_ref(purity)
}

pub fn monomorphise(mta: &TyArgs<ty::Mono>, poly: &ty::Ref<ty::Poly>) -> ty::Ref<ty::Mono> {
    let stx = Monomorphise::new(mta);
    stx.subst_ty_ref(poly)
}

pub fn monomorphise_purity(mta: &TyArgs<ty::Mono>, poly: &purity::Ref) -> purity::Ref {
    let stx = Monomorphise::new(mta);
    stx.subst_purity_ref(poly)
}

pub fn monomorphise_list(mta: &TyArgs<ty::Mono>, poly: &ty::List<ty::Poly>) -> ty::List<ty::Mono> {
    let stx = Monomorphise::new(mta);
    subst_list(&stx, poly)
}
