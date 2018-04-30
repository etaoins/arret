#[cfg(test)]
mod datum;
mod intersect;
pub mod is_a;
#[cfg(test)]
pub mod pred;
pub mod resolve;
#[cfg(test)]
pub mod subst;
pub mod unify;

use std::ops::Range;

pub trait TyRef: PartialEq + Clone + Sized {
    fn from_ty(Ty<Self>) -> Self;

    fn from_vec(mut members: Vec<Self>) -> Self {
        // TODO: Use a slice pattern here once they're stable
        if members.len() == 1 {
            members.pop().unwrap()
        } else {
            Self::from_ty(Ty::Union(members))
        }
    }
}

#[derive(PartialEq, Eq, Debug, Hash, Clone)]
pub enum Ty<S>
where
    S: TyRef,
{
    Any,
    Bool,
    Char,
    Float,
    Map(Box<S>, Box<S>),
    Int,
    LitBool(bool),
    LitSym(String),
    Set(Box<S>),
    Str,
    Sym,
    Union(Vec<S>),

    // Function types
    Fun(Box<Fun<S>>),
    TyPred(Box<S>),

    // Vector types
    Vec(Vec<S>),
    Vecof(Box<S>),

    // List types
    Listof(Box<S>),
    Cons(Box<S>, Box<S>),
    Nil,
}

impl<S> Ty<S>
where
    S: TyRef,
{
    pub fn new_simple_list_type<I>(fixed: I, rest: Option<S>) -> S
    where
        I: DoubleEndedIterator<Item = S>,
    {
        let tail_poly = rest.map(|t| S::from_ty(Ty::Listof(Box::new(t))))
            .unwrap_or_else(|| S::from_ty(Ty::Nil));

        fixed.rev().fold(tail_poly, |tail_ref, fixed_ref| {
            S::from_ty(Ty::Cons(Box::new(fixed_ref), Box::new(tail_ref)))
        })
    }

    pub fn new_fun(impure: bool, pvar_ids: Range<PVarId>, params: S, ret: S) -> Ty<S> {
        Ty::Fun(Box::new(Fun::new(impure, pvar_ids, params, ret)))
    }
}

#[derive(PartialEq, Eq, Debug, Hash, Clone)]
pub struct Fun<S>
where
    S: TyRef,
{
    impure: bool,
    pvar_ids: Range<PVarId>,
    params: S,
    ret: S,
}

impl<S> Fun<S>
where
    S: TyRef,
{
    pub fn new(impure: bool, pvar_ids: Range<PVarId>, params: S, ret: S) -> Fun<S> {
        Fun {
            impure,
            pvar_ids,
            params,
            ret,
        }
    }

    /// Returns the `Fun` supertype for all type predicate functions
    ///
    /// This is the type `(Any -> Bool)`. It captures the signature of the type predicates; however
    /// it does not support occurrence typing.
    pub fn new_for_ty_pred() -> Fun<S> {
        use std::iter;

        Self::new(
            false,
            PVarId::new(0)..PVarId::new(0),
            Ty::new_simple_list_type(iter::once(S::from_ty(Ty::Any)), None),
            S::from_ty(Ty::Bool),
        )
    }

    /// Returns the top function type
    pub fn new_top(impure: bool) -> Fun<S> {
        Self::new(
            impure,
            PVarId::new(0)..PVarId::new(0),
            S::from_ty(Ty::Union(vec![])),
            S::from_ty(Ty::Any),
        )
    }

    pub fn impure(&self) -> bool {
        self.impure
    }

    pub fn pvar_ids(&self) -> &Range<PVarId> {
        &self.pvar_ids
    }

    pub fn params(&self) -> &S {
        &self.params
    }

    pub fn ret(&self) -> &S {
        &self.ret
    }

    pub fn is_polymorphic(&self) -> bool {
        self.pvar_ids.start < self.pvar_ids.end
    }
}

#[derive(Debug, PartialEq, Eq, Clone, Copy, Hash, PartialOrd)]
pub struct PVarId(u32);

impl PVarId {
    pub fn new(id: usize) -> PVarId {
        PVarId(id as u32)
    }

    pub fn to_usize(self) -> usize {
        self.0 as usize
    }
}

#[derive(PartialEq, Eq, Debug, Hash, Clone)]
pub struct PVar {
    source_name: String,
    bound: Poly,
}

impl PVar {
    pub fn new(source_name: String, bound: Poly) -> PVar {
        PVar { source_name, bound }
    }

    pub fn source_name(&self) -> &String {
        &self.source_name
    }
}

#[derive(PartialEq, Eq, Debug, Hash, Clone)]
pub enum Poly {
    Var(PVarId),
    Fixed(Ty<Poly>),
}

impl Poly {
    pub fn into_decl(self) -> Decl {
        match self {
            Poly::Var(pvar_id) => Decl::Var(pvar_id),
            Poly::Fixed(ty) => Decl::Fixed(ty),
        }
    }
}

impl TyRef for Poly {
    fn from_ty(ty: Ty<Poly>) -> Poly {
        ty.into_poly()
    }
}

impl Ty<Poly> {
    pub fn into_poly(self) -> Poly {
        Poly::Fixed(self)
    }
}

#[derive(PartialEq, Eq, Debug, Clone)]
pub struct Mono(Ty<Mono>);

impl Mono {
    pub fn as_ty(&self) -> &Ty<Mono> {
        &self.0
    }
}

impl Ty<Mono> {
    pub fn into_mono(self) -> Mono {
        Mono(self)
    }
}

impl TyRef for Mono {
    fn from_ty(ty: Ty<Mono>) -> Mono {
        ty.into_mono()
    }
}

/// Decl is a type declared by a user
///
/// It is identical to a Poly type except there is an additional `Free` variant that indicates the
/// user did not specify an explicit type and it must be inferred. `Free` takes a `Span` of the
/// declaration for the purposes of reporting inference errors.
#[derive(PartialEq, Eq, Debug, Clone)]
pub enum Decl {
    Var(PVarId),
    Fixed(Ty<Poly>),
    Free(FreeTyId),
}

#[derive(Debug, PartialEq, Eq, Clone, Copy, Hash)]
pub struct FreeTyId(u32);

impl FreeTyId {
    pub fn new(id: usize) -> FreeTyId {
        FreeTyId(id as u32)
    }
}

impl Ty<Poly> {
    #[cfg(test)]
    pub fn into_decl(self) -> Decl {
        Decl::Fixed(self)
    }
}
