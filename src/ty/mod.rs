#[cfg(test)]
mod datum;
mod intersect;
pub mod is_a;
#[cfg(test)]
pub mod pred;
pub mod purity;
pub mod resolve;
#[cfg(test)]
pub mod subst;
pub mod unify;

use std;
use std::ops::Range;
use ty::purity::PRef;

/// Abstracts over a reference to a type
///
/// This allows the implementation of our type system to be generic over `Mono` versus `Poly` types.
pub trait TyRef: PartialEq + Clone + Sized {
    /// Type used to store the type variables introduced by a function
    type TVarIds: TVarIds;

    // Type used to refer to function purity
    type PRef: purity::PRef;

    /// Constructs a fixed TyRef from the passed Ty
    fn from_ty(Ty<Self>) -> Self;

    /// Constructs a fixed TyRef from a union of the passed vector `members`
    ///
    /// `members` should already be unified by the type system; this cannot be used to construct
    /// arbitrary valid unions.
    fn from_vec(mut members: Vec<Self>) -> Self {
        // TODO: Use a slice pattern here once they're stable
        if members.len() == 1 {
            members.pop().unwrap()
        } else {
            Self::from_ty(Ty::Union(members))
        }
    }
}

pub trait TVarIds: PartialEq + Eq + Clone + std::fmt::Debug + std::hash::Hash + Sized {
    fn empty() -> Self;
    fn is_empty(&self) -> bool;
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

    pub fn new_fun(purity: S::PRef, tvar_ids: S::TVarIds, params: S, ret: S) -> Ty<S> {
        Ty::Fun(Box::new(Fun::new(purity, tvar_ids, params, ret)))
    }
}

#[derive(PartialEq, Eq, Debug, Hash, Clone)]
pub struct Fun<S>
where
    S: TyRef,
{
    purity: S::PRef,
    tvar_ids: S::TVarIds,
    params: S,
    ret: S,
}

impl<S> Fun<S>
where
    S: TyRef,
{
    pub fn new(purity: S::PRef, tvar_ids: S::TVarIds, params: S, ret: S) -> Fun<S> {
        Fun {
            purity,
            tvar_ids,
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
            S::PRef::from_purity(purity::Purity::Pure),
            S::TVarIds::empty(),
            Ty::new_simple_list_type(iter::once(S::from_ty(Ty::Any)), None),
            S::from_ty(Ty::Bool),
        )
    }

    /// Returns a top function type
    pub fn new_top(purity: S::PRef, ret: S) -> Fun<S> {
        Self::new(
            purity,
            S::TVarIds::empty(),
            S::from_ty(Ty::Union(vec![])),
            ret,
        )
    }

    pub fn purity(&self) -> &S::PRef {
        &self.purity
    }

    pub fn params(&self) -> &S {
        &self.params
    }

    pub fn ret(&self) -> &S {
        &self.ret
    }

    pub fn is_polymorphic(&self) -> bool {
        !self.tvar_ids.is_empty()
    }
}

#[derive(Debug, PartialEq, Eq, Clone, Copy, Hash, PartialOrd)]
pub struct TVarId(u32);

impl TVarId {
    pub fn new(id: usize) -> TVarId {
        TVarId(id as u32)
    }

    pub fn to_usize(self) -> usize {
        self.0 as usize
    }
}

#[derive(PartialEq, Eq, Debug, Hash, Clone)]
pub struct TVar {
    source_name: String,
    bound: Poly,
}

impl TVar {
    pub fn new(source_name: String, bound: Poly) -> TVar {
        TVar { source_name, bound }
    }

    pub fn source_name(&self) -> &String {
        &self.source_name
    }
}

#[derive(PartialEq, Eq, Debug, Hash, Clone)]
pub enum Poly {
    Var(TVarId),
    Fixed(Ty<Poly>),
}

impl Poly {
    pub fn into_decl(self) -> Decl {
        match self {
            Poly::Var(tvar_id) => Decl::Var(tvar_id),
            Poly::Fixed(ty) => Decl::Fixed(ty),
        }
    }
}

impl TyRef for Poly {
    type TVarIds = Range<TVarId>;
    type PRef = purity::Purity;

    fn from_ty(ty: Ty<Poly>) -> Poly {
        ty.into_poly()
    }
}

impl Ty<Poly> {
    pub fn into_poly(self) -> Poly {
        Poly::Fixed(self)
    }
}

impl TVarIds for Range<TVarId> {
    fn empty() -> Range<TVarId> {
        TVarId::new(0)..TVarId::new(0)
    }

    fn is_empty(&self) -> bool {
        self.start >= self.end
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
    type TVarIds = EmptyTVarIds;
    type PRef = purity::Purity;

    fn from_ty(ty: Ty<Mono>) -> Mono {
        ty.into_mono()
    }
}

#[derive(PartialEq, Eq, Debug, Hash, Clone)]
pub struct EmptyTVarIds();

impl TVarIds for EmptyTVarIds {
    fn empty() -> EmptyTVarIds {
        EmptyTVarIds()
    }

    fn is_empty(&self) -> bool {
        true
    }
}

/// Decl is a type declared by a user
///
/// It is identical to a Poly type except there is an additional `Free` variant that indicates the
/// user did not specify an explicit type and it must be inferred.
#[derive(PartialEq, Eq, Debug, Clone)]
pub enum Decl {
    Var(TVarId),
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
