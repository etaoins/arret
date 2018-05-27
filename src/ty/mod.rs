pub mod datum;
pub mod intersect;
pub mod is_a;
pub mod list_iter;
pub mod pred;
pub mod purity;
pub mod resolve;
pub mod select;
pub mod subst;
pub mod unify;

use std;
use std::ops::Range;
use ty::purity::PRef;
use ty::purity::PVarIds;

/// Abstracts over a reference to a type
///
/// This allows the implementation of our type system to be generic over `Mono` versus `Poly` types.
pub trait TyRef: PartialEq + Clone + Sized {
    /// Type used to store the purity variables introduced by a function
    type PVarIds: purity::PVarIds;

    /// Type used to store the type variables introduced by a function
    type TVarIds: TVarIds;

    // Type used to refer to function purity
    type PRef: purity::PRef;

    /// Constructs a fixed TyRef from the passed Ty
    fn from_ty(Ty<Self>) -> Self;

    /// Tries to convert the TyRef to a fixed Ty
    fn try_to_fixed(&self) -> Option<&Ty<Self>>;

    /// Constructs a fixed TyRef from a union of the passed vector `members`
    ///
    /// `members` should already be unified by the type system; this cannot be used to construct
    /// arbitrary valid unions.
    fn from_vec(mut members: Vec<Self>) -> Self {
        if members.len() == 1 {
            members.pop().unwrap()
        } else {
            Self::from_ty(Ty::Union(members))
        }
    }
}

pub trait TVarIds: PartialEq + Eq + Clone + std::fmt::Debug + std::hash::Hash + Sized {
    fn monomorphic() -> Self;
    fn is_monomorphic(&self) -> bool;
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
    TopFun(Box<TopFun<S>>),
    Fun(Box<Fun<S>>),
    TyPred(Box<S>),

    // Vector types
    Vec(Vec<S>),
    Vecof(Box<S>),

    // List types
    List(List<S>),
}

impl<S> Ty<S>
where
    S: TyRef,
{
    // TODO: Rename into_ty_ref
    pub fn into_ref(self) -> S {
        S::from_ty(self)
    }
}

#[derive(PartialEq, Eq, Debug, Hash, Clone)]
pub struct TopFun<S>
where
    S: TyRef,
{
    purity: S::PRef,
    ret: S,
}

impl<S> TopFun<S>
where
    S: TyRef,
{
    /// Returns a top function type
    pub fn new(purity: S::PRef, ret: S) -> TopFun<S> {
        TopFun { purity, ret }
    }

    /// Returns the `Fun` top type for all type predicate functions
    pub fn new_for_ty_pred() -> TopFun<S> {
        Self::new(
            S::PRef::from_purity(purity::Purity::Pure),
            Ty::Bool.into_ref(),
        )
    }

    pub fn purity(&self) -> &S::PRef {
        &self.purity
    }

    pub fn ret(&self) -> &S {
        &self.ret
    }

    pub fn into_ty(self) -> Ty<S> {
        Ty::TopFun(Box::new(self))
    }

    pub fn into_ref(self) -> S {
        self.into_ty().into_ref()
    }
}

#[derive(PartialEq, Eq, Debug, Hash, Clone)]
pub struct List<S>
where
    S: TyRef,
{
    fixed: Vec<S>,
    rest: Option<Box<S>>,
}

impl<S> List<S>
where
    S: TyRef,
{
    pub fn new(fixed: Vec<S>, rest: Option<S>) -> List<S> {
        List {
            fixed,
            rest: rest.map(Box::new),
        }
    }

    pub fn fixed(&self) -> &Vec<S> {
        &self.fixed
    }

    pub fn rest(&self) -> Option<&S> {
        match self.rest {
            Some(ref rest) => Some(rest.as_ref()),
            None => None,
        }
    }

    fn size_range(&self) -> Range<usize> {
        if self.rest.is_some() {
            (self.fixed.len()..usize::max_value())
        } else {
            (self.fixed.len()..self.fixed.len())
        }
    }

    pub fn has_disjoint_arity(&self, other: &Self) -> bool {
        let range1 = self.size_range();
        let range2 = other.size_range();

        range2.start > range1.end || range2.end < range1.start
    }
}

#[derive(PartialEq, Eq, Debug, Hash, Clone)]
pub struct Fun<S>
where
    S: TyRef,
{
    pvar_ids: S::PVarIds,
    tvar_ids: S::TVarIds,
    top_fun: TopFun<S>,
    params: List<S>,
}

impl<S> Fun<S>
where
    S: TyRef,
{
    pub fn new(
        pvar_ids: S::PVarIds,
        tvar_ids: S::TVarIds,
        top_fun: TopFun<S>,
        params: List<S>,
    ) -> Fun<S> {
        Fun {
            pvar_ids,
            tvar_ids,
            top_fun,
            params,
        }
    }

    /// Returns the `Fun` supertype for all type predicate functions
    ///
    /// This is the type `(Any -> Bool)`. It captures the signature of the type predicates; however
    /// it does not support occurrence typing.
    pub fn new_for_ty_pred() -> Fun<S> {
        Self::new(
            S::PVarIds::monomorphic(),
            S::TVarIds::monomorphic(),
            TopFun::new_for_ty_pred(),
            List::new(vec![Ty::Any.into_ref()], None),
        )
    }

    pub fn pvar_ids(&self) -> &S::PVarIds {
        &self.pvar_ids
    }

    pub fn tvar_ids(&self) -> &S::TVarIds {
        &self.tvar_ids
    }

    pub fn top_fun(&self) -> &TopFun<S> {
        &self.top_fun
    }

    pub fn purity(&self) -> &S::PRef {
        &self.top_fun.purity
    }

    pub fn params(&self) -> &List<S> {
        &self.params
    }

    pub fn ret(&self) -> &S {
        &self.top_fun.ret
    }

    pub fn is_monomorphic(&self) -> bool {
        self.pvar_ids.is_monomorphic() && self.tvar_ids.is_monomorphic()
    }

    pub fn into_ty(self) -> Ty<S> {
        Ty::Fun(Box::new(self))
    }

    pub fn into_ref(self) -> S {
        S::from_ty(self.into_ty())
    }
}

new_indexing_id_type!(TVarId, u32);

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

    pub fn bound(&self) -> &Poly {
        &self.bound
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
    type PVarIds = Range<purity::PVarId>;
    type TVarIds = Range<TVarId>;
    type PRef = purity::Poly;

    fn from_ty(ty: Ty<Poly>) -> Poly {
        Poly::Fixed(ty)
    }

    fn try_to_fixed(&self) -> Option<&Ty<Poly>> {
        match self {
            Poly::Fixed(fixed) => Some(fixed),
            _ => None,
        }
    }
}

impl Ty<Poly> {
    pub fn into_poly(self) -> Poly {
        Poly::Fixed(self)
    }
}

impl TVarIds for Range<TVarId> {
    fn monomorphic() -> Range<TVarId> {
        TVarId::new(0)..TVarId::new(0)
    }

    fn is_monomorphic(&self) -> bool {
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
    #[cfg(test)]
    pub fn into_mono(self) -> Mono {
        Mono(self)
    }
}

impl TyRef for Mono {
    type PVarIds = purity::EmptyPVarIds;
    type TVarIds = EmptyTVarIds;
    type PRef = purity::Purity;

    fn from_ty(ty: Ty<Mono>) -> Mono {
        Mono(ty)
    }

    fn try_to_fixed(&self) -> Option<&Ty<Mono>> {
        Some(self.as_ty())
    }
}

#[derive(PartialEq, Eq, Debug, Hash, Clone)]
pub struct EmptyTVarIds();

impl TVarIds for EmptyTVarIds {
    fn monomorphic() -> EmptyTVarIds {
        EmptyTVarIds()
    }

    fn is_monomorphic(&self) -> bool {
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
    Free,
}

impl Decl {
    pub fn try_to_poly(&self) -> Option<Poly> {
        match self {
            Decl::Fixed(ref fixed) => Some(Poly::Fixed(fixed.clone())),
            Decl::Var(tvar_id) => Some(Poly::Var(*tvar_id)),
            Decl::Free => None,
        }
    }
}

impl Ty<Poly> {
    #[cfg(test)]
    pub fn into_decl(self) -> Decl {
        Decl::Fixed(self)
    }
}
