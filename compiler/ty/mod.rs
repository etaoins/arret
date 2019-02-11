pub mod conv_abi;
pub mod datum;
pub mod intersect;
pub mod is_a;
pub mod list_iter;
pub mod pred;
pub mod props;
pub mod purity;
pub mod resolve;
pub mod select;
pub mod subst;
pub mod subtract;
pub mod ty_args;
pub mod unify;

use std::collections::BTreeMap;
use std::fmt;
use std::ops::Range;

/// Abstracts over a reference to a type
///
/// This allows the implementation of our type system to be generic over `Mono` versus `Poly` types.
pub trait TyRef:
    PartialEq + Clone + Sized + fmt::Debug + From<Ty<Self>> + From<Fun> + From<TopFun>
{
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
            Ty::Union(members.into_boxed_slice()).into()
        }
    }

    /// Combination of find + map looking for a particular fixed type
    ///
    /// This is identical to `try_to_fixed().and_then(pred)` except it iterates inside unions.
    fn find_member<'a, F, T>(&'a self, f: F) -> Option<T>
    where
        F: Fn(&'a Ty<Self>) -> Option<T> + Copy,
        T: 'a,
    {
        match self.try_to_fixed() {
            Some(Ty::Union(members)) => members
                .iter()
                .filter_map(|member| member.find_member(f))
                .next(),
            Some(other) => f(other),
            None => None,
        }
    }

    fn is_never(&self) -> bool {
        self.try_to_fixed() == Some(&Ty::never())
    }
}

#[derive(PartialEq, Eq, Debug, Hash, Clone)]
pub enum Ty<S: TyRef> {
    Any,
    Bool,
    Char,
    Float,
    Map(Box<Map<S>>),
    Int,
    Num,
    LitBool(bool),
    LitSym(Box<str>),
    Set(Box<S>),
    Str,
    Sym,
    Union(Box<[S]>),
    Intersect(Box<[S]>),

    // Function types
    TopFun(Box<TopFun>),
    Fun(Box<Fun>),
    TyPred(pred::TestTy),
    EqPred,

    // Vector types
    Vector(Box<[S]>),
    Vectorof(Box<S>),

    // List types
    List(List<S>),
}

impl<S: TyRef> Ty<S> {
    /// Returns the canonical unit type
    pub fn unit() -> Ty<S> {
        Ty::List(List::new(Box::new([]), None))
    }

    /// Returns the canonical never type
    pub fn never() -> Ty<S> {
        Ty::Union(Box::new([]))
    }
}

#[derive(PartialEq, Eq, Debug, Hash, Clone)]
pub struct Map<S: TyRef> {
    key: S,
    value: S,
}

impl<S: TyRef> Map<S> {
    pub fn new(key: S, value: S) -> Map<S> {
        Map { key, value }
    }

    pub fn key(&self) -> &S {
        &self.key
    }

    pub fn value(&self) -> &S {
        &self.value
    }
}

#[derive(PartialEq, Eq, Debug, Hash, Clone)]
pub struct List<S: TyRef> {
    fixed: Box<[S]>,
    rest: Option<Box<S>>,
}

impl<S: TyRef> List<S> {
    pub fn new(fixed: Box<[S]>, rest: Option<S>) -> List<S> {
        List {
            fixed,
            rest: rest.map(Box::new),
        }
    }

    pub fn empty() -> List<S> {
        List::new(Box::new([]), None)
    }

    pub fn fixed(&self) -> &[S] {
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

    pub fn is_empty(&self) -> bool {
        self.fixed.is_empty() && self.rest.is_none()
    }
}

#[derive(PartialEq, Eq, Debug, Hash, Clone)]
pub struct TopFun {
    purity: purity::Poly,
    ret: Poly,
}

impl TopFun {
    /// Returns a top function type
    pub fn new(purity: purity::Poly, ret: Poly) -> TopFun {
        TopFun { purity, ret }
    }

    /// Returns the `Fun` top type for all predicate functions
    pub fn new_for_pred() -> TopFun {
        Self::new(purity::Purity::Pure.into(), Ty::Bool.into())
    }

    pub fn purity(&self) -> &purity::Poly {
        &self.purity
    }

    pub fn ret(&self) -> &Poly {
        &self.ret
    }
}

impl<S: TyRef> From<TopFun> for Ty<S> {
    fn from(top_fun: TopFun) -> Self {
        Ty::TopFun(Box::new(top_fun))
    }
}

impl From<TopFun> for Poly {
    fn from(top_fun: TopFun) -> Self {
        Poly::Fixed(Ty::TopFun(Box::new(top_fun)))
    }
}

impl From<TopFun> for Mono {
    fn from(top_fun: TopFun) -> Self {
        Mono(Ty::TopFun(Box::new(top_fun)))
    }
}

#[derive(PartialEq, Eq, Debug, Hash, Clone)]
pub struct Fun {
    pvars: purity::PVars,
    tvars: TVars,
    top_fun: TopFun,
    params: List<Poly>,
}

impl Fun {
    pub fn new(pvars: purity::PVars, tvars: TVars, top_fun: TopFun, params: List<Poly>) -> Fun {
        Fun {
            pvars,
            tvars,
            top_fun,
            params,
        }
    }

    // Returns the `Fun` type for the `(main!)` function
    pub fn new_for_main() -> Fun {
        Self::new(
            purity::PVars::new(),
            TVars::new(),
            TopFun::new(purity::Purity::Impure.into(), Ty::unit().into()),
            List::empty(),
        )
    }

    /// Returns the `Fun` supertype for all type predicate functions
    ///
    /// This is the type `(Any -> Bool)`. It captures the signature of the type predicates; however,
    /// it does not support occurrence typing.
    pub fn new_for_ty_pred() -> Fun {
        Self::new(
            purity::PVars::new(),
            TVars::new(),
            TopFun::new_for_pred(),
            List::new(Box::new([Ty::Any.into()]), None),
        )
    }

    /// Returns the `Fun` supertype for the equality predicate
    ///
    /// This is the type `(Any Any -> Bool)`. It captures the signature of the equality predicate;
    /// however, it does not support occurrence typing.
    pub fn new_for_eq_pred() -> Fun {
        Self::new(
            purity::PVars::new(),
            TVars::new(),
            TopFun::new_for_pred(),
            List::new(Box::new([Ty::Any.into(), Ty::Any.into()]), None),
        )
    }

    pub fn pvars(&self) -> &purity::PVars {
        &self.pvars
    }

    pub fn tvars(&self) -> &TVars {
        &self.tvars
    }

    pub fn top_fun(&self) -> &TopFun {
        &self.top_fun
    }

    pub fn purity(&self) -> &purity::Poly {
        &self.top_fun.purity
    }

    pub fn params(&self) -> &List<Poly> {
        &self.params
    }

    pub fn ret(&self) -> &Poly {
        &self.top_fun.ret
    }

    pub fn has_polymorphic_vars(&self) -> bool {
        !self.pvars.is_empty() || !self.tvars.is_empty()
    }

    pub fn with_polymorphic_vars(self, pvars: purity::PVars, tvars: TVars) -> Fun {
        Fun {
            pvars,
            tvars,
            ..self
        }
    }
}

impl<S: TyRef> From<Fun> for Ty<S> {
    fn from(fun: Fun) -> Self {
        Ty::Fun(Box::new(fun))
    }
}

impl From<Fun> for Poly {
    fn from(fun: Fun) -> Self {
        Poly::Fixed(Ty::Fun(Box::new(fun)))
    }
}

impl From<Fun> for Mono {
    fn from(fun: Fun) -> Self {
        Mono(Ty::Fun(Box::new(fun)))
    }
}

new_global_id_type!(TVarId);

#[derive(PartialEq, Eq, Debug, Hash, Clone)]
pub struct TVar {
    source_name: Box<str>,
    bound: Poly,
}

pub type TVars = BTreeMap<TVarId, TVar>;

impl TVar {
    pub fn new(source_name: Box<str>, bound: Poly) -> TVar {
        TVar { source_name, bound }
    }

    pub fn source_name(&self) -> &str {
        &self.source_name
    }

    pub fn bound(&self) -> &Poly {
        &self.bound
    }
}

pub fn merge_tvars(outer: &TVars, inner: &TVars) -> TVars {
    outer
        .iter()
        .map(|(tvar_id, tvar)| (*tvar_id, tvar.clone()))
        .chain(inner.iter().map(|(tvar_id, tvar)| (*tvar_id, tvar.clone())))
        .collect()
}

pub fn merge_three_tvars(one: &TVars, two: &TVars, three: &TVars) -> TVars {
    one.iter()
        .map(|(tvar_id, tvar)| (*tvar_id, tvar.clone()))
        .chain(two.iter().map(|(tvar_id, tvar)| (*tvar_id, tvar.clone())))
        .chain(three.iter().map(|(tvar_id, tvar)| (*tvar_id, tvar.clone())))
        .collect()
}

#[derive(PartialEq, Eq, Debug, Hash, Clone)]
pub enum Poly {
    Var(TVarId),
    Fixed(Ty<Poly>),
}

impl TyRef for Poly {
    fn try_to_fixed(&self) -> Option<&Ty<Poly>> {
        match self {
            Poly::Fixed(fixed) => Some(fixed),
            _ => None,
        }
    }
}

impl From<Ty<Poly>> for Poly {
    fn from(ty: Ty<Poly>) -> Self {
        Poly::Fixed(ty)
    }
}

#[derive(PartialEq, Eq, Debug, Clone)]
pub struct Mono(Ty<Mono>);

impl Mono {
    pub fn as_ty(&self) -> &Ty<Mono> {
        &self.0
    }

    pub fn into_ty(self) -> Ty<Mono> {
        self.0
    }
}

impl TyRef for Mono {
    fn try_to_fixed(&self) -> Option<&Ty<Mono>> {
        Some(self.as_ty())
    }
}

impl From<Ty<Mono>> for Mono {
    fn from(ty: Ty<Mono>) -> Self {
        Mono(ty)
    }
}

/// Decl is a type declared by a user
///
/// The `Known` variant indicates the type is specified while `Free` indicates it must be inferred.
#[derive(PartialEq, Eq, Debug, Clone)]
pub enum Decl {
    Known(Poly),
    Free,
}

impl From<Ty<Poly>> for Decl {
    fn from(ty: Ty<Poly>) -> Self {
        Decl::Known(Poly::Fixed(ty))
    }
}

impl From<Poly> for Decl {
    fn from(poly: Poly) -> Self {
        Decl::Known(poly)
    }
}
