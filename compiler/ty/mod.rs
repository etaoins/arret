pub mod conv_abi;
pub mod datum;
pub mod intersect;
pub mod is_a;
pub mod list_iter;
pub mod pred;
pub mod props;
pub mod purity;
pub mod select;
pub mod subst;
pub mod subtract;
pub mod ty_args;
pub mod unify;

use std::collections::BTreeMap;
use std::fmt;
use std::hash;
use std::ops::Range;

new_global_id_type!(TVarId);

#[derive(PartialEq, Eq, Debug, Hash, Clone)]
pub struct TVar {
    source_name: Box<str>,
    bound: Ref<Poly>,
}

pub type TVars = BTreeMap<TVarId, TVar>;

impl TVar {
    pub fn new(source_name: Box<str>, bound: Ref<Poly>) -> TVar {
        TVar { source_name, bound }
    }

    pub fn source_name(&self) -> &str {
        &self.source_name
    }

    pub fn bound(&self) -> &Ref<Poly> {
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
/// Marker that determines if type variables are allowed within a type
pub trait PM: PartialEq + Eq + Clone + Copy + Sized + fmt::Debug + hash::Hash {
    /// Resolves a possibly variable type to its bound
    fn resolve_ref_to_ty<'ty>(tvars: &'ty TVars, ty_ref: &'ty Ref<Self>) -> &'ty Ty<Self>;
}

#[derive(Clone, Copy, PartialEq, Eq, Debug, Hash)]
pub enum Mono {}
impl PM for Mono {
    fn resolve_ref_to_ty<'ty>(_tvars: &'ty TVars, ty_ref: &'ty Ref<Mono>) -> &'ty Ty<Mono> {
        match ty_ref {
            Ref::Fixed(ty) => ty,
            Ref::Var(_, _) => unreachable!(),
        }
    }
}

#[derive(Clone, Copy, PartialEq, Eq, Debug, Hash)]
pub struct Poly {}
impl PM for Poly {
    fn resolve_ref_to_ty<'ty>(tvars: &'ty TVars, ty_ref: &'ty Ref<Poly>) -> &'ty Ty<Poly> {
        match ty_ref {
            Ref::Fixed(ty) => ty,
            Ref::Var(tvar_id, _) => Self::resolve_ref_to_ty(
                tvars,
                &tvars
                    .get(tvar_id)
                    .expect("TVar not found while resolving TVarId")
                    .bound,
            ),
        }
    }
}

#[derive(PartialEq, Eq, Debug, Hash, Clone)]
pub enum Ref<M: PM> {
    Var(TVarId, M),
    Fixed(Ty<M>),
}

impl<M: PM> Ref<M> {
    /// Tries to convert the TyRef to a fixed Ty
    pub fn try_to_fixed(&self) -> Option<&Ty<M>> {
        match self {
            Ref::Var(_, _) => None,
            Ref::Fixed(ty) => Some(ty),
        }
    }

    /// Constructs a fixed TyRef from a union of the passed vector `members`
    ///
    /// `members` should already be unified by the type system; this cannot be used to construct
    /// arbitrary valid unions.
    pub fn from_vec(mut members: Vec<Self>) -> Self {
        if members.len() == 1 {
            members.pop().unwrap()
        } else {
            Ty::Union(members.into_boxed_slice()).into()
        }
    }

    /// Combination of find + map looking for a particular fixed type
    ///
    /// This is identical to `try_to_fixed().and_then(pred)` except it iterates inside unions.
    pub fn find_member<'a, F, T>(&'a self, f: F) -> Option<T>
    where
        F: Fn(&'a Ty<M>) -> Option<T> + Copy,
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

    pub fn resolve_to_ty<'ty>(&'ty self, tvars: &'ty TVars) -> &'ty Ty<M> {
        M::resolve_ref_to_ty(tvars, self)
    }

    pub fn is_never(&self) -> bool {
        self.try_to_fixed() == Some(&Ty::never())
    }
}

impl Ref<Mono> {
    pub fn as_ty(&self) -> &Ty<Mono> {
        match self {
            Ref::Fixed(ty) => ty,
            Ref::Var(_, _) => {
                unreachable!();
            }
        }
    }

    pub fn into_ty(self) -> Ty<Mono> {
        match self {
            Ref::Fixed(ty) => ty,
            Ref::Var(_, _) => {
                unreachable!();
            }
        }
    }
}

impl<M: PM> From<Ty<M>> for Ref<M> {
    fn from(ty: Ty<M>) -> Self {
        Ref::Fixed(ty)
    }
}

#[derive(PartialEq, Eq, Debug, Hash, Clone)]
pub enum Ty<M: PM> {
    Any,
    Bool,
    Char,
    Float,
    Map(Box<Map<M>>),
    Int,
    Num,
    LitBool(bool),
    LitSym(Box<str>),
    Set(Box<Ref<M>>),
    Str,
    Sym,
    Union(Box<[Ref<M>]>),
    Intersect(Box<[Ref<M>]>),

    // Function types
    TopFun(Box<TopFun>),
    Fun(Box<Fun>),
    TyPred(pred::TestTy),
    EqPred,

    // Vector types
    Vector(Box<[Ref<M>]>),
    Vectorof(Box<Ref<M>>),

    // List types
    List(List<M>),
}

impl<M: PM> Ty<M> {
    /// Returns the canonical unit type
    pub fn unit() -> Ty<M> {
        Ty::List(List::new(Box::new([]), None))
    }

    /// Returns the canonical never type
    pub fn never() -> Ty<M> {
        Ty::Union(Box::new([]))
    }
}

#[derive(PartialEq, Eq, Debug, Hash, Clone)]
pub struct Map<M: PM> {
    key: Ref<M>,
    value: Ref<M>,
}

impl<M: PM> Map<M> {
    pub fn new(key: Ref<M>, value: Ref<M>) -> Map<M> {
        Map { key, value }
    }

    pub fn key(&self) -> &Ref<M> {
        &self.key
    }

    pub fn value(&self) -> &Ref<M> {
        &self.value
    }
}

#[derive(PartialEq, Eq, Debug, Hash, Clone)]
pub struct List<M: PM> {
    fixed: Box<[Ref<M>]>,
    rest: Option<Box<Ref<M>>>,
}

impl<M: PM> List<M> {
    pub fn new(fixed: Box<[Ref<M>]>, rest: Option<Ref<M>>) -> List<M> {
        List {
            fixed,
            rest: rest.map(Box::new),
        }
    }

    pub fn empty() -> List<M> {
        List::new(Box::new([]), None)
    }

    pub fn fixed(&self) -> &[Ref<M>] {
        &self.fixed
    }

    pub fn rest(&self) -> Option<&Ref<M>> {
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
    ret: Ref<Poly>,
}

impl TopFun {
    /// Returns a top function type
    pub fn new(purity: purity::Poly, ret: Ref<Poly>) -> TopFun {
        TopFun { purity, ret }
    }

    /// Returns the `Fun` top type for all predicate functions
    pub fn new_for_pred() -> TopFun {
        Self::new(purity::Purity::Pure.into(), Ty::Bool.into())
    }

    pub fn purity(&self) -> &purity::Poly {
        &self.purity
    }

    pub fn ret(&self) -> &Ref<Poly> {
        &self.ret
    }
}

impl<M: PM> From<TopFun> for Ty<M> {
    fn from(top_fun: TopFun) -> Self {
        Ty::TopFun(Box::new(top_fun))
    }
}

impl<M: PM> From<TopFun> for Ref<M> {
    fn from(top_fun: TopFun) -> Self {
        Ref::Fixed(Ty::TopFun(Box::new(top_fun)))
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

    pub fn ret(&self) -> &Ref<Poly> {
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

impl<M: PM> From<Fun> for Ty<M> {
    fn from(fun: Fun) -> Self {
        Ty::Fun(Box::new(fun))
    }
}

impl<M: PM> From<Fun> for Ref<M> {
    fn from(fun: Fun) -> Self {
        Ref::Fixed(Ty::Fun(Box::new(fun)))
    }
}

/// Decl is a type declared by a user
///
/// The `Known` variant indicates the type is specified while `Free` indicates it must be inferred.
#[derive(PartialEq, Eq, Debug, Clone)]
pub enum Decl {
    Known(Ref<Poly>),
    Free,
}

impl From<Ty<Poly>> for Decl {
    fn from(ty: Ty<Poly>) -> Self {
        Decl::Known(Ref::Fixed(ty))
    }
}

impl From<Ref<Poly>> for Decl {
    fn from(poly: Ref<Poly>) -> Self {
        Decl::Known(poly)
    }
}
