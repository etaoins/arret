pub mod conv_abi;
pub mod datum;
pub mod intersect;
pub mod is_a;
pub mod list_iter;
pub mod pred;
pub mod props;
pub mod purity;
pub mod record;
pub mod select;
pub mod subst;
pub mod subtract;
pub mod ty_args;
pub mod unify;
pub mod var_usage;

use std::fmt;
use std::ops::Range;

use arret_syntax::datum::DataStr;
use arret_syntax::span::Span;

use crate::id_type::ArcId;

#[derive(PartialEq, Debug, Clone)]
pub struct TVar {
    span: Span,
    source_name: DataStr,
    bound: Ref<Poly>,
}

pub type TVarId = ArcId<TVar>;
pub type TVars = Vec<TVarId>;

impl TVar {
    pub fn new(span: Span, source_name: DataStr, bound: Ref<Poly>) -> TVarId {
        TVarId::new(TVar {
            span,
            source_name,
            bound,
        })
    }

    pub fn span(&self) -> Span {
        self.span
    }

    pub fn source_name(&self) -> &str {
        &self.source_name
    }

    pub fn bound(&self) -> &Ref<Poly> {
        &self.bound
    }
}

/// Marker that determines if type variables are allowed within a type
pub trait Pm: PartialEq + Clone + Copy + Sized + fmt::Debug {
    /// Resolves a possibly variable type to its bound
    fn resolve_ref_to_ty(ty_ref: &Ref<Self>) -> &Ty<Self>;
}

#[derive(Clone, Copy, PartialEq, Debug)]
pub enum Mono {}
impl Pm for Mono {
    fn resolve_ref_to_ty(ty_ref: &Ref<Mono>) -> &Ty<Mono> {
        match ty_ref {
            Ref::Fixed(ty) => ty,
            Ref::Var(_, _) => unreachable!(),
        }
    }
}

#[derive(Clone, Copy, PartialEq, Debug)]
pub struct Poly {}
impl Pm for Poly {
    fn resolve_ref_to_ty(ty_ref: &Ref<Poly>) -> &Ty<Poly> {
        match ty_ref {
            Ref::Fixed(ty) => ty,
            Ref::Var(tvar, _) => Self::resolve_ref_to_ty(tvar.bound()),
        }
    }
}

#[derive(PartialEq, Debug, Clone)]
pub enum Ref<M: Pm> {
    Var(TVarId, M),
    Fixed(Ty<M>),
}

impl<M: Pm> Ref<M> {
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

    pub fn resolve_to_ty(&self) -> &Ty<M> {
        M::resolve_ref_to_ty(self)
    }

    pub fn is_never(&self) -> bool {
        self.try_to_fixed().map_or(false, |fixed| fixed.is_never())
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

impl<M: Pm> From<Ty<M>> for Ref<M> {
    fn from(ty: Ty<M>) -> Self {
        Ref::Fixed(ty)
    }
}

impl From<TVarId> for Ref<Poly> {
    fn from(tvar: TVarId) -> Self {
        Ref::Var(tvar, Poly {})
    }
}

#[derive(PartialEq, Debug, Clone)]
pub enum Ty<M: Pm> {
    Any,
    Bool,
    Char,
    Float,
    Map(Box<Map<M>>),
    Int,
    Num,
    LitBool(bool),
    LitSym(DataStr),
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

    // Record types
    TopRecord,
    RecordClass(record::ConsId),
    Record(Box<record::Instance<M>>),
}

impl<M: Pm> Ty<M> {
    /// Returns the canonical unit type
    pub fn unit() -> Ty<M> {
        List::empty().into()
    }

    /// Returns the canonical never type
    pub fn never() -> Ty<M> {
        Ty::Union(Box::new([]))
    }

    /// Returns if this is the never type
    pub fn is_never(&self) -> bool {
        self == &Ty::Union(Box::new([]))
    }
}

#[derive(PartialEq, Debug, Clone)]
pub struct Map<M: Pm> {
    key: Ref<M>,
    value: Ref<M>,
}

impl<M: Pm> Map<M> {
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

impl<M: Pm> From<Map<M>> for Ty<M> {
    fn from(map: Map<M>) -> Self {
        Ty::Map(Box::new(map))
    }
}

impl<M: Pm> From<Map<M>> for Ref<M> {
    fn from(map: Map<M>) -> Self {
        Ref::Fixed(Ty::Map(Box::new(map)))
    }
}

#[derive(PartialEq, Debug, Clone)]
pub struct List<M: Pm> {
    fixed: Box<[Ref<M>]>,
    rest: Box<Ref<M>>,
}

impl<M: Pm> List<M> {
    /// Creates a list with the given fixed member types and a uniform tail member type
    pub fn new(fixed: Box<[Ref<M>]>, rest: Ref<M>) -> List<M> {
        List {
            fixed,
            rest: Box::new(rest),
        }
    }

    /// Creates a list of zero or more members with a uniform type
    pub fn new_uniform(rest: Ref<M>) -> List<M> {
        List {
            fixed: Box::new([]),
            rest: Box::new(rest),
        }
    }

    /// Creates a fixed sized list with the given member types
    pub fn new_tuple(fixed: Box<[Ref<M>]>) -> List<M> {
        List {
            fixed,
            rest: Box::new(Ty::never().into()),
        }
    }

    /// Creates an empty list
    pub fn empty() -> List<M> {
        List::new_tuple(Box::new([]))
    }

    pub fn fixed(&self) -> &[Ref<M>] {
        &self.fixed
    }

    /// Returns the member type of our uniform tail
    ///
    /// This will be [`Ty::never()`] if the list has no tail.
    pub fn rest(&self) -> &Ref<M> {
        self.rest.as_ref()
    }

    pub fn size_range(&self) -> Range<usize> {
        if self.rest.is_never() {
            self.fixed.len()..self.fixed.len()
        } else {
            self.fixed.len()..usize::max_value()
        }
    }

    pub fn has_disjoint_arity(&self, other: &Self) -> bool {
        let range1 = self.size_range();
        let range2 = other.size_range();

        range2.start > range1.end || range2.end < range1.start
    }

    /// Return true is the list is empty
    pub fn is_empty(&self) -> bool {
        self.fixed.is_empty() && self.rest.is_never()
    }

    /// Returns true if the list has a uniform tail of zero or more members
    pub fn has_rest(&self) -> bool {
        !self.rest.is_never()
    }
}

impl<M: Pm> From<List<M>> for Ty<M> {
    fn from(list: List<M>) -> Self {
        Ty::List(list)
    }
}

impl<M: Pm> From<List<M>> for Ref<M> {
    fn from(list: List<M>) -> Self {
        Ref::Fixed(Ty::List(list))
    }
}

#[derive(PartialEq, Debug, Clone)]
pub struct TopFun {
    purity: purity::Ref,
    ret: Ref<Poly>,
}

impl TopFun {
    /// Returns a top function type
    pub fn new(purity: purity::Ref, ret: Ref<Poly>) -> TopFun {
        TopFun { purity, ret }
    }

    /// Returns the `Fun` top type for all predicate functions
    pub fn new_for_pred() -> TopFun {
        Self::new(purity::Purity::Pure.into(), Ty::Bool.into())
    }

    pub fn purity(&self) -> &purity::Ref {
        &self.purity
    }

    pub fn ret(&self) -> &Ref<Poly> {
        &self.ret
    }
}

impl<M: Pm> From<TopFun> for Ty<M> {
    fn from(top_fun: TopFun) -> Self {
        Ty::TopFun(Box::new(top_fun))
    }
}

impl<M: Pm> From<TopFun> for Ref<M> {
    fn from(top_fun: TopFun) -> Self {
        Ref::Fixed(Ty::TopFun(Box::new(top_fun)))
    }
}

#[derive(PartialEq, Debug, Clone)]
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

    /// Creates a new function without polymorphic variables
    pub fn new_mono(params: List<Poly>, purity: purity::Ref, ret: Ref<Poly>) -> Fun {
        Fun {
            pvars: purity::PVars::new(),
            tvars: TVars::new(),
            top_fun: TopFun::new(purity, ret),
            params,
        }
    }

    /// Returns the `Fun` type for the `(main!)` function
    pub fn new_for_main() -> Fun {
        Self::new_mono(
            List::empty(),
            purity::Purity::Impure.into(),
            Ty::unit().into(),
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
            List::new_tuple(Box::new([Ty::Any.into()])),
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
            List::new_tuple(Box::new([Ty::Any.into(), Ty::Any.into()])),
        )
    }

    pub fn pvars(&self) -> &[purity::PVarId] {
        &self.pvars
    }

    pub fn tvars(&self) -> &[TVarId] {
        &self.tvars
    }

    pub fn top_fun(&self) -> &TopFun {
        &self.top_fun
    }

    pub fn purity(&self) -> &purity::Ref {
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

impl<M: Pm> From<Fun> for Ty<M> {
    fn from(fun: Fun) -> Self {
        Ty::Fun(Box::new(fun))
    }
}

impl<M: Pm> From<Fun> for Ref<M> {
    fn from(fun: Fun) -> Self {
        Ref::Fixed(Ty::Fun(Box::new(fun)))
    }
}
