pub mod is_a;
pub mod resolve;
#[cfg(test)]
pub mod subst;
pub mod unify;
mod seq_ty_iter;

use syntax::span::Span;

pub trait TyRef: PartialEq + Clone + Sized {
    fn from_ty(Ty<Self>) -> Self;
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
    Fun(Box<Fun<S>>),
    Hash(Box<S>, Box<S>),
    Int,
    List(Vec<S>, Option<Box<S>>),
    LitBool(bool),
    LitSym(String),
    Set(Box<S>),
    Str,
    Sym,
    Union(Vec<S>),
    Vec(Option<Box<S>>, Vec<S>),
}

impl<S> Ty<S>
where
    S: TyRef,
{
    pub fn new_fun(impure: bool, params: S, ret: S) -> Ty<S> {
        Ty::Fun(Box::new(Fun::new(impure, params, ret)))
    }
}

#[derive(PartialEq, Eq, Debug, Hash, Clone)]
pub struct Fun<S> {
    impure: bool,
    params: S,
    ret: S,
}

impl<S> Fun<S> {
    pub fn new(impure: bool, params: S, ret: S) -> Fun<S> {
        Fun {
            impure,
            params,
            ret,
        }
    }

    pub fn impure(&self) -> bool {
        self.impure
    }

    pub fn params(&self) -> &S {
        &self.params
    }

    pub fn ret(&self) -> &S {
        &self.ret
    }
}

#[derive(Debug, PartialEq, Eq, Clone, Copy, Hash)]
pub struct PVarId(usize);

impl PVarId {
    pub fn new(id: usize) -> PVarId {
        PVarId(id)
    }

    pub fn to_usize(self) -> usize {
        self.0
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
    Free(Span),
}

impl Ty<Poly> {
    #[cfg(test)]
    pub fn into_decl(self) -> Decl {
        Decl::Fixed(self)
    }
}
