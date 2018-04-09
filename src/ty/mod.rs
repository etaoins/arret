#[cfg(test)]
pub mod is_a;
#[cfg(test)]
pub mod subst;

use syntax::span::Span;

pub trait TyRef: PartialEq + Clone + Sized {}

#[derive(PartialEq, Eq, Debug, Hash, Clone)]
pub enum Ty<S>
where
    S: TyRef,
{
    Any,
    Bool,
    Sym,
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
}

#[derive(Debug, PartialEq, Eq, Clone, Copy, Hash)]
pub struct PVarId(usize);

impl PVarId {
    pub fn new(id: usize) -> PVarId {
        PVarId(id)
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

impl TyRef for Poly {}

impl Ty<Poly> {
    pub fn into_poly(self) -> Poly {
        Poly::Fixed(self)
    }
}

#[derive(PartialEq, Eq, Debug, Clone)]
pub struct Mono(Ty<Mono>);

impl Mono {
    #[cfg(test)]
    fn as_ty(&self) -> &Ty<Mono> {
        &self.0
    }
}

impl Ty<Mono> {
    #[cfg(test)]
    pub fn into_mono(self) -> Mono {
        Mono(self)
    }
}

impl TyRef for Mono {}

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
