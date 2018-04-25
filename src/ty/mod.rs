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
    Fun(Box<Fun<S>>),
    Map(Box<S>, Box<S>),
    Int,
    LitBool(bool),
    LitSym(String),
    Set(Box<S>),
    Str,
    Sym,
    Union(Vec<S>),

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
