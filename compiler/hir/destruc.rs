use crate::hir;
use crate::ty;
use syntax::span::Span;

#[derive(Debug, PartialEq, Clone)]
pub enum Destruc<P: hir::Phase> {
    Scalar(Span, Scalar<P>),
    List(Span, List<P>),
}

#[derive(Debug, PartialEq, Clone)]
pub struct List<P: hir::Phase> {
    fixed: Vec<Destruc<P>>,
    rest: Option<Box<Scalar<P>>>,
}

impl<P: hir::Phase> List<P> {
    pub fn new(fixed: Vec<Destruc<P>>, rest: Option<Box<Scalar<P>>>) -> List<P> {
        List { fixed, rest }
    }

    pub fn fixed(&self) -> &Vec<Destruc<P>> {
        &self.fixed
    }

    pub fn rest(&self) -> &Option<Box<Scalar<P>>> {
        &self.rest
    }
}

#[derive(Debug, PartialEq, Clone)]
pub struct Scalar<P: hir::Phase> {
    /// ID of the variable. If this is None it's treated as a wildcard.
    var_id: Option<hir::VarId>,
    source_name: Box<str>,
    ty: P::DeclType,
}

impl<P: hir::Phase> Scalar<P> {
    pub fn new(var_id: Option<hir::VarId>, source_name: Box<str>, ty: P::DeclType) -> Scalar<P> {
        Scalar {
            var_id,
            source_name,
            ty,
        }
    }

    pub fn var_id(&self) -> &Option<hir::VarId> {
        &self.var_id
    }

    pub fn source_name(&self) -> &str {
        &self.source_name
    }

    pub fn ty(&self) -> &P::DeclType {
        &self.ty
    }
}

pub fn subst_list_destruc(
    free_ty_polys: &mut impl Iterator<Item = ty::Poly>,
    list: List<hir::Lowered>,
) -> List<hir::Inferred> {
    let fixed = list
        .fixed
        .into_iter()
        .map(|fixed_destruc| subst_destruc(free_ty_polys, fixed_destruc))
        .collect();

    let rest = list
        .rest
        .map(|rest_destruc| Box::new(subst_scalar_destruc(free_ty_polys, *rest_destruc)));

    List::new(fixed, rest)
}

pub fn subst_scalar_destruc(
    free_ty_polys: &mut impl Iterator<Item = ty::Poly>,
    scalar: Scalar<hir::Lowered>,
) -> Scalar<hir::Inferred> {
    let Scalar {
        var_id,
        ty,
        source_name,
    } = scalar;

    let poly_type = match ty {
        ty::Decl::Known(poly) => poly,
        ty::Decl::Free => free_ty_polys.next().unwrap(),
    };

    Scalar::new(var_id, source_name, poly_type)
}

/// Substitutes free types with their inferred types
///
/// `free_ty_polys` must be ordered in the same way the types appear in the destruc type in
/// depth-first order
pub fn subst_destruc(
    free_ty_polys: &mut impl Iterator<Item = ty::Poly>,
    destruc: Destruc<hir::Lowered>,
) -> Destruc<hir::Inferred> {
    match destruc {
        Destruc::Scalar(span, scalar) => {
            Destruc::Scalar(span, subst_scalar_destruc(free_ty_polys, scalar))
        }
        Destruc::List(span, list) => Destruc::List(span, subst_list_destruc(free_ty_polys, list)),
    }
}

pub fn poly_for_list_destruc(list: &List<hir::Inferred>) -> ty::List<ty::Poly> {
    let fixed_polys = list.fixed().iter().map(poly_for_destruc).collect();

    let rest_poly = match list.rest() {
        Some(rest) => Some(rest.ty().clone()),
        None => None,
    };

    ty::List::new(fixed_polys, rest_poly)
}

pub fn poly_for_destruc(destruc: &Destruc<hir::Inferred>) -> ty::Poly {
    match destruc {
        Destruc::Scalar(_, scalar) => scalar.ty().clone(),
        Destruc::List(_, list) => ty::Ty::List(poly_for_list_destruc(list)).into_poly(),
    }
}
