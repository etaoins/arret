use hir;
use hir::HirType;
use syntax::span::Span;
use ty;

#[derive(Debug, PartialEq)]
pub enum Destruc<T>
where
    T: HirType,
{
    Scalar(Span, Scalar<T>),
    List(Span, List<T>),
}

#[derive(Debug, PartialEq)]
pub struct List<T>
where
    T: HirType,
{
    fixed: Vec<Destruc<T>>,
    rest: Option<Box<Scalar<T>>>,
}

impl<T> List<T>
where
    T: HirType,
{
    pub fn new(fixed: Vec<Destruc<T>>, rest: Option<Box<Scalar<T>>>) -> List<T> {
        List { fixed, rest }
    }

    pub fn fixed(&self) -> &Vec<Destruc<T>> {
        &self.fixed
    }

    pub fn rest(&self) -> &Option<Box<Scalar<T>>> {
        &self.rest
    }
}

#[derive(Debug, PartialEq)]
pub struct Scalar<T>
where
    T: HirType,
{
    /// ID of the variable. If this is None it's treated as a wildcard.
    var_id: Option<hir::VarId>,
    source_name: Box<str>,
    ty: T,
}

impl<T> Scalar<T>
where
    T: HirType,
{
    pub fn new(var_id: Option<hir::VarId>, source_name: Box<str>, ty: T) -> Scalar<T> {
        Scalar {
            var_id,
            source_name,
            ty,
        }
    }

    pub fn var_id(&self) -> &Option<hir::VarId> {
        &self.var_id
    }

    pub fn ty(&self) -> &T {
        &self.ty
    }

    pub fn into_source_name(self) -> Box<str> {
        self.source_name
    }
}

pub fn subst_list_destruc(
    free_ty_polys: &mut impl Iterator<Item = ty::Poly>,
    list: List<ty::Decl>,
) -> List<ty::Poly> {
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
    scalar: Scalar<ty::Decl>,
) -> Scalar<ty::Poly> {
    let var_id = *scalar.var_id();
    let poly_type = match scalar.ty() {
        ty::Decl::Known(poly) => poly.clone(),
        ty::Decl::Free => free_ty_polys.next().unwrap(),
    };

    Scalar::new(var_id, scalar.into_source_name(), poly_type)
}

/// Substitutes free types with their inferred types
///
/// `free_ty_polys` must be ordered in the same way the types appear in the destruc type in
/// depth-first order
pub fn subst_destruc(
    free_ty_polys: &mut impl Iterator<Item = ty::Poly>,
    destruc: Destruc<ty::Decl>,
) -> Destruc<ty::Poly> {
    match destruc {
        Destruc::Scalar(span, scalar) => {
            Destruc::Scalar(span, subst_scalar_destruc(free_ty_polys, scalar))
        }
        Destruc::List(span, list) => Destruc::List(span, subst_list_destruc(free_ty_polys, list)),
    }
}

pub fn poly_for_list_destruc(list: &List<ty::Poly>) -> ty::List<ty::Poly> {
    let fixed_polys = list
        .fixed()
        .iter()
        .map(|fixed_destruc| poly_for_destruc(fixed_destruc))
        .collect::<Vec<ty::Poly>>()
        .into_boxed_slice();

    let rest_poly = match list.rest() {
        Some(rest) => Some(rest.ty().clone()),
        None => None,
    };

    ty::List::new(fixed_polys, rest_poly)
}

pub fn poly_for_destruc(destruc: &Destruc<ty::Poly>) -> ty::Poly {
    match destruc {
        Destruc::Scalar(_, scalar) => scalar.ty().clone(),
        Destruc::List(_, list) => ty::Ty::List(poly_for_list_destruc(list)).into_poly(),
    }
}
