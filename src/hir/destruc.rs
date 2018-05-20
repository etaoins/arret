use std::collections::HashMap;

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
    source_name: String,
    ty: T,
}

impl<T> Scalar<T>
where
    T: HirType,
{
    pub fn new(var_id: Option<hir::VarId>, source_name: String, ty: T) -> Scalar<T> {
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

    pub fn into_source_name(self) -> String {
        self.source_name
    }
}

pub fn subst_list_destruc(
    list: List<ty::Decl>,
    free_ty_to_poly: &HashMap<ty::FreeTyId, ty::Poly>,
) -> List<ty::Poly> {
    let fixed = list.fixed
        .into_iter()
        .map(|fixed_destruc| subst_destruc(fixed_destruc, free_ty_to_poly))
        .collect();

    let rest = list.rest
        .map(|rest_destruc| Box::new(subst_scalar_destruc(*rest_destruc, free_ty_to_poly)));

    List::new(fixed, rest)
}

pub fn subst_scalar_destruc(
    scalar: Scalar<ty::Decl>,
    free_ty_to_poly: &HashMap<ty::FreeTyId, ty::Poly>,
) -> Scalar<ty::Poly> {
    let var_id = *scalar.var_id();
    let poly_type = ty::subst::subst_decl_ty(free_ty_to_poly, scalar.ty());

    Scalar::new(var_id, scalar.into_source_name(), poly_type)
}

pub fn subst_destruc(
    destruc: Destruc<ty::Decl>,
    free_ty_to_poly: &HashMap<ty::FreeTyId, ty::Poly>,
) -> Destruc<ty::Poly> {
    match destruc {
        Destruc::Scalar(span, scalar) => {
            Destruc::Scalar(span, subst_scalar_destruc(scalar, free_ty_to_poly))
        }
        Destruc::List(span, list) => Destruc::List(span, subst_list_destruc(list, free_ty_to_poly)),
    }
}
