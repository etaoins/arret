use hir;
use hir::destruc;
use ty;
use ty::list_iter::ListIterator;

pub fn type_for_decl_list_destruc(
    tvars: &[ty::TVar],
    list: &destruc::List<ty::Decl>,
    mut guide_type_iter: Option<ListIterator<ty::Poly>>,
) -> ty::List<ty::Poly> {
    let mut fixed_polys = vec![];

    for fixed_destruc in list.fixed() {
        let guide_type = if let Some(guide_type_iter) = guide_type_iter.as_mut() {
            guide_type_iter.next()
        } else {
            None
        };

        fixed_polys.push(type_for_decl_destruc(tvars, fixed_destruc, guide_type));
    }

    let rest_poly = match list.rest() {
        Some(rest) => Some(match rest.ty() {
            ty::Decl::Known(poly) => poly.clone(),
            ty::Decl::Free => guide_type_iter
                .and_then(|guide_type_iter| guide_type_iter.collect_rest(tvars).unwrap_or(None))
                .unwrap_or_else(|| ty::Ty::Any.into_poly()),
        }),
        None => None,
    };

    ty::List::new(fixed_polys.into_boxed_slice(), rest_poly)
}

/// Returns the required type for a destruc
pub fn type_for_decl_destruc(
    tvars: &[ty::TVar],
    destruc: &destruc::Destruc<ty::Decl>,
    guide_type: Option<&ty::Poly>,
) -> ty::Poly {
    match *destruc {
        destruc::Destruc::Scalar(_, ref scalar) => match scalar.ty() {
            ty::Decl::Known(poly) => poly.clone(),
            ty::Decl::Free => guide_type
                .cloned()
                .unwrap_or_else(|| ty::Ty::Any.into_poly()),
        },

        destruc::Destruc::List(_, ref list) => {
            let guide_type_iter =
                guide_type.and_then(|guide_type| ListIterator::try_new_from_ty_ref(guide_type));

            ty::Ty::List(type_for_decl_list_destruc(tvars, list, guide_type_iter)).into_poly()
        }
    }
}

struct VisitVarCtx<F>
where
    F: FnMut(hir::VarId, &ty::Decl) -> (),
{
    visitor: F,
}

impl<F> VisitVarCtx<F>
where
    F: FnMut(hir::VarId, &ty::Decl) -> (),
{
    fn visit_scalar_vars(&mut self, scalar: &destruc::Scalar<ty::Decl>) {
        if let Some(var_id) = scalar.var_id() {
            (self.visitor)(*var_id, scalar.ty());
        }
    }

    fn visit_vars(&mut self, destruc: &destruc::Destruc<ty::Decl>) {
        match destruc {
            destruc::Destruc::Scalar(_, ref scalar) => {
                self.visit_scalar_vars(scalar);
            }
            destruc::Destruc::List(_, ref list) => {
                for fixed in list.fixed() {
                    self.visit_vars(fixed);
                }

                if let Some(rest) = list.rest() {
                    self.visit_scalar_vars(rest);
                }
            }
        }
    }
}

/// Visits the variables in the passed destruc with the given visitor function
///
/// If the root destruc is scalar its VarId will be returned, otherwise None
pub fn visit_vars<F>(destruc: &destruc::Destruc<ty::Decl>, visitor: F) -> Option<hir::VarId>
where
    F: FnMut(hir::VarId, &ty::Decl) -> (),
{
    let mut vcx = VisitVarCtx { visitor };
    vcx.visit_vars(destruc);

    if let destruc::Destruc::Scalar(_, scalar) = destruc {
        *scalar.var_id()
    } else {
        None
    }
}
