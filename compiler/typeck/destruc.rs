use crate::hir;
use crate::hir::destruc;
use crate::ty;
use crate::ty::list_iter::ListIterator;

pub fn type_for_decl_list_destruc(
    list: &destruc::List<hir::Lowered>,
    mut guide_type_iter: Option<ListIterator<'_, ty::Poly>>,
) -> ty::List<ty::Poly> {
    let mut fixed_polys = vec![];

    for fixed_destruc in list.fixed() {
        let guide_type = if let Some(guide_type_iter) = guide_type_iter.as_mut() {
            guide_type_iter.next()
        } else {
            None
        };

        fixed_polys.push(type_for_decl_destruc(fixed_destruc, guide_type));
    }

    let rest_poly = match list.rest() {
        Some(rest) => match rest.ty() {
            hir::DeclTy::Known(poly) => poly.clone(),
            hir::DeclTy::Free => guide_type_iter
                .map(ListIterator::collect_rest)
                .unwrap_or_else(|| ty::Ty::Any.into()),
        },
        None => ty::Ty::never().into(),
    };

    ty::List::new(fixed_polys.into_boxed_slice(), rest_poly)
}

/// Returns the required type for a destruc
pub fn type_for_decl_destruc(
    destruc: &destruc::Destruc<hir::Lowered>,
    guide_type: Option<&ty::Ref<ty::Poly>>,
) -> ty::Ref<ty::Poly> {
    match destruc {
        destruc::Destruc::Scalar(_, scalar) => match scalar.ty() {
            hir::DeclTy::Known(poly) => poly.clone(),
            hir::DeclTy::Free => guide_type.cloned().unwrap_or_else(|| ty::Ty::Any.into()),
        },

        destruc::Destruc::List(_, list) => {
            let guide_type_iter =
                guide_type.and_then(|guide_type| ListIterator::try_new_from_ty_ref(guide_type));

            type_for_decl_list_destruc(list, guide_type_iter).into()
        }
    }
}

fn visit_scalar_vars<F>(scalar: &destruc::Scalar<hir::Lowered>, visitor: &mut F)
where
    F: FnMut(hir::VarId, &hir::DeclTy) -> (),
{
    if let Some(var_id) = scalar.var_id() {
        visitor(*var_id, scalar.ty());
    }
}

/// Visits the variables in the passed destruc with the given visitor function
///
/// If the root destruc is scalar its VarId will be returned, otherwise None
pub fn visit_vars<F>(
    destruc: &destruc::Destruc<hir::Lowered>,
    visitor: &mut F,
) -> Option<hir::VarId>
where
    F: FnMut(hir::VarId, &hir::DeclTy) -> (),
{
    match destruc {
        destruc::Destruc::Scalar(_, ref scalar) => {
            visit_scalar_vars(scalar, visitor);
            *scalar.var_id()
        }
        destruc::Destruc::List(_, ref list) => {
            for fixed in list.fixed() {
                visit_vars(fixed, visitor);
            }

            if let Some(rest) = list.rest() {
                visit_scalar_vars(rest, visitor);
            }

            None
        }
    }
}
