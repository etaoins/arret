use crate::hir;
use crate::hir::destruc;
use crate::ty;
use crate::ty::list_iter::ListIterator;
use crate::ty::Ty;

pub fn type_for_decl_list_destruc(
    list: &destruc::List<hir::Lowered>,
    mut guide_type_iter: Option<ListIterator<'_, ty::Poly>>,
) -> ty::List<ty::Poly> {
    let fixed_polys = list
        .fixed()
        .iter()
        .map(|fixed_destruc| {
            let guide_type = if let Some(guide_type_iter) = guide_type_iter.as_mut() {
                guide_type_iter.next()
            } else {
                None
            };

            type_for_decl_destruc(fixed_destruc, guide_type)
        })
        .collect();

    let rest_poly = match list.rest() {
        Some(rest) => match rest.ty() {
            hir::DeclTy::Known(poly) => poly.clone(),
            hir::DeclTy::Free => guide_type_iter
                .map(ListIterator::collect_rest)
                .unwrap_or_else(|| Ty::Any.into()),
        },
        None => Ty::never().into(),
    };

    ty::List::new(fixed_polys, rest_poly)
}

/// Returns the required type for a destruc
pub fn type_for_decl_destruc(
    destruc: &destruc::Destruc<hir::Lowered>,
    guide_type: Option<&ty::Ref<ty::Poly>>,
) -> ty::Ref<ty::Poly> {
    match destruc {
        destruc::Destruc::Scalar(_, scalar) => match scalar.ty() {
            hir::DeclTy::Known(poly) => poly.clone(),
            hir::DeclTy::Free => guide_type.cloned().unwrap_or_else(|| Ty::Any.into()),
        },

        destruc::Destruc::List(_, list) => {
            let guide_type_iter =
                guide_type.and_then(|guide_type| ListIterator::try_new_from_ty_ref(guide_type));

            type_for_decl_list_destruc(list, guide_type_iter).into()
        }
    }
}

fn visit_scalar_locals<F>(scalar: &destruc::Scalar<hir::Lowered>, visitor: &mut F)
where
    F: FnMut(hir::LocalId, &hir::DeclTy) -> (),
{
    if let Some(local_id) = scalar.local_id() {
        visitor(*local_id, scalar.ty());
    }
}

/// Visits the local variables in the passed destruc with the given visitor function
///
/// If the root destruc is scalar its VarId will be returned, otherwise None
pub fn visit_locals<F>(
    destruc: &destruc::Destruc<hir::Lowered>,
    visitor: &mut F,
) -> Option<hir::LocalId>
where
    F: FnMut(hir::LocalId, &hir::DeclTy) -> (),
{
    match destruc {
        destruc::Destruc::Scalar(_, ref scalar) => {
            visit_scalar_locals(scalar, visitor);
            *scalar.local_id()
        }
        destruc::Destruc::List(_, ref list) => {
            for fixed in list.fixed() {
                visit_locals(fixed, visitor);
            }

            if let Some(rest) = list.rest() {
                visit_scalar_locals(rest, visitor);
            }

            None
        }
    }
}
