use std::collections::HashMap;

use hir::destruc;
use ty;
use typeck::list_type;
use typeck::list_type::{FromListMembers, ListTypeIterator};

pub fn type_for_list_destruc<R, I>(
    tvars: &[ty::TVar],
    list: &destruc::List,
    mut guide_type_iter: Option<I>,
) -> R
where
    R: FromListMembers,
    I: ListTypeIterator,
{
    let mut fixed_polys = vec![];

    for fixed_destruc in list.fixed() {
        let guide_type = if let Some(guide_type_iter) = guide_type_iter.as_mut() {
            guide_type_iter.next().unwrap_or(None)
        } else {
            None
        };

        fixed_polys.push(type_for_destruc(tvars, fixed_destruc, guide_type));
    }

    let rest_poly = match list.rest() {
        Some(rest) => Some(
            rest.ty()
                .try_to_poly()
                .or_else(|| {
                    guide_type_iter.and_then(|guide_type_iter| {
                        guide_type_iter.collect_rest(tvars).unwrap_or(None)
                    })
                })
                .unwrap_or_else(|| ty::Ty::Any.into_poly()),
        ),
        None => None,
    };

    R::from_list_members(fixed_polys.into_iter(), rest_poly)
}

/// Returns the required type for a destruc
pub fn type_for_destruc(
    tvars: &[ty::TVar],
    destruc: &destruc::Destruc,
    guide_type: Option<&ty::Poly>,
) -> ty::Poly {
    match *destruc {
        destruc::Destruc::Scalar(_, ref scalar) => scalar
            .ty()
            .try_to_poly()
            .or_else(|| guide_type.cloned())
            .unwrap_or_else(|| ty::Ty::Any.into_poly()),

        destruc::Destruc::List(_, ref list) => {
            let guide_type_iter =
                guide_type.map(|guide_type| list_type::PolyIterator::new(guide_type));

            type_for_list_destruc(tvars, list, guide_type_iter)
        }
    }
}

pub fn subst_list_destruc<R>(
    list: &destruc::List,
    free_ty_to_poly: &HashMap<ty::FreeTyId, ty::Poly>,
) -> R
where
    R: FromListMembers,
{
    let fixed_polys = list.fixed()
        .iter()
        .map(|fixed_destruc| subst_destruc(fixed_destruc, free_ty_to_poly));

    let rest_poly = list.rest()
        .as_ref()
        .map(|rest_destruc| ty::subst::subst_decl_ty(free_ty_to_poly, rest_destruc.ty()));

    R::from_list_members(fixed_polys, rest_poly)
}

/// Returns the required type for a destruc
pub fn subst_destruc(
    destruc: &destruc::Destruc,
    free_ty_to_poly: &HashMap<ty::FreeTyId, ty::Poly>,
) -> ty::Poly {
    match *destruc {
        destruc::Destruc::Scalar(_, ref scalar) => {
            ty::subst::subst_decl_ty(free_ty_to_poly, scalar.ty())
        }
        destruc::Destruc::List(_, ref list) => subst_list_destruc(list, free_ty_to_poly),
    }
}
