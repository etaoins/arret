use std::ops::Range;

use ty;
use ty::list_iter::ListIterator;
use ty::purity::Purity;

/// Selects a set of polymorphic variables for a function application
///
/// This context is constructed with a set of purity and type variables the applied function is
/// polymorphic on. Evidence from the return and argument types can then be incrementally added to
/// the context. The calculated polymorphic types and purities can be retrieved from the
/// `pvar_purities` and `tvar_types` methods.
#[derive(Clone, Debug)]
pub struct SelectContext<'a> {
    pvar_ids: Range<ty::purity::PVarId>,
    tvar_ids: Range<ty::TVarId>,

    tvars: &'a [ty::TVar],

    pvar_purities: Vec<Option<ty::purity::Poly>>,
    tvar_types: Vec<Option<ty::Poly>>,
}

impl<'a> SelectContext<'a> {
    pub fn new(
        pvar_ids: Range<ty::purity::PVarId>,
        tvar_ids: Range<ty::TVarId>,
        tvars: &[ty::TVar],
    ) -> SelectContext {
        let pvar_ids_usize = pvar_ids.start.to_usize()..pvar_ids.end.to_usize();
        let tvar_ids_usize = tvar_ids.start.to_usize()..tvar_ids.end.to_usize();

        let pvar_purities = vec![None; pvar_ids_usize.len()];
        let tvar_types = vec![None; tvar_ids_usize.len()];

        SelectContext {
            pvar_ids,
            tvar_ids,
            tvars,
            pvar_purities,
            tvar_types,
        }
    }

    pub fn selected_pvar_idx(&self, pvar_id: ty::purity::PVarId) -> Option<usize> {
        if pvar_id >= self.pvar_ids.start && pvar_id < self.pvar_ids.end {
            Some(pvar_id.to_usize() - self.pvar_ids.start.to_usize())
        } else {
            None
        }
    }

    pub fn selected_tvar_idx(&self, tvar_id: ty::TVarId) -> Option<usize> {
        if tvar_id >= self.tvar_ids.start && tvar_id < self.tvar_ids.end {
            Some(tvar_id.to_usize() - self.tvar_ids.start.to_usize())
        } else {
            None
        }
    }

    fn add_evidence_purity(
        &mut self,
        target_purity: &ty::purity::Poly,
        evidence_purity: &ty::purity::Poly,
    ) {
        let pvar_id = if let ty::purity::Poly::Var(pvar_id) = target_purity {
            pvar_id
        } else {
            return;
        };

        let pvar_idx = if let Some(pvar_idx) = self.selected_pvar_idx(*pvar_id) {
            pvar_idx
        } else {
            return;
        };

        self.pvar_purities[pvar_idx] = Some(match self.pvar_purities[pvar_idx] {
            None => evidence_purity.clone(),
            Some(ref existing) => {
                // Much like the type case we default to the "bound" (impure) if we get a poly
                // conflict
                ty::unify::poly_unify_purity(existing, evidence_purity)
                    .unwrap_or_else(|_| Purity::Impure.into_poly())
            }
        });
    }

    fn add_evidence_top_fun(
        &mut self,
        target_top_fun: &ty::TopFun<ty::Poly>,
        evidence_top_fun: &ty::TopFun<ty::Poly>,
    ) {
        self.add_evidence_purity(target_top_fun.purity(), evidence_top_fun.purity());
        self.add_evidence(target_top_fun.ret(), evidence_top_fun.ret());
    }

    fn add_evidence_list(
        &mut self,
        target_list: &ty::List<ty::Poly>,
        evidence_list: &ty::List<ty::Poly>,
    ) {
        let mut target_iter = ListIterator::new(target_list);
        let mut evidence_iter = ListIterator::new(evidence_list);

        while target_iter.fixed_len() > 0 {
            let target_fixed = target_iter.next().unwrap();
            let evidence_fixed = if let Some(evidence_fixed) = evidence_iter.next() {
                evidence_fixed
            } else {
                return;
            };

            self.add_evidence(target_fixed, evidence_fixed);
        }

        if let Some(target_rest) = target_iter.next() {
            if let Ok(Some(evidence_rest)) = evidence_iter.collect_rest(self.tvars) {
                self.add_evidence(target_rest, &evidence_rest);
            }
        }
    }

    fn add_evidence_ty(
        &mut self,
        target_ty: &ty::Ty<ty::Poly>,
        evidence_poly: &ty::Poly,
        evidence_ty: &ty::Ty<ty::Poly>,
    ) {
        match (target_ty, evidence_ty) {
            (ty::Ty::Set(target_member), ty::Ty::Set(evidence_member)) => {
                self.add_evidence(target_member, evidence_member);
            }
            (ty::Ty::Map(target_key, target_value), ty::Ty::Map(evidence_key, evidence_value)) => {
                self.add_evidence(target_key, evidence_key);
                self.add_evidence(target_value, evidence_value);
            }
            (ty::Ty::List(target_list), ty::Ty::List(evidence_list)) => {
                self.add_evidence_list(target_list, evidence_list);
            }
            (ty::Ty::Vec(target_members), ty::Ty::Vec(evidence_members)) => {
                for (target_member, evidence_member) in
                    target_members.iter().zip(evidence_members.iter())
                {
                    self.add_evidence(target_member, evidence_member);
                }
            }
            (ty::Ty::Vecof(target_member), ty::Ty::Vecof(evidence_member)) => {
                self.add_evidence(target_member, evidence_member);
            }
            (ty::Ty::Vecof(target_member), ty::Ty::Vec(evidence_members)) => {
                for evidence_member in evidence_members {
                    self.add_evidence(target_member, evidence_member);
                }
            }
            (ty::Ty::TopFun(target_top_fun), ty::Ty::TopFun(evidence_top_fun)) => {
                self.add_evidence_top_fun(target_top_fun, evidence_top_fun);
            }
            (ty::Ty::TopFun(target_top_fun), ty::Ty::Fun(evidence_fun)) => {
                self.add_evidence_top_fun(target_top_fun, evidence_fun.top_fun());
            }
            (ty::Ty::TopFun(target_top_fun), ty::Ty::TyPred(_)) => {
                self.add_evidence_top_fun(target_top_fun, &ty::TopFun::new_for_ty_pred());
            }
            (ty::Ty::Fun(target_fun), ty::Ty::Fun(evidence_fun)) => {
                self.add_evidence_top_fun(target_fun.top_fun(), evidence_fun.top_fun());
            }
            (ty::Ty::Fun(target_fun), ty::Ty::TyPred(_)) => {
                self.add_evidence_top_fun(target_fun.top_fun(), &ty::TopFun::new_for_ty_pred());
            }
            (ty::Ty::TyPred(target_test), ty::Ty::TyPred(evidence_test)) => {
                self.add_evidence(target_test, evidence_test);
            }
            (ty::Ty::Union(target_members), _) => {
                for target_member in target_members {
                    self.add_evidence(target_member, evidence_poly);
                }
            }
            _ => {}
        }
    }

    fn add_var_evidence(&mut self, tvar_id: ty::TVarId, evidence_poly: &ty::Poly) {
        let tvar_idx = if let Some(tvar_idx) = self.selected_tvar_idx(tvar_id) {
            // We're selecting on this
            tvar_idx
        } else {
            return;
        };

        let var_bound = self.tvars[tvar_id.to_usize()].bound();
        if !ty::is_a::poly_is_a(self.tvars, evidence_poly, var_bound).to_bool() {
            return;
        }

        self.tvar_types[tvar_idx] = Some(match self.tvar_types[tvar_idx] {
            None => evidence_poly.clone(),
            Some(ref existing) => {
                match ty::unify::poly_unify_to_poly(self.tvars, existing, evidence_poly) {
                    Ok(unified) => unified,
                    Err(_) => {
                        // We tried to build a union type with conflicting poly members. It isn't
                        // the caller's fault we tried to build a union we can't represent;
                        // silently fall back to the bound.
                        var_bound.clone()
                    }
                }
            }
        });
    }

    pub fn add_evidence(&mut self, target_poly: &ty::Poly, evidence_poly: &ty::Poly) {
        match target_poly {
            ty::Poly::Var(tvar_id) => self.add_var_evidence(*tvar_id, evidence_poly),
            ty::Poly::Fixed(target_ty) => {
                let evidence_ty = ty::resolve::resolve_poly_ty(self.tvars, evidence_poly).as_ty();
                self.add_evidence_ty(target_ty, evidence_poly, evidence_ty)
            }
        }
    }

    pub fn tvars(&self) -> &[ty::TVar] {
        &self.tvars
    }

    pub fn pvar_purities(&self) -> &[Option<ty::purity::Poly>] {
        self.pvar_purities.as_slice()
    }

    pub fn tvar_types(&self) -> &[Option<ty::Poly>] {
        self.tvar_types.as_slice()
    }
}

#[cfg(test)]
mod test {
    use super::*;
    use hir::poly_for_str;
    use ty::purity::Purity;

    fn add_str_evidence(ctx: &mut SelectContext, target_str: &str, evidence_str: &str) {
        ctx.add_evidence(
            &poly_for_str(target_str).unwrap(),
            &poly_for_str(evidence_str).unwrap(),
        );
    }

    fn assert_unselected_type(ctx: &SelectContext, name: char) {
        let idx = (name as usize) - ('A' as usize);
        assert_eq!(&ctx.tvar_types()[idx], &None);
    }

    fn assert_selected_type(ctx: &SelectContext, name: char, selected_str: &str) {
        let idx = (name as usize) - ('A' as usize);
        assert_eq!(
            &Some(poly_for_str(selected_str).unwrap()),
            &ctx.tvar_types()[idx]
        );
    }

    fn assert_unselected_purity(ctx: &SelectContext, name: char) {
        let idx = (name as usize) - ('A' as usize);
        assert_eq!(&None, &ctx.pvar_purities()[idx]);
    }

    fn assert_selected_purity(ctx: &SelectContext, name: char, selected_purity: Purity) {
        let idx = (name as usize) - ('A' as usize);
        assert_eq!(
            &Some(ty::purity::Poly::Fixed(selected_purity)),
            &ctx.pvar_purities()[idx]
        );
    }

    #[test]
    fn trivial_tvar() {
        let pvar_ids = ty::purity::PVarIds::monomorphic();
        let tvars = [ty::TVar::new("A".to_owned(), ty::Ty::Any.into_poly())];
        let tvar_ids = ty::TVarId::new(0)..ty::TVarId::new(tvars.len());

        let mut ctx = SelectContext::new(pvar_ids, tvar_ids, &tvars);
        assert_unselected_type(&ctx, 'A');

        add_str_evidence(&mut ctx, "A", "true");
        assert_selected_type(&ctx, 'A', "true");

        add_str_evidence(&mut ctx, "A", "false");
        assert_selected_type(&ctx, 'A', "Bool");
    }

    #[test]
    fn poly_conflicing_tvar() {
        let pvar_ids = ty::purity::PVarIds::monomorphic();
        let tvars = [
            ty::TVar::new("A".to_owned(), poly_for_str("(... -> Any)").unwrap()),
            ty::TVar::new("B".to_owned(), poly_for_str("(... -> Symbol)").unwrap()),
            ty::TVar::new("C".to_owned(), poly_for_str("(... -> 'foo)").unwrap()),
        ];
        let tvar_ids = ty::TVarId::new(0)..ty::TVarId::new(tvars.len());

        let mut ctx = SelectContext::new(pvar_ids, tvar_ids, &tvars);
        assert_unselected_type(&ctx, 'A');

        // We can handle one tvar as evidence
        add_str_evidence(&mut ctx, "A", "B");
        assert_selected_type(&ctx, 'A', "B");

        // However, these cannot be distinguished so need to fall back to our bound
        add_str_evidence(&mut ctx, "A", "C");
        assert_selected_type(&ctx, 'A', "(... -> Any)");
    }

    #[test]
    fn set_types() {
        let pvar_ids = ty::purity::PVarIds::monomorphic();
        let tvars = [ty::TVar::new("A".to_owned(), ty::Ty::Any.into_poly())];
        let tvar_ids = ty::TVarId::new(0)..ty::TVarId::new(tvars.len());

        let mut ctx = SelectContext::new(pvar_ids, tvar_ids, &tvars);

        add_str_evidence(&mut ctx, "(Setof A)", "(Setof Bool)");
        assert_selected_type(&ctx, 'A', "Bool");
    }

    #[test]
    fn map_types() {
        let pvar_ids = ty::purity::PVarIds::monomorphic();
        let tvars = [
            ty::TVar::new("A".to_owned(), ty::Ty::Any.into_poly()),
            ty::TVar::new("B".to_owned(), ty::Ty::Any.into_poly()),
        ];
        let tvar_ids = ty::TVarId::new(0)..ty::TVarId::new(tvars.len());

        let mut ctx = SelectContext::new(pvar_ids, tvar_ids, &tvars);

        add_str_evidence(&mut ctx, "(Map A B)", "(Map true false)");
        assert_selected_type(&ctx, 'A', "true");
        assert_selected_type(&ctx, 'B', "false");
    }

    #[test]
    fn fixed_list_types() {
        let pvar_ids = ty::purity::PVarIds::monomorphic();
        let tvars = [
            ty::TVar::new("A".to_owned(), ty::Ty::Any.into_poly()),
            ty::TVar::new("B".to_owned(), ty::Ty::Any.into_poly()),
        ];
        let tvar_ids = ty::TVarId::new(0)..ty::TVarId::new(tvars.len());

        let mut ctx = SelectContext::new(pvar_ids, tvar_ids, &tvars);

        add_str_evidence(&mut ctx, "(List A B)", "(List true false)");
        assert_selected_type(&ctx, 'A', "true");
        assert_selected_type(&ctx, 'B', "false");
    }

    #[test]
    fn listof_types() {
        let pvar_ids = ty::purity::PVarIds::monomorphic();
        let tvars = [ty::TVar::new("A".to_owned(), ty::Ty::Any.into_poly())];
        let tvar_ids = ty::TVarId::new(0)..ty::TVarId::new(tvars.len());

        let mut ctx = SelectContext::new(pvar_ids, tvar_ids, &tvars);

        add_str_evidence(&mut ctx, "(Listof A)", "(Listof true)");
        assert_selected_type(&ctx, 'A', "true");
    }

    #[test]
    fn listof_from_fixed_list() {
        let pvar_ids = ty::purity::PVarIds::monomorphic();
        let tvars = [ty::TVar::new("A".to_owned(), ty::Ty::Any.into_poly())];
        let tvar_ids = ty::TVarId::new(0)..ty::TVarId::new(tvars.len());

        let mut ctx = SelectContext::new(pvar_ids, tvar_ids, &tvars);

        add_str_evidence(&mut ctx, "(Listof A)", "(List true false)");
        assert_selected_type(&ctx, 'A', "Bool");
    }

    #[test]
    fn fixed_vector_types() {
        let pvar_ids = ty::purity::PVarIds::monomorphic();
        let tvars = [
            ty::TVar::new("A".to_owned(), ty::Ty::Any.into_poly()),
            ty::TVar::new("B".to_owned(), ty::Ty::Any.into_poly()),
        ];
        let tvar_ids = ty::TVarId::new(0)..ty::TVarId::new(tvars.len());

        let mut ctx = SelectContext::new(pvar_ids, tvar_ids, &tvars);

        add_str_evidence(&mut ctx, "(Vector A B)", "(Vector true false)");
        assert_selected_type(&ctx, 'A', "true");
        assert_selected_type(&ctx, 'B', "false");
    }

    #[test]
    fn vectorof_types() {
        let pvar_ids = ty::purity::PVarIds::monomorphic();
        let tvars = [ty::TVar::new("A".to_owned(), ty::Ty::Any.into_poly())];
        let tvar_ids = ty::TVarId::new(0)..ty::TVarId::new(tvars.len());

        let mut ctx = SelectContext::new(pvar_ids, tvar_ids, &tvars);

        add_str_evidence(&mut ctx, "(Vectorof A)", "(Vectorof true)");
        assert_selected_type(&ctx, 'A', "true");
    }

    #[test]
    fn vectorof_from_fixed_vector() {
        let pvar_ids = ty::purity::PVarIds::monomorphic();
        let tvars = [ty::TVar::new("A".to_owned(), ty::Ty::Any.into_poly())];
        let tvar_ids = ty::TVarId::new(0)..ty::TVarId::new(tvars.len());

        let mut ctx = SelectContext::new(pvar_ids, tvar_ids, &tvars);

        add_str_evidence(&mut ctx, "(Vectorof A)", "(Vector true false)");
        assert_selected_type(&ctx, 'A', "Bool");
    }

    #[test]
    fn union_types() {
        let pvar_ids = ty::purity::PVarIds::monomorphic();
        let tvars = [ty::TVar::new("A".to_owned(), ty::Ty::Any.into_poly())];
        let tvar_ids = ty::TVarId::new(0)..ty::TVarId::new(tvars.len());

        let mut ctx = SelectContext::new(pvar_ids, tvar_ids, &tvars);

        add_str_evidence(&mut ctx, "(RawU A Symbol)", "true");
        assert_selected_type(&ctx, 'A', "true");
    }

    #[test]
    fn bounded_union_types() {
        let pvar_ids = ty::purity::PVarIds::monomorphic();
        let tvars = [
            ty::TVar::new("A".to_owned(), ty::Ty::Sym.into_poly()),
            ty::TVar::new("B".to_owned(), ty::Ty::Bool.into_poly()),
        ];
        let tvar_ids = ty::TVarId::new(0)..ty::TVarId::new(tvars.len());

        let mut ctx = SelectContext::new(pvar_ids, tvar_ids, &tvars);

        // A and B are bounded. We should ensure we only use evidence on the members with satisfied
        // bounds.
        add_str_evidence(&mut ctx, "(RawU A B)", "'foo");
        assert_selected_type(&ctx, 'A', "'foo");
        assert_unselected_type(&ctx, 'B');

        add_str_evidence(&mut ctx, "(RawU A B)", "true");
        assert_selected_type(&ctx, 'A', "'foo");
        assert_selected_type(&ctx, 'B', "true");

        add_str_evidence(&mut ctx, "(RawU A B)", "false");
        assert_selected_type(&ctx, 'A', "'foo");
        assert_selected_type(&ctx, 'B', "Bool");
    }

    #[test]
    fn top_fun_types() {
        let pvar_ids = ty::purity::PVarIds::monomorphic();
        let tvars = [ty::TVar::new("A".to_owned(), ty::Ty::Any.into_poly())];
        let tvar_ids = ty::TVarId::new(0)..ty::TVarId::new(tvars.len());

        let mut ctx = SelectContext::new(pvar_ids, tvar_ids, &tvars);

        add_str_evidence(&mut ctx, "(... -> A)", "(... -> true)");
        assert_selected_type(&ctx, 'A', "true");
    }

    #[test]
    fn top_fun_purities() {
        let pvars = [ty::purity::PVar::new("->A".to_owned())];
        let pvar_ids = ty::purity::PVarId::new(0)..ty::purity::PVarId::new(pvars.len());
        let tvar_ids = ty::TVarIds::monomorphic();

        let mut ctx = SelectContext::new(pvar_ids, tvar_ids, &[]);
        assert_unselected_purity(&ctx, 'A');

        add_str_evidence(&mut ctx, "(... ->A true)", "(... -> true)");
        assert_selected_purity(&ctx, 'A', Purity::Pure);

        add_str_evidence(&mut ctx, "(... ->A true)", "(... ->! true)");
        assert_selected_purity(&ctx, 'A', Purity::Impure);
    }

    #[test]
    fn top_fun_from_fun() {
        let pvar_ids = ty::purity::PVarIds::monomorphic();
        let tvars = [ty::TVar::new("A".to_owned(), ty::Ty::Any.into_poly())];
        let tvar_ids = ty::TVarId::new(0)..ty::TVarId::new(tvars.len());

        let mut ctx = SelectContext::new(pvar_ids, tvar_ids, &tvars);

        add_str_evidence(&mut ctx, "(... -> A)", "(false -> true)");
        assert_selected_type(&ctx, 'A', "true");
    }

    #[test]
    fn top_fun_from_ty_pred() {
        let pvar_ids = ty::purity::PVarIds::monomorphic();
        let tvars = [ty::TVar::new("A".to_owned(), ty::Ty::Any.into_poly())];
        let tvar_ids = ty::TVarId::new(0)..ty::TVarId::new(tvars.len());

        let mut ctx = SelectContext::new(pvar_ids, tvar_ids, &tvars);

        add_str_evidence(&mut ctx, "(... -> A)", "(Type? Symbol)");
        assert_selected_type(&ctx, 'A', "Bool");
    }

    #[test]
    fn fun_types() {
        let pvar_ids = ty::purity::PVarIds::monomorphic();
        let tvars = [
            ty::TVar::new("A".to_owned(), ty::Ty::Any.into_poly()),
            ty::TVar::new("B".to_owned(), ty::Ty::Any.into_poly()),
        ];
        let tvar_ids = ty::TVarId::new(0)..ty::TVarId::new(tvars.len());

        let mut ctx = SelectContext::new(pvar_ids, tvar_ids, &tvars);

        add_str_evidence(&mut ctx, "(A -> B)", "(true -> false)");
        // We intentionally do not use function type params as evidence
        assert_unselected_type(&ctx, 'A');
        assert_selected_type(&ctx, 'B', "false");
    }

    #[test]
    fn fun_purities() {
        let pvars = [ty::purity::PVar::new("->A".to_owned())];
        let pvar_ids = ty::purity::PVarId::new(0)..ty::purity::PVarId::new(pvars.len());
        let tvar_ids = ty::TVarIds::monomorphic();

        let mut ctx = SelectContext::new(pvar_ids, tvar_ids, &[]);
        assert_unselected_purity(&ctx, 'A');

        add_str_evidence(&mut ctx, "(->A true)", "(->! true)");
        assert_selected_purity(&ctx, 'A', Purity::Impure);
    }

    #[test]
    fn fun_purity_conflict() {
        let pvars = [ty::purity::PVar::new("->A".to_owned())];
        let pvar_ids = ty::purity::PVarId::new(0)..ty::purity::PVarId::new(pvars.len());
        let tvar_ids = ty::TVarIds::monomorphic();

        let mut ctx = SelectContext::new(pvar_ids, tvar_ids, &[]);
        assert_unselected_purity(&ctx, 'A');

        add_str_evidence(&mut ctx, "(->A true)", "(->B true)");
        add_str_evidence(&mut ctx, "(->A true)", "(->C true)");

        assert_selected_purity(&ctx, 'A', Purity::Impure);
    }

    #[test]
    fn fun_type_from_ty_pred() {
        let pvar_ids = ty::purity::PVarIds::monomorphic();
        let tvars = [
            ty::TVar::new("A".to_owned(), ty::Ty::Any.into_poly()),
            ty::TVar::new("B".to_owned(), ty::Ty::Any.into_poly()),
        ];
        let tvar_ids = ty::TVarId::new(0)..ty::TVarId::new(tvars.len());

        let mut ctx = SelectContext::new(pvar_ids, tvar_ids, &tvars);

        add_str_evidence(&mut ctx, "(A -> B)", "(Type? Symbol)");
        assert_unselected_type(&ctx, 'A');
        assert_selected_type(&ctx, 'B', "Bool");
    }

    #[test]
    fn ty_pred_types() {
        let pvar_ids = ty::purity::PVarIds::monomorphic();
        let tvars = [ty::TVar::new("A".to_owned(), ty::Ty::Any.into_poly())];
        let tvar_ids = ty::TVarId::new(0)..ty::TVarId::new(tvars.len());

        let mut ctx = SelectContext::new(pvar_ids, tvar_ids, &tvars);

        add_str_evidence(&mut ctx, "(Type? A)", "(Type? true)");
        assert_selected_type(&ctx, 'A', "true");
    }

    #[test]
    fn ty_pred_purity() {
        let pvars = [ty::purity::PVar::new("->A".to_owned())];
        let pvar_ids = ty::purity::PVarId::new(0)..ty::purity::PVarId::new(pvars.len());
        let tvar_ids = ty::TVarIds::monomorphic();

        let mut ctx = SelectContext::new(pvar_ids, tvar_ids, &[]);
        assert_unselected_purity(&ctx, 'A');

        add_str_evidence(&mut ctx, "(->A true)", "(Type? Symbol)");
        assert_selected_purity(&ctx, 'A', Purity::Pure);
    }
}
