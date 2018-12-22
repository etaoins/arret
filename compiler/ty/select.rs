use std::collections::HashMap;

use crate::ty;
use crate::ty::list_iter::ListIterator;
use crate::ty::purity;
use crate::ty::purity::Purity;
use crate::ty::ty_args::PolyTyArgs;

/// Selects a set of polymorphic variables for a function application
///
/// This context is constructed with a set of purity and type variables the applied function is
/// polymorphic on. Evidence from the return and argument types can then be incrementally added to
/// the context. The calculated polymorphic types and purities can be retrieved from the
/// `pvar_purities` and `tvar_types` methods.
#[derive(Clone, Debug)]
pub struct SelectCtx<'vars> {
    all_tvars: &'vars ty::TVars,

    selecting_pvars: &'vars purity::PVars,
    selecting_tvars: &'vars ty::TVars,

    pvar_purities: HashMap<purity::PVarId, purity::Poly>,
    tvar_types: HashMap<ty::TVarId, ty::Poly>,
}

impl<'vars> SelectCtx<'vars> {
    pub fn new(
        all_tvars: &'vars ty::TVars,
        selecting_pvars: &'vars purity::PVars,
        selecting_tvars: &'vars ty::TVars,
    ) -> SelectCtx<'vars> {
        SelectCtx {
            all_tvars,
            selecting_pvars,
            selecting_tvars,
            pvar_purities: HashMap::new(),
            tvar_types: HashMap::new(),
        }
    }

    fn add_evidence_purity(
        &mut self,
        target_purity: &purity::Poly,
        evidence_purity: &purity::Poly,
    ) {
        let pvar_id = if let purity::Poly::Var(pvar_id) = target_purity {
            pvar_id
        } else {
            return;
        };

        if !self.selecting_pvars.contains_key(&pvar_id) {
            return;
        }

        self.pvar_purities
            .entry(*pvar_id)
            .and_modify(|existing| {
                *existing = ty::unify::unify_purity_refs(existing, evidence_purity);
            })
            .or_insert_with(|| evidence_purity.clone());
    }

    fn add_evidence_top_fun(&mut self, target_top_fun: &ty::TopFun, evidence_top_fun: &ty::TopFun) {
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
            if let Some(evidence_rest) = evidence_iter.collect_rest(self.all_tvars) {
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
            (ty::Ty::Map(target_map), ty::Ty::Map(evidence_map)) => {
                self.add_evidence(target_map.key(), evidence_map.key());
                self.add_evidence(target_map.value(), evidence_map.value());
            }
            (ty::Ty::List(target_list), ty::Ty::List(evidence_list)) => {
                self.add_evidence_list(target_list, evidence_list);
            }
            (ty::Ty::Vector(target_members), ty::Ty::Vector(evidence_members)) => {
                for (target_member, evidence_member) in
                    target_members.iter().zip(evidence_members.iter())
                {
                    self.add_evidence(target_member, evidence_member);
                }
            }
            (ty::Ty::Vectorof(target_member), ty::Ty::Vectorof(evidence_member)) => {
                self.add_evidence(target_member, evidence_member);
            }
            (ty::Ty::Vectorof(target_member), ty::Ty::Vector(evidence_members)) => {
                for evidence_member in evidence_members.iter() {
                    self.add_evidence(target_member, evidence_member);
                }
            }
            (ty::Ty::TopFun(target_top_fun), ty::Ty::TopFun(evidence_top_fun)) => {
                self.add_evidence_top_fun(target_top_fun, evidence_top_fun);
            }
            (ty::Ty::TopFun(target_top_fun), ty::Ty::Fun(evidence_fun)) => {
                self.add_evidence_top_fun(target_top_fun, evidence_fun.top_fun());
            }
            (ty::Ty::TopFun(target_top_fun), ty::Ty::TyPred(_))
            | (ty::Ty::TopFun(target_top_fun), ty::Ty::EqPred) => {
                self.add_evidence_top_fun(target_top_fun, &ty::TopFun::new_for_pred());
            }
            (ty::Ty::Fun(target_fun), ty::Ty::Fun(evidence_fun)) => {
                self.add_evidence_top_fun(target_fun.top_fun(), evidence_fun.top_fun());
            }
            (ty::Ty::Fun(target_fun), ty::Ty::TyPred(_))
            | (ty::Ty::Fun(target_fun), ty::Ty::EqPred) => {
                self.add_evidence_top_fun(target_fun.top_fun(), &ty::TopFun::new_for_pred());
            }
            (ty::Ty::Union(target_members), _) => {
                for target_member in target_members.iter() {
                    self.add_evidence(target_member, evidence_poly);
                }
            }
            _ => {}
        }
    }

    fn add_var_evidence(&mut self, tvar_id: ty::TVarId, evidence_poly: &ty::Poly) {
        if let Some(tvar) = self.selecting_tvars.get(&tvar_id) {
            if !ty::is_a::ty_ref_is_a(self.all_tvars, evidence_poly, &tvar.bound).to_bool() {
                return;
            }
        } else {
            return;
        };

        let all_tvars = &self.all_tvars;
        self.tvar_types
            .entry(tvar_id)
            .and_modify(|existing| {
                *existing = ty::unify::unify_to_ty_ref(all_tvars, existing, evidence_poly);
            })
            .or_insert_with(|| evidence_poly.clone());
    }

    pub fn add_evidence(&mut self, target_poly: &ty::Poly, evidence_poly: &ty::Poly) {
        match target_poly {
            ty::Poly::Var(tvar_id) => self.add_var_evidence(*tvar_id, evidence_poly),
            ty::Poly::Fixed(target_ty) => {
                let evidence_ty =
                    ty::resolve::resolve_poly_ty(self.all_tvars, evidence_poly).as_ty();
                self.add_evidence_ty(target_ty, evidence_poly, evidence_ty)
            }
        }
    }

    pub fn into_poly_ty_args(self) -> PolyTyArgs {
        let pvar_purities = self
            .selecting_pvars
            .keys()
            .map(|pvar_id| {
                let pvar_purity = self
                    .pvar_purities
                    .get(pvar_id)
                    .cloned()
                    .unwrap_or_else(|| Purity::Impure.into_poly());

                (*pvar_id, pvar_purity)
            })
            .collect();

        let tvar_types = self
            .selecting_tvars
            .iter()
            .map(|(tvar_id, tvar)| {
                let tvar_type = self
                    .tvar_types
                    .get(tvar_id)
                    .cloned()
                    .unwrap_or_else(|| tvar.bound.clone());

                (*tvar_id, tvar_type)
            })
            .collect();

        PolyTyArgs::new(pvar_purities, tvar_types)
    }
}

#[cfg(test)]
mod test {
    use super::*;
    use crate::hir::ns::{NsDatum, NsId};
    use crate::hir::scope::Scope;
    use crate::ty::purity::Purity;
    use syntax::parser::{data_from_str, datum_from_str};

    struct TestScope {
        test_ns_id: NsId,
        scope: Scope,
        pvars: purity::PVars,
        tvars: ty::TVars,
    }

    impl TestScope {
        fn new(polymorphic_str: &str) -> TestScope {
            use crate::hir::lower_polymorphic_vars;

            let test_ns_id = Scope::root_ns_id();
            let outer_scope = Scope::new_with_primitives();
            let mut inner_scope = Scope::new_child(&outer_scope);

            let polymorphic_data = data_from_str(polymorphic_str)
                .unwrap()
                .into_iter()
                .map(|value| NsDatum::from_syntax_datum(test_ns_id, value))
                .collect::<Vec<NsDatum>>();

            let (pvars, tvars) = lower_polymorphic_vars(
                polymorphic_data.into_iter(),
                &outer_scope,
                &mut inner_scope,
            )
            .unwrap();

            TestScope {
                test_ns_id,
                scope: inner_scope,
                pvars,
                tvars,
            }
        }

        fn poly_for_str(&self, poly_str: &str) -> ty::Poly {
            use crate::hir::lower_poly;
            let test_datum = datum_from_str(poly_str).unwrap();

            lower_poly(
                &self.scope,
                NsDatum::from_syntax_datum(self.test_ns_id, test_datum),
            )
            .unwrap()
        }

        fn purity_for_str(&self, poly_str: &str) -> purity::Poly {
            use crate::hir::try_lower_purity;
            let test_datum = datum_from_str(poly_str).unwrap();

            try_lower_purity(
                &self.scope,
                &NsDatum::from_syntax_datum(self.test_ns_id, test_datum),
            )
            .unwrap()
        }

        fn select_ctx(&self) -> SelectCtx<'_> {
            SelectCtx::new(&self.tvars, &self.pvars, &self.tvars)
        }
    }

    fn assert_unselected_type(ctx: &SelectCtx<'_>, poly_var: &ty::Poly) {
        let tvar_id = if let ty::Poly::Var(tvar_id) = poly_var {
            tvar_id
        } else {
            panic!("Can't find tvar ID")
        };

        assert_eq!(None, ctx.tvar_types.get(&tvar_id));
    }

    fn assert_selected_type(ctx: &SelectCtx<'_>, poly_var: &ty::Poly, selected_poly: &ty::Poly) {
        let tvar_id = if let ty::Poly::Var(tvar_id) = poly_var {
            tvar_id
        } else {
            panic!("Can't find tvar ID")
        };

        assert_eq!(Some(selected_poly), ctx.tvar_types.get(&tvar_id));
    }

    fn assert_unselected_purity(ctx: &SelectCtx<'_>, poly_var: &purity::Poly) {
        let pvar_id = if let purity::Poly::Var(pvar_id) = poly_var {
            pvar_id
        } else {
            panic!("Can't find pvar ID")
        };

        assert_eq!(None, ctx.pvar_purities.get(&pvar_id));
    }

    fn assert_selected_purity(
        ctx: &SelectCtx<'_>,
        poly_var: &purity::Poly,
        selected_purity: Purity,
    ) {
        let pvar_id = if let purity::Poly::Var(pvar_id) = poly_var {
            pvar_id
        } else {
            panic!("Can't find pvar ID")
        };

        assert_eq!(
            Some(&purity::Poly::Fixed(selected_purity)),
            ctx.pvar_purities.get(&pvar_id)
        );
    }

    #[test]
    fn trivial_tvar() {
        let scope = TestScope::new("A");

        let poly_a = scope.poly_for_str("A");
        let mut stx = scope.select_ctx();
        assert_unselected_type(&stx, &poly_a);

        stx.add_evidence(&poly_a, &scope.poly_for_str("true"));
        assert_selected_type(&stx, &poly_a, &scope.poly_for_str("true"));

        stx.add_evidence(&poly_a, &scope.poly_for_str("false"));
        assert_selected_type(&stx, &poly_a, &scope.poly_for_str("Bool"));
    }

    #[test]
    fn poly_conflicing_tvar() {
        let scope = TestScope::new("[A : (... -> Any)] [B : (... -> Sym)] [C : (... -> 'foo)]");

        let poly_a = scope.poly_for_str("A");
        let poly_b = scope.poly_for_str("B");
        let poly_c = scope.poly_for_str("C");

        let mut stx = scope.select_ctx();
        assert_unselected_type(&stx, &poly_a);

        // We can handle one tvar as evidence
        stx.add_evidence(&poly_a, &poly_b);
        assert_selected_type(&stx, &poly_a, &poly_b);

        stx.add_evidence(&poly_a, &poly_c);
        assert_selected_type(&stx, &poly_a, &scope.poly_for_str("(U B C)"));
    }

    #[test]
    fn set_types() {
        let scope = TestScope::new("A");

        let poly_a = scope.poly_for_str("A");
        let mut stx = scope.select_ctx();

        stx.add_evidence(
            &scope.poly_for_str("(Setof A)"),
            &scope.poly_for_str("(Setof Bool)"),
        );
        assert_selected_type(&stx, &poly_a, &scope.poly_for_str("Bool"));
    }

    #[test]
    fn map_types() {
        let scope = TestScope::new("A B");
        let poly_a = scope.poly_for_str("A");
        let poly_b = scope.poly_for_str("B");

        let mut stx = scope.select_ctx();

        stx.add_evidence(
            &scope.poly_for_str("(Map A B)"),
            &scope.poly_for_str("(Map true false)"),
        );
        assert_selected_type(&stx, &poly_a, &scope.poly_for_str("true"));
        assert_selected_type(&stx, &poly_b, &scope.poly_for_str("false"));
    }

    #[test]
    fn fixed_list_types() {
        let scope = TestScope::new("A B");
        let poly_a = scope.poly_for_str("A");
        let poly_b = scope.poly_for_str("B");

        let mut stx = scope.select_ctx();

        stx.add_evidence(
            &scope.poly_for_str("(List A B)"),
            &scope.poly_for_str("(List true false)"),
        );
        assert_selected_type(&stx, &poly_a, &scope.poly_for_str("true"));
        assert_selected_type(&stx, &poly_b, &scope.poly_for_str("false"));
    }

    #[test]
    fn listof_types() {
        let scope = TestScope::new("A");
        let poly_a = scope.poly_for_str("A");

        let mut stx = scope.select_ctx();

        stx.add_evidence(
            &scope.poly_for_str("(Listof A)"),
            &scope.poly_for_str("(Listof true)"),
        );
        assert_selected_type(&stx, &poly_a, &scope.poly_for_str("true"));
    }

    #[test]
    fn listof_from_fixed_list() {
        let scope = TestScope::new("A");
        let poly_a = scope.poly_for_str("A");

        let mut stx = scope.select_ctx();

        stx.add_evidence(
            &scope.poly_for_str("(Listof A)"),
            &scope.poly_for_str("(List true false)"),
        );
        assert_selected_type(&stx, &poly_a, &scope.poly_for_str("Bool"));
    }

    #[test]
    fn fixed_vector_types() {
        let scope = TestScope::new("A B");
        let poly_a = scope.poly_for_str("A");
        let poly_b = scope.poly_for_str("B");

        let mut stx = scope.select_ctx();

        stx.add_evidence(
            &scope.poly_for_str("(Vector A B)"),
            &scope.poly_for_str("(Vector true false)"),
        );
        assert_selected_type(&stx, &poly_a, &scope.poly_for_str("true"));
        assert_selected_type(&stx, &poly_b, &scope.poly_for_str("false"));
    }

    #[test]
    fn vectorof_types() {
        let scope = TestScope::new("A");
        let poly_a = scope.poly_for_str("A");

        let mut stx = scope.select_ctx();

        stx.add_evidence(
            &scope.poly_for_str("(Vectorof A)"),
            &scope.poly_for_str("(Vectorof true)"),
        );
        assert_selected_type(&stx, &poly_a, &scope.poly_for_str("true"));
    }

    #[test]
    fn vectorof_from_fixed_vector() {
        let scope = TestScope::new("A");
        let poly_a = scope.poly_for_str("A");

        let mut stx = scope.select_ctx();

        stx.add_evidence(
            &scope.poly_for_str("(Vectorof A)"),
            &scope.poly_for_str("(Vector true false)"),
        );
        assert_selected_type(&stx, &poly_a, &scope.poly_for_str("Bool"));
    }

    #[test]
    fn union_types() {
        let scope = TestScope::new("A");
        let poly_a = scope.poly_for_str("A");

        let mut stx = scope.select_ctx();

        stx.add_evidence(
            &scope.poly_for_str("(U A Sym)"),
            &scope.poly_for_str("true"),
        );
        assert_selected_type(&stx, &poly_a, &scope.poly_for_str("true"));
    }

    #[test]
    fn bounded_union_types() {
        let scope = TestScope::new("[A : Sym] [B : Bool]");
        let poly_a = scope.poly_for_str("A");
        let poly_b = scope.poly_for_str("B");

        let mut stx = scope.select_ctx();

        // A and B are bounded. We should ensure we only use evidence on the members with satisfied
        // bounds.
        stx.add_evidence(&scope.poly_for_str("(U A B)"), &scope.poly_for_str("'foo"));
        assert_selected_type(&stx, &poly_a, &scope.poly_for_str("'foo"));
        assert_unselected_type(&stx, &poly_b);

        stx.add_evidence(&scope.poly_for_str("(U A B)"), &scope.poly_for_str("true"));
        assert_selected_type(&stx, &poly_a, &scope.poly_for_str("'foo"));
        assert_selected_type(&stx, &poly_b, &scope.poly_for_str("true"));

        stx.add_evidence(&scope.poly_for_str("(U A B)"), &scope.poly_for_str("false"));
        assert_selected_type(&stx, &poly_a, &scope.poly_for_str("'foo"));
        assert_selected_type(&stx, &poly_b, &scope.poly_for_str("Bool"));
    }

    #[test]
    fn top_fun_types() {
        let scope = TestScope::new("A");
        let poly_a = scope.poly_for_str("A");

        let mut stx = scope.select_ctx();

        stx.add_evidence(
            &scope.poly_for_str("(... -> A)"),
            &scope.poly_for_str("(... -> true)"),
        );
        assert_selected_type(&stx, &poly_a, &scope.poly_for_str("true"));
    }

    #[test]
    fn top_fun_purities() {
        let scope = TestScope::new("[->A : ->!]");
        let purity_a = scope.purity_for_str("->A");

        let mut stx = scope.select_ctx();
        assert_unselected_purity(&stx, &purity_a);

        stx.add_evidence(
            &scope.poly_for_str("(... ->A true)"),
            &scope.poly_for_str("(... -> true)"),
        );
        assert_selected_purity(&stx, &purity_a, Purity::Pure);

        stx.add_evidence(
            &scope.poly_for_str("(... ->A true)"),
            &scope.poly_for_str("(... ->! true)"),
        );
        assert_selected_purity(&stx, &purity_a, Purity::Impure);
    }

    #[test]
    fn top_fun_from_fun() {
        let scope = TestScope::new("A");
        let poly_a = scope.poly_for_str("A");

        let mut stx = scope.select_ctx();

        stx.add_evidence(
            &scope.poly_for_str("(... -> A)"),
            &scope.poly_for_str("(false -> true)"),
        );
        assert_selected_type(&stx, &poly_a, &scope.poly_for_str("true"));
    }

    #[test]
    fn top_fun_from_ty_pred() {
        let scope = TestScope::new("A");
        let poly_a = scope.poly_for_str("A");

        let mut stx = scope.select_ctx();

        stx.add_evidence(
            &scope.poly_for_str("(... -> A)"),
            &scope.poly_for_str("sym?"),
        );
        assert_selected_type(&stx, &poly_a, &scope.poly_for_str("Bool"));
    }

    #[test]
    fn top_fun_from_eq_pred() {
        let scope = TestScope::new("A");
        let poly_a = scope.poly_for_str("A");

        let mut stx = scope.select_ctx();

        stx.add_evidence(&scope.poly_for_str("(... -> A)"), &scope.poly_for_str("="));
        assert_selected_type(&stx, &poly_a, &scope.poly_for_str("Bool"));
    }

    #[test]
    fn fun_types() {
        let scope = TestScope::new("A B");
        let poly_a = scope.poly_for_str("A");
        let poly_b = scope.poly_for_str("B");

        let mut stx = scope.select_ctx();

        stx.add_evidence(
            &scope.poly_for_str("(A -> B)"),
            &scope.poly_for_str("(true -> false)"),
        );
        // We intentionally do not use function type params as evidence
        assert_unselected_type(&stx, &poly_a);
        assert_selected_type(&stx, &poly_b, &scope.poly_for_str("false"));
    }

    #[test]
    fn fun_purities() {
        let scope = TestScope::new("[->A : ->!]");
        let purity_a = scope.purity_for_str("->A");

        let mut stx = scope.select_ctx();
        assert_unselected_purity(&stx, &purity_a);

        stx.add_evidence(
            &scope.poly_for_str("(->A true)"),
            &scope.poly_for_str("(->! true)"),
        );
        assert_selected_purity(&stx, &purity_a, Purity::Impure);
    }

    #[test]
    fn fun_purity_conflict() {
        let scope = TestScope::new("[->A : ->!] [->B : ->!] [->C : ->!]");
        let purity_a = scope.purity_for_str("->A");

        let mut stx = scope.select_ctx();
        assert_unselected_purity(&stx, &purity_a);

        stx.add_evidence(
            &scope.poly_for_str("(->A true)"),
            &scope.poly_for_str("(->B true)"),
        );
        stx.add_evidence(
            &scope.poly_for_str("(->A true)"),
            &scope.poly_for_str("(->C true)"),
        );

        assert_selected_purity(&stx, &purity_a, Purity::Impure);
    }

    #[test]
    fn fun_type_from_ty_pred() {
        let scope = TestScope::new("A B");
        let poly_a = scope.poly_for_str("A");
        let poly_b = scope.poly_for_str("B");

        let mut stx = scope.select_ctx();

        stx.add_evidence(&scope.poly_for_str("(A -> B)"), &scope.poly_for_str("sym?"));
        assert_unselected_type(&stx, &poly_a);
        assert_selected_type(&stx, &poly_b, &scope.poly_for_str("Bool"));
    }

    #[test]
    fn fun_type_from_eq_pred() {
        let scope = TestScope::new("A B");
        let poly_a = scope.poly_for_str("A");
        let poly_b = scope.poly_for_str("B");

        let mut stx = scope.select_ctx();

        stx.add_evidence(&scope.poly_for_str("(A A -> B)"), &scope.poly_for_str("="));
        assert_unselected_type(&stx, &poly_a);
        assert_selected_type(&stx, &poly_b, &scope.poly_for_str("Bool"));
    }

    #[test]
    fn ty_pred_purity() {
        let scope = TestScope::new("[->A : ->!]");
        let purity_a = scope.purity_for_str("->A");

        let mut stx = scope.select_ctx();
        assert_unselected_purity(&stx, &purity_a);

        stx.add_evidence(
            &scope.poly_for_str("(->A true)"),
            &scope.poly_for_str("sym?"),
        );
        assert_selected_purity(&stx, &purity_a, Purity::Pure);
    }
}
