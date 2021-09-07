//! Calculates the usage of a polymorphic variable in type

use std::collections::HashMap;
use std::ops;

use crate::ty;
use crate::ty::purity;
use crate::ty::record;
use crate::ty::Ty;

/// Indicates the variance of a polymorphic parameter
///
/// By default variables are covariant. Whenever a variable appears inside a function's parameter
/// list it becomes contravariant, flipping again for each nested function type. If a variable
/// appears in both a covariant and contravariant position then it becomes invariant.
#[derive(PartialEq, Debug, Clone, Copy)]
pub enum Variance {
    /// Subtypes of the record have subtypes of this variable
    Covariant,
    /// Subtypes of the record have supertypes of this variable
    Contravariant,
    /// No subtype relationship exists between records with different types of this variable
    Invariant,
}

impl ops::Mul for Variance {
    type Output = Variance;

    /// Multiplies two variances
    ///
    /// This is used to calculate our new polarity when visiting a nested record type's polymorphic
    /// variables.
    ///
    /// When a contravariant relationship appears in a contravariant polarity it's actually
    /// covariant with respect to the root type. This makes contravariance analogous to a negative
    /// number for the purposes of multiplication.
    ///
    /// An invariant relationship is invariant in any polarity and any relationship is invariant in
    /// an invariant polarity. This makes invariance analogous to zero.
    fn mul(self, rhs: Variance) -> Variance {
        match (self, rhs) {
            (Variance::Invariant, _) | (_, Variance::Invariant) => Variance::Invariant,

            (Variance::Contravariant, Variance::Covariant)
            | (Variance::Covariant, Variance::Contravariant) => Variance::Contravariant,

            (Variance::Covariant, Variance::Covariant)
            | (Variance::Contravariant, Variance::Contravariant) => Variance::Covariant,
        }
    }
}

impl ops::BitAndAssign for Variance {
    /// Combines the variance from two different usages
    fn bitand_assign(&mut self, rhs: Variance) {
        if *self != rhs {
            *self = Variance::Invariant;
        }
    }
}

#[derive(Clone, Debug, Default)]
pub struct VarUsages {
    pvar_variances: HashMap<purity::PVarId, Variance>,
    tvar_variances: HashMap<ty::TVarId, Variance>,
}

fn visit_list(var_usages: &mut VarUsages, polarity: Variance, list: &ty::List<ty::Poly>) {
    for member in list.fixed() {
        visit_poly(var_usages, polarity, member);
    }

    visit_poly(var_usages, polarity, list.rest());
}

fn visit_top_fun(var_usages: &mut VarUsages, polarity: Variance, top_fun: &ty::TopFun) {
    visit_poly(var_usages, polarity, top_fun.ret());
}

fn visit_ty(var_usages: &mut VarUsages, polarity: Variance, poly_ty: &Ty<ty::Poly>) {
    match poly_ty {
        Ty::Any
        | Ty::Bool
        | Ty::Char
        | Ty::LitBool(_)
        | Ty::Sym
        | Ty::LitSym(_)
        | Ty::Int
        | Ty::Float
        | Ty::Num
        | Ty::Str
        | Ty::TyPred(_)
        | Ty::EqPred
        | Ty::TopRecord
        | Ty::RecordClass(_) => {
            // Terminal type
        }

        Ty::List(list) => {
            visit_list(var_usages, polarity, list);
        }

        Ty::Map(map) => {
            visit_poly(var_usages, polarity, map.key());
            visit_poly(var_usages, polarity, map.value());
        }

        Ty::Set(member) | Ty::Vectorof(member) => {
            visit_poly(var_usages, polarity, member);
        }

        Ty::Union(members) | Ty::Intersect(members) | Ty::Vector(members) => {
            for member in members.iter() {
                visit_poly(var_usages, polarity, member);
            }
        }

        Ty::Fun(fun) => {
            visit_top_fun(var_usages, polarity, fun.top_fun());
            visit_list(var_usages, polarity * Variance::Contravariant, fun.params());
        }

        Ty::TopFun(top_fun) => {
            visit_top_fun(var_usages, polarity, top_fun);
        }

        Ty::Record(record_instance) => {
            let record_cons = record_instance.cons();

            for poly_param in record_cons.poly_params() {
                match poly_param {
                    record::PolyParam::PVar(variance, pvar) => {
                        let purity_ref = &record_instance.ty_args().pvar_purities()[pvar];
                        visit_purity(var_usages, polarity * *variance, purity_ref);
                    }
                    record::PolyParam::TVar(variance, tvar) => {
                        let poly_ref = &record_instance.ty_args().tvar_types()[tvar];
                        visit_poly(var_usages, polarity * *variance, poly_ref);
                    }
                    record::PolyParam::Pure(_) | record::PolyParam::TFixed(_, _) => {}
                }
            }
        }
    }
}

fn visit_purity(var_usages: &mut VarUsages, polarity: Variance, purity_ref: &purity::Ref) {
    match purity_ref {
        purity::Ref::Fixed(_) => {}
        purity::Ref::Var(pvar) => {
            var_usages
                .pvar_variances
                .entry(pvar.clone())
                .and_modify(|existing_usage| *existing_usage &= polarity)
                .or_insert(polarity);
        }
    }
}

fn visit_poly(var_usages: &mut VarUsages, polarity: Variance, poly_ref: &ty::Ref<ty::Poly>) {
    match poly_ref {
        ty::Ref::Fixed(poly_ty) => {
            visit_ty(var_usages, polarity, poly_ty);
        }
        ty::Ref::Var(tvar, _) => {
            var_usages
                .tvar_variances
                .entry(tvar.clone())
                .and_modify(|existing_usage| *existing_usage &= polarity)
                .or_insert(polarity);
        }
    }
}

impl VarUsages {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn add_poly_usages(&mut self, poly_ref: &ty::Ref<ty::Poly>) {
        visit_poly(self, Variance::Covariant, poly_ref);
    }

    pub fn pvar_variance(&self, pvar: &purity::PVarId) -> Option<Variance> {
        self.pvar_variances.get(pvar).copied()
    }

    pub fn tvar_variance(&self, tvar: &ty::TVarId) -> Option<Variance> {
        self.tvar_variances.get(tvar).copied()
    }
}

#[cfg(test)]
mod test {
    use super::*;

    use crate::source::EMPTY_SPAN;
    use crate::ty::purity::Purity;

    #[test]
    fn test_variance_mul() {
        assert_eq!(
            Variance::Covariant,
            Variance::Covariant * Variance::Covariant
        );

        assert_eq!(
            Variance::Contravariant,
            Variance::Covariant * Variance::Contravariant
        );

        assert_eq!(
            Variance::Invariant,
            Variance::Covariant * Variance::Invariant
        );
    }

    #[test]
    fn convariant_usage() {
        let tvar = ty::TVar::new(EMPTY_SPAN, "tvar".into(), Ty::Any.into());

        let mut var_usages = VarUsages::new();
        var_usages.add_poly_usages(&tvar.clone().into());

        assert_eq!(Some(Variance::Covariant), var_usages.tvar_variance(&tvar));
    }

    #[test]
    fn contravariant_usage() {
        let tvar = ty::TVar::new(EMPTY_SPAN, "tvar".into(), Ty::Any.into());

        let mut var_usages = VarUsages::new();

        var_usages.add_poly_usages(
            &ty::Fun::new_mono(
                ty::List::new_uniform(tvar.clone().into()),
                Purity::Pure.into(),
                Ty::Any.into(),
            )
            .into(),
        );

        assert_eq!(
            Some(Variance::Contravariant),
            var_usages.tvar_variance(&tvar)
        );
    }

    #[test]
    fn invariant_usage() {
        let tvar = ty::TVar::new(EMPTY_SPAN, "tvar".into(), Ty::Any.into());

        let mut var_usages = VarUsages::new();

        var_usages.add_poly_usages(
            &ty::Fun::new_mono(
                ty::List::new_uniform(tvar.clone().into()),
                Purity::Pure.into(),
                tvar.clone().into(),
            )
            .into(),
        );

        assert_eq!(Some(Variance::Invariant), var_usages.tvar_variance(&tvar));
    }
}
