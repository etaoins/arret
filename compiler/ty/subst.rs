use ty;
use ty::purity::Purity;

trait SubstCtx<I, O, E>
where
    I: ty::TyRef,
    O: ty::TyRef,
{
    fn subst_purity_ref(&self, input: &I::PRef) -> Result<O::PRef, E>;
    fn subst_ty_ref(&self, input: &I) -> Result<O, E>;

    fn subst_ty_ref_slice(&self, inputs: &[I]) -> Result<Box<[O]>, E> {
        Ok(inputs
            .iter()
            .map(|i| self.subst_ty_ref(i))
            .collect::<Result<Vec<O>, E>>()?
            .into_boxed_slice())
    }

    fn subst_ty_ref_option(&self, ty: Option<&I>) -> Result<Option<O>, E> {
        Ok(match ty {
            Some(ty) => Some(self.subst_ty_ref(ty)?),
            None => None,
        })
    }

    fn subst_list(&self, list: &ty::List<I>) -> Result<ty::List<O>, E> {
        Ok(ty::List::new(
            self.subst_ty_ref_slice(list.fixed())?,
            self.subst_ty_ref_option(list.rest())?,
        ))
    }

    fn subst_fun(&self, fun: &ty::Fun<I>) -> Result<ty::Fun<O>, E> {
        Ok(ty::Fun::new(
            ty::PVarIds::monomorphic(),
            ty::TVarIds::monomorphic(),
            ty::TopFun::new(
                self.subst_purity_ref(fun.purity())?,
                self.subst_ty_ref(fun.ret())?,
            ),
            self.subst_list(fun.params())?,
        ))
    }

    fn subst_ty(&self, ty: &ty::Ty<I>) -> Result<ty::Ty<O>, E> {
        Ok(match ty {
            ty::Ty::Any => ty::Ty::Any,
            ty::Ty::Bool => ty::Ty::Bool,
            ty::Ty::Char => ty::Ty::Char,
            ty::Ty::Float => ty::Ty::Float,
            ty::Ty::Int => ty::Ty::Int,
            ty::Ty::Str => ty::Ty::Str,
            ty::Ty::Sym => ty::Ty::Sym,
            ty::Ty::TopFun(top_fun) => ty::TopFun::new(
                self.subst_purity_ref(top_fun.purity())?,
                self.subst_ty_ref(top_fun.ret())?,
            ).into_ty(),
            ty::Ty::Fun(fun) => self.subst_fun(fun)?.into_ty(),
            ty::Ty::TyPred(test_ty) => ty::Ty::TyPred(Box::new(self.subst_ty_ref(test_ty)?)),
            ty::Ty::Map(map) => ty::Ty::Map(Box::new(ty::Map::new(
                self.subst_ty_ref(map.key())?,
                self.subst_ty_ref(map.value())?,
            ))),
            ty::Ty::LitBool(val) => ty::Ty::LitBool(*val),
            ty::Ty::LitSym(val) => ty::Ty::LitSym(val.clone()),
            ty::Ty::Set(member) => ty::Ty::Set(Box::new(self.subst_ty_ref(&member)?)),
            ty::Ty::Union(members) => ty::Ty::Union(self.subst_ty_ref_slice(members)?),
            ty::Ty::Vec(members) => ty::Ty::Vec(self.subst_ty_ref_slice(members)?),
            ty::Ty::Vecof(member) => ty::Ty::Vecof(Box::new(self.subst_ty_ref(member)?)),
            ty::Ty::List(list) => ty::Ty::List(self.subst_list(list)?),
        })
    }
}

struct InstPolySelectionCtx<'scx> {
    select_ctx: &'scx ty::select::SelectCtx<'scx>,
}

// TODO: Replace with bang type once it's stable
#[derive(Debug, PartialEq)]
pub enum InstPolySelectionError {}

impl<'scx> SubstCtx<ty::Poly, ty::Poly, InstPolySelectionError> for InstPolySelectionCtx<'scx> {
    fn subst_purity_ref(
        &self,
        poly: &ty::purity::Poly,
    ) -> Result<ty::purity::Poly, InstPolySelectionError> {
        match poly {
            ty::purity::Poly::Fixed(_) => Ok(poly.clone()),
            ty::purity::Poly::Var(pvar_id) => {
                if let Some(pvar_idx) = self.select_ctx.selected_pvar_idx(*pvar_id) {
                    Ok(self.select_ctx.pvar_purities()[pvar_idx]
                        .clone()
                        .unwrap_or_else(|| ty::purity::Poly::Fixed(Purity::Impure)))
                } else {
                    Ok(poly.clone())
                }
            }
        }
    }

    fn subst_ty_ref(&self, poly: &ty::Poly) -> Result<ty::Poly, InstPolySelectionError> {
        match poly {
            ty::Poly::Fixed(fixed) => self.subst_ty(fixed).map(|t| t.into_poly()),
            ty::Poly::Var(tvar_id) => {
                if let Some(tvar_idx) = self.select_ctx.selected_tvar_idx(*tvar_id) {
                    // We're selecting on this
                    Ok(self.select_ctx.tvar_types()[tvar_idx]
                        .clone()
                        .unwrap_or_else(|| {
                            self.select_ctx.tvars()[tvar_id.to_usize()].bound().clone()
                        }))
                } else {
                    Ok(poly.clone())
                }
            }
        }
    }
}

pub fn inst_ty_selection(select_ctx: &ty::select::SelectCtx, poly: &ty::Poly) -> ty::Poly {
    let ctx = InstPolySelectionCtx { select_ctx };
    ctx.subst_ty_ref(poly).unwrap()
}

pub fn inst_fun_selection(
    select_ctx: &ty::select::SelectCtx,
    fun: &ty::Fun<ty::Poly>,
) -> ty::Fun<ty::Poly> {
    let ctx = InstPolySelectionCtx { select_ctx };
    ctx.subst_fun(fun).unwrap()
}

pub fn inst_purity_selection(
    select_ctx: &ty::select::SelectCtx,
    purity: &ty::purity::Poly,
) -> ty::purity::Poly {
    let ctx = InstPolySelectionCtx { select_ctx };
    ctx.subst_purity_ref(purity).unwrap()
}

#[cfg(test)]
mod test {
    use super::*;
    use std::collections::HashMap;

    struct PolyToMonoCtx<'vars> {
        pvar_purities: &'vars HashMap<ty::purity::PVarId, Purity>,
        tvar_types: &'vars HashMap<ty::TVarId, ty::Mono>,
    }

    #[derive(Debug, PartialEq)]
    pub enum MonoToPolyError {
        UnresolvedPurity(ty::purity::PVarId),
        UnresolvedType(ty::TVarId),
    }

    impl<'vars> SubstCtx<ty::Poly, ty::Mono, MonoToPolyError> for PolyToMonoCtx<'vars> {
        fn subst_purity_ref(&self, poly: &ty::purity::Poly) -> Result<Purity, MonoToPolyError> {
            match poly {
                ty::purity::Poly::Fixed(fixed) => Ok(*fixed),
                ty::purity::Poly::Var(pvar_id) => self
                    .pvar_purities
                    .get(&pvar_id)
                    .cloned()
                    .ok_or_else(|| MonoToPolyError::UnresolvedPurity(*pvar_id)),
            }
        }

        fn subst_ty_ref(&self, poly: &ty::Poly) -> Result<ty::Mono, MonoToPolyError> {
            match poly {
                ty::Poly::Fixed(fixed) => self.subst_ty(fixed).map(|t| t.into_mono()),
                ty::Poly::Var(tvar_id) => self
                    .tvar_types
                    .get(&tvar_id)
                    .cloned()
                    .ok_or_else(|| MonoToPolyError::UnresolvedType(*tvar_id)),
            }
        }
    }

    /// Completely replaces all variables in a polymorphic type
    ///
    /// This will return an error on failure
    pub fn poly_to_mono(
        pvar_purities: &HashMap<ty::purity::PVarId, Purity>,
        tvar_types: &HashMap<ty::TVarId, ty::Mono>,
        poly: &ty::Poly,
    ) -> Result<ty::Mono, MonoToPolyError> {
        let ctx = PolyToMonoCtx {
            pvar_purities,
            tvar_types,
        };
        ctx.subst_ty_ref(poly)
    }

    #[test]
    fn poly_to_mono_pvar() {
        let pvar_id = ty::purity::PVarId::new(1);
        let purity_var = ty::purity::Poly::Var(pvar_id);
        let poly_top_fun = ty::TopFun::new(purity_var, ty::Ty::Any.into_poly()).into_ty_ref();
        let mut pvars = HashMap::new();
        pvars.insert(pvar_id, Purity::Pure);

        let tvars = HashMap::new();

        let expected = ty::TopFun::new(Purity::Pure, ty::Ty::Any.into_mono()).into_ty_ref();
        assert_eq!(
            expected,
            poly_to_mono(&pvars, &tvars, &poly_top_fun).unwrap()
        );
    }

    #[test]
    fn poly_to_mono_pvar_unresolved() {
        let pvar_id = ty::purity::PVarId::new(1);
        let purity_var = ty::purity::Poly::Var(pvar_id);
        let poly_top_fun = ty::TopFun::new(purity_var, ty::Ty::Any.into_poly()).into_ty_ref();
        let pvars = HashMap::new();
        // Don't include the pvar

        let tvars = HashMap::new();

        let err = MonoToPolyError::UnresolvedPurity(pvar_id);
        assert_eq!(
            err,
            poly_to_mono(&pvars, &tvars, &poly_top_fun).unwrap_err()
        );
    }

    #[test]
    fn subst_tvar() {
        let pvars = HashMap::new();

        let tvar_id = ty::TVarId::new(1);
        let type_var = ty::Poly::Var(tvar_id);
        let mut tvars = HashMap::new();
        tvars.insert(tvar_id, ty::Ty::Int.into_mono());

        let expected = ty::Ty::Int.into_mono();
        assert_eq!(expected, poly_to_mono(&pvars, &tvars, &type_var).unwrap());
    }

    #[test]
    fn subst_tvar_unresolved() {
        let pvars = HashMap::new();

        let tvar_id = ty::TVarId::new(1);
        let poly_var = ty::Poly::Var(tvar_id);
        let tvars = HashMap::new();
        // Do not include the tvar

        let err = MonoToPolyError::UnresolvedType(tvar_id);
        assert_eq!(err, poly_to_mono(&pvars, &tvars, &poly_var).unwrap_err());
    }

    #[test]
    fn subst_set() {
        let pvars = HashMap::new();

        let tvar_id = ty::TVarId::new(1);
        let poly_set = ty::Ty::Set(Box::new(ty::Poly::Var(tvar_id))).into_poly();
        let mut tvars = HashMap::new();
        tvars.insert(tvar_id, ty::Ty::Int.into_mono());

        let expected = ty::Ty::Set(Box::new(ty::Ty::Int.into_mono())).into_mono();
        assert_eq!(expected, poly_to_mono(&pvars, &tvars, &poly_set).unwrap());
    }

    #[test]
    fn subst_set_unresolved() {
        let pvars = HashMap::new();

        let tvar_id = ty::TVarId::new(1);
        let poly_set = ty::Ty::Set(Box::new(ty::Poly::Var(tvar_id))).into_poly();
        let tvars = HashMap::new();
        // Do not include the tvar

        let err = MonoToPolyError::UnresolvedType(tvar_id);
        assert_eq!(err, poly_to_mono(&pvars, &tvars, &poly_set).unwrap_err());
    }
}
