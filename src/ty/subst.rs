use std::collections::HashMap;
use std::result;

use ty;
use ty::purity::Purity;

#[derive(Debug, PartialEq)]
pub enum Error {
    UnresolvedPurity(ty::purity::PVarId),
    UnresolvedType(ty::TVarId),
}

pub type Result<T> = result::Result<T, Error>;

struct SubstContext<'a> {
    pvars: &'a HashMap<ty::purity::PVarId, Purity>,
    tvars: &'a HashMap<ty::TVarId, ty::Mono>,
}

impl<'a> SubstContext<'a> {
    fn subst_ty_ref_slice(&self, polys: &[ty::Poly]) -> Result<Vec<ty::Mono>> {
        polys
            .iter()
            .map(|p| self.subst_ty_ref(p))
            .collect::<Result<Vec<ty::Mono>>>()
    }

    fn subst_ty_ref_option(&self, ty: &Option<ty::Poly>) -> Result<Option<ty::Mono>> {
        Ok(match *ty {
            Some(ref ty) => Some(self.subst_ty_ref(ty)?),
            None => None,
        })
    }

    fn subst_ty(&self, ty: &ty::Ty<ty::Poly>) -> Result<ty::Ty<ty::Mono>> {
        Ok(match *ty {
            ty::Ty::Any => ty::Ty::Any,
            ty::Ty::Bool => ty::Ty::Bool,
            ty::Ty::Char => ty::Ty::Char,
            ty::Ty::Float => ty::Ty::Float,
            ty::Ty::Int => ty::Ty::Int,
            ty::Ty::Nil => ty::Ty::Nil,
            ty::Ty::Str => ty::Ty::Str,
            ty::Ty::Sym => ty::Ty::Sym,
            ty::Ty::TopFun(ref top_fun) => ty::TopFun::new(
                self.subst_purity_ref(top_fun.purity())?,
                self.subst_ty_ref(top_fun.ret())?,
            ).into_ty(),
            ty::Ty::Fun(ref fun) => ty::Fun::new(
                ty::TVarIds::empty(),
                ty::TopFun::new(
                    self.subst_purity_ref(fun.purity())?,
                    self.subst_ty_ref(fun.ret())?,
                ),
                ty::Params::new(
                    self.subst_ty_ref_slice(fun.params().fixed())?,
                    self.subst_ty_ref_option(fun.params().rest())?,
                ),
            ).into_ty(),
            ty::Ty::TyPred(ref test_ty) => ty::Ty::TyPred(Box::new(self.subst_ty_ref(test_ty)?)),
            ty::Ty::Map(ref key, ref value) => ty::Ty::Map(
                Box::new(self.subst_ty_ref(key)?),
                Box::new(self.subst_ty_ref(value)?),
            ),
            ty::Ty::LitBool(val) => ty::Ty::LitBool(val),
            ty::Ty::LitSym(ref val) => ty::Ty::LitSym(val.clone()),
            ty::Ty::Set(ref member) => ty::Ty::Set(Box::new(self.subst_ty_ref(&member)?)),
            ty::Ty::Union(ref members) => ty::Ty::Union(self.subst_ty_ref_slice(members)?),
            ty::Ty::Vec(ref members) => {
                let members_mono = self.subst_ty_ref_slice(members)?;
                ty::Ty::Vec(members_mono)
            }
            ty::Ty::Vecof(ref member) => ty::Ty::Vecof(Box::new(self.subst_ty_ref(member)?)),
            ty::Ty::Listof(ref member) => ty::Ty::Listof(Box::new(self.subst_ty_ref(member)?)),
            ty::Ty::Cons(ref car, ref cdr) => ty::Ty::Cons(
                Box::new(self.subst_ty_ref(car)?),
                Box::new(self.subst_ty_ref(cdr)?),
            ),
        })
    }

    fn subst_purity_ref(&self, poly: &ty::purity::Poly) -> Result<Purity> {
        match *poly {
            ty::purity::Poly::Fixed(ref fixed) => Ok(*fixed),
            ty::purity::Poly::Var(pvar_id) => self.pvars
                .get(&pvar_id)
                .cloned()
                .ok_or_else(|| Error::UnresolvedPurity(pvar_id)),
        }
    }

    fn subst_ty_ref(&self, poly: &ty::Poly) -> Result<ty::Mono> {
        match *poly {
            ty::Poly::Fixed(ref fixed) => self.subst_ty(fixed).map(|t| t.into_mono()),
            ty::Poly::Var(tvar_id) => self.tvars
                .get(&tvar_id)
                .cloned()
                .ok_or_else(|| Error::UnresolvedType(tvar_id)),
        }
    }
}

pub fn subst(
    pvars: &HashMap<ty::purity::PVarId, Purity>,
    tvars: &HashMap<ty::TVarId, ty::Mono>,
    poly: &ty::Poly,
) -> Result<ty::Mono> {
    let ctx = SubstContext { pvars, tvars };
    ctx.subst_ty_ref(poly)
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn subst_pvar() {
        let pvar_id = ty::purity::PVarId::new(1);
        let purity_var = ty::purity::Poly::Var(pvar_id);
        let poly_top_fun = ty::TopFun::new(purity_var, ty::Ty::Any.into_poly()).into_ref();
        let mut pvars = HashMap::new();
        pvars.insert(pvar_id, Purity::Pure);

        let tvars = HashMap::new();

        let expected = ty::TopFun::new(Purity::Pure, ty::Ty::Any.into_mono()).into_ref();
        assert_eq!(expected, subst(&pvars, &tvars, &poly_top_fun).unwrap());
    }

    #[test]
    fn subst_pvar_unresolved() {
        let pvar_id = ty::purity::PVarId::new(1);
        let purity_var = ty::purity::Poly::Var(pvar_id);
        let poly_top_fun = ty::TopFun::new(purity_var, ty::Ty::Any.into_poly()).into_ref();
        let pvars = HashMap::new();
        // Don't include the pvar

        let tvars = HashMap::new();

        let err = Error::UnresolvedPurity(pvar_id);
        assert_eq!(err, subst(&pvars, &tvars, &poly_top_fun).unwrap_err());
    }

    #[test]
    fn subst_tvar() {
        let pvars = HashMap::new();

        let tvar_id = ty::TVarId::new(1);
        let type_var = ty::Poly::Var(tvar_id);
        let mut tvars = HashMap::new();
        tvars.insert(tvar_id, ty::Ty::Int.into_mono());

        let expected = ty::Ty::Int.into_mono();
        assert_eq!(expected, subst(&pvars, &tvars, &type_var).unwrap());
    }

    #[test]
    fn subst_tvar_unresolved() {
        let pvars = HashMap::new();

        let tvar_id = ty::TVarId::new(1);
        let poly_var = ty::Poly::Var(tvar_id);
        let tvars = HashMap::new();
        // Do not include the tvar

        let err = Error::UnresolvedType(tvar_id);
        assert_eq!(err, subst(&pvars, &tvars, &poly_var).unwrap_err());
    }

    #[test]
    fn subst_set() {
        let pvars = HashMap::new();

        let tvar_id = ty::TVarId::new(1);
        let poly_set = ty::Ty::Set(Box::new(ty::Poly::Var(tvar_id))).into_poly();
        let mut tvars = HashMap::new();
        tvars.insert(tvar_id, ty::Ty::Int.into_mono());

        let expected = ty::Ty::Set(Box::new(ty::Ty::Int.into_mono())).into_mono();
        assert_eq!(expected, subst(&pvars, &tvars, &poly_set).unwrap());
    }

    #[test]
    fn subst_set_unresolved() {
        let pvars = HashMap::new();

        let tvar_id = ty::TVarId::new(1);
        let poly_set = ty::Ty::Set(Box::new(ty::Poly::Var(tvar_id))).into_poly();
        let tvars = HashMap::new();
        // Do not include the tvar

        let err = Error::UnresolvedType(tvar_id);
        assert_eq!(err, subst(&pvars, &tvars, &poly_set).unwrap_err());
    }
}
