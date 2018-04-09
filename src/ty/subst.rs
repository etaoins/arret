use std::result;
use std::collections::HashMap;

use ty;

#[derive(Debug, PartialEq)]
pub enum Error {
    Unresolved(ty::PVarId),
}

pub type Result<T> = result::Result<T, Error>;

fn subst_ty_vec(
    fixed: &Vec<ty::Poly>,
    pvars: &HashMap<ty::PVarId, ty::Mono>,
) -> Result<Vec<ty::Mono>> {
    fixed
        .iter()
        .map(|p| subst(p, pvars))
        .collect::<Result<Vec<ty::Mono>>>()
}

fn subst_ty(
    fixed: &ty::Ty<ty::Poly>,
    pvars: &HashMap<ty::PVarId, ty::Mono>,
) -> Result<ty::Ty<ty::Mono>> {
    match *fixed {
        ty::Ty::Any => Ok(ty::Ty::Any),
        ty::Ty::Bool => Ok(ty::Ty::Bool),
        ty::Ty::Char => Ok(ty::Ty::Char),
        ty::Ty::Float => Ok(ty::Ty::Float),
        ty::Ty::Fun(ref fun) => Ok(ty::Ty::new_fun(
            fun.impure,
            subst(&fun.params, pvars)?,
            subst(&fun.ret, pvars)?,
        )),
        ty::Ty::Hash(ref key, ref value) => Ok(ty::Ty::Hash(
            Box::new(subst(key, pvars)?),
            Box::new(subst(value, pvars)?),
        )),
        ty::Ty::Int => Ok(ty::Ty::Int),
        ty::Ty::List(ref fixed, ref rest) => {
            let fixed_mono = subst_ty_vec(fixed, pvars)?;
            let rest_mono = match *rest {
                Some(ref rest) => Some(Box::new(subst(rest, pvars)?)),
                None => None,
            };

            Ok(ty::Ty::List(fixed_mono, rest_mono))
        }
        ty::Ty::LitBool(val) => Ok(ty::Ty::LitBool(val)),
        ty::Ty::LitSym(ref val) => Ok(ty::Ty::LitSym(val.clone())),
        ty::Ty::Set(ref member) => Ok(ty::Ty::Set(Box::new(subst(&member, pvars)?))),
        ty::Ty::Str => Ok(ty::Ty::Str),
        ty::Ty::Sym => Ok(ty::Ty::Sym),
        ty::Ty::Union(ref members) => Ok(ty::Ty::Union(subst_ty_vec(members, pvars)?)),
        ty::Ty::Vec(ref start, ref fixed) => {
            let start_mono = match *start {
                Some(ref start) => Some(Box::new(subst(start, pvars)?)),
                None => None,
            };
            let fixed_mono = subst_ty_vec(fixed, pvars)?;

            Ok(ty::Ty::Vec(start_mono, fixed_mono))
        }
    }
}

pub fn subst(poly: &ty::Poly, pvars: &HashMap<ty::PVarId, ty::Mono>) -> Result<ty::Mono> {
    match *poly {
        ty::Poly::Fixed(ref fixed) => subst_ty(fixed, pvars).map(|t| t.into_mono()),
        ty::Poly::Var(pvar_id) => pvars
            .get(&pvar_id)
            .map(|mono| mono.clone())
            .ok_or(Error::Unresolved(pvar_id)),
    }
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn subst_pvar() {
        let pvar_id = ty::PVarId::new(1);

        let poly_var = ty::Poly::Var(pvar_id);
        let mut pvars = HashMap::new();
        pvars.insert(pvar_id, ty::Ty::Int.into_mono());

        let expected = ty::Ty::Int.into_mono();
        assert_eq!(expected, subst(&poly_var, &pvars).unwrap());
    }

    #[test]
    fn subst_pvar_unresolved() {
        let pvar_id = ty::PVarId::new(1);

        let poly_var = ty::Poly::Var(pvar_id);
        let pvars = HashMap::new();
        // Do not include the pvar

        let err = Error::Unresolved(pvar_id);
        assert_eq!(err, subst(&poly_var, &pvars).unwrap_err());
    }

    #[test]
    fn subst_set() {
        let pvar_id = ty::PVarId::new(1);

        let poly_set = ty::Ty::Set(Box::new(ty::Poly::Var(pvar_id))).into_poly();
        let mut pvars = HashMap::new();
        pvars.insert(pvar_id, ty::Ty::Int.into_mono());

        let expected = ty::Ty::Set(Box::new(ty::Ty::Int.into_mono())).into_mono();
        assert_eq!(expected, subst(&poly_set, &pvars).unwrap());
    }

    #[test]
    fn subst_set_unresolved() {
        let pvar_id = ty::PVarId::new(1);

        let poly_set = ty::Ty::Set(Box::new(ty::Poly::Var(pvar_id))).into_poly();
        let pvars = HashMap::new();
        // Do not include the pvar

        let err = Error::Unresolved(pvar_id);
        assert_eq!(err, subst(&poly_set, &pvars).unwrap_err());
    }
}
