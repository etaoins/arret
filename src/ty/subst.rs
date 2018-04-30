use std::collections::HashMap;
use std::result;

use ty;

#[derive(Debug, PartialEq)]
pub enum Error {
    Unresolved(ty::PVarId),
}

pub type Result<T> = result::Result<T, Error>;

fn subst_ty_slice(
    fixed: &[ty::Poly],
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
    Ok(match *fixed {
        ty::Ty::Any => ty::Ty::Any,
        ty::Ty::Bool => ty::Ty::Bool,
        ty::Ty::Char => ty::Ty::Char,
        ty::Ty::Float => ty::Ty::Float,
        ty::Ty::Int => ty::Ty::Int,
        ty::Ty::Nil => ty::Ty::Nil,
        ty::Ty::Str => ty::Ty::Str,
        ty::Ty::Sym => ty::Ty::Sym,
        ty::Ty::Fun(ref fun) => ty::Ty::new_fun(
            fun.impure,
            // Once we subst we should have no type variables
            ty::PVarId::new(0)..ty::PVarId::new(0),
            subst(&fun.params, pvars)?,
            subst(&fun.ret, pvars)?,
        ),
        ty::Ty::TyPred(ref test_ty) => ty::Ty::TyPred(Box::new(subst(test_ty, pvars)?)),
        ty::Ty::Map(ref key, ref value) => {
            ty::Ty::Map(Box::new(subst(key, pvars)?), Box::new(subst(value, pvars)?))
        }
        ty::Ty::LitBool(val) => ty::Ty::LitBool(val),
        ty::Ty::LitSym(ref val) => ty::Ty::LitSym(val.clone()),
        ty::Ty::Set(ref member) => ty::Ty::Set(Box::new(subst(&member, pvars)?)),
        ty::Ty::Union(ref members) => ty::Ty::Union(subst_ty_slice(members, pvars)?),
        ty::Ty::Vec(ref members) => {
            let members_mono = subst_ty_slice(members, pvars)?;
            ty::Ty::Vec(members_mono)
        }
        ty::Ty::Vecof(ref member) => ty::Ty::Vecof(Box::new(subst(member, pvars)?)),
        ty::Ty::Listof(ref member) => ty::Ty::Listof(Box::new(subst(member, pvars)?)),
        ty::Ty::Cons(ref car, ref cdr) => {
            ty::Ty::Cons(Box::new(subst(car, pvars)?), Box::new(subst(cdr, pvars)?))
        }
    })
}

pub fn subst(poly: &ty::Poly, pvars: &HashMap<ty::PVarId, ty::Mono>) -> Result<ty::Mono> {
    match *poly {
        ty::Poly::Fixed(ref fixed) => subst_ty(fixed, pvars).map(|t| t.into_mono()),
        ty::Poly::Var(pvar_id) => pvars
            .get(&pvar_id)
            .cloned()
            .ok_or_else(|| Error::Unresolved(pvar_id)),
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
