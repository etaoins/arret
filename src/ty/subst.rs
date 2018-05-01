use std::collections::HashMap;
use std::result;

use ty;

#[derive(Debug, PartialEq)]
pub enum Error {
    Unresolved(ty::TVarId),
}

pub type Result<T> = result::Result<T, Error>;

fn subst_ty_slice(
    fixed: &[ty::Poly],
    tvars: &HashMap<ty::TVarId, ty::Mono>,
) -> Result<Vec<ty::Mono>> {
    fixed
        .iter()
        .map(|p| subst(p, tvars))
        .collect::<Result<Vec<ty::Mono>>>()
}

fn subst_ty(
    fixed: &ty::Ty<ty::Poly>,
    tvars: &HashMap<ty::TVarId, ty::Mono>,
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
            ty::TVarIds::empty(),
            subst(&fun.params, tvars)?,
            subst(&fun.ret, tvars)?,
        ),
        ty::Ty::TyPred(ref test_ty) => ty::Ty::TyPred(Box::new(subst(test_ty, tvars)?)),
        ty::Ty::Map(ref key, ref value) => {
            ty::Ty::Map(Box::new(subst(key, tvars)?), Box::new(subst(value, tvars)?))
        }
        ty::Ty::LitBool(val) => ty::Ty::LitBool(val),
        ty::Ty::LitSym(ref val) => ty::Ty::LitSym(val.clone()),
        ty::Ty::Set(ref member) => ty::Ty::Set(Box::new(subst(&member, tvars)?)),
        ty::Ty::Union(ref members) => ty::Ty::Union(subst_ty_slice(members, tvars)?),
        ty::Ty::Vec(ref members) => {
            let members_mono = subst_ty_slice(members, tvars)?;
            ty::Ty::Vec(members_mono)
        }
        ty::Ty::Vecof(ref member) => ty::Ty::Vecof(Box::new(subst(member, tvars)?)),
        ty::Ty::Listof(ref member) => ty::Ty::Listof(Box::new(subst(member, tvars)?)),
        ty::Ty::Cons(ref car, ref cdr) => {
            ty::Ty::Cons(Box::new(subst(car, tvars)?), Box::new(subst(cdr, tvars)?))
        }
    })
}

pub fn subst(poly: &ty::Poly, tvars: &HashMap<ty::TVarId, ty::Mono>) -> Result<ty::Mono> {
    match *poly {
        ty::Poly::Fixed(ref fixed) => subst_ty(fixed, tvars).map(|t| t.into_mono()),
        ty::Poly::Var(tvar_id) => tvars
            .get(&tvar_id)
            .cloned()
            .ok_or_else(|| Error::Unresolved(tvar_id)),
    }
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn subst_tvar() {
        let tvar_id = ty::TVarId::new(1);

        let type_var = ty::Poly::Var(tvar_id);
        let mut tvars = HashMap::new();
        tvars.insert(tvar_id, ty::Ty::Int.into_mono());

        let expected = ty::Ty::Int.into_mono();
        assert_eq!(expected, subst(&type_var, &tvars).unwrap());
    }

    #[test]
    fn subst_tvar_unresolved() {
        let tvar_id = ty::TVarId::new(1);

        let poly_var = ty::Poly::Var(tvar_id);
        let tvars = HashMap::new();
        // Do not include the tvar

        let err = Error::Unresolved(tvar_id);
        assert_eq!(err, subst(&poly_var, &tvars).unwrap_err());
    }

    #[test]
    fn subst_set() {
        let tvar_id = ty::TVarId::new(1);

        let poly_set = ty::Ty::Set(Box::new(ty::Poly::Var(tvar_id))).into_poly();
        let mut tvars = HashMap::new();
        tvars.insert(tvar_id, ty::Ty::Int.into_mono());

        let expected = ty::Ty::Set(Box::new(ty::Ty::Int.into_mono())).into_mono();
        assert_eq!(expected, subst(&poly_set, &tvars).unwrap());
    }

    #[test]
    fn subst_set_unresolved() {
        let tvar_id = ty::TVarId::new(1);

        let poly_set = ty::Ty::Set(Box::new(ty::Poly::Var(tvar_id))).into_poly();
        let tvars = HashMap::new();
        // Do not include the tvar

        let err = Error::Unresolved(tvar_id);
        assert_eq!(err, subst(&poly_set, &tvars).unwrap_err());
    }
}
