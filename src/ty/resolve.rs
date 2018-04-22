use ty;

pub enum Result<'a> {
    Fixed(&'a ty::Ty<ty::Poly>),
    Bound(&'a ty::Ty<ty::Poly>),
}

impl<'a> Result<'a> {
    pub fn as_ty(&self) -> &'a ty::Ty<ty::Poly> {
        match *self {
            Result::Fixed(ty) => ty,
            Result::Bound(ty) => ty,
        }
    }
}

fn poly_ty_has_subtypes(pvars: &[ty::PVar], poly_ty: &ty::Ty<ty::Poly>) -> bool {
    match *poly_ty {
        ty::Ty::Any | ty::Ty::Bool | ty::Ty::Sym => true,
        ty::Ty::Char
        | ty::Ty::Float
        | ty::Ty::Int
        | ty::Ty::LitBool(_)
        | ty::Ty::LitSym(_)
        | ty::Ty::Str
        | ty::Ty::Nil => false,
        ty::Ty::Fun(ref fun) => {
            fun.impure
                || fun.params != ty::Ty::Listof(Box::new(ty::Ty::Any.into_poly())).into_poly()
                || poly_has_subtypes(pvars, &fun.ret)
        }
        ty::Ty::Map(ref key, ref value) => [key, value]
            .iter()
            .any(|poly| poly_has_subtypes(pvars, poly)),
        ty::Ty::Set(ref member) => poly_has_subtypes(pvars, member),
        ty::Ty::Vec(ref members) => members
            .iter()
            .any(|member| poly_has_subtypes(pvars, member)),
        ty::Ty::Union(ref members) => !members.is_empty(),
        ty::Ty::Cons(ref car, ref cdr) => {
            poly_has_subtypes(pvars, car) || poly_has_subtypes(pvars, cdr)
        }
        ty::Ty::Listof(_) | ty::Ty::Vecof(_) => {
            // Any arbitrary fixed length sequence is a subtype of this sequence
            true
        }
    }
}

fn poly_has_subtypes(pvars: &[ty::PVar], poly: &ty::Poly) -> bool {
    let poly_ty = resolve_poly_ty(pvars, poly).as_ty();
    poly_ty_has_subtypes(pvars, poly_ty)
}

fn resolve_pvar_id_bound(pvars: &[ty::PVar], pvar_id: ty::PVarId) -> &ty::Ty<ty::Poly> {
    match pvars[pvar_id.to_usize()].bound {
        ty::Poly::Fixed(ref fixed_ty) => fixed_ty,
        ty::Poly::Var(pvar_id) => resolve_pvar_id_bound(pvars, pvar_id),
    }
}

/// Resolves a Poly to either a fixed type or polymorphic variable's bound
///
/// If a polymorphic variable is bounded by a type with no subtypes (e.g. Str or LitBool) then it's
/// treated as fixed.
pub fn resolve_poly_ty<'a>(pvars: &'a [ty::PVar], poly: &'a ty::Poly) -> Result<'a> {
    match *poly {
        ty::Poly::Fixed(ref ty) => Result::Fixed(ty),
        ty::Poly::Var(pvar_id) => {
            let ty = resolve_pvar_id_bound(pvars, pvar_id);

            if poly_ty_has_subtypes(pvars, ty) {
                Result::Bound(ty)
            } else {
                Result::Fixed(ty)
            }
        }
    }
}

#[cfg(test)]
mod test {
    use super::*;

    fn poly_for_str(datum_str: &str) -> ty::Poly {
        use hir;
        hir::poly_for_str(datum_str).unwrap()
    }

    fn str_has_subtypes(datum_str: &str) -> bool {
        let poly = poly_for_str(datum_str);
        poly_has_subtypes(&[], &poly)
    }

    #[test]
    fn poly_subtypes() {
        assert_eq!(true, str_has_subtypes("Any"));
        assert_eq!(true, str_has_subtypes("Bool"));
        assert_eq!(false, str_has_subtypes("true"));
        assert_eq!(false, str_has_subtypes("Char"));
        assert_eq!(false, str_has_subtypes("Float"));
        assert_eq!(false, str_has_subtypes("String"));
        assert_eq!(true, str_has_subtypes("Symbol"));

        assert_eq!(false, str_has_subtypes("(Any ... -> true)"));
        assert_eq!(true, str_has_subtypes("(Any ... ->! true)"));
        assert_eq!(true, str_has_subtypes("(Any -> true)"));
        assert_eq!(true, str_has_subtypes("(Int ... -> true)"));
        assert_eq!(true, str_has_subtypes("(Any ... -> Any)"));

        assert_eq!(true, str_has_subtypes("(Map Symbol Int)"));
        assert_eq!(false, str_has_subtypes("(Map Float Int)"));

        assert_eq!(true, str_has_subtypes("(List Symbol Int)"));
        assert_eq!(true, str_has_subtypes("(List String Int ...)"));
        assert_eq!(false, str_has_subtypes("(List String Int)"));

        assert_eq!(true, str_has_subtypes("(Setof Symbol)"));
        assert_eq!(false, str_has_subtypes("(Setof Float)"));

        assert_eq!(true, str_has_subtypes("(Vectorof false)"));
        assert_eq!(false, str_has_subtypes("(Vector false true)"));

        assert_eq!(
            false,
            poly_has_subtypes(&[], &ty::Ty::Union(vec![]).into_poly())
        );
    }
}
