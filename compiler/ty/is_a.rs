use crate::ty;
use crate::ty::list_iter::ListIterator;
use crate::ty::purity;
use crate::ty::purity::Purity;

#[derive(Clone, Copy, PartialEq, Debug)]
pub enum Result {
    /// All values of the subtype satisfy the parent type
    Yes,
    /// Some values of the subtype satisfy the parent type
    May,
    /// None of the values of the subtype satisfy the parent type
    No,
}

impl Result {
    pub fn to_bool(self) -> bool {
        self == Result::Yes
    }

    fn and_then<F>(self, op: F) -> Result
    where
        F: FnOnce() -> Result,
    {
        match self {
            Result::Yes => op(),
            Result::May => {
                if op() == Result::No {
                    Result::No
                } else {
                    Result::May
                }
            }
            Result::No => Result::No,
        }
    }

    fn from_iter<I>(mut iter: I) -> Result
    where
        I: Iterator<Item = Result>,
    {
        let mut best_result = Result::Yes;

        loop {
            match iter.next() {
                Some(Result::Yes) => {}
                Some(Result::May) => {
                    best_result = Result::May;
                }
                Some(Result::No) => {
                    return Result::No;
                }
                None => return best_result,
            }
        }
    }
}

pub trait Isable: ty::TyRef {
    fn ty_ref_is_a(tvars: &ty::TVars, sub: &Self, parent: &Self) -> Result;
}

impl Isable for ty::Mono {
    fn ty_ref_is_a(tvars: &ty::TVars, sub: &ty::Mono, parent: &ty::Mono) -> Result {
        ty_is_a(tvars, sub, sub.as_ty(), parent, parent.as_ty())
    }
}

impl Isable for ty::Poly {
    fn ty_ref_is_a(tvars: &ty::TVars, sub: &ty::Poly, parent: &ty::Poly) -> Result {
        if let ty::Poly::Var(parent_tvar_id) = *parent {
            if let ty::Poly::Var(sub_tvar_id) = *sub {
                if tvar_id_is_bounded_by(tvars, sub_tvar_id, parent_tvar_id) {
                    return Result::Yes;
                }
            }
        }

        let sub_ty = ty::resolve::resolve_poly_ty(tvars, sub).as_ty();
        if sub_ty == &ty::Ty::never() {
            // (U) is a definite subtype of every type, regardless if the parent is bound. This is
            // important as (U) is used as a placeholder for parameters with unknown type. More
            // generally, it's the contravariant equivalent of Any.
            return Result::Yes;
        }

        let (parent_ty, parent_is_bound) = match ty::resolve::resolve_poly_ty(tvars, parent) {
            ty::resolve::Result::Bound(ty) => (ty, true),
            ty::resolve::Result::Fixed(ty) => (ty, false),
        };

        let result = ty_is_a(tvars, sub, sub_ty, parent, parent_ty);

        if parent_is_bound {
            // The parent is polymorphic and has child types. We can't ensure that the sub
            // satisfies all child types of the parent bound.
            Result::May.and_then(|| result)
        } else {
            result
        }
    }
}

fn top_fun_is_a(tvars: &ty::TVars, sub_top_fun: &ty::TopFun, par_top_fun: &ty::TopFun) -> Result {
    purity_ref_is_a(sub_top_fun.purity(), par_top_fun.purity())
        .and_then(|| ty_ref_is_a(tvars, sub_top_fun.ret(), par_top_fun.ret()))
}

fn list_is_a<S: Isable>(
    tvars: &ty::TVars,
    sub_list: &ty::List<S>,
    par_list: &ty::List<S>,
) -> Result {
    let mut sub_iter = ListIterator::new(sub_list);
    let mut par_iter = ListIterator::new(par_list);

    let mut is_exact = (sub_iter.fixed_len() >= par_iter.fixed_len())
        && !(sub_list.rest().is_some() && par_list.rest().is_none());

    // Compare our fixed types. If one of the fixed lists ends early then its rest will be used
    // for the remaining list
    while sub_iter.fixed_len() > 0 || par_iter.fixed_len() > 0 {
        match (sub_iter.next(), par_iter.next()) {
            (Some(sub), Some(par)) => match ty_ref_is_a(tvars, sub, par) {
                Result::Yes => {}
                Result::May => {
                    is_exact = false;
                }
                Result::No => {
                    // Member type does not match
                    return Result::No;
                }
            },
            (None, None) => {
                // Fixed lists ended at the same length
                break;
            }
            (_, _) => {
                // Fixed lists ended at different lengths
                return Result::No;
            }
        };
    }

    if let (Some(sub_rest), Some(par_rest)) = (sub_list.rest(), par_list.rest()) {
        // If the rest doesn't match it's a May as the rest might not be present
        if ty_ref_is_a(tvars, sub_rest, par_rest) != Result::Yes {
            is_exact = false;
        }
    }

    if is_exact {
        Result::Yes
    } else {
        Result::May
    }
}

fn monomorphic_fun_is_a(tvars: &ty::TVars, sub_fun: &ty::Fun, par_fun: &ty::Fun) -> Result {
    top_fun_is_a(tvars, sub_fun.top_fun(), par_fun.top_fun())
        // Note that parameters are contravariant
        .and_then(|| list_is_a(tvars, par_fun.params(), sub_fun.params()))
}

fn fun_is_a(tvars: &ty::TVars, sub_fun: &ty::Fun, par_fun: &ty::Fun) -> Result {
    if sub_fun.has_polymorphic_vars() {
        let inner_tvars = ty::merge_three_tvars(tvars, sub_fun.tvars(), par_fun.tvars());
        let sub_mono = inst_polymorphic_fun(&inner_tvars, sub_fun, par_fun.ret());

        monomorphic_fun_is_a(&inner_tvars, &sub_mono, par_fun)
    } else {
        monomorphic_fun_is_a(tvars, sub_fun, par_fun)
    }
}

fn ty_is_a<S: Isable>(
    tvars: &ty::TVars,
    sub_ref: &S,
    sub_ty: &ty::Ty<S>,
    parent_ref: &S,
    parent_ty: &ty::Ty<S>,
) -> Result {
    if sub_ty == parent_ty {
        return Result::Yes;
    }

    match (sub_ty, parent_ty) {
        // Union types
        (ty::Ty::Union(sub_members), _) => {
            let results = sub_members
                .iter()
                .map(|sub_member| ty_ref_is_a(tvars, sub_member, parent_ref));

            let mut contains_yes = false;
            let mut contains_no = false;
            for result in results {
                match result {
                    Result::Yes => {
                        if contains_no {
                            return Result::May;
                        }
                        contains_yes = true;
                    }
                    Result::May => {
                        return Result::May;
                    }
                    Result::No => {
                        if contains_yes {
                            return Result::May;
                        }
                        contains_no = true;
                    }
                }
            }

            if !contains_no {
                Result::Yes
            } else if !contains_yes {
                Result::No
            } else {
                Result::May
            }
        }
        (_, ty::Ty::Union(par_members)) => {
            let results = par_members
                .iter()
                .map(|par_member| ty_ref_is_a(tvars, sub_ref, par_member));

            // Manually consume the iterator to avoid building a temporary Vec and to let us bail
            // early on Yes
            let mut best_result = Result::No;
            for result in results {
                if result == Result::Yes {
                    return Result::Yes;
                } else if result == Result::May {
                    best_result = Result::May;
                }
            }

            best_result
        }

        // Any type
        (_, ty::Ty::Any) => Result::Yes,
        (ty::Ty::Any, _) => Result::May,

        // Sym types
        (ty::Ty::LitSym(_), ty::Ty::Sym) => Result::Yes,
        (ty::Ty::Sym, ty::Ty::LitSym(_)) => Result::May,

        // Bool types
        (ty::Ty::LitBool(_), ty::Ty::Bool) => Result::Yes,
        (ty::Ty::Bool, ty::Ty::LitBool(_)) => Result::May,

        // Sets
        (ty::Ty::Set(sub), ty::Ty::Set(par)) => ty_ref_is_a(tvars, sub.as_ref(), par.as_ref()),

        // Maps
        (ty::Ty::Map(sub_map), ty::Ty::Map(par_map)) => {
            ty_ref_is_a(tvars, sub_map.key(), par_map.key())
                .and_then(|| ty_ref_is_a(tvars, sub_map.value(), par_map.value()))
        }

        // Vector types
        (ty::Ty::Vector(sub_members), ty::Ty::Vector(par_members)) => {
            if sub_members.len() != par_members.len() {
                Result::No
            } else {
                Result::from_iter(
                    sub_members
                        .iter()
                        .zip(par_members.iter())
                        .map(|(sub_member, par_member)| ty_ref_is_a(tvars, sub_member, par_member)),
                )
            }
        }
        (ty::Ty::Vectorof(sub_member), ty::Ty::Vectorof(par_member)) => {
            ty_ref_is_a(tvars, sub_member.as_ref(), par_member.as_ref())
        }
        (ty::Ty::Vector(sub_members), ty::Ty::Vectorof(par_member)) => Result::from_iter(
            sub_members
                .iter()
                .map(|sub_member| ty_ref_is_a(tvars, sub_member, par_member)),
        ),
        (ty::Ty::Vectorof(sub_member), ty::Ty::Vector(par_members)) => Result::May.and_then(|| {
            Result::from_iter(
                par_members
                    .iter()
                    .map(|par_member| ty_ref_is_a(tvars, sub_member.as_ref(), par_member)),
            )
        }),

        // Functions
        (ty::Ty::TopFun(sub_top_fun), ty::Ty::TopFun(par_top_fun)) => {
            top_fun_is_a(tvars, sub_top_fun, par_top_fun)
        }
        (ty::Ty::Fun(sub_fun), ty::Ty::TopFun(par_top_fun)) => {
            if sub_fun.has_polymorphic_vars() {
                let inner_tvars = ty::merge_tvars(tvars, sub_fun.tvars());
                let sub_mono = inst_polymorphic_fun(&inner_tvars, sub_fun, par_top_fun.ret());
                top_fun_is_a(&inner_tvars, sub_mono.top_fun(), par_top_fun)
            } else {
                top_fun_is_a(tvars, sub_fun.top_fun(), par_top_fun)
            }
        }
        (ty::Ty::TopFun(sub_top_fun), ty::Ty::Fun(par_fun)) => Result::May.and_then(|| {
            let inner_tvars = ty::merge_tvars(tvars, par_fun.tvars());
            top_fun_is_a(&inner_tvars, sub_top_fun, par_fun.top_fun())
        }),
        (ty::Ty::Fun(sub_fun), ty::Ty::Fun(par_fun)) => fun_is_a(tvars, sub_fun, par_fun),

        // All predicate types
        (ty::Ty::TyPred(_), ty::Ty::TopFun(par_top_fun))
        | (ty::Ty::EqPred, ty::Ty::TopFun(par_top_fun)) => {
            top_fun_is_a(tvars, &ty::TopFun::new_for_pred(), par_top_fun)
        }
        (ty::Ty::TopFun(sub_top_fun), ty::Ty::TyPred(_))
        | (ty::Ty::TopFun(sub_top_fun), ty::Ty::EqPred) => {
            Result::May.and_then(|| top_fun_is_a(tvars, sub_top_fun, &ty::TopFun::new_for_pred()))
        }

        // Type predicate types
        (ty::Ty::TyPred(_), ty::Ty::Fun(par_fun)) => {
            fun_is_a(tvars, &ty::Fun::new_for_ty_pred(), par_fun)
        }
        (ty::Ty::Fun(sub_fun), ty::Ty::TyPred(_)) => {
            Result::May.and_then(|| fun_is_a(tvars, sub_fun, &ty::Fun::new_for_ty_pred()))
        }

        // Equality predicate type
        (ty::Ty::EqPred, ty::Ty::Fun(par_fun)) => {
            fun_is_a(tvars, &ty::Fun::new_for_eq_pred(), par_fun)
        }
        (ty::Ty::Fun(sub_fun), ty::Ty::EqPred) => {
            Result::May.and_then(|| fun_is_a(tvars, sub_fun, &ty::Fun::new_for_eq_pred()))
        }

        // List types
        (ty::Ty::List(sub_list), ty::Ty::List(par_list)) => list_is_a(tvars, sub_list, par_list),

        _ => Result::No,
    }
}

fn tvar_id_is_bounded_by(
    tvars: &ty::TVars,
    sub_tvar_id: ty::TVarId,
    parent_tvar_id: ty::TVarId,
) -> bool {
    if sub_tvar_id == parent_tvar_id {
        return true;
    }

    match tvars[&sub_tvar_id].bound {
        ty::Poly::Fixed(_) => false,
        ty::Poly::Var(tvar_id) => tvar_id_is_bounded_by(tvars, tvar_id, parent_tvar_id),
    }
}

fn purity_ref_is_a(sub: &purity::Poly, parent: &purity::Poly) -> Result {
    if sub == parent {
        return Result::Yes;
    }

    match (sub, parent) {
        (_, purity::Poly::Fixed(Purity::Impure)) => Result::Yes,
        (purity::Poly::Fixed(Purity::Impure), &purity::Poly::Fixed(Purity::Pure)) => Result::No,
        _ => Result::May,
    }
}

fn inst_polymorphic_fun(tvars: &ty::TVars, sub_fun: &ty::Fun, par_ret: &ty::Poly) -> ty::Fun {
    let mut stx = ty::select::SelectCtx::new(tvars, sub_fun.pvars(), sub_fun.tvars());

    stx.add_evidence(sub_fun.ret(), par_ret);
    let pta = stx.into_poly_ty_args();

    ty::subst::subst_poly_fun(&pta, sub_fun)
}

pub fn ty_ref_is_a<S: Isable>(tvars: &ty::TVars, sub: &S, parent: &S) -> Result {
    S::ty_ref_is_a(tvars, sub, parent)
}

#[cfg(test)]
mod test {
    use super::*;

    fn poly_for_str(datum_str: &str) -> ty::Poly {
        use crate::hir;
        hir::poly_for_str(datum_str)
    }

    #[test]
    fn sym_types() {
        let foo_sym = poly_for_str("'foo");
        let bar_sym = poly_for_str("'bar");
        let any_sym = poly_for_str("Sym");
        let any_int = poly_for_str("Int");

        assert_eq!(
            Result::Yes,
            ty_ref_is_a(&ty::TVars::new(), &foo_sym, &foo_sym)
        );
        assert_eq!(
            Result::No,
            ty_ref_is_a(&ty::TVars::new(), &foo_sym, &bar_sym)
        );

        assert_eq!(
            Result::Yes,
            ty_ref_is_a(&ty::TVars::new(), &foo_sym, &any_sym)
        );
        assert_eq!(
            Result::May,
            ty_ref_is_a(&ty::TVars::new(), &any_sym, &foo_sym)
        );

        assert_eq!(
            Result::No,
            ty_ref_is_a(&ty::TVars::new(), &any_sym, &any_int)
        );
        assert_eq!(
            Result::No,
            ty_ref_is_a(&ty::TVars::new(), &any_int, &any_sym)
        );
    }

    #[test]
    fn set_types() {
        let foo_set = poly_for_str("(Setof 'foo)");
        let bar_set = poly_for_str("(Setof 'bar)");
        let sym_set = poly_for_str("(Setof Sym)");

        assert_eq!(
            Result::Yes,
            ty_ref_is_a(&ty::TVars::new(), &foo_set, &foo_set)
        );
        assert_eq!(
            Result::No,
            ty_ref_is_a(&ty::TVars::new(), &foo_set, &bar_set)
        );

        assert_eq!(
            Result::Yes,
            ty_ref_is_a(&ty::TVars::new(), &foo_set, &sym_set)
        );
        assert_eq!(
            Result::May,
            ty_ref_is_a(&ty::TVars::new(), &sym_set, &foo_set)
        );
    }

    #[test]
    fn map_types() {
        let foo_sym = poly_for_str("'foo");
        let any_sym = poly_for_str("Sym");
        let any_int = poly_for_str("Int");

        let int_to_any_sym =
            ty::Ty::Map(Box::new(ty::Map::new(any_int.clone(), any_sym.clone()))).into_poly();
        let int_to_foo_sym =
            ty::Ty::Map(Box::new(ty::Map::new(any_int.clone(), foo_sym.clone()))).into_poly();
        let any_sym_to_any_sym =
            ty::Ty::Map(Box::new(ty::Map::new(any_sym.clone(), any_sym.clone()))).into_poly();

        assert_eq!(
            Result::Yes,
            ty_ref_is_a(&ty::TVars::new(), &int_to_foo_sym, &int_to_any_sym)
        );
        assert_eq!(
            Result::May,
            ty_ref_is_a(&ty::TVars::new(), &int_to_any_sym, &int_to_foo_sym)
        );
        assert_eq!(
            Result::No,
            ty_ref_is_a(&ty::TVars::new(), &int_to_any_sym, &any_sym_to_any_sym)
        );
    }

    #[test]
    fn union_types() {
        let foo_sym = poly_for_str("'foo");
        let baz_sym = poly_for_str("'baz");

        let foo_bar_union = poly_for_str("(RawU 'foo 'bar)");
        let bar_baz_union = poly_for_str("(RawU 'bar 'baz)");
        let foo_bar_baz_union = poly_for_str("(RawU 'foo 'bar 'baz)");
        let never = poly_for_str("(RawU)");

        assert_eq!(
            Result::Yes,
            ty_ref_is_a(&ty::TVars::new(), &foo_sym, &foo_bar_union)
        );
        assert_eq!(
            Result::No,
            ty_ref_is_a(&ty::TVars::new(), &baz_sym, &foo_bar_union)
        );
        assert_eq!(Result::No, ty_ref_is_a(&ty::TVars::new(), &baz_sym, &never));

        assert_eq!(
            Result::May,
            ty_ref_is_a(&ty::TVars::new(), &foo_bar_union, &foo_sym)
        );
        assert_eq!(
            Result::No,
            ty_ref_is_a(&ty::TVars::new(), &foo_bar_union, &baz_sym)
        );
        assert_eq!(
            Result::Yes,
            ty_ref_is_a(&ty::TVars::new(), &never, &foo_sym)
        );

        assert_eq!(
            Result::May,
            ty_ref_is_a(&ty::TVars::new(), &foo_bar_union, &bar_baz_union)
        );
        assert_eq!(
            Result::Yes,
            ty_ref_is_a(&ty::TVars::new(), &foo_bar_union, &foo_bar_union)
        );
        assert_eq!(
            Result::Yes,
            ty_ref_is_a(&ty::TVars::new(), &never, &foo_bar_union)
        );

        assert_eq!(
            Result::Yes,
            ty_ref_is_a(&ty::TVars::new(), &foo_bar_union, &foo_bar_baz_union)
        );
    }

    #[test]
    fn any_and_never_types() {
        let any = poly_for_str("Any");
        let never = ty::Ty::never().into_poly();
        let foo_sym = poly_for_str("'foo");

        assert_eq!(Result::Yes, ty_ref_is_a(&ty::TVars::new(), &foo_sym, &any));
        assert_eq!(Result::May, ty_ref_is_a(&ty::TVars::new(), &any, &foo_sym));
        assert_eq!(Result::Yes, ty_ref_is_a(&ty::TVars::new(), &never, &any));
        assert_eq!(Result::Yes, ty_ref_is_a(&ty::TVars::new(), &never, &never));
        assert_eq!(Result::No, ty_ref_is_a(&ty::TVars::new(), &any, &never));
    }

    #[test]
    fn list_types() {
        let empty_list = poly_for_str("()");
        let listof_any = poly_for_str("(Listof Any)");
        let listof_int = poly_for_str("(Listof Int)");
        let two_ints_list = poly_for_str("(List Int Int)");
        let three_ints_list = poly_for_str("(List Int Int Int)");
        let at_least_one_int_list = poly_for_str("(List Int Int ...)");

        assert_eq!(
            Result::Yes,
            ty_ref_is_a(&ty::TVars::new(), &empty_list, &listof_any)
        );
        assert_eq!(
            Result::May,
            ty_ref_is_a(&ty::TVars::new(), &listof_any, &empty_list)
        );

        assert_eq!(
            Result::Yes,
            ty_ref_is_a(&ty::TVars::new(), &listof_int, &listof_any)
        );
        assert_eq!(
            Result::May,
            ty_ref_is_a(&ty::TVars::new(), &listof_any, &listof_int)
        );

        assert_eq!(
            Result::Yes,
            ty_ref_is_a(&ty::TVars::new(), &two_ints_list, &listof_int)
        );
        assert_eq!(
            Result::May,
            ty_ref_is_a(&ty::TVars::new(), &listof_int, &two_ints_list)
        );
        assert_eq!(
            Result::Yes,
            ty_ref_is_a(&ty::TVars::new(), &two_ints_list, &listof_any)
        );

        assert_eq!(
            Result::No,
            ty_ref_is_a(&ty::TVars::new(), &two_ints_list, &three_ints_list)
        );
        assert_eq!(
            Result::No,
            ty_ref_is_a(&ty::TVars::new(), &three_ints_list, &two_ints_list)
        );

        assert_eq!(
            Result::Yes,
            ty_ref_is_a(&ty::TVars::new(), &at_least_one_int_list, &listof_int)
        );
        assert_eq!(
            Result::May,
            ty_ref_is_a(&ty::TVars::new(), &listof_int, &at_least_one_int_list)
        );
    }

    #[test]
    fn vec_types() {
        let vecof_any = poly_for_str("(Vectorof Any)");
        let vecof_int = poly_for_str("(Vectorof Int)");
        let two_ints_vec = poly_for_str("(Vector Int Int)");
        let three_ints_vec = poly_for_str("(Vector Int Int Int)");

        assert_eq!(
            Result::Yes,
            ty_ref_is_a(&ty::TVars::new(), &vecof_int, &vecof_any)
        );
        assert_eq!(
            Result::May,
            ty_ref_is_a(&ty::TVars::new(), &vecof_any, &vecof_int)
        );

        assert_eq!(
            Result::Yes,
            ty_ref_is_a(&ty::TVars::new(), &two_ints_vec, &vecof_int)
        );
        assert_eq!(
            Result::May,
            ty_ref_is_a(&ty::TVars::new(), &vecof_int, &two_ints_vec)
        );
        assert_eq!(
            Result::Yes,
            ty_ref_is_a(&ty::TVars::new(), &two_ints_vec, &vecof_any)
        );

        assert_eq!(
            Result::No,
            ty_ref_is_a(&ty::TVars::new(), &two_ints_vec, &three_ints_vec)
        );
        assert_eq!(
            Result::No,
            ty_ref_is_a(&ty::TVars::new(), &three_ints_vec, &two_ints_vec)
        );
    }

    #[test]
    fn fun_types() {
        let impure_any_to_sym = poly_for_str("(Any ->! Sym)");
        let impure_sym_to_any = poly_for_str("(Sym ->! Any)");
        let impure_sym_to_sym = poly_for_str("(Sym ->! Sym)");
        let pure_sym_to_sym = poly_for_str("(Sym -> Sym)");

        assert_eq!(
            Result::Yes,
            ty_ref_is_a(&ty::TVars::new(), &impure_sym_to_sym, &impure_sym_to_any)
        );
        assert_eq!(
            Result::Yes,
            ty_ref_is_a(&ty::TVars::new(), &impure_any_to_sym, &impure_sym_to_sym)
        );
        assert_eq!(
            Result::May,
            ty_ref_is_a(&ty::TVars::new(), &impure_sym_to_any, &impure_sym_to_sym)
        );

        assert_eq!(
            Result::Yes,
            ty_ref_is_a(&ty::TVars::new(), &pure_sym_to_sym, &impure_sym_to_sym)
        );
        assert_eq!(
            Result::No,
            ty_ref_is_a(&ty::TVars::new(), &impure_sym_to_sym, &pure_sym_to_sym)
        );
    }

    #[test]
    fn ty_pred_types() {
        let sym_ty_pred = poly_for_str("sym?");
        let str_ty_pred = poly_for_str("str?");
        let general_ty_pred = poly_for_str("(Any -> Bool)");
        let pred_top_fun = poly_for_str("(... -> Bool)");

        // Type predicates always equal themselves
        assert_eq!(
            Result::Yes,
            ty_ref_is_a(&ty::TVars::new(), &sym_ty_pred, &sym_ty_pred)
        );

        // Type predicates never equal other type predicates
        assert_eq!(
            Result::No,
            ty_ref_is_a(&ty::TVars::new(), &sym_ty_pred, &str_ty_pred)
        );
        assert_eq!(
            Result::No,
            ty_ref_is_a(&ty::TVars::new(), &str_ty_pred, &sym_ty_pred)
        );

        // Type predicates are a subtype of (Any -> Bool)
        assert_eq!(
            Result::Yes,
            ty_ref_is_a(&ty::TVars::new(), &sym_ty_pred, &general_ty_pred)
        );
        assert_eq!(
            Result::May,
            ty_ref_is_a(&ty::TVars::new(), &general_ty_pred, &sym_ty_pred)
        );

        // Type predicates are a subtype of (... -> Bool)
        assert_eq!(
            Result::Yes,
            ty_ref_is_a(&ty::TVars::new(), &sym_ty_pred, &pred_top_fun)
        );
        assert_eq!(
            Result::May,
            ty_ref_is_a(&ty::TVars::new(), &pred_top_fun, &sym_ty_pred)
        );
    }

    #[test]
    fn eq_pred_type() {
        let eq_pred = poly_for_str("=");
        let general_eq_pred = poly_for_str("(Any Any -> Bool)");
        let pred_top_fun = poly_for_str("(... -> Bool)");

        // Equality predicate equals itself
        assert_eq!(
            Result::Yes,
            ty_ref_is_a(&ty::TVars::new(), &eq_pred, &eq_pred)
        );

        // Equality predicate is a subtype of (Any Any -> Bool)
        assert_eq!(
            Result::Yes,
            ty_ref_is_a(&ty::TVars::new(), &eq_pred, &general_eq_pred)
        );
        assert_eq!(
            Result::May,
            ty_ref_is_a(&ty::TVars::new(), &general_eq_pred, &eq_pred)
        );

        // Equality predicate is a subtype of (... -> Bool)
        assert_eq!(
            Result::Yes,
            ty_ref_is_a(&ty::TVars::new(), &eq_pred, &pred_top_fun)
        );
        assert_eq!(
            Result::May,
            ty_ref_is_a(&ty::TVars::new(), &pred_top_fun, &eq_pred)
        );
    }

    #[test]
    fn bool_types() {
        let true_type = poly_for_str("true");
        let false_type = poly_for_str("false");
        let bool_type = poly_for_str("Bool");

        assert_eq!(
            Result::Yes,
            ty_ref_is_a(&ty::TVars::new(), &true_type, &bool_type)
        );
        assert_eq!(
            Result::May,
            ty_ref_is_a(&ty::TVars::new(), &bool_type, &true_type)
        );
        assert_eq!(
            Result::No,
            ty_ref_is_a(&ty::TVars::new(), &false_type, &true_type)
        );
    }

    #[test]
    fn poly_bool_types() {
        let true_type = poly_for_str("true");
        let false_type = poly_for_str("false");
        let bool_type = poly_for_str("Bool");

        assert_eq!(
            Result::Yes,
            ty_ref_is_a(&ty::TVars::new(), &true_type, &bool_type)
        );
        assert_eq!(
            Result::May,
            ty_ref_is_a(&ty::TVars::new(), &bool_type, &true_type)
        );
        assert_eq!(
            Result::No,
            ty_ref_is_a(&ty::TVars::new(), &false_type, &true_type)
        );
    }

    #[test]
    fn unbounded_poly_vars() {
        let mut tvars = ty::TVars::new();

        let tvar_id1 = ty::TVarId::alloc();
        tvars.insert(
            tvar_id1,
            ty::TVar::new("one".into(), ty::Ty::Any.into_poly()),
        );
        let ptype1 = ty::Poly::Var(tvar_id1);

        let tvar_id2 = ty::TVarId::alloc();
        tvars.insert(
            tvar_id2,
            ty::TVar::new("two".into(), ty::Ty::Any.into_poly()),
        );
        let ptype2 = ty::Poly::Var(tvar_id2);

        let poly_bool = poly_for_str("Bool");

        assert_eq!(Result::Yes, ty_ref_is_a(&tvars, &ptype1, &ptype1));
        assert_eq!(Result::May, ty_ref_is_a(&tvars, &ptype1, &ptype2));
        assert_eq!(Result::May, ty_ref_is_a(&tvars, &ptype1, &poly_bool));
    }

    #[test]
    fn bounded_poly_vars() {
        let mut tvars = ty::TVars::new();

        let tvar_id1 = ty::TVarId::alloc();
        tvars.insert(tvar_id1, ty::TVar::new("sym".into(), poly_for_str("Sym")));
        let sym_ptype = ty::Poly::Var(tvar_id1);

        let tvar_id2 = ty::TVarId::alloc();
        tvars.insert(tvar_id2, ty::TVar::new("str".into(), poly_for_str("Str")));
        let str_ptype = ty::Poly::Var(tvar_id2);

        let poly_foo_sym = poly_for_str("'foo");

        // A type var always satisfies itself
        assert_eq!(Result::Yes, ty_ref_is_a(&tvars, &sym_ptype, &sym_ptype));

        // The bounds of these vars are disjoint
        assert_eq!(Result::No, ty_ref_is_a(&tvars, &sym_ptype, &str_ptype));

        // The type var may satisfy a more specific bound
        assert_eq!(Result::May, ty_ref_is_a(&tvars, &sym_ptype, &poly_foo_sym));

        // A sub never satisfies a type var with a disjoint bound
        assert_eq!(Result::No, ty_ref_is_a(&tvars, &poly_foo_sym, &str_ptype));

        // The sub has a fixed type while the parent has a poly type. We can't ensure that 'foo
        // satisfies all possible Sym subtypes (such as 'bar)
        assert_eq!(Result::May, ty_ref_is_a(&tvars, &poly_foo_sym, &sym_ptype));
    }

    #[test]
    fn related_poly_bounds() {
        let mut tvars = ty::TVars::new();

        let tvar_id1 = ty::TVarId::alloc();
        tvars.insert(tvar_id1, ty::TVar::new("1".into(), poly_for_str("Any")));
        let ptype1_unbounded = ty::Poly::Var(tvar_id1);

        let tvar_id2 = ty::TVarId::alloc();
        tvars.insert(
            tvar_id2,
            ty::TVar::new("2".into(), ptype1_unbounded.clone()),
        );
        let ptype2_bounded_by_1 = ty::Poly::Var(tvar_id2);

        let tvar_id3 = ty::TVarId::alloc();
        tvars.insert(
            tvar_id3,
            ty::TVar::new("3".into(), ptype2_bounded_by_1.clone()),
        );
        let ptype3_bounded_by_2 = ty::Poly::Var(tvar_id3);

        // Direct bounding
        assert_eq!(
            Result::Yes,
            ty_ref_is_a(&tvars, &ptype2_bounded_by_1, &ptype1_unbounded)
        );
        assert_eq!(
            Result::Yes,
            ty_ref_is_a(&tvars, &ptype3_bounded_by_2, &ptype2_bounded_by_1)
        );

        // Commutative bounding
        assert_eq!(
            Result::Yes,
            ty_ref_is_a(&tvars, &ptype3_bounded_by_2, &ptype1_unbounded)
        );

        // Inverse bounding relationship may not satisfy - the bounded type can have arbitrary
        // subtypes
        assert_eq!(
            Result::May,
            ty_ref_is_a(&tvars, &ptype1_unbounded, &ptype2_bounded_by_1)
        );
    }

    #[test]
    fn polymorphic_funs() {
        let pidentity_fun = poly_for_str("(All #{A} A -> A)");
        let pidentity_sym_fun = poly_for_str("(All #{[A : Sym]} A -> A)");
        let pidentity_impure_string_fun = poly_for_str("(All #{[A : Str]} A ->! A)");

        let empty_tvars = ty::TVars::new();

        // All functions should have the top function type
        let top_fun = poly_for_str("(... ->! Any)");
        assert_eq!(
            Result::Yes,
            ty_ref_is_a(&empty_tvars, &pidentity_fun, &top_fun)
        );
        assert_eq!(
            Result::Yes,
            ty_ref_is_a(&empty_tvars, &pidentity_sym_fun, &top_fun)
        );
        assert_eq!(
            Result::Yes,
            ty_ref_is_a(&empty_tvars, &pidentity_impure_string_fun, &top_fun)
        );

        // We should take in to account purity
        let top_pure_fun = poly_for_str("(... -> Any)");
        assert_eq!(
            Result::Yes,
            ty_ref_is_a(&empty_tvars, &pidentity_fun, &top_pure_fun)
        );
        assert_eq!(
            Result::No,
            ty_ref_is_a(&empty_tvars, &pidentity_impure_string_fun, &top_pure_fun)
        );

        // All functions should have the top one param function type except panys
        let top_one_param_fun = poly_for_str("((RawU) ->! Any)");
        assert_eq!(
            Result::Yes,
            ty_ref_is_a(&empty_tvars, &pidentity_fun, &top_one_param_fun)
        );
        assert_eq!(
            Result::Yes,
            ty_ref_is_a(&empty_tvars, &pidentity_sym_fun, &top_one_param_fun)
        );
        assert_eq!(
            Result::Yes,
            ty_ref_is_a(
                &empty_tvars,
                &pidentity_impure_string_fun,
                &top_one_param_fun
            )
        );

        // The identity function is (Any -> Any)
        let any_to_any_fun = poly_for_str("(Any ->! Any)");
        assert_eq!(
            Result::Yes,
            ty_ref_is_a(&empty_tvars, &pidentity_fun, &any_to_any_fun)
        );
        // However, (Any -> Any) is not the identity function because it can take mismatched types
        // (e.g. Int -> Float)
        assert_eq!(
            Result::No,
            ty_ref_is_a(&empty_tvars, &any_to_any_fun, &pidentity_fun)
        );

        // The identity function is (true -> true)
        let true_to_true_fun = poly_for_str("(true ->! true)");
        assert_eq!(
            Result::Yes,
            ty_ref_is_a(&empty_tvars, &pidentity_fun, &true_to_true_fun)
        );
        assert_eq!(
            Result::No,
            ty_ref_is_a(&empty_tvars, &true_to_true_fun, &pidentity_fun)
        );

        // The identity function is not (true -> false)
        let true_to_true_fun = poly_for_str("(true ->! false)");
        assert_eq!(
            Result::No,
            ty_ref_is_a(&empty_tvars, &pidentity_fun, &true_to_true_fun)
        );
        assert_eq!(
            Result::No,
            ty_ref_is_a(&empty_tvars, &true_to_true_fun, &pidentity_fun)
        );

        // The symbol function satisfies ((U) -> Sym) as all of its returns must be bounded by
        // that
        let top_to_sym_fun = poly_for_str("(... ->! Sym)");
        assert_eq!(
            Result::Yes,
            ty_ref_is_a(&empty_tvars, &pidentity_fun, &top_to_sym_fun)
        );
        assert_eq!(
            Result::Yes,
            ty_ref_is_a(&empty_tvars, &pidentity_sym_fun, &top_to_sym_fun)
        );

        // The identity string function satisfies (Str -> Str)
        let str_to_str_fun = poly_for_str("(Str ->! Str)");
        assert_eq!(
            Result::Yes,
            ty_ref_is_a(&empty_tvars, &pidentity_fun, &str_to_str_fun)
        );
        assert_eq!(
            Result::No,
            ty_ref_is_a(&empty_tvars, &pidentity_sym_fun, &str_to_str_fun)
        );
        assert_eq!(
            Result::Yes,
            ty_ref_is_a(&empty_tvars, &pidentity_impure_string_fun, &str_to_str_fun)
        );

        // The polymorphic identity string function satisfies (... ->! Str)
        let top_impure_str_fun = poly_for_str("(... ->! Str)");
        assert_eq!(
            Result::Yes,
            ty_ref_is_a(
                &empty_tvars,
                &pidentity_impure_string_fun,
                &top_impure_str_fun
            )
        );

        // As does the unbounded identity function
        assert_eq!(
            Result::Yes,
            ty_ref_is_a(&empty_tvars, &pidentity_fun, &top_impure_str_fun)
        );

        // But not the polymorphic symbol function
        assert_eq!(
            Result::No,
            ty_ref_is_a(&empty_tvars, &pidentity_sym_fun, &top_impure_str_fun)
        );
    }
}
