use ty;
use ty::list_iter::ListIterator;
use ty::purity::Purity;

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

    fn and_then<F>(&self, op: F) -> Result
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

trait IsACtx<S>
where
    S: ty::TyRef,
{
    fn ty_ref_is_a(&self, &S, &S) -> Result;
    fn purity_ref_is_a(&self, &S::PRef, &S::PRef) -> Result;

    fn top_fun_is_a(&self, sub_top_fun: &ty::TopFun<S>, par_top_fun: &ty::TopFun<S>) -> Result {
        self.purity_ref_is_a(sub_top_fun.purity(), par_top_fun.purity())
            .and_then(|| self.ty_ref_is_a(sub_top_fun.ret(), par_top_fun.ret()))
    }

    fn list_is_a(&self, sub_list: &ty::List<S>, par_list: &ty::List<S>) -> Result {
        let mut sub_iter = ListIterator::new(sub_list);
        let mut par_iter = ListIterator::new(par_list);

        let mut is_exact = (sub_iter.fixed_len() >= par_iter.fixed_len())
            && !(sub_list.rest().is_some() && par_list.rest().is_none());

        // Compare our fixed types. If one of the fixed lists ends early then its rest will be used
        // for the remaining list
        while sub_iter.fixed_len() > 0 || par_iter.fixed_len() > 0 {
            match (sub_iter.next(), par_iter.next()) {
                (Some(sub), Some(par)) => match self.ty_ref_is_a(sub, par) {
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
            if self.ty_ref_is_a(sub_rest, par_rest) != Result::Yes {
                is_exact = false;
            }
        }

        if is_exact {
            Result::Yes
        } else {
            Result::May
        }
    }

    fn fun_is_a(&self, sub_fun: &ty::Fun<S>, par_fun: &ty::Fun<S>) -> Result {
        self.top_fun_is_a(sub_fun.top_fun(), par_fun.top_fun())
            // Note that parameters are contravariant
            .and_then(|| self.list_is_a(par_fun.params(), sub_fun.params()))
    }

    fn ty_is_a(
        &self,
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
                    .map(|sub_member| self.ty_ref_is_a(sub_member, parent_ref));

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
                    .map(|par_member| self.ty_ref_is_a(sub_ref, par_member));

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

            // Symbol types
            (ty::Ty::LitSym(_), ty::Ty::Sym) => Result::Yes,
            (ty::Ty::Sym, ty::Ty::LitSym(_)) => Result::May,

            // Bool types
            (ty::Ty::LitBool(_), ty::Ty::Bool) => Result::Yes,
            (ty::Ty::Bool, ty::Ty::LitBool(_)) => Result::May,

            // Sets
            (ty::Ty::Set(sub), ty::Ty::Set(par)) => self.ty_ref_is_a(sub, par),

            // Maps
            (ty::Ty::Map(sub_key, sub_value), ty::Ty::Map(par_key, par_value)) => self.ty_ref_is_a(
                sub_key, par_key,
            ).and_then(|| self.ty_ref_is_a(sub_value, par_value)),

            // Vector types
            (ty::Ty::Vec(sub_members), ty::Ty::Vec(par_members)) => {
                if sub_members.len() != par_members.len() {
                    Result::No
                } else {
                    Result::from_iter(
                        sub_members.iter().zip(par_members.iter()).map(
                            |(sub_member, par_member)| self.ty_ref_is_a(sub_member, par_member),
                        ),
                    )
                }
            }
            (ty::Ty::Vecof(sub_member), ty::Ty::Vecof(par_member)) => {
                self.ty_ref_is_a(sub_member, par_member)
            }
            (ty::Ty::Vec(sub_members), ty::Ty::Vecof(par_member)) => Result::from_iter(
                sub_members
                    .iter()
                    .map(|sub_member| self.ty_ref_is_a(sub_member, par_member)),
            ),
            (ty::Ty::Vecof(sub_member), ty::Ty::Vec(par_members)) => Result::May.and_then(|| {
                Result::from_iter(
                    par_members
                        .iter()
                        .map(|par_member| self.ty_ref_is_a(sub_member, par_member)),
                )
            }),

            // Functions
            (ty::Ty::TopFun(sub_top_fun), ty::Ty::TopFun(par_top_fun)) => {
                self.top_fun_is_a(sub_top_fun, par_top_fun)
            }
            (ty::Ty::Fun(sub_fun), ty::Ty::TopFun(par_top_fun)) => {
                self.top_fun_is_a(sub_fun.top_fun(), par_top_fun)
            }
            (ty::Ty::TopFun(sub_top_fun), ty::Ty::Fun(par_fun)) => {
                Result::May.and_then(|| self.top_fun_is_a(sub_top_fun, par_fun.top_fun()))
            }
            (ty::Ty::Fun(sub_fun), ty::Ty::Fun(par_fun)) => self.fun_is_a(sub_fun, par_fun),

            (ty::Ty::TyPred(_), ty::Ty::TopFun(par_top_fun)) => {
                self.top_fun_is_a(&ty::TopFun::new_for_ty_pred(), par_top_fun)
            }
            (ty::Ty::TopFun(sub_top_fun), ty::Ty::TyPred(_)) => Result::May
                .and_then(|| self.top_fun_is_a(sub_top_fun, &ty::TopFun::new_for_ty_pred())),
            (ty::Ty::TyPred(_), ty::Ty::Fun(par_fun)) => {
                self.fun_is_a(&ty::Fun::new_for_ty_pred(), par_fun)
            }
            (ty::Ty::Fun(sub_fun), ty::Ty::TyPred(_)) => {
                Result::May.and_then(|| self.fun_is_a(sub_fun, &ty::Fun::new_for_ty_pred()))
            }

            // List types
            (ty::Ty::List(sub_list), ty::Ty::List(par_list)) => self.list_is_a(sub_list, par_list),

            _ => Result::No,
        }
    }
}

struct PolyIsACtx<'a> {
    tvars: &'a [ty::TVar],
}

impl<'a> PolyIsACtx<'a> {
    fn tvar_id_is_bounded_by(&self, sub_tvar_id: ty::TVarId, parent_tvar_id: ty::TVarId) -> bool {
        if sub_tvar_id == parent_tvar_id {
            return true;
        }

        match self.tvars[sub_tvar_id.to_usize()].bound {
            ty::Poly::Fixed(_) => false,
            ty::Poly::Var(tvar_id) => self.tvar_id_is_bounded_by(tvar_id, parent_tvar_id),
        }
    }
}

impl<'a> IsACtx<ty::Poly> for PolyIsACtx<'a> {
    fn ty_ref_is_a(&self, sub: &ty::Poly, parent: &ty::Poly) -> Result {
        if let ty::Poly::Var(parent_tvar_id) = *parent {
            if let ty::Poly::Var(sub_tvar_id) = *sub {
                if self.tvar_id_is_bounded_by(sub_tvar_id, parent_tvar_id) {
                    return Result::Yes;
                }
            }
        }

        let sub_ty = ty::resolve::resolve_poly_ty(self.tvars, sub).as_ty();
        if sub_ty == &ty::Ty::Union(vec![]) {
            // (U) is a definite subtype of every type, regardless if the parent is bound. This is
            // important as (U) is used as a placeholder for parameters with unknown type. More
            // generally, it's the contravariant equivalent of Any.
            return Result::Yes;
        }

        let (parent_ty, parent_is_bound) = match ty::resolve::resolve_poly_ty(self.tvars, parent) {
            ty::resolve::Result::Bound(ty) => (ty, true),
            ty::resolve::Result::Fixed(ty) => (ty, false),
        };

        let result = self.ty_is_a(sub, sub_ty, parent, parent_ty);

        if parent_is_bound {
            // The parent is polymorphic and has child types. We can't ensure that the sub satisfies
            // all child types of the parent bound.
            Result::May.and_then(|| result)
        } else {
            result
        }
    }

    fn purity_ref_is_a(&self, sub: &ty::purity::Poly, parent: &ty::purity::Poly) -> Result {
        if sub == parent {
            return Result::Yes;
        }

        match (sub, parent) {
            (_, ty::purity::Poly::Fixed(Purity::Impure)) => Result::Yes,
            (ty::purity::Poly::Fixed(Purity::Impure), &ty::purity::Poly::Fixed(Purity::Pure)) => {
                Result::No
            }
            _ => Result::May,
        }
    }
}

pub fn poly_is_a(tvars: &[ty::TVar], sub: &ty::Poly, parent: &ty::Poly) -> Result {
    let ctx = PolyIsACtx { tvars };
    ctx.ty_ref_is_a(sub, parent)
}

struct MonoIsACtx {}

impl<'a> IsACtx<ty::Mono> for MonoIsACtx {
    fn ty_ref_is_a(&self, sub: &ty::Mono, parent: &ty::Mono) -> Result {
        self.ty_is_a(sub, sub.as_ty(), parent, parent.as_ty())
    }

    fn purity_ref_is_a(&self, sub: &Purity, parent: &Purity) -> Result {
        if sub == &Purity::Impure && parent == &Purity::Pure {
            Result::No
        } else {
            Result::Yes
        }
    }
}

pub fn mono_is_a(sub: &ty::Mono, parent: &ty::Mono) -> Result {
    let ctx = MonoIsACtx {};
    ctx.ty_ref_is_a(sub, parent)
}

#[cfg(test)]
mod test {
    use super::*;

    fn poly_for_str(datum_str: &str) -> ty::Poly {
        use hir;
        hir::poly_for_str(datum_str).unwrap()
    }

    #[test]
    fn sym_types() {
        let foo_sym = poly_for_str("'foo");
        let bar_sym = poly_for_str("'bar");
        let any_sym = poly_for_str("Symbol");
        let any_int = poly_for_str("Int");

        assert_eq!(Result::Yes, poly_is_a(&[], &foo_sym, &foo_sym));
        assert_eq!(Result::No, poly_is_a(&[], &foo_sym, &bar_sym));

        assert_eq!(Result::Yes, poly_is_a(&[], &foo_sym, &any_sym));
        assert_eq!(Result::May, poly_is_a(&[], &any_sym, &foo_sym));

        assert_eq!(Result::No, poly_is_a(&[], &any_sym, &any_int));
        assert_eq!(Result::No, poly_is_a(&[], &any_int, &any_sym));
    }

    #[test]
    fn set_types() {
        let foo_set = poly_for_str("(Setof 'foo)");
        let bar_set = poly_for_str("(Setof 'bar)");
        let any_set = poly_for_str("(Setof Symbol)");

        assert_eq!(Result::Yes, poly_is_a(&[], &foo_set, &foo_set));
        assert_eq!(Result::No, poly_is_a(&[], &foo_set, &bar_set));

        assert_eq!(Result::Yes, poly_is_a(&[], &foo_set, &any_set));
        assert_eq!(Result::May, poly_is_a(&[], &any_set, &foo_set));
    }

    #[test]
    fn map_types() {
        let foo_sym = poly_for_str("'foo");
        let any_sym = poly_for_str("Symbol");
        let any_int = poly_for_str("Int");

        let int_to_any_sym =
            ty::Ty::Map(Box::new(any_int.clone()), Box::new(any_sym.clone())).into_poly();
        let int_to_foo_sym =
            ty::Ty::Map(Box::new(any_int.clone()), foo_sym.clone().into()).into_poly();
        let any_sym_to_any_sym =
            ty::Ty::Map(Box::new(any_sym.clone()), Box::new(any_sym.clone())).into_poly();

        assert_eq!(
            Result::Yes,
            poly_is_a(&[], &int_to_foo_sym, &int_to_any_sym)
        );
        assert_eq!(
            Result::May,
            poly_is_a(&[], &int_to_any_sym, &int_to_foo_sym)
        );
        assert_eq!(
            Result::No,
            poly_is_a(&[], &int_to_any_sym, &any_sym_to_any_sym)
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

        assert_eq!(Result::Yes, poly_is_a(&[], &foo_sym, &foo_bar_union));
        assert_eq!(Result::No, poly_is_a(&[], &baz_sym, &foo_bar_union));
        assert_eq!(Result::No, poly_is_a(&[], &baz_sym, &never));

        assert_eq!(Result::May, poly_is_a(&[], &foo_bar_union, &foo_sym));
        assert_eq!(Result::No, poly_is_a(&[], &foo_bar_union, &baz_sym));
        assert_eq!(Result::Yes, poly_is_a(&[], &never, &foo_sym));

        assert_eq!(Result::May, poly_is_a(&[], &foo_bar_union, &bar_baz_union));
        assert_eq!(Result::Yes, poly_is_a(&[], &foo_bar_union, &foo_bar_union));
        assert_eq!(Result::Yes, poly_is_a(&[], &never, &foo_bar_union));

        assert_eq!(
            Result::Yes,
            poly_is_a(&[], &foo_bar_union, &foo_bar_baz_union)
        );
    }

    #[test]
    fn any_and_never_types() {
        let any = poly_for_str("Any");
        let never = ty::Ty::Union(vec![]).into_poly();
        let foo_sym = poly_for_str("'foo");

        assert_eq!(Result::Yes, poly_is_a(&[], &foo_sym, &any));
        assert_eq!(Result::May, poly_is_a(&[], &any, &foo_sym));
        assert_eq!(Result::Yes, poly_is_a(&[], &never, &any));
        assert_eq!(Result::Yes, poly_is_a(&[], &never, &never));
        assert_eq!(Result::No, poly_is_a(&[], &any, &never));
    }

    #[test]
    fn list_types() {
        let empty_list = poly_for_str("()");
        let listof_any = poly_for_str("(Listof Any)");
        let listof_int = poly_for_str("(Listof Int)");
        let two_ints_list = poly_for_str("(List Int Int)");
        let three_ints_list = poly_for_str("(List Int Int Int)");
        let at_least_one_int_list = poly_for_str("(List Int Int ...)");

        assert_eq!(Result::Yes, poly_is_a(&[], &empty_list, &listof_any));
        assert_eq!(Result::May, poly_is_a(&[], &listof_any, &empty_list));

        assert_eq!(Result::Yes, poly_is_a(&[], &listof_int, &listof_any));
        assert_eq!(Result::May, poly_is_a(&[], &listof_any, &listof_int));

        assert_eq!(Result::Yes, poly_is_a(&[], &two_ints_list, &listof_int));
        assert_eq!(Result::May, poly_is_a(&[], &listof_int, &two_ints_list));
        assert_eq!(Result::Yes, poly_is_a(&[], &two_ints_list, &listof_any));

        assert_eq!(Result::No, poly_is_a(&[], &two_ints_list, &three_ints_list));
        assert_eq!(Result::No, poly_is_a(&[], &three_ints_list, &two_ints_list));

        assert_eq!(
            Result::Yes,
            poly_is_a(&[], &at_least_one_int_list, &listof_int)
        );
        assert_eq!(
            Result::May,
            poly_is_a(&[], &listof_int, &at_least_one_int_list)
        );
    }

    #[test]
    fn vec_types() {
        let vecof_any = poly_for_str("(Vectorof Any)");
        let vecof_int = poly_for_str("(Vectorof Int)");
        let two_ints_vec = poly_for_str("(Vector Int Int)");
        let three_ints_vec = poly_for_str("(Vector Int Int Int)");

        assert_eq!(Result::Yes, poly_is_a(&[], &vecof_int, &vecof_any));
        assert_eq!(Result::May, poly_is_a(&[], &vecof_any, &vecof_int));

        assert_eq!(Result::Yes, poly_is_a(&[], &two_ints_vec, &vecof_int));
        assert_eq!(Result::May, poly_is_a(&[], &vecof_int, &two_ints_vec));
        assert_eq!(Result::Yes, poly_is_a(&[], &two_ints_vec, &vecof_any));

        assert_eq!(Result::No, poly_is_a(&[], &two_ints_vec, &three_ints_vec));
        assert_eq!(Result::No, poly_is_a(&[], &three_ints_vec, &two_ints_vec));
    }

    #[test]
    fn fun_types() {
        let impure_any_to_sym = poly_for_str("(Any ->! Symbol)");
        let impure_sym_to_any = poly_for_str("(Symbol ->! Any)");
        let impure_sym_to_sym = poly_for_str("(Symbol ->! Symbol)");
        let pure_sym_to_sym = poly_for_str("(Symbol -> Symbol)");

        assert_eq!(
            Result::Yes,
            poly_is_a(&[], &impure_sym_to_sym, &impure_sym_to_any)
        );
        assert_eq!(
            Result::Yes,
            poly_is_a(&[], &impure_any_to_sym, &impure_sym_to_sym)
        );
        assert_eq!(
            Result::May,
            poly_is_a(&[], &impure_sym_to_any, &impure_sym_to_sym)
        );

        assert_eq!(
            Result::Yes,
            poly_is_a(&[], &pure_sym_to_sym, &impure_sym_to_sym)
        );
        assert_eq!(
            Result::No,
            poly_is_a(&[], &impure_sym_to_sym, &pure_sym_to_sym)
        );
    }

    #[test]
    fn ty_pred_types() {
        let sym_ty_pred = poly_for_str("(Type? Symbol)");
        let lit_sym_ty_pred = poly_for_str("(Type? 'foo)");
        let general_pred = poly_for_str("(Any -> Bool)");
        let pred_top_fun = poly_for_str("(... -> Bool)");

        // Type predicates always equal themselves
        assert_eq!(Result::Yes, poly_is_a(&[], &sym_ty_pred, &sym_ty_pred));

        // Type predicates never equal other type predicates
        assert_eq!(Result::No, poly_is_a(&[], &sym_ty_pred, &lit_sym_ty_pred));
        assert_eq!(Result::No, poly_is_a(&[], &lit_sym_ty_pred, &sym_ty_pred));

        // Type predicates are a subtype of (Any -> Bool)
        assert_eq!(Result::Yes, poly_is_a(&[], &sym_ty_pred, &general_pred));
        assert_eq!(Result::May, poly_is_a(&[], &general_pred, &sym_ty_pred));

        // Type predicates are a subtype of (... -> Bool)
        assert_eq!(Result::Yes, poly_is_a(&[], &sym_ty_pred, &pred_top_fun));
        assert_eq!(Result::May, poly_is_a(&[], &pred_top_fun, &sym_ty_pred));
    }

    #[test]
    fn bool_types() {
        let true_type = poly_for_str("true");
        let false_type = poly_for_str("false");
        let bool_type = poly_for_str("Bool");

        assert_eq!(Result::Yes, poly_is_a(&[], &true_type, &bool_type));
        assert_eq!(Result::May, poly_is_a(&[], &bool_type, &true_type));
        assert_eq!(Result::No, poly_is_a(&[], &false_type, &true_type));
    }

    #[test]
    fn poly_bool_types() {
        let true_type = poly_for_str("true");
        let false_type = poly_for_str("false");
        let bool_type = poly_for_str("Bool");

        assert_eq!(Result::Yes, poly_is_a(&[], &true_type, &bool_type));
        assert_eq!(Result::May, poly_is_a(&[], &bool_type, &true_type));
        assert_eq!(Result::No, poly_is_a(&[], &false_type, &true_type));
    }

    #[test]
    fn unbounded_poly_vars() {
        let tvar_id1 = ty::TVarId::new(0);
        let ptype1 = ty::Poly::Var(tvar_id1);

        let tvar_id2 = ty::TVarId::new(1);
        let ptype2 = ty::Poly::Var(tvar_id2);

        let poly_bool = poly_for_str("Bool");

        let tvars = [
            ty::TVar::new("one".to_owned(), ty::Ty::Any.into_poly()),
            ty::TVar::new("two".to_owned(), ty::Ty::Any.into_poly()),
        ];

        assert_eq!(Result::Yes, poly_is_a(&tvars, &ptype1, &ptype1));
        assert_eq!(Result::May, poly_is_a(&tvars, &ptype1, &ptype2));
        assert_eq!(Result::May, poly_is_a(&tvars, &ptype1, &poly_bool));
    }

    #[test]
    fn bounded_poly_vars() {
        let sym_ptype = ty::Poly::Var(ty::TVarId::new(0));
        let str_ptype = ty::Poly::Var(ty::TVarId::new(1));
        let foo_sym_ptype = ty::Poly::Var(ty::TVarId::new(2));

        let tvars = [
            ty::TVar::new("sym".to_owned(), poly_for_str("Symbol")),
            ty::TVar::new("str".to_owned(), poly_for_str("String")),
            ty::TVar::new("foo".to_owned(), poly_for_str("'foo")),
        ];

        let poly_foo_sym = poly_for_str("'foo");
        let any = poly_for_str("Any");

        // A type var always satisfies itself
        assert_eq!(Result::Yes, poly_is_a(&tvars, &sym_ptype, &sym_ptype));

        // The bounds of these vars are disjoint
        assert_eq!(Result::No, poly_is_a(&tvars, &sym_ptype, &str_ptype));

        // The type var may satisfy a more specific bound
        assert_eq!(Result::May, poly_is_a(&tvars, &sym_ptype, &poly_foo_sym));

        // A sub never satisfies a type var with a disjoint bound
        assert_eq!(Result::No, poly_is_a(&tvars, &poly_foo_sym, &str_ptype));

        // The sub has a fixed type while the parent has a poly type. We can't ensure that 'foo
        // satisfies all possible Symbol subtypes (such as 'bar)
        assert_eq!(Result::May, poly_is_a(&tvars, &poly_foo_sym, &sym_ptype));

        // Both the sub and parent have fixed types by virtue of having no subtypes
        assert_eq!(
            Result::Yes,
            poly_is_a(&tvars, &poly_foo_sym, &foo_sym_ptype)
        );

        // The sub has a poly type while the parent has a fixed type. If the sub satisfies the
        // parent type so should all of its subtypes.
        assert_eq!(Result::Yes, poly_is_a(&tvars, &sym_ptype, &any));
    }

    #[test]
    fn related_poly_bounds() {
        let ptype1_unbounded = ty::Poly::Var(ty::TVarId::new(0));
        let ptype2_bounded_by_1 = ty::Poly::Var(ty::TVarId::new(1));
        let ptype3_bounded_by_2 = ty::Poly::Var(ty::TVarId::new(2));

        let tvars = [
            ty::TVar::new("1".to_owned(), poly_for_str("Any")),
            ty::TVar::new("2".to_owned(), ptype1_unbounded.clone()),
            ty::TVar::new("3".to_owned(), ptype2_bounded_by_1.clone()),
        ];

        // Direct bounding
        assert_eq!(
            Result::Yes,
            poly_is_a(&tvars, &ptype2_bounded_by_1, &ptype1_unbounded)
        );
        assert_eq!(
            Result::Yes,
            poly_is_a(&tvars, &ptype3_bounded_by_2, &ptype2_bounded_by_1)
        );

        // Commutative bounding
        assert_eq!(
            Result::Yes,
            poly_is_a(&tvars, &ptype3_bounded_by_2, &ptype1_unbounded)
        );

        // Inverse bounding relationship may not satisfy - the bounded type can have arbitrary
        // subtypes
        assert_eq!(
            Result::May,
            poly_is_a(&tvars, &ptype1_unbounded, &ptype2_bounded_by_1)
        );
    }

    #[test]
    fn polymorphic_funs() {
        let ptype1_unbounded = ty::Poly::Var(ty::TVarId::new(0));
        let ptype2_symbol = ty::Poly::Var(ty::TVarId::new(1));
        let ptype3_string = ty::Poly::Var(ty::TVarId::new(2));

        let tvars = [
            ty::TVar::new("TAny".to_owned(), poly_for_str("Any")),
            ty::TVar::new("TSymbol".to_owned(), poly_for_str("Symbol")),
            ty::TVar::new("TString".to_owned(), poly_for_str("String")),
        ];

        // (All A (A -> A))
        let pidentity_fun = ty::Fun::new(
            ty::purity::PVarIds::monomorphic(),
            ty::TVarId::new(0)..ty::TVarId::new(1),
            ty::TopFun::new(Purity::Pure.into_poly(), ptype1_unbounded.clone()),
            ty::List::new(vec![ptype1_unbounded.clone()], None),
        ).into_ty_ref();

        // (All [A : Symbol] (A -> A))
        let pidentity_sym_fun = ty::Fun::new(
            ty::purity::PVarIds::monomorphic(),
            ty::TVarId::new(1)..ty::TVarId::new(2),
            ty::TopFun::new(Purity::Pure.into_poly(), ptype2_symbol.clone()),
            ty::List::new(vec![ptype2_symbol.clone()], None),
        ).into_ty_ref();

        // (All [A : String] (A ->! A))
        let pidentity_impure_string_fun = ty::Fun::new(
            ty::purity::PVarIds::monomorphic(),
            ty::TVarId::new(2)..ty::TVarId::new(3),
            ty::TopFun::new(Purity::Impure.into_poly(), ptype3_string.clone()),
            ty::List::new(vec![ptype3_string.clone()], None),
        ).into_ty_ref();

        // All functions should have the top function type
        let top_fun = poly_for_str("(... ->! Any)");
        assert_eq!(Result::Yes, poly_is_a(&tvars, &pidentity_fun, &top_fun));
        assert_eq!(Result::Yes, poly_is_a(&tvars, &pidentity_sym_fun, &top_fun));
        assert_eq!(
            Result::Yes,
            poly_is_a(&tvars, &pidentity_impure_string_fun, &top_fun)
        );

        // We should take in to account purity
        let top_pure_fun = poly_for_str("(... -> Any)");
        assert_eq!(
            Result::Yes,
            poly_is_a(&tvars, &pidentity_fun, &top_pure_fun)
        );
        assert_eq!(
            Result::No,
            poly_is_a(&tvars, &pidentity_impure_string_fun, &top_pure_fun)
        );

        // All functions should have the top one param function type except panys
        let top_one_param_fun = poly_for_str("((RawU) ->! Any)");
        assert_eq!(
            Result::Yes,
            poly_is_a(&tvars, &pidentity_fun, &top_one_param_fun)
        );
        assert_eq!(
            Result::Yes,
            poly_is_a(&tvars, &pidentity_sym_fun, &top_one_param_fun)
        );
        assert_eq!(
            Result::Yes,
            poly_is_a(&tvars, &pidentity_impure_string_fun, &top_one_param_fun)
        );

        // The identity function is *not* (Any -> Any) as that would allow the types to be
        // unrelated
        let any_to_any_fun = poly_for_str("(Any ->! Any)");
        assert_eq!(
            Result::May,
            poly_is_a(&tvars, &pidentity_fun, &any_to_any_fun)
        );

        // The symbol function satisfies ((U) -> Symbol) as all of its returns must be bounded by
        // that
        let top_to_sym_fun = poly_for_str("(... ->! Symbol)");
        assert_eq!(
            Result::May,
            poly_is_a(&tvars, &pidentity_fun, &top_to_sym_fun)
        );
        assert_eq!(
            Result::Yes,
            poly_is_a(&tvars, &pidentity_sym_fun, &top_to_sym_fun)
        );

        // The identity string function satisfies (String -> String) as it has no subtypes
        let str_to_str_fun = poly_for_str("(String ->! String)");
        assert_eq!(
            Result::May,
            poly_is_a(&tvars, &pidentity_fun, &str_to_str_fun)
        );
        assert_eq!(
            Result::No,
            poly_is_a(&tvars, &pidentity_sym_fun, &str_to_str_fun)
        );
        assert_eq!(
            Result::Yes,
            poly_is_a(&tvars, &pidentity_impure_string_fun, &str_to_str_fun)
        );
    }
}
