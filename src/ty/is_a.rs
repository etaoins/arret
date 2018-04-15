use ty;
use ty::seq_ty_iter::{ListTyIterator, RevVecTyIterator, SeqTyIterator};

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
        match *self {
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
}

trait IsACtx<S>
where
    S: ty::TyRef,
{
    fn ref_is_a(&self, &S, &S) -> Result;

    fn fun_is_a(&self, sub_fun: &ty::Fun<S>, par_fun: &ty::Fun<S>) -> Result {
        if sub_fun.impure && !par_fun.impure {
            // Impure functions cannot satisfy pure function types
            return Result::No;
        }

        // Note that the param type is contravariant
        self.ref_is_a(&par_fun.params, &sub_fun.params)
            .and_then(|| self.ref_is_a(&sub_fun.ret, &par_fun.ret))
    }

    fn seq_is_a<'a, I>(&self, mut sub_iter: I, mut par_iter: I) -> Result
    where
        S: 'a,
        I: SeqTyIterator<'a, S>,
    {
        let mut is_exact = sub_iter.fixed_len() >= par_iter.fixed_len();

        // Compare our fixed types
        while sub_iter.fixed_len() > 0 || par_iter.fixed_len() > 0 {
            let sub_next = sub_iter.next();
            let par_next = par_iter.next();

            match (sub_next, par_next) {
                (Some(sub), Some(par)) => match self.ref_is_a(sub, par) {
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
                    // Fixed sequence ended at the same length
                    break;
                }
                (_, _) => {
                    // Type sequence ended at different lengths
                    return Result::No;
                }
            };
        }

        if sub_iter.is_infinite() && par_iter.is_infinite() {
            let sub_rest = sub_iter.next().unwrap();
            let par_rest = par_iter.next().unwrap();

            // If the rest doesn't match it's a May as the rest might not be present
            if self.ref_is_a(sub_rest, par_rest) != Result::Yes {
                is_exact = false;
            }
        }

        if is_exact {
            Result::Yes
        } else {
            Result::May
        }
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
            (&ty::Ty::Union(ref sub_members), _) => {
                let results = sub_members
                    .iter()
                    .map(|sub_member| self.ref_is_a(sub_member, parent_ref));

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
            (_, &ty::Ty::Union(ref par_members)) => {
                let results = par_members
                    .iter()
                    .map(|par_member| self.ref_is_a(sub_ref, par_member));

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
            (_, &ty::Ty::Any) => Result::Yes,
            (&ty::Ty::Any, _) => Result::May,
            (&ty::Ty::LitSym(_), &ty::Ty::Sym) => Result::Yes,
            (&ty::Ty::Sym, &ty::Ty::LitSym(_)) => Result::May,
            (&ty::Ty::LitBool(_), &ty::Ty::Bool) => Result::Yes,
            (&ty::Ty::Bool, &ty::Ty::LitBool(_)) => Result::May,
            (&ty::Ty::Set(ref sub), &ty::Ty::Set(ref par)) => self.ref_is_a(sub, par),
            (
                &ty::Ty::Hash(ref sub_key, ref sub_value),
                &ty::Ty::Hash(ref par_key, ref par_value),
            ) => self.ref_is_a(sub_key, par_key)
                .and_then(|| self.ref_is_a(sub_value, par_value)),
            (
                &ty::Ty::List(ref sub_fixed, ref sub_rest),
                &ty::Ty::List(ref par_fixed, ref par_rest),
            ) => self.seq_is_a(
                ListTyIterator::new(sub_fixed, sub_rest),
                ListTyIterator::new(par_fixed, par_rest),
            ),
            (
                &ty::Ty::Vec(ref sub_start, ref sub_fixed),
                &ty::Ty::Vec(ref par_start, ref par_fixed),
            ) => self.seq_is_a(
                RevVecTyIterator::new(sub_start, sub_fixed),
                RevVecTyIterator::new(par_start, par_fixed),
            ),
            (&ty::Ty::Fun(ref sub_fun), &ty::Ty::Fun(ref par_fun)) => {
                self.fun_is_a(sub_fun, par_fun)
            }
            _ => Result::No,
        }
    }
}

struct PolyIsACtx<'a> {
    pvars: &'a [ty::PVar],
}

impl<'a> PolyIsACtx<'a> {
    fn pvar_id_is_bounded_by(&self, sub_pvar_id: ty::PVarId, parent_pvar_id: ty::PVarId) -> bool {
        if sub_pvar_id == parent_pvar_id {
            return true;
        }

        match self.pvars[sub_pvar_id.to_usize()].bound {
            ty::Poly::Fixed(_) => false,
            ty::Poly::Var(pvar_id) => self.pvar_id_is_bounded_by(pvar_id, parent_pvar_id),
        }
    }
}

impl<'a> IsACtx<ty::Poly> for PolyIsACtx<'a> {
    fn ref_is_a(&self, sub: &ty::Poly, parent: &ty::Poly) -> Result {
        if let ty::Poly::Var(parent_pvar_id) = *parent {
            if let ty::Poly::Var(sub_pvar_id) = *sub {
                if self.pvar_id_is_bounded_by(sub_pvar_id, parent_pvar_id) {
                    return Result::Yes;
                }
            }
        }

        let sub_ty = ty::resolve::resolve_poly_ty(self.pvars, sub).as_ty();
        let (parent_ty, parent_is_bound) = match ty::resolve::resolve_poly_ty(self.pvars, parent) {
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
}

pub fn poly_is_a(pvars: &[ty::PVar], sub: &ty::Poly, parent: &ty::Poly) -> Result {
    let ctx = PolyIsACtx { pvars };
    ctx.ref_is_a(sub, parent)
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
    fn hash_types() {
        let foo_sym = poly_for_str("'foo");
        let any_sym = poly_for_str("Symbol");
        let any_int = poly_for_str("Int");

        let int_to_any_sym =
            ty::Ty::Hash(Box::new(any_int.clone()), Box::new(any_sym.clone())).into_poly();
        let int_to_foo_sym =
            ty::Ty::Hash(Box::new(any_int.clone()), foo_sym.clone().into()).into_poly();
        let any_sym_to_any_sym =
            ty::Ty::Hash(Box::new(any_sym.clone()), Box::new(any_sym.clone())).into_poly();

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
        let listof_any = poly_for_str("(Listof Any)");
        let listof_int = poly_for_str("(Listof Int)");
        let two_ints_list = poly_for_str("(List Int Int)");
        let three_ints_list = poly_for_str("(List Int Int Int)");
        let at_least_one_int_list = poly_for_str("(List Int Int ...)");

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
        let at_least_one_int_vec = poly_for_str("(Vector Int ... Int)");

        let foos_then_int = poly_for_str("(Vector 'foo ... Int)");
        let bars_then_int = poly_for_str("(Vector 'bar ... Int)");

        assert_eq!(Result::Yes, poly_is_a(&[], &vecof_int, &vecof_any));
        assert_eq!(Result::May, poly_is_a(&[], &vecof_any, &vecof_int));

        assert_eq!(Result::Yes, poly_is_a(&[], &two_ints_vec, &vecof_int));
        assert_eq!(Result::May, poly_is_a(&[], &vecof_int, &two_ints_vec));
        assert_eq!(Result::Yes, poly_is_a(&[], &two_ints_vec, &vecof_any));

        assert_eq!(Result::No, poly_is_a(&[], &two_ints_vec, &three_ints_vec));
        assert_eq!(Result::No, poly_is_a(&[], &three_ints_vec, &two_ints_vec));

        assert_eq!(
            Result::Yes,
            poly_is_a(&[], &at_least_one_int_vec, &vecof_int)
        );
        assert_eq!(
            Result::May,
            poly_is_a(&[], &vecof_int, &at_least_one_int_vec)
        );

        assert_eq!(Result::May, poly_is_a(&[], &foos_then_int, &bars_then_int));
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
        let pvar_id1 = ty::PVarId::new(0);
        let ptype1 = ty::Poly::Var(pvar_id1);

        let pvar_id2 = ty::PVarId::new(1);
        let ptype2 = ty::Poly::Var(pvar_id2);

        let poly_bool = poly_for_str("Bool");

        let pvars = [
            ty::PVar::new("one".to_owned(), ty::Ty::Any.into_poly()),
            ty::PVar::new("two".to_owned(), ty::Ty::Any.into_poly()),
        ];

        assert_eq!(Result::Yes, poly_is_a(&pvars, &ptype1, &ptype1));
        assert_eq!(Result::May, poly_is_a(&pvars, &ptype1, &ptype2));
        assert_eq!(Result::May, poly_is_a(&pvars, &ptype1, &poly_bool));
    }

    #[test]
    fn bounded_poly_vars() {
        let sym_ptype = ty::Poly::Var(ty::PVarId::new(0));
        let str_ptype = ty::Poly::Var(ty::PVarId::new(1));
        let foo_sym_ptype = ty::Poly::Var(ty::PVarId::new(2));

        let pvars = [
            ty::PVar::new("sym".to_owned(), poly_for_str("Symbol")),
            ty::PVar::new("str".to_owned(), poly_for_str("String")),
            ty::PVar::new("foo".to_owned(), poly_for_str("'foo")),
        ];

        let poly_foo_sym = poly_for_str("'foo");
        let any = poly_for_str("Any");

        // A poly var always satisfies itself
        assert_eq!(Result::Yes, poly_is_a(&pvars, &sym_ptype, &sym_ptype));

        // The bounds of these vars are disjoint
        assert_eq!(Result::No, poly_is_a(&pvars, &sym_ptype, &str_ptype));

        // The poly var may satisfy a more specific bound
        assert_eq!(Result::May, poly_is_a(&pvars, &sym_ptype, &poly_foo_sym));

        // A sub never satisfies a poly var with a disjoint bound
        assert_eq!(Result::No, poly_is_a(&pvars, &poly_foo_sym, &str_ptype));

        // The sub has a fixed type while the parent has a poly type. We can't ensure that 'foo
        // satisfies all possible Symbol subtypes (such as 'bar)
        assert_eq!(Result::May, poly_is_a(&pvars, &poly_foo_sym, &sym_ptype));

        // Both the sub and parent have fixed types by virtue of having no subtypes
        assert_eq!(
            Result::Yes,
            poly_is_a(&pvars, &poly_foo_sym, &foo_sym_ptype)
        );

        // The sub has a poly type while the parent has a fixed type. If the sub satisfies the
        // parent type so should all of its subtypes.
        assert_eq!(Result::Yes, poly_is_a(&pvars, &sym_ptype, &any));
    }

    #[test]
    fn related_poly_bounds() {
        let ptype1_unbounded = ty::Poly::Var(ty::PVarId::new(0));
        let ptype2_bounded_by_1 = ty::Poly::Var(ty::PVarId::new(1));
        let ptype3_bounded_by_2 = ty::Poly::Var(ty::PVarId::new(2));

        let pvars = [
            ty::PVar::new("1".to_owned(), poly_for_str("Any")),
            ty::PVar::new("2".to_owned(), ptype1_unbounded.clone()),
            ty::PVar::new("3".to_owned(), ptype2_bounded_by_1.clone()),
        ];

        // Direct bounding
        assert_eq!(
            Result::Yes,
            poly_is_a(&pvars, &ptype2_bounded_by_1, &ptype1_unbounded)
        );
        assert_eq!(
            Result::Yes,
            poly_is_a(&pvars, &ptype3_bounded_by_2, &ptype2_bounded_by_1)
        );

        // Commutative bounding
        assert_eq!(
            Result::Yes,
            poly_is_a(&pvars, &ptype3_bounded_by_2, &ptype1_unbounded)
        );

        // Inverse bounding relationship may not satisfy - the bounded type can have arbitrary
        // subtypes
        assert_eq!(
            Result::May,
            poly_is_a(&pvars, &ptype1_unbounded, &ptype2_bounded_by_1)
        );
    }
}
