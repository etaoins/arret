use ty;

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

            // Any type
            (_, &ty::Ty::Any) => Result::Yes,
            (&ty::Ty::Any, _) => Result::May,

            // Symbol types
            (&ty::Ty::LitSym(_), &ty::Ty::Sym) => Result::Yes,
            (&ty::Ty::Sym, &ty::Ty::LitSym(_)) => Result::May,

            // Bool types
            (&ty::Ty::LitBool(_), &ty::Ty::Bool) => Result::Yes,
            (&ty::Ty::Bool, &ty::Ty::LitBool(_)) => Result::May,

            // Sets
            (&ty::Ty::Set(ref sub), &ty::Ty::Set(ref par)) => self.ref_is_a(sub, par),

            // Maps
            (
                &ty::Ty::Map(ref sub_key, ref sub_value),
                &ty::Ty::Map(ref par_key, ref par_value),
            ) => self.ref_is_a(sub_key, par_key)
                .and_then(|| self.ref_is_a(sub_value, par_value)),

            // Vector types
            (&ty::Ty::Vec(ref sub_members), &ty::Ty::Vec(ref par_members)) => {
                if sub_members.len() != par_members.len() {
                    Result::No
                } else {
                    Result::from_iter(
                        sub_members
                            .iter()
                            .zip(par_members.iter())
                            .map(|(sub_member, par_member)| self.ref_is_a(sub_member, par_member)),
                    )
                }
            }
            (&ty::Ty::Vecof(ref sub_member), &ty::Ty::Vecof(ref par_member)) => {
                self.ref_is_a(sub_member, par_member)
            }
            (&ty::Ty::Vec(ref sub_members), &ty::Ty::Vecof(ref par_member)) => Result::from_iter(
                sub_members
                    .iter()
                    .map(|sub_member| self.ref_is_a(sub_member, par_member)),
            ),
            (&ty::Ty::Vecof(ref sub_member), &ty::Ty::Vec(ref par_members)) => Result::May
                .and_then(|| {
                    Result::from_iter(
                        par_members
                            .iter()
                            .map(|par_member| self.ref_is_a(sub_member, par_member)),
                    )
                }),

            // Functions
            (&ty::Ty::Fun(ref sub_fun), &ty::Ty::Fun(ref par_fun)) => {
                self.fun_is_a(sub_fun, par_fun)
            }
            (&ty::Ty::TyPred(_), &ty::Ty::Fun(ref par_fun)) => {
                self.fun_is_a(&ty::Fun::new_for_ty_pred(), par_fun)
            }
            (&ty::Ty::Fun(ref sub_fun), &ty::Ty::TyPred(_)) => {
                Result::May.and_then(|| self.fun_is_a(sub_fun, &ty::Fun::new_for_ty_pred()))
            }

            // List types
            (&ty::Ty::Cons(ref sub_car, ref sub_cdr), &ty::Ty::Cons(ref par_car, ref par_cdr)) => {
                self.ref_is_a(sub_car, par_car)
                    .and_then(|| self.ref_is_a(sub_cdr, par_cdr))
            }
            (&ty::Ty::Listof(ref sub_member), &ty::Ty::Listof(ref par_member)) => {
                self.ref_is_a(sub_member, par_member)
            }
            (&ty::Ty::Nil, &ty::Ty::Listof(_)) => Result::Yes,
            (&ty::Ty::Listof(_), &ty::Ty::Nil) => Result::May,
            (&ty::Ty::Cons(ref sub_car, ref sub_cdr), &ty::Ty::Listof(ref par_member)) => {
                self.ref_is_a(sub_car, par_member)
                    .and_then(|| self.ref_is_a(sub_cdr, parent_ref))
            }
            (&ty::Ty::Listof(ref sub_member), &ty::Ty::Cons(ref par_car, ref par_cdr)) => {
                Result::May.and_then(|| {
                    self.ref_is_a(sub_member, par_car)
                        .and_then(|| self.ref_is_a(sub_ref, par_cdr))
                })
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

struct MonoIsACtx {}

impl<'a> IsACtx<ty::Mono> for MonoIsACtx {
    fn ref_is_a(&self, sub: &ty::Mono, parent: &ty::Mono) -> Result {
        self.ty_is_a(sub, sub.as_ty(), parent, parent.as_ty())
    }
}

pub fn mono_is_a(sub: &ty::Mono, parent: &ty::Mono) -> Result {
    let ctx = MonoIsACtx {};
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
    fn cons_types() {
        let cons_bool_bool = poly_for_str("(Cons Bool Bool)");
        let cons_any_any = poly_for_str("(Cons Any Any)");

        assert_eq!(Result::Yes, poly_is_a(&[], &cons_bool_bool, &cons_any_any));
        assert_eq!(Result::May, poly_is_a(&[], &cons_any_any, &cons_bool_bool));
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

        // Type predicates always equal themselves
        assert_eq!(Result::Yes, poly_is_a(&[], &sym_ty_pred, &sym_ty_pred));

        // Type predicates never equal other type predicates
        assert_eq!(Result::No, poly_is_a(&[], &sym_ty_pred, &lit_sym_ty_pred));
        assert_eq!(Result::No, poly_is_a(&[], &lit_sym_ty_pred, &sym_ty_pred));

        // Type predicates are a subtype of (Any -> Bool)
        assert_eq!(Result::Yes, poly_is_a(&[], &sym_ty_pred, &general_pred));
        assert_eq!(Result::May, poly_is_a(&[], &general_pred, &sym_ty_pred));
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
