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

trait SeqTyIterator<'a, S> {
    /// Returns the remaining number of fixed member types
    fn fixed_len(&self) -> usize;
    /// Returns the next member type or None if there are no more types
    fn next(&mut self) -> Option<&'a S>;
}

/// Iterates through the member types of a list in forward order
struct ListTyIterator<'a, S: 'a> {
    fixed: &'a [S],
    rest: &'a Option<Box<S>>,
}

impl<'a, S> ListTyIterator<'a, S> {
    fn new(fixed: &'a [S], rest: &'a Option<Box<S>>) -> ListTyIterator<'a, S> {
        ListTyIterator { fixed, rest }
    }
}

impl<'a, S> SeqTyIterator<'a, S> for ListTyIterator<'a, S> {
    fn fixed_len(&self) -> usize {
        self.fixed.len()
    }

    fn next(&mut self) -> Option<&'a S> {
        if self.fixed.is_empty() {
            match *self.rest {
                Some(ref boxed) => Some(boxed.as_ref()),
                None => None,
            }
        } else {
            let next = self.fixed.first();
            self.fixed = &self.fixed[1..];
            next
        }
    }
}

/// Iterates through the member types of a vector in reverse order
struct RevVecTyIterator<'a, S: 'a> {
    begin: &'a Option<Box<S>>,
    fixed: &'a [S],
}

impl<'a, S> RevVecTyIterator<'a, S> {
    fn new(begin: &'a Option<Box<S>>, fixed: &'a [S]) -> RevVecTyIterator<'a, S> {
        RevVecTyIterator { begin, fixed }
    }
}

impl<'a, S> SeqTyIterator<'a, S> for RevVecTyIterator<'a, S> {
    fn fixed_len(&self) -> usize {
        self.fixed.len()
    }

    fn next(&mut self) -> Option<&'a S> {
        if self.fixed.is_empty() {
            match *self.begin {
                Some(ref boxed) => Some(boxed.as_ref()),
                None => None,
            }
        } else {
            let next = self.fixed.last();
            self.fixed = &self.fixed[..self.fixed.len() - 1];
            next
        }
    }
}

fn fun_is_a<S, F>(ref_is_a: F, sub_fun: &ty::Fun<S>, par_fun: &ty::Fun<S>) -> Result
where
    S: ty::TyRef,
    F: Fn(&S, &S) -> Result,
{
    if sub_fun.impure && !par_fun.impure {
        // Impure functions cannot satisfy pure function types
        return Result::No;
    }

    // Note that the param type is contravariant
    ref_is_a(&par_fun.params, &sub_fun.params).and_then(|| ref_is_a(&sub_fun.ret, &par_fun.ret))
}

fn seq_is_a<'a, S, T, F>(ref_is_a: F, mut sub_iter: T, mut par_iter: T) -> Result
where
    S: 'a + ty::TyRef,
    T: SeqTyIterator<'a, S>,
    F: Fn(&S, &S) -> Result,
{
    let mut is_exact = sub_iter.fixed_len() >= par_iter.fixed_len();
    loop {
        let sub_next = sub_iter.next();
        let par_next = par_iter.next();

        match (sub_next, par_next) {
            (Some(sub), Some(par)) => match ref_is_a(sub, par) {
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
                // Fixed sequence ended at different lengths
                return Result::No;
            }
        };

        if sub_iter.fixed_len() == 0 && par_iter.fixed_len() == 0 {
            // We have no more fixed types to evaluate so we can exit. This has to happen at the
            // end of the loop body to ensure we have checked our rest type if present.
            break;
        }
    }

    if is_exact {
        Result::Yes
    } else {
        Result::May
    }
}

fn ty_is_a<S, F>(
    ref_is_a: F,
    sub_ref: &S,
    sub_ty: &ty::Ty<S>,
    parent_ref: &S,
    parent_ty: &ty::Ty<S>,
) -> Result
where
    S: ty::TyRef,
    F: Fn(&S, &S) -> Result,
{
    if sub_ty == parent_ty {
        return Result::Yes;
    }

    match (sub_ty, parent_ty) {
        (_, &ty::Ty::Union(ref par_members)) => {
            let results = par_members
                .iter()
                .map(|par_member| ref_is_a(sub_ref, par_member));

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
        (&ty::Ty::Union(ref sub_members), _) => {
            let results = sub_members
                .iter()
                .map(|sub_member| ref_is_a(sub_member, parent_ref));

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
        (_, &ty::Ty::Any) => Result::Yes,
        (&ty::Ty::Any, _) => Result::May,
        (&ty::Ty::LitSym(_), &ty::Ty::Sym) => Result::Yes,
        (&ty::Ty::Sym, &ty::Ty::LitSym(_)) => Result::May,
        (&ty::Ty::LitBool(_), &ty::Ty::Bool) => Result::Yes,
        (&ty::Ty::Bool, &ty::Ty::LitBool(_)) => Result::May,
        (&ty::Ty::Set(ref sub), &ty::Ty::Set(ref par)) => ref_is_a(sub, par),
        (&ty::Ty::Hash(ref sub_key, ref sub_value), &ty::Ty::Hash(ref par_key, ref par_value)) => {
            ref_is_a(sub_key, par_key).and_then(|| ref_is_a(sub_value, par_value))
        }
        (
            &ty::Ty::List(ref sub_fixed, ref sub_rest),
            &ty::Ty::List(ref par_fixed, ref par_rest),
        ) => seq_is_a(
            ref_is_a,
            ListTyIterator::new(sub_fixed, sub_rest),
            ListTyIterator::new(par_fixed, par_rest),
        ),
        (
            &ty::Ty::Vec(ref sub_start, ref sub_fixed),
            &ty::Ty::Vec(ref par_start, ref par_fixed),
        ) => seq_is_a(
            ref_is_a,
            RevVecTyIterator::new(sub_start, sub_fixed),
            RevVecTyIterator::new(par_start, par_fixed),
        ),
        (&ty::Ty::Fun(ref sub_fun), &ty::Ty::Fun(ref par_fun)) => {
            fun_is_a(ref_is_a, sub_fun, par_fun)
        }
        _ => Result::No,
    }
}

pub fn mono_is_a(sub: &ty::Mono, parent: &ty::Mono) -> Result {
    ty_is_a(mono_is_a, sub, sub.as_ty(), parent, parent.as_ty())
}

fn poly_ty_has_subtypes(pvars: &[ty::PVar], poly_ty: &ty::Ty<ty::Poly>) -> bool {
    match *poly_ty {
        ty::Ty::Any | ty::Ty::Bool | ty::Ty::Sym => true,
        ty::Ty::Char
        | ty::Ty::Float
        | ty::Ty::Int
        | ty::Ty::LitBool(_)
        | ty::Ty::LitSym(_)
        | ty::Ty::Str => false,
        ty::Ty::Fun(ref fun) => {
            fun.impure
                || fun.params
                    != ty::Ty::List(vec![], Some(Box::new(ty::Ty::Any.into_poly()))).into_poly()
                || poly_has_subtypes(pvars, &fun.ret)
        }
        ty::Ty::Hash(ref key, ref value) => [key, value]
            .iter()
            .any(|poly| poly_has_subtypes(pvars, poly)),
        ty::Ty::List(ref fixed, ref rest) => {
            rest.is_some() || fixed.iter().any(|poly| poly_has_subtypes(pvars, poly))
        }
        ty::Ty::Set(ref member) => poly_has_subtypes(pvars, member),
        ty::Ty::Vec(ref begin, ref fixed) => {
            begin.is_some() || fixed.iter().any(|poly| poly_has_subtypes(pvars, poly))
        }
        ty::Ty::Union(ref members) => !members.is_empty(),
    }
}

fn poly_has_subtypes(pvars: &[ty::PVar], poly: &ty::Poly) -> bool {
    let poly_ty = resolve_poly_ty(pvars, poly).as_ty();
    poly_ty_has_subtypes(pvars, poly_ty)
}

enum ResolveResult<'a> {
    Fixed(&'a ty::Ty<ty::Poly>),
    Bound(&'a ty::Ty<ty::Poly>),
}

impl<'a> ResolveResult<'a> {
    fn as_ty(&self) -> &'a ty::Ty<ty::Poly> {
        match *self {
            ResolveResult::Fixed(ty) => ty,
            ResolveResult::Bound(ty) => ty,
        }
    }
}

fn resolve_pvar_id_bound(pvars: &[ty::PVar], pvar_id: ty::PVarId) -> &ty::Ty<ty::Poly> {
    match pvars[pvar_id.to_usize()].bound {
        ty::Poly::Fixed(ref fixed_ty) => fixed_ty,
        ty::Poly::Var(pvar_id) => resolve_pvar_id_bound(pvars, pvar_id),
    }
}

fn pvar_id_is_bounded_by(
    pvars: &[ty::PVar],
    sub_pvar_id: ty::PVarId,
    parent_pvar_id: ty::PVarId,
) -> bool {
    if sub_pvar_id == parent_pvar_id {
        return true;
    }

    match pvars[sub_pvar_id.to_usize()].bound {
        ty::Poly::Fixed(_) => false,
        ty::Poly::Var(pvar_id) => pvar_id_is_bounded_by(pvars, pvar_id, parent_pvar_id),
    }
}

/// Resolves a Poly to either a fixed type or polymorphic variable's bound
fn resolve_poly_ty<'a>(pvars: &'a [ty::PVar], poly: &'a ty::Poly) -> ResolveResult<'a> {
    match *poly {
        ty::Poly::Fixed(ref ty) => ResolveResult::Fixed(ty),
        ty::Poly::Var(pvar_id) => {
            let ty = resolve_pvar_id_bound(pvars, pvar_id);
            ResolveResult::Bound(ty)
        }
    }
}

pub fn poly_is_a(pvars: &[ty::PVar], sub: &ty::Poly, parent: &ty::Poly) -> Result {
    if let &ty::Poly::Var(parent_pvar_id) = parent {
        if let &ty::Poly::Var(sub_pvar_id) = sub {
            if pvar_id_is_bounded_by(pvars, sub_pvar_id, parent_pvar_id) {
                return Result::Yes;
            }
        }
    }

    let sub_ty = resolve_poly_ty(pvars, sub).as_ty();
    let (parent_ty, parent_is_bound) = match resolve_poly_ty(pvars, parent) {
        ResolveResult::Bound(ty) => (ty, true),
        ResolveResult::Fixed(ty) => (ty, false),
    };

    let result = ty_is_a(
        |sub, parent| poly_is_a(pvars, sub, parent),
        sub,
        sub_ty,
        parent,
        parent_ty,
    );

    if parent_is_bound && poly_ty_has_subtypes(pvars, parent_ty) {
        // The parent is polymorphic and has child types. We can't ensure that the sub satisfies
        // all child types of the parent bound.
        Result::May.and_then(|| result)
    } else {
        result
    }
}

#[cfg(test)]
mod test {
    use super::*;

    fn poly_for_str(datum_str: &str) -> ty::Poly {
        use hir;
        hir::ty_for_str(datum_str).unwrap()
    }

    fn str_has_subtypes(datum_str: &str) -> bool {
        let poly = poly_for_str(datum_str);
        poly_has_subtypes(&[], &poly)
    }

    fn mono_ty_for_str(datum_str: &str) -> ty::Mono {
        use std::collections::HashMap;

        let poly = poly_for_str(datum_str);
        ty::subst::subst(&poly, &HashMap::new()).unwrap()
    }

    #[test]
    fn sym_types() {
        let foo_sym = mono_ty_for_str("'foo");
        let bar_sym = mono_ty_for_str("'bar");
        let any_sym = mono_ty_for_str("Symbol");
        let any_int = mono_ty_for_str("Int");

        assert_eq!(Result::Yes, mono_is_a(&foo_sym, &foo_sym));
        assert_eq!(Result::No, mono_is_a(&foo_sym, &bar_sym));

        assert_eq!(Result::Yes, mono_is_a(&foo_sym, &any_sym));
        assert_eq!(Result::May, mono_is_a(&any_sym, &foo_sym));

        assert_eq!(Result::No, mono_is_a(&any_sym, &any_int));
        assert_eq!(Result::No, mono_is_a(&any_int, &any_sym));
    }

    #[test]
    fn set_types() {
        let foo_set = mono_ty_for_str("(Setof 'foo)");
        let bar_set = mono_ty_for_str("(Setof 'bar)");
        let any_set = mono_ty_for_str("(Setof Symbol)");

        assert_eq!(Result::Yes, mono_is_a(&foo_set, &foo_set));
        assert_eq!(Result::No, mono_is_a(&foo_set, &bar_set));

        assert_eq!(Result::Yes, mono_is_a(&foo_set, &any_set));
        assert_eq!(Result::May, mono_is_a(&any_set, &foo_set));
    }

    #[test]
    fn hash_types() {
        let foo_sym = mono_ty_for_str("'foo");
        let any_sym = mono_ty_for_str("Symbol");
        let any_int = mono_ty_for_str("Int");

        let int_to_any_sym =
            ty::Ty::Hash(Box::new(any_int.clone()), Box::new(any_sym.clone())).into_mono();
        let int_to_foo_sym =
            ty::Ty::Hash(Box::new(any_int.clone()), foo_sym.clone().into()).into_mono();
        let any_sym_to_any_sym =
            ty::Ty::Hash(Box::new(any_sym.clone()), Box::new(any_sym.clone())).into_mono();

        assert_eq!(Result::Yes, mono_is_a(&int_to_foo_sym, &int_to_any_sym));
        assert_eq!(Result::May, mono_is_a(&int_to_any_sym, &int_to_foo_sym));
        assert_eq!(Result::No, mono_is_a(&int_to_any_sym, &any_sym_to_any_sym));
    }

    #[test]
    fn union_types() {
        let foo_sym = mono_ty_for_str("'foo");
        let bar_sym = mono_ty_for_str("'bar");
        let baz_sym = mono_ty_for_str("'baz");

        // TODO: Union type lowering
        let foo_bar_union = ty::Ty::Union(vec![foo_sym.clone(), bar_sym.clone()]).into_mono();
        let bar_baz_union = ty::Ty::Union(vec![bar_sym.clone(), baz_sym.clone()]).into_mono();
        let never = ty::Ty::Union(vec![]).into_mono();

        assert_eq!(Result::Yes, mono_is_a(&foo_sym, &foo_bar_union));
        assert_eq!(Result::No, mono_is_a(&baz_sym, &foo_bar_union));
        assert_eq!(Result::No, mono_is_a(&baz_sym, &never));

        assert_eq!(Result::May, mono_is_a(&foo_bar_union, &foo_sym));
        assert_eq!(Result::No, mono_is_a(&foo_bar_union, &baz_sym));
        assert_eq!(Result::Yes, mono_is_a(&never, &foo_sym));

        assert_eq!(Result::May, mono_is_a(&foo_bar_union, &bar_baz_union));
        assert_eq!(Result::Yes, mono_is_a(&foo_bar_union, &foo_bar_union));
        assert_eq!(Result::Yes, mono_is_a(&never, &foo_bar_union));
    }

    #[test]
    fn any_and_never_types() {
        let any = mono_ty_for_str("Any");
        let never = ty::Ty::Union(vec![]).into_mono();
        let foo_sym = mono_ty_for_str("'foo");

        assert_eq!(Result::Yes, mono_is_a(&foo_sym, &any));
        assert_eq!(Result::May, mono_is_a(&any, &foo_sym));
        assert_eq!(Result::Yes, mono_is_a(&never, &any));
        assert_eq!(Result::Yes, mono_is_a(&never, &never));
        assert_eq!(Result::No, mono_is_a(&any, &never));
    }

    #[test]
    fn list_types() {
        let listof_any = mono_ty_for_str("(Listof Any)");
        let listof_int = mono_ty_for_str("(Listof Int)");
        let two_ints_list = mono_ty_for_str("(List Int Int)");
        let three_ints_list = mono_ty_for_str("(List Int Int Int)");
        let at_least_one_int_list = mono_ty_for_str("(List Int Int ...)");

        assert_eq!(Result::Yes, mono_is_a(&listof_int, &listof_any));
        assert_eq!(Result::May, mono_is_a(&listof_any, &listof_int));

        assert_eq!(Result::Yes, mono_is_a(&two_ints_list, &listof_int));
        assert_eq!(Result::May, mono_is_a(&listof_int, &two_ints_list));
        assert_eq!(Result::Yes, mono_is_a(&two_ints_list, &listof_any));

        assert_eq!(Result::No, mono_is_a(&two_ints_list, &three_ints_list));
        assert_eq!(Result::No, mono_is_a(&three_ints_list, &two_ints_list));

        assert_eq!(Result::Yes, mono_is_a(&at_least_one_int_list, &listof_int));
        assert_eq!(Result::May, mono_is_a(&listof_int, &at_least_one_int_list));
    }

    #[test]
    fn vec_types() {
        let vecof_any = mono_ty_for_str("(Vectorof Any)");
        let vecof_int = mono_ty_for_str("(Vectorof Int)");
        let two_ints_vec = mono_ty_for_str("(Vector Int Int)");
        let three_ints_vec = mono_ty_for_str("(Vector Int Int Int)");
        let at_least_one_int_vec = mono_ty_for_str("(Vector Int ... Int)");

        assert_eq!(Result::Yes, mono_is_a(&vecof_int, &vecof_any));
        assert_eq!(Result::May, mono_is_a(&vecof_any, &vecof_int));

        assert_eq!(Result::Yes, mono_is_a(&two_ints_vec, &vecof_int));
        assert_eq!(Result::May, mono_is_a(&vecof_int, &two_ints_vec));
        assert_eq!(Result::Yes, mono_is_a(&two_ints_vec, &vecof_any));

        assert_eq!(Result::No, mono_is_a(&two_ints_vec, &three_ints_vec));
        assert_eq!(Result::No, mono_is_a(&three_ints_vec, &two_ints_vec));

        assert_eq!(Result::Yes, mono_is_a(&at_least_one_int_vec, &vecof_int));
        assert_eq!(Result::May, mono_is_a(&vecof_int, &at_least_one_int_vec));
    }

    #[test]
    fn fun_types() {
        let impure_any_to_sym = mono_ty_for_str("(->! Any Symbol)");
        let impure_sym_to_any = mono_ty_for_str("(->! Symbol Any)");
        let impure_sym_to_sym = mono_ty_for_str("(->! Symbol Symbol)");
        let pure_sym_to_sym = mono_ty_for_str("(-> Symbol Symbol)");

        assert_eq!(
            Result::Yes,
            mono_is_a(&impure_sym_to_sym, &impure_sym_to_any)
        );
        assert_eq!(
            Result::Yes,
            mono_is_a(&impure_any_to_sym, &impure_sym_to_sym)
        );
        assert_eq!(
            Result::May,
            mono_is_a(&impure_sym_to_any, &impure_sym_to_sym)
        );

        assert_eq!(Result::Yes, mono_is_a(&pure_sym_to_sym, &impure_sym_to_sym));
        assert_eq!(Result::No, mono_is_a(&impure_sym_to_sym, &pure_sym_to_sym));
    }

    #[test]
    fn bool_types() {
        let true_type = mono_ty_for_str("true");
        let false_type = mono_ty_for_str("false");
        let bool_type = mono_ty_for_str("Bool");

        assert_eq!(Result::Yes, mono_is_a(&true_type, &bool_type));
        assert_eq!(Result::May, mono_is_a(&bool_type, &true_type));
        assert_eq!(Result::No, mono_is_a(&false_type, &true_type));
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

    #[test]
    fn poly_subtypes() {
        assert_eq!(true, str_has_subtypes("Any"));
        assert_eq!(true, str_has_subtypes("Bool"));
        assert_eq!(false, str_has_subtypes("true"));
        assert_eq!(false, str_has_subtypes("Char"));
        assert_eq!(false, str_has_subtypes("Float"));
        assert_eq!(false, str_has_subtypes("String"));
        assert_eq!(true, str_has_subtypes("Symbol"));

        assert_eq!(false, str_has_subtypes("(-> Any ... true)"));
        assert_eq!(true, str_has_subtypes("(->! Any ... true)"));
        assert_eq!(true, str_has_subtypes("(-> Any true)"));
        assert_eq!(true, str_has_subtypes("(-> Int ... true)"));
        assert_eq!(true, str_has_subtypes("(-> Any ... Any)"));

        assert_eq!(true, str_has_subtypes("(Hash Symbol Int)"));
        assert_eq!(false, str_has_subtypes("(Hash Float Int)"));

        assert_eq!(true, str_has_subtypes("(List Symbol Int)"));
        assert_eq!(true, str_has_subtypes("(List String Int ...)"));
        assert_eq!(false, str_has_subtypes("(List String Int)"));

        assert_eq!(true, str_has_subtypes("(Setof Symbol)"));
        assert_eq!(false, str_has_subtypes("(Setof Float)"));

        assert_eq!(true, str_has_subtypes("(Vector false ...)"));
        assert_eq!(false, str_has_subtypes("(Vector false true)"));

        assert_eq!(
            false,
            poly_has_subtypes(&[], &ty::Ty::Union(vec![]).into_poly())
        );
    }
}
