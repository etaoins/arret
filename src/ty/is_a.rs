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

trait SeqTyIterator<'a> {
    /// Returns the remaining number of fixed member types
    fn fixed_len(&self) -> usize;
    /// Returns the next member type or None if there are no more types
    fn next(&mut self) -> Option<&'a ty::Mono>;
}

/// Iterates through the member types of a list in forward order
struct ListTyIterator<'a> {
    fixed: &'a [ty::Mono],
    rest: &'a Option<Box<ty::Mono>>,
}

impl<'a> ListTyIterator<'a> {
    fn new(fixed: &'a [ty::Mono], rest: &'a Option<Box<ty::Mono>>) -> ListTyIterator<'a> {
        ListTyIterator { fixed, rest }
    }
}

impl<'a> SeqTyIterator<'a> for ListTyIterator<'a> {
    fn fixed_len(&self) -> usize {
        self.fixed.len()
    }

    fn next(&mut self) -> Option<&'a ty::Mono> {
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
struct RevVecTyIterator<'a> {
    begin: &'a Option<Box<ty::Mono>>,
    fixed: &'a [ty::Mono],
}

impl<'a> RevVecTyIterator<'a> {
    fn new(begin: &'a Option<Box<ty::Mono>>, fixed: &'a [ty::Mono]) -> RevVecTyIterator<'a> {
        RevVecTyIterator { begin, fixed }
    }
}

impl<'a> SeqTyIterator<'a> for RevVecTyIterator<'a> {
    fn fixed_len(&self) -> usize {
        self.fixed.len()
    }

    fn next(&mut self) -> Option<&'a ty::Mono> {
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

fn merge_field_results(results: &[Result]) -> Result {
    if results.contains(&Result::No) {
        Result::No
    } else if results.contains(&Result::May) {
        Result::May
    } else {
        Result::Yes
    }
}

fn fun_is_a(sub_fun: &ty::Fun<ty::Mono>, par_fun: &ty::Fun<ty::Mono>) -> Result {
    if sub_fun.impure && !par_fun.impure {
        // Impure functions cannot satisfy pure function types
        return Result::No;
    }

    let results = [
        // Note that the param type is contravariant
        is_a(&par_fun.params, &sub_fun.params),
        is_a(&sub_fun.ret, &par_fun.ret),
    ];

    merge_field_results(&results)
}

fn seq_is_a<'a, 'b, T, U>(mut sub_iter: T, mut par_iter: U) -> Result
where
    T: SeqTyIterator<'a>,
    U: SeqTyIterator<'b>,
{
    let mut is_exact = sub_iter.fixed_len() >= par_iter.fixed_len();
    loop {
        let sub_next = sub_iter.next();
        let par_next = par_iter.next();

        match (sub_next, par_next) {
            (Some(sub), Some(par)) => match is_a(sub, par) {
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

fn ty_is_a(sub: &ty::Ty<ty::Mono>, parent: &ty::Ty<ty::Mono>) -> Result {
    if sub == parent {
        return Result::Yes;
    }

    match (sub, parent) {
        (_, &ty::Ty::Union(ref par_members)) => {
            let results = par_members
                .iter()
                .map(|par_member| ty_is_a(sub, par_member.as_ty()));

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
                .map(|sub_member| ty_is_a(sub_member.as_ty(), parent));

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
        (&ty::Ty::Sym(_), &ty::Ty::AnySym) => Result::Yes,
        (&ty::Ty::AnySym, &ty::Ty::Sym(_)) => Result::May,
        (&ty::Ty::Bool(_), &ty::Ty::AnyBool) => Result::Yes,
        (&ty::Ty::AnyBool, &ty::Ty::Bool(_)) => Result::May,
        (&ty::Ty::Set(ref sub), &ty::Ty::Set(ref par)) => is_a(sub, par),
        (&ty::Ty::Hash(ref sub_key, ref sub_value), &ty::Ty::Hash(ref par_key, ref par_value)) => {
            merge_field_results(&[is_a(sub_key, par_key), is_a(sub_value, par_value)])
        }
        (
            &ty::Ty::List(ref sub_fixed, ref sub_rest),
            &ty::Ty::List(ref par_fixed, ref par_rest),
        ) => seq_is_a(
            ListTyIterator::new(sub_fixed, sub_rest),
            ListTyIterator::new(par_fixed, par_rest),
        ),
        (
            &ty::Ty::Vec(ref sub_start, ref sub_fixed),
            &ty::Ty::Vec(ref par_start, ref par_fixed),
        ) => seq_is_a(
            RevVecTyIterator::new(sub_start, sub_fixed),
            RevVecTyIterator::new(par_start, par_fixed),
        ),
        (&ty::Ty::Fun(ref sub_fun), &ty::Ty::Fun(ref par_fun)) => fun_is_a(sub_fun, par_fun),
        _ => Result::No,
    }
}

pub fn is_a(sub: &ty::Mono, parent: &ty::Mono) -> Result {
    ty_is_a(sub.as_ty(), parent.as_ty())
}

#[cfg(test)]
mod test {
    use super::*;

    fn ty_for_str(datum_str: &str) -> ty::Mono {
        use hir;
        use std::collections::HashMap;

        let poly = hir::ty_for_str(datum_str).unwrap();
        ty::subst::subst(&poly, &HashMap::new()).unwrap()
    }

    #[test]
    fn sym_types() {
        let foo_sym = ty_for_str("'foo");
        let bar_sym = ty_for_str("'bar");
        let any_sym = ty_for_str("Symbol");
        let any_int = ty_for_str("Int");

        assert_eq!(Result::Yes, is_a(&foo_sym, &foo_sym));
        assert_eq!(Result::No, is_a(&foo_sym, &bar_sym));

        assert_eq!(Result::Yes, is_a(&foo_sym, &any_sym));
        assert_eq!(Result::May, is_a(&any_sym, &foo_sym));

        assert_eq!(Result::No, is_a(&any_sym, &any_int));
        assert_eq!(Result::No, is_a(&any_int, &any_sym));
    }

    #[test]
    fn set_types() {
        let foo_set = ty_for_str("(Setof 'foo)");
        let bar_set = ty_for_str("(Setof 'bar)");
        let any_set = ty_for_str("(Setof Symbol)");

        assert_eq!(Result::Yes, is_a(&foo_set, &foo_set));
        assert_eq!(Result::No, is_a(&foo_set, &bar_set));

        assert_eq!(Result::Yes, is_a(&foo_set, &any_set));
        assert_eq!(Result::May, is_a(&any_set, &foo_set));
    }

    #[test]
    fn hash_types() {
        let foo_sym = ty_for_str("'foo");
        let any_sym = ty_for_str("Symbol");
        let any_int = ty_for_str("Int");

        let int_to_any_sym =
            ty::Ty::Hash(Box::new(any_int.clone()), Box::new(any_sym.clone())).into_mono();
        let int_to_foo_sym =
            ty::Ty::Hash(Box::new(any_int.clone()), foo_sym.clone().into()).into_mono();
        let any_sym_to_any_sym =
            ty::Ty::Hash(Box::new(any_sym.clone()), Box::new(any_sym.clone())).into_mono();

        assert_eq!(Result::Yes, is_a(&int_to_foo_sym, &int_to_any_sym));
        assert_eq!(Result::May, is_a(&int_to_any_sym, &int_to_foo_sym));
        assert_eq!(Result::No, is_a(&int_to_any_sym, &any_sym_to_any_sym));
    }

    #[test]
    fn union_types() {
        let foo_sym = ty_for_str("'foo");
        let bar_sym = ty_for_str("'bar");
        let baz_sym = ty_for_str("'baz");

        // TODO: Union type lowering
        let foo_bar_union = ty::Ty::Union(vec![foo_sym.clone(), bar_sym.clone()]).into_mono();
        let bar_baz_union = ty::Ty::Union(vec![bar_sym.clone(), baz_sym.clone()]).into_mono();
        let never = ty::Ty::Union(vec![]).into_mono();

        assert_eq!(Result::Yes, is_a(&foo_sym, &foo_bar_union));
        assert_eq!(Result::No, is_a(&baz_sym, &foo_bar_union));
        assert_eq!(Result::No, is_a(&baz_sym, &never));

        assert_eq!(Result::May, is_a(&foo_bar_union, &foo_sym));
        assert_eq!(Result::No, is_a(&foo_bar_union, &baz_sym));
        assert_eq!(Result::Yes, is_a(&never, &foo_sym));

        assert_eq!(Result::May, is_a(&foo_bar_union, &bar_baz_union));
        assert_eq!(Result::Yes, is_a(&foo_bar_union, &foo_bar_union));
        assert_eq!(Result::Yes, is_a(&never, &foo_bar_union));
    }

    #[test]
    fn any_and_never_types() {
        let any = ty_for_str("Any");
        let never = ty::Ty::Union(vec![]).into_mono();
        let foo_sym = ty_for_str("'foo");

        assert_eq!(Result::Yes, is_a(&foo_sym, &any));
        assert_eq!(Result::May, is_a(&any, &foo_sym));
        assert_eq!(Result::Yes, is_a(&never, &any));
        assert_eq!(Result::Yes, is_a(&never, &never));
        assert_eq!(Result::No, is_a(&any, &never));
    }

    #[test]
    fn list_types() {
        let listof_any = ty_for_str("(Listof Any)");
        let listof_int = ty_for_str("(Listof Int)");
        let two_ints_list = ty_for_str("(List Int Int)");
        let three_ints_list = ty_for_str("(List Int Int Int)");
        let at_least_one_int_list = ty_for_str("(List Int Int ...)");

        assert_eq!(Result::Yes, is_a(&listof_int, &listof_any));
        assert_eq!(Result::May, is_a(&listof_any, &listof_int));

        assert_eq!(Result::Yes, is_a(&two_ints_list, &listof_int));
        assert_eq!(Result::May, is_a(&listof_int, &two_ints_list));
        assert_eq!(Result::Yes, is_a(&two_ints_list, &listof_any));

        assert_eq!(Result::No, is_a(&two_ints_list, &three_ints_list));
        assert_eq!(Result::No, is_a(&three_ints_list, &two_ints_list));

        assert_eq!(Result::Yes, is_a(&at_least_one_int_list, &listof_int));
        assert_eq!(Result::May, is_a(&listof_int, &at_least_one_int_list));
    }

    #[test]
    fn vec_types() {
        let vecof_any = ty_for_str("(Vectorof Any)");
        let vecof_int = ty_for_str("(Vectorof Int)");
        let two_ints_vec = ty_for_str("(Vector Int Int)");
        let three_ints_vec = ty_for_str("(Vector Int Int Int)");
        let at_least_one_int_vec = ty_for_str("(Vector Int ... Int)");

        assert_eq!(Result::Yes, is_a(&vecof_int, &vecof_any));
        assert_eq!(Result::May, is_a(&vecof_any, &vecof_int));

        assert_eq!(Result::Yes, is_a(&two_ints_vec, &vecof_int));
        assert_eq!(Result::May, is_a(&vecof_int, &two_ints_vec));
        assert_eq!(Result::Yes, is_a(&two_ints_vec, &vecof_any));

        assert_eq!(Result::No, is_a(&two_ints_vec, &three_ints_vec));
        assert_eq!(Result::No, is_a(&three_ints_vec, &two_ints_vec));

        assert_eq!(Result::Yes, is_a(&at_least_one_int_vec, &vecof_int));
        assert_eq!(Result::May, is_a(&vecof_int, &at_least_one_int_vec));
    }

    #[test]
    fn fun_types() {
        let impure_any_to_sym = ty_for_str("(->! Any Symbol)");
        let impure_sym_to_any = ty_for_str("(->! Symbol Any)");
        let impure_sym_to_sym = ty_for_str("(->! Symbol Symbol)");
        let pure_sym_to_sym = ty_for_str("(-> Symbol Symbol)");

        assert_eq!(Result::Yes, is_a(&impure_sym_to_sym, &impure_sym_to_any));
        assert_eq!(Result::Yes, is_a(&impure_any_to_sym, &impure_sym_to_sym));
        assert_eq!(Result::May, is_a(&impure_sym_to_any, &impure_sym_to_sym));

        assert_eq!(Result::Yes, is_a(&pure_sym_to_sym, &impure_sym_to_sym));
        assert_eq!(Result::No, is_a(&impure_sym_to_sym, &pure_sym_to_sym));
    }

    #[test]
    fn bool_types() {
        let true_type = ty_for_str("true");
        let false_type = ty_for_str("false");
        let bool_type = ty_for_str("Bool");

        assert_eq!(Result::Yes, is_a(&true_type, &bool_type));
        assert_eq!(Result::May, is_a(&bool_type, &true_type));
        assert_eq!(Result::No, is_a(&false_type, &true_type));
    }
}
