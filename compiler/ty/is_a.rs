use crate::ty;
use crate::ty::list_iter::ListIterator;
use crate::ty::purity;
use crate::ty::purity::Purity;
use crate::ty::record;
use crate::ty::var_usage::Variance;
use crate::ty::Ty;

fn top_fun_is_a(sub_top_fun: &ty::TopFun, par_top_fun: &ty::TopFun) -> bool {
    purity_ref_is_a(sub_top_fun.purity(), par_top_fun.purity())
        && ty_ref_is_a(sub_top_fun.ret(), par_top_fun.ret())
}

fn list_is_a<M: ty::Pm>(sub_list: &ty::List<M>, par_list: &ty::List<M>) -> bool {
    if (sub_list.fixed().len() > par_list.fixed().len()) && !par_list.has_rest() {
        // sub is longer than par
        return false;
    }

    if sub_list.fixed().len() < par_list.fixed().len() {
        // sub is less specific due to less fixed types
        return false;
    }

    if !ty_ref_is_a(sub_list.rest(), par_list.rest()) {
        return false;
    }

    // Compare our fixed types. If the par fixed ends early we'll use the par rest.
    sub_list
        .fixed()
        .iter()
        .zip(ListIterator::new(par_list))
        .all(|(sub, par)| ty_ref_is_a(sub, par))
}

fn record_field_is_a<F, R>(variance: Variance, is_a: &F, sub: &R, par: &R) -> bool
where
    F: Fn(&R, &R) -> bool,
{
    match variance {
        Variance::Covariant => is_a(sub, par),
        Variance::Contravariant => is_a(par, sub),
        Variance::Invariant => is_a(sub, par) && is_a(par, sub),
    }
}

fn record_instance_is_a<M: ty::Pm>(
    sub_instance: &record::Instance<M>,
    par_instance: &record::Instance<M>,
) -> bool {
    // Make sure they came from the same constructor and satisfy their params
    sub_instance.cons() == par_instance.cons()
        && sub_instance
            .cons()
            .poly_params()
            .iter()
            .all(|poly_param| match poly_param {
                record::PolyParam::PVar(variance, pvar) => record_field_is_a(
                    *variance,
                    &purity_ref_is_a,
                    &sub_instance.ty_args().pvar_purities()[pvar],
                    &par_instance.ty_args().pvar_purities()[pvar],
                ),
                record::PolyParam::TVar(variance, tvar) => record_field_is_a(
                    *variance,
                    &ty_ref_is_a,
                    &sub_instance.ty_args().tvar_types()[tvar],
                    &par_instance.ty_args().tvar_types()[tvar],
                ),
                record::PolyParam::Pure(_) | record::PolyParam::TFixed(_, _) => true,
            })
}

fn monomorphic_fun_is_a(sub_fun: &ty::Fun, par_fun: &ty::Fun) -> bool {
    top_fun_is_a(sub_fun.top_fun(), par_fun.top_fun()) &&
        // Note that parameters are contravariant
        list_is_a(par_fun.params(), sub_fun.params())
}

fn fun_is_a(sub_fun: &ty::Fun, par_fun: &ty::Fun) -> bool {
    if sub_fun.has_polymorphic_vars() {
        let sub_mono = inst_polymorphic_fun(sub_fun, par_fun.top_fun());
        monomorphic_fun_is_a(&sub_mono, par_fun)
    } else {
        monomorphic_fun_is_a(sub_fun, par_fun)
    }
}

fn ty_is_a<M: ty::Pm>(
    sub_ref: &ty::Ref<M>,
    sub_ty: &Ty<M>,
    parent_ref: &ty::Ref<M>,
    parent_ty: &Ty<M>,
) -> bool {
    if sub_ty == parent_ty {
        return true;
    }

    match (sub_ty, parent_ty) {
        // Union types
        (Ty::Union(sub_members), _) => sub_members
            .iter()
            .all(|sub_member| ty_ref_is_a(sub_member, parent_ref)),
        (_, Ty::Union(par_members)) => par_members
            .iter()
            .any(|par_member| ty_ref_is_a(sub_ref, par_member)),

        // Intersection types
        (_, Ty::Intersect(par_members)) => par_members
            .iter()
            .all(|par_member| ty_ref_is_a(sub_ref, par_member)),
        (Ty::Intersect(sub_members), _) => sub_members
            .iter()
            .any(|sub_member| ty_ref_is_a(sub_member, parent_ref)),

        // Any type
        (_, Ty::Any) => true,

        // Sym types
        (Ty::LitSym(_), Ty::Sym) => true,

        // Bool types
        (Ty::LitBool(_), Ty::Bool) => true,

        // Floats
        (Ty::Float, Ty::Num) => true,

        // Ints
        (Ty::Int, Ty::Num) => true,

        // Sets
        (Ty::Set(sub), Ty::Set(par)) => ty_ref_is_a(sub.as_ref(), par.as_ref()),

        // Maps
        (Ty::Map(sub_map), Ty::Map(par_map)) => {
            ty_ref_is_a(sub_map.key(), par_map.key())
                && ty_ref_is_a(sub_map.value(), par_map.value())
        }

        // Vector types
        (Ty::Vector(sub_members), Ty::Vector(par_members)) => {
            (sub_members.len() == par_members.len())
                && sub_members
                    .iter()
                    .zip(par_members.iter())
                    .all(|(sub_member, par_member)| ty_ref_is_a(sub_member, par_member))
        }
        (Ty::Vectorof(sub_member), Ty::Vectorof(par_member)) => {
            ty_ref_is_a(sub_member.as_ref(), par_member.as_ref())
        }
        (Ty::Vector(sub_members), Ty::Vectorof(par_member)) => sub_members
            .iter()
            .all(|sub_member| ty_ref_is_a(sub_member, par_member)),

        // Functions
        (Ty::TopFun(sub_top_fun), Ty::TopFun(par_top_fun)) => {
            top_fun_is_a(sub_top_fun, par_top_fun)
        }
        (Ty::Fun(sub_fun), Ty::TopFun(par_top_fun)) => {
            if sub_fun.has_polymorphic_vars() {
                let sub_mono = inst_polymorphic_fun(sub_fun, par_top_fun);
                top_fun_is_a(sub_mono.top_fun(), par_top_fun)
            } else {
                top_fun_is_a(sub_fun.top_fun(), par_top_fun)
            }
        }
        (Ty::Fun(sub_fun), Ty::Fun(par_fun)) => fun_is_a(sub_fun, par_fun),

        // All predicate types
        (Ty::TyPred(_), Ty::TopFun(par_top_fun)) | (Ty::EqPred, Ty::TopFun(par_top_fun)) => {
            top_fun_is_a(&ty::TopFun::new_for_pred(), par_top_fun)
        }

        // Type predicate types
        (Ty::TyPred(_), Ty::Fun(par_fun)) => fun_is_a(&ty::Fun::new_for_ty_pred(), par_fun),

        // Equality predicate type
        (Ty::EqPred, Ty::Fun(par_fun)) => fun_is_a(&ty::Fun::new_for_eq_pred(), par_fun),

        // List types
        (Ty::List(sub_list), Ty::List(par_list)) => list_is_a(sub_list, par_list),

        // Record types
        (Ty::RecordClass(_), Ty::TopRecord) => true,
        (Ty::Record(_), Ty::TopRecord) => true,

        (Ty::Record(sub_instance), Ty::Record(par_instance)) => {
            record_instance_is_a(sub_instance, par_instance)
        }
        (Ty::RecordClass(sub_cons), Ty::RecordClass(par_cons)) => sub_cons == par_cons,
        (Ty::Record(sub_instance), Ty::RecordClass(par_cons)) => sub_instance.cons() == par_cons,
        (Ty::RecordClass(sub_cons), Ty::Record(par_instance)) => {
            // If the record class has no polymorphic params then it only has one instance
            sub_cons == par_instance.cons() && sub_cons.poly_params().is_empty()
        }

        _ => false,
    }
}

fn tvar_is_bounded_by(sub_tvar: &ty::TVarId, parent_tvar: &ty::TVarId) -> bool {
    if sub_tvar == parent_tvar {
        return true;
    }

    match &sub_tvar.bound {
        ty::Ref::Fixed(_) => false,
        ty::Ref::Var(tvar, _) => tvar_is_bounded_by(tvar, parent_tvar),
    }
}

fn purity_ref_is_a(sub: &purity::Ref, parent: &purity::Ref) -> bool {
    sub == &purity::Ref::Fixed(Purity::Pure)
        || sub == parent
        || parent == &purity::Ref::Fixed(Purity::Impure)
}

fn inst_polymorphic_fun(sub_fun: &ty::Fun, par_top_fun: &ty::TopFun) -> ty::Fun {
    let mut stx = ty::select::SelectCtx::new(sub_fun.pvars(), sub_fun.tvars());

    stx.add_evidence(sub_fun.ret(), par_top_fun.ret());
    stx.add_evidence_purity(sub_fun.purity(), par_top_fun.purity());
    let pta = stx.into_poly_ty_args();

    ty::subst::subst_poly_fun(&pta, sub_fun)
}

pub fn ty_ref_is_a<M: ty::Pm>(sub: &ty::Ref<M>, parent: &ty::Ref<M>) -> bool {
    if let ty::Ref::Var(parent_tvar, _) = parent {
        // Typically `parent_is_bound` makes the best result for a polymorphic parent `May`.
        // These are overrides for cases where they can be `Yes`.
        match sub {
            ty::Ref::Var(sub_tvar, _) => {
                // Are we either the same var our bounded by the same var?
                if tvar_is_bounded_by(sub_tvar, parent_tvar) {
                    return true;
                }
            }
            ty::Ref::Fixed(Ty::Intersect(sub_members)) => {
                // Do we satisfy any of the members of the intersection?
                if sub_members
                    .iter()
                    .any(|sub_member| ty_ref_is_a(sub_member, parent))
                {
                    return true;
                }
            }
            _ => {}
        };
    }

    let sub_ty = sub.resolve_to_ty();
    if sub_ty.is_never() {
        // (U) is a definite subtype of every type, regardless if the parent is bound. This is
        // important as (U) is used as a placeholder for parameters with unknown type. More
        // generally, it's the contravariant equivalent of Any.
        return true;
    }

    if let ty::Ref::Var(_, _) = parent {
        return false;
    }

    let parent_ty = parent.resolve_to_ty();
    ty_is_a(sub, sub_ty, parent, parent_ty)
}

/// Determines if two type references are equivalent
///
/// Our type system has no canonical union order, allows record class types with only a single
/// possible instance, etc. This makes normal `PartialEq` unreliable for determining if the type
/// system would treat two types identically. This function is more expensive but can reliably
/// detect equivalent types with different representations.
pub fn ty_refs_equivalent<M: ty::Pm>(ty_ref1: &ty::Ref<M>, ty_ref2: &ty::Ref<M>) -> bool {
    ty_ref_is_a(ty_ref1, ty_ref2) && ty_ref_is_a(ty_ref2, ty_ref1)
}

/// Determines if two purity refs are equivalent
///
/// This is for symmetry with [`ty_refs_equivalent`]
pub fn purity_refs_equivalent(purity_ref1: &purity::Ref, purity_ref2: &purity::Ref) -> bool {
    purity_ref1 == purity_ref2
}

#[cfg(test)]
mod test {
    use super::*;

    use crate::hir::{poly_for_str, tvar_bounded_by};
    use crate::source::EMPTY_SPAN;

    #[test]
    fn sym_types() {
        let foo_sym = poly_for_str("'foo");
        let bar_sym = poly_for_str("'bar");
        let any_sym = poly_for_str("Sym");
        let any_int = poly_for_str("Int");

        assert!(ty_ref_is_a(&foo_sym, &foo_sym));
        assert!(!ty_ref_is_a(&foo_sym, &bar_sym));

        assert!(ty_ref_is_a(&foo_sym, &any_sym));
        assert!(!ty_ref_is_a(&any_sym, &foo_sym));

        assert!(!ty_ref_is_a(&any_sym, &any_int));
        assert!(!ty_ref_is_a(&any_int, &any_sym));
    }

    #[test]
    fn set_types() {
        let foo_set = poly_for_str("(Setof 'foo)");
        let bar_set = poly_for_str("(Setof 'bar)");
        let sym_set = poly_for_str("(Setof Sym)");

        assert!(ty_ref_is_a(&foo_set, &foo_set));
        assert!(!ty_ref_is_a(&foo_set, &bar_set));

        assert!(ty_ref_is_a(&foo_set, &sym_set));
        assert!(!ty_ref_is_a(&sym_set, &foo_set));
    }

    #[test]
    fn map_types() {
        let foo_sym = poly_for_str("'foo");
        let any_sym = poly_for_str("Sym");
        let any_int = poly_for_str("Int");

        let int_to_any_sym = ty::Map::new(any_int.clone(), any_sym.clone()).into();

        let int_to_foo_sym = ty::Map::new(any_int, foo_sym).into();

        let any_sym_to_any_sym = ty::Map::new(any_sym.clone(), any_sym).into();

        assert!(ty_ref_is_a(&int_to_foo_sym, &int_to_any_sym));
        assert!(!ty_ref_is_a(&int_to_any_sym, &int_to_foo_sym));
        assert!(!ty_ref_is_a(&int_to_any_sym, &any_sym_to_any_sym));
    }

    #[test]
    fn union_types() {
        let foo_sym = poly_for_str("'foo");
        let baz_sym = poly_for_str("'baz");

        let foo_bar_union = poly_for_str("(RawU 'foo 'bar)");
        let bar_baz_union = poly_for_str("(RawU 'bar 'baz)");
        let foo_bar_baz_union = poly_for_str("(RawU 'foo 'bar 'baz)");
        let never = poly_for_str("(RawU)");

        assert!(ty_ref_is_a(&foo_sym, &foo_bar_union));
        assert!(!ty_ref_is_a(&baz_sym, &foo_bar_union));
        assert!(!ty_ref_is_a(&baz_sym, &never));

        assert!(!ty_ref_is_a(&foo_bar_union, &foo_sym));
        assert!(!ty_ref_is_a(&foo_bar_union, &baz_sym));
        assert!(ty_ref_is_a(&never, &foo_sym));

        assert!(!ty_ref_is_a(&foo_bar_union, &bar_baz_union));
        assert!(ty_ref_is_a(&foo_bar_union, &foo_bar_union));
        assert!(ty_ref_is_a(&never, &foo_bar_union));

        assert!(ty_ref_is_a(&foo_bar_union, &foo_bar_baz_union));
    }

    #[test]
    fn intersect_types() {
        let ptype1 = tvar_bounded_by(Ty::Any.into());
        let ptype2 = tvar_bounded_by(Ty::Any.into());

        let any_sym = poly_for_str("Sym");
        let foo_sym = poly_for_str("'foo");

        let sym_poly1_intersection =
            Ty::Intersect(Box::new([ptype1.clone(), any_sym.clone()])).into();
        let sym_poly2_intersection =
            Ty::Intersect(Box::new([ptype2.clone(), any_sym.clone()])).into();
        let sym_poly1_poly2_intersection =
            Ty::Intersect(Box::new([ptype1.clone(), ptype2, any_sym.clone()])).into();

        // `Sym` might not be `Poly`
        assert!(!ty_ref_is_a(&any_sym, &sym_poly1_intersection));

        // Our intersection must be both `Sym` and `Poly
        assert!(ty_ref_is_a(&sym_poly1_intersection, &any_sym));
        assert!(ty_ref_is_a(&sym_poly1_intersection, &ptype1));

        // However, it might not be a 'foo
        assert!(!ty_ref_is_a(&sym_poly1_intersection, &foo_sym));

        // A more specific intersection must satisfy a less specific one
        assert!(ty_ref_is_a(
            &sym_poly1_poly2_intersection,
            &sym_poly1_intersection
        ));

        // A less specific intersection may satisfy a more specific one
        assert!(!ty_ref_is_a(
            &sym_poly1_intersection,
            &sym_poly1_poly2_intersection,
        ));

        // Partially disjoint intersections may satisfy each other
        assert!(!ty_ref_is_a(
            &sym_poly1_intersection,
            &sym_poly2_intersection
        ));
    }

    #[test]
    fn any_and_never_types() {
        let any = poly_for_str("Any");
        let never = Ty::never().into();
        let foo_sym = poly_for_str("'foo");

        assert!(ty_ref_is_a(&foo_sym, &any));
        assert!(!ty_ref_is_a(&any, &foo_sym));
        assert!(ty_ref_is_a(&never, &any));
        assert!(ty_ref_is_a(&never, &never));
        assert!(!ty_ref_is_a(&any, &never));
    }

    #[test]
    fn list_types() {
        let empty_list = poly_for_str("()");
        let listof_any = poly_for_str("(List & Any)");
        let listof_int = poly_for_str("(List & Int)");
        let two_ints_list = poly_for_str("(List Int Int)");
        let three_ints_list = poly_for_str("(List Int Int Int)");
        let at_least_one_int_list = poly_for_str("(List Int & Int)");

        assert!(ty_ref_is_a(&empty_list, &listof_any));
        assert!(!ty_ref_is_a(&listof_any, &empty_list));

        assert!(ty_ref_is_a(&listof_int, &listof_any));
        assert!(!ty_ref_is_a(&listof_any, &listof_int));

        assert!(ty_ref_is_a(&two_ints_list, &listof_int));
        assert!(!ty_ref_is_a(&listof_int, &two_ints_list));
        assert!(ty_ref_is_a(&two_ints_list, &listof_any));

        assert!(!ty_ref_is_a(&two_ints_list, &three_ints_list));
        assert!(!ty_ref_is_a(&three_ints_list, &two_ints_list));

        assert!(ty_ref_is_a(&at_least_one_int_list, &listof_int));
        assert!(!ty_ref_is_a(&listof_int, &at_least_one_int_list));
    }

    #[test]
    fn vec_types() {
        let vecof_any = poly_for_str("(Vectorof Any)");
        let vecof_int = poly_for_str("(Vectorof Int)");
        let two_ints_vec = poly_for_str("(Vector Int Int)");
        let three_ints_vec = poly_for_str("(Vector Int Int Int)");

        assert!(ty_ref_is_a(&vecof_int, &vecof_any));
        assert!(!ty_ref_is_a(&vecof_any, &vecof_int));

        assert!(ty_ref_is_a(&two_ints_vec, &vecof_int));
        assert!(!ty_ref_is_a(&vecof_int, &two_ints_vec));
        assert!(ty_ref_is_a(&two_ints_vec, &vecof_any));

        assert!(!ty_ref_is_a(&two_ints_vec, &three_ints_vec));
        assert!(!ty_ref_is_a(&three_ints_vec, &two_ints_vec));
    }

    #[test]
    fn num_types() {
        let int = poly_for_str("Int");
        let float = poly_for_str("Float");
        let num = poly_for_str("Num");

        assert!(ty_ref_is_a(&int, &num));
        assert!(ty_ref_is_a(&float, &num));
        assert!(ty_ref_is_a(&num, &num));
        assert!(!ty_ref_is_a(&float, &int));
        assert!(!ty_ref_is_a(&num, &int));
        assert!(!ty_ref_is_a(&num, &float));
    }

    #[test]
    fn fun_types() {
        let impure_any_to_sym = poly_for_str("(Any ->! Sym)");
        let impure_sym_to_any = poly_for_str("(Sym ->! Any)");
        let impure_sym_to_sym = poly_for_str("(Sym ->! Sym)");
        let pure_sym_to_sym = poly_for_str("(Sym -> Sym)");

        assert!(ty_ref_is_a(&impure_sym_to_sym, &impure_sym_to_any));
        assert!(ty_ref_is_a(&impure_any_to_sym, &impure_sym_to_sym));
        assert!(!ty_ref_is_a(&impure_sym_to_any, &impure_sym_to_sym));

        assert!(ty_ref_is_a(&pure_sym_to_sym, &impure_sym_to_sym));
        assert!(!ty_ref_is_a(&impure_sym_to_sym, &pure_sym_to_sym));
    }

    #[test]
    fn ty_pred_types() {
        let sym_ty_pred = poly_for_str("sym?");
        let str_ty_pred = poly_for_str("str?");
        let general_ty_pred = poly_for_str("(Any -> Bool)");
        let pred_top_fun = poly_for_str("(... -> Bool)");

        // Type predicates always equal themselves
        assert!(ty_ref_is_a(&sym_ty_pred, &sym_ty_pred));

        // Type predicates never equal other type predicates
        assert!(!ty_ref_is_a(&sym_ty_pred, &str_ty_pred));
        assert!(!ty_ref_is_a(&str_ty_pred, &sym_ty_pred));

        // Type predicates are a subtype of (Any -> Bool)
        assert!(ty_ref_is_a(&sym_ty_pred, &general_ty_pred));
        assert!(!ty_ref_is_a(&general_ty_pred, &sym_ty_pred));

        // Type predicates are a subtype of (... -> Bool)
        assert!(ty_ref_is_a(&sym_ty_pred, &pred_top_fun));
        assert!(!ty_ref_is_a(&pred_top_fun, &sym_ty_pred));
    }

    #[test]
    fn eq_pred_type() {
        let eq_pred = poly_for_str("=");
        let general_eq_pred = poly_for_str("(Any Any -> Bool)");
        let pred_top_fun = poly_for_str("(... -> Bool)");

        // Equality predicate equals itself
        assert!(ty_ref_is_a(&eq_pred, &eq_pred));

        // Equality predicate is a subtype of (Any Any -> Bool)
        assert!(ty_ref_is_a(&eq_pred, &general_eq_pred));
        assert!(!ty_ref_is_a(&general_eq_pred, &eq_pred));

        // Equality predicate is a subtype of (... -> Bool)
        assert!(ty_ref_is_a(&eq_pred, &pred_top_fun));
        assert!(!ty_ref_is_a(&pred_top_fun, &eq_pred));
    }

    #[test]
    fn bool_types() {
        let true_type = poly_for_str("true");
        let false_type = poly_for_str("false");
        let bool_type = poly_for_str("Bool");

        assert!(ty_ref_is_a(&true_type, &bool_type));
        assert!(!ty_ref_is_a(&bool_type, &true_type));
        assert!(!ty_ref_is_a(&false_type, &true_type));
    }

    #[test]
    fn poly_bool_types() {
        let true_type = poly_for_str("true");
        let false_type = poly_for_str("false");
        let bool_type = poly_for_str("Bool");

        assert!(ty_ref_is_a(&true_type, &bool_type));
        assert!(!ty_ref_is_a(&bool_type, &true_type));
        assert!(!ty_ref_is_a(&false_type, &true_type));
    }

    #[test]
    fn unbounded_poly_vars() {
        let ptype1 = tvar_bounded_by(Ty::Any.into());
        let ptype2 = tvar_bounded_by(Ty::Any.into());

        let poly_bool = poly_for_str("Bool");

        assert!(ty_ref_is_a(&ptype1, &ptype1));
        assert!(!ty_ref_is_a(&ptype1, &ptype2));
        assert!(!ty_ref_is_a(&ptype1, &poly_bool));
    }

    #[test]
    fn bounded_poly_vars() {
        let ptype1_sym = tvar_bounded_by(Ty::Sym.into());
        let ptype2_str = tvar_bounded_by(Ty::Str.into());

        let poly_foo_sym = poly_for_str("'foo");

        // A type var always satisfies itself
        assert!(ty_ref_is_a(&ptype1_sym, &ptype1_sym));

        // The bounds of these vars are disjoint
        assert!(!ty_ref_is_a(&ptype1_sym, &ptype2_str));

        // The type var may satisfy a more specific bound
        assert!(!ty_ref_is_a(&ptype1_sym, &poly_foo_sym));

        // A sub never satisfies a type var with a disjoint bound
        assert!(!ty_ref_is_a(&poly_foo_sym, &ptype2_str));

        // The sub has a fixed type while the parent has a poly type. We can't ensure that 'foo
        // satisfies all possible Sym subtypes (such as 'bar)
        assert!(!ty_ref_is_a(&poly_foo_sym, &ptype1_sym));
    }

    #[test]
    fn related_poly_bounds() {
        let ptype1_unbounded = tvar_bounded_by(Ty::Any.into());
        let ptype2_bounded_by_1 = tvar_bounded_by(ptype1_unbounded.clone());
        let ptype3_bounded_by_2 = tvar_bounded_by(ptype2_bounded_by_1.clone());

        // Direct bounding
        assert!(ty_ref_is_a(&ptype2_bounded_by_1, &ptype1_unbounded));
        assert!(ty_ref_is_a(&ptype3_bounded_by_2, &ptype2_bounded_by_1));

        // Commutative bounding
        assert!(ty_ref_is_a(&ptype3_bounded_by_2, &ptype1_unbounded));

        // Inverse bounding relationship may not satisfy - the bounded type can have arbitrary
        // subtypes
        assert!(!ty_ref_is_a(&ptype1_unbounded, &ptype2_bounded_by_1));
    }

    #[test]
    fn polymorphic_funs() {
        let pidentity_fun = poly_for_str("(All #{A} A -> A)");
        let pidentity_sym_fun = poly_for_str("(All #{[A Sym]} A -> A)");
        let pidentity_impure_string_fun = poly_for_str("(All #{[A Str]} A ->! A)");

        // All functions should have the top function type
        let top_fun = poly_for_str("(... ->! Any)");
        assert!(ty_ref_is_a(&pidentity_fun, &top_fun));
        assert!(ty_ref_is_a(&pidentity_sym_fun, &top_fun));
        assert!(ty_ref_is_a(&pidentity_impure_string_fun, &top_fun));

        // We should take in to account purity
        let top_pure_fun = poly_for_str("(... -> Any)");
        assert!(ty_ref_is_a(&pidentity_fun, &top_pure_fun));
        assert!(!ty_ref_is_a(&pidentity_impure_string_fun, &top_pure_fun));

        // All functions should have the top one param function type except panys
        let top_one_param_fun = poly_for_str("((RawU) ->! Any)");
        assert!(ty_ref_is_a(&pidentity_fun, &top_one_param_fun));
        assert!(ty_ref_is_a(&pidentity_sym_fun, &top_one_param_fun));
        assert!(ty_ref_is_a(
            &pidentity_impure_string_fun,
            &top_one_param_fun
        ));

        // The identity function is (Any -> Any)
        let any_to_any_fun = poly_for_str("(Any ->! Any)");
        assert!(ty_ref_is_a(&pidentity_fun, &any_to_any_fun));
        // However, (Any -> Any) is not the identity function because it can take mismatched types
        // (e.g. Int -> Float)
        assert!(!ty_ref_is_a(&any_to_any_fun, &pidentity_fun));

        // The identity function is (true -> true)
        let true_to_true_fun = poly_for_str("(true ->! true)");
        assert!(ty_ref_is_a(&pidentity_fun, &true_to_true_fun));
        assert!(!ty_ref_is_a(&true_to_true_fun, &pidentity_fun));

        // The identity function is not (true -> false)
        let true_to_true_fun = poly_for_str("(true ->! false)");
        assert!(!ty_ref_is_a(&pidentity_fun, &true_to_true_fun));
        assert!(!ty_ref_is_a(&true_to_true_fun, &pidentity_fun));

        // The symbol function satisfies ((U) -> Sym) as all of its returns must be bounded by
        // that
        let top_to_sym_fun = poly_for_str("(... ->! Sym)");
        assert!(ty_ref_is_a(&pidentity_fun, &top_to_sym_fun));
        assert!(ty_ref_is_a(&pidentity_sym_fun, &top_to_sym_fun));

        // The identity string function satisfies (Str -> Str)
        let str_to_str_fun = poly_for_str("(Str ->! Str)");
        assert!(ty_ref_is_a(&pidentity_fun, &str_to_str_fun));
        assert!(!ty_ref_is_a(&pidentity_sym_fun, &str_to_str_fun));
        assert!(ty_ref_is_a(&pidentity_impure_string_fun, &str_to_str_fun));

        // The polymorphic identity string function satisfies (... ->! Str)
        let top_impure_str_fun = poly_for_str("(... ->! Str)");
        assert!(ty_ref_is_a(
            &pidentity_impure_string_fun,
            &top_impure_str_fun
        ));

        // As does the unbounded identity function
        assert!(ty_ref_is_a(&pidentity_fun, &top_impure_str_fun));

        // But not the polymorphic symbol function
        assert!(!ty_ref_is_a(&pidentity_sym_fun, &top_impure_str_fun));
    }

    #[test]
    fn polymorphic_purity_funs() {
        let poly_purity_fun = poly_for_str("(All #{[->_ ->!]} (->_ Str) ->_ Str)");
        // This is the upper bound of `poly_purity_fun
        let mono_purity_fun = poly_for_str("((->! Str) -> Str)");
        let top_to_str_fun = poly_for_str("(... -> Str)");

        assert!(ty_ref_is_a(&poly_purity_fun, &top_to_str_fun));
        assert!(ty_ref_is_a(&mono_purity_fun, &poly_purity_fun));
        assert!(!ty_ref_is_a(&top_to_str_fun, &poly_purity_fun));
    }

    #[test]
    fn distinct_record_cons_instances() {
        use crate::ty::ty_args::TyArgs;

        let cons1 = record::Cons::new(
            EMPTY_SPAN,
            "cons1".into(),
            "cons1?".into(),
            None,
            Box::new([]),
        );
        let cons2 = record::Cons::new(
            EMPTY_SPAN,
            "cons2".into(),
            "cons2?".into(),
            None,
            Box::new([]),
        );

        let instance1_poly: ty::Ref<ty::Poly> =
            record::Instance::new(cons1, TyArgs::empty()).into();
        let instance2_poly: ty::Ref<ty::Poly> =
            record::Instance::new(cons2, TyArgs::empty()).into();

        // Different record constructors
        assert!(!ty_ref_is_a(&instance1_poly, &instance2_poly));
        assert!(!ty_ref_is_a(&instance2_poly, &instance1_poly));

        // They're both top records
        assert!(ty_ref_is_a(&instance1_poly, &Ty::TopRecord.into()));
        assert!(ty_ref_is_a(&instance2_poly, &Ty::TopRecord.into()));
    }

    #[test]
    fn same_cons_record_instances() {
        use crate::ty::ty_args::TyArgs;
        use std::collections::HashMap;

        let tvar1 = ty::TVar::new(EMPTY_SPAN, "tvar1".into(), Ty::Any.into());
        let tvar2 = ty::TVar::new(EMPTY_SPAN, "tvar2".into(), Ty::Any.into());
        let tvar3 = ty::TVar::new(EMPTY_SPAN, "tvar3".into(), Ty::Any.into());

        let cons = record::Cons::new(
            EMPTY_SPAN,
            "cons".into(),
            "cons?".into(),
            Some(Box::new([
                record::PolyParam::TVar(Variance::Covariant, tvar1.clone()),
                record::PolyParam::TVar(Variance::Contravariant, tvar2.clone()),
                record::PolyParam::TVar(Variance::Invariant, tvar3.clone()),
            ])),
            Box::new([
                record::Field::new(EMPTY_SPAN, "covariant".into(), tvar1.clone().into()),
                record::Field::new(EMPTY_SPAN, "contravariant".into(), tvar2.clone().into()),
                record::Field::new(EMPTY_SPAN, "invariant".into(), tvar3.clone().into()),
            ]),
        );

        let num_num_num_instance_poly: ty::Ref<ty::Poly> = record::Instance::new(
            cons.clone(),
            TyArgs::new(
                HashMap::new(),
                std::iter::once((tvar1.clone(), Ty::Num.into()))
                    .chain(std::iter::once((tvar2.clone(), Ty::Num.into())))
                    .chain(std::iter::once((tvar3.clone(), Ty::Num.into())))
                    .collect(),
            ),
        )
        .into();

        let int_any_num_instance_poly: ty::Ref<ty::Poly> = record::Instance::new(
            cons,
            TyArgs::new(
                HashMap::new(),
                std::iter::once((tvar1, Ty::Int.into()))
                    .chain(std::iter::once((tvar2, Ty::Any.into())))
                    .chain(std::iter::once((tvar3, Ty::Num.into())))
                    .collect(),
            ),
        )
        .into();

        assert!(ty_ref_is_a(
            &int_any_num_instance_poly,
            &num_num_num_instance_poly
        ));
        assert!(!ty_ref_is_a(
            &num_num_num_instance_poly,
            &int_any_num_instance_poly
        ));

        assert!(ty_ref_is_a(
            &num_num_num_instance_poly,
            &num_num_num_instance_poly
        ));
        assert!(ty_ref_is_a(
            &int_any_num_instance_poly,
            &int_any_num_instance_poly
        ));
    }
}
