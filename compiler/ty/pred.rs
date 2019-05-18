use crate::ty;
use crate::ty::purity::Purity;
use crate::ty::record;

#[derive(PartialEq, Eq, Debug, Clone, Hash)]
pub enum TestTy {
    Sym,
    Str,
    Bool,
    Num,
    Int,
    Float,
    Char,
    List,
    Vector,
    Set,
    Map,
    Fun,
    Nil,
    TopRecord(record::ConsId),
}

impl TestTy {
    pub fn match_subject_ref<M: ty::PM>(&self, ty_ref: &ty::Ref<M>) -> Option<bool> {
        let resolved_ty = ty_ref.resolve_to_ty();
        match resolved_ty {
            ty::Ty::Any => None,
            ty::Ty::Sym | ty::Ty::LitSym(_) => Some(self == &TestTy::Sym),
            ty::Ty::Bool | ty::Ty::LitBool(_) => Some(self == &TestTy::Bool),
            ty::Ty::Char => Some(self == &TestTy::Char),
            ty::Ty::Float => Some(self == &TestTy::Float || self == &TestTy::Num),
            ty::Ty::Map(_) => Some(self == &TestTy::Map),
            ty::Ty::Int => Some(self == &TestTy::Int || self == &TestTy::Num),
            ty::Ty::Num => match self {
                TestTy::Num => Some(true),
                TestTy::Int | TestTy::Float => None,
                _ => Some(false),
            },
            ty::Ty::Set(_) => Some(self == &TestTy::Set),
            ty::Ty::Str => Some(self == &TestTy::Str),
            ty::Ty::Fun(_) | ty::Ty::TopFun(_) | ty::Ty::TyPred(_) | ty::Ty::EqPred => {
                Some(self == &TestTy::Fun)
            }
            ty::Ty::List(list) => match self {
                TestTy::Nil => {
                    if list.is_empty() {
                        Some(true)
                    } else if list.fixed().is_empty() {
                        None
                    } else {
                        Some(false)
                    }
                }
                TestTy::List => Some(true),
                _ => Some(false),
            },
            ty::Ty::Vector(_) | ty::Ty::Vectorof(_) => Some(self == &TestTy::Vector),
            ty::Ty::TopRecord(cons) => {
                if let TestTy::TopRecord(test_cons) = self {
                    Some(cons == test_cons)
                } else {
                    Some(false)
                }
            }
            ty::Ty::Record(instance) => {
                if let TestTy::TopRecord(test_cons) = self {
                    Some(instance.cons() == test_cons)
                } else {
                    Some(false)
                }
            }
            ty::Ty::Union(members) => {
                let results: Vec<Option<bool>> = members
                    .iter()
                    .map(|member| self.match_subject_ref(member))
                    .collect();

                if results.contains(&None) {
                    None
                } else if !results.contains(&Some(false)) {
                    Some(true)
                } else if !results.contains(&Some(true)) {
                    Some(false)
                } else {
                    None
                }
            }
            ty::Ty::Intersect(members) => {
                let results: Vec<Option<bool>> = members
                    .iter()
                    .map(|member| self.match_subject_ref(member))
                    .collect();

                if results.contains(&Some(true)) {
                    Some(true)
                } else if results.contains(&None) {
                    None
                } else {
                    Some(false)
                }
            }
        }
    }

    pub fn to_ty(&self) -> ty::Ty<ty::Poly> {
        use crate::ty::ty_args::TyArgs;

        match self {
            TestTy::Sym => ty::Ty::Sym,
            TestTy::Str => ty::Ty::Str,
            TestTy::Bool => ty::Ty::Bool,
            TestTy::Num => ty::Ty::Num,
            TestTy::Int => ty::Ty::Int,
            TestTy::Float => ty::Ty::Float,
            TestTy::Char => ty::Ty::Char,
            TestTy::List => ty::List::new(Box::new([]), ty::Ty::Any.into()).into(),
            TestTy::Vector => ty::Ty::Vectorof(Box::new(ty::Ty::Any.into())),
            TestTy::Set => ty::Ty::Set(Box::new(ty::Ty::Any.into())),
            TestTy::Map => ty::Map::new(ty::Ty::Any.into(), ty::Ty::Any.into()).into(),
            TestTy::Fun => ty::TopFun::new(Purity::Impure.into(), ty::Ty::Any.into()).into(),
            TestTy::Nil => ty::List::empty().into(),
            TestTy::TopRecord(cons) => {
                if cons.poly_params().is_empty() {
                    // There's a single instance of this record; we can return the instance type.
                    // Instance types can be used in more situations than top types.
                    record::Instance::new(cons.clone(), TyArgs::empty()).into()
                } else {
                    ty::Ty::TopRecord(cons.clone())
                }
            }
        }
    }

    pub fn to_string(&self) -> String {
        match self {
            TestTy::Str => "str?".to_owned(),
            TestTy::Sym => "sym?".to_owned(),
            TestTy::Num => "num?".to_owned(),
            TestTy::Int => "int?".to_owned(),
            TestTy::Float => "float?".to_owned(),
            TestTy::Bool => "bool?".to_owned(),
            TestTy::Char => "char?".to_owned(),
            TestTy::List => "list?".to_owned(),
            TestTy::Vector => "vector?".to_owned(),
            TestTy::Set => "set?".to_owned(),
            TestTy::Map => "map?".to_owned(),
            TestTy::Fun => "fn?".to_owned(),
            TestTy::Nil => "nil?".to_owned(),
            TestTy::TopRecord(cons) => format!("{}?", cons.name()),
        }
    }
}

#[cfg(test)]
mod test {
    use super::*;
    use arret_syntax::span::EMPTY_SPAN;

    fn assert_test_ty_will_match(test_ty: &TestTy, subject_ref: impl Into<ty::Ref<ty::Poly>>) {
        let subject_ref = subject_ref.into();
        assert_eq!(
            true,
            ty::is_a::ty_ref_is_a(&subject_ref, &test_ty.to_ty().into()),
            "Subject type is not a definite subtype of the test type"
        );
        assert_eq!(Some(true), test_ty.match_subject_ref(&subject_ref),);
    }

    fn assert_test_ty_may_match(test_ty: &TestTy, subject_ref: impl Into<ty::Ref<ty::Poly>>) {
        let subject_ref = subject_ref.into();
        assert_eq!(
            false,
            ty::is_a::ty_ref_is_a(&subject_ref, &test_ty.to_ty().into()),
            "Subject type is a definite subtype of the test type"
        );
        assert_eq!(None, test_ty.match_subject_ref(&subject_ref),);
    }

    fn assert_test_ty_wont_match(test_ty: &TestTy, subject_ref: impl Into<ty::Ref<ty::Poly>>) {
        let subject_ref = subject_ref.into();
        assert_eq!(
            false,
            ty::is_a::ty_ref_is_a(&subject_ref, &test_ty.to_ty().into()),
            "Subject type is a definite subtype of the test type"
        );
        assert_eq!(Some(false), test_ty.match_subject_ref(&subject_ref),);
    }

    fn assert_trivial_test_ty(expected_ty: ty::Ty<ty::Poly>, test_ty: TestTy) {
        let unrelated_ty: ty::Ty<ty::Poly> = if expected_ty == ty::Ty::Char {
            ty::Ty::Str
        } else {
            ty::Ty::Char
        };

        assert_eq!(expected_ty, test_ty.to_ty());

        assert_test_ty_will_match(&test_ty, expected_ty);
        assert_test_ty_wont_match(&test_ty, unrelated_ty);
    }

    #[test]
    fn sym_test_ty() {
        let test_ty = TestTy::Sym;

        assert_test_ty_will_match(&test_ty, ty::Ty::Sym);
        assert_test_ty_will_match(&test_ty, ty::Ty::LitSym("foo".into()));
        assert_test_ty_wont_match(&test_ty, ty::Ty::Str);
    }

    #[test]
    fn str_test_ty() {
        assert_trivial_test_ty(ty::Ty::Str, TestTy::Str);
    }

    #[test]
    fn bool_test_ty() {
        let test_ty = TestTy::Bool;

        assert_test_ty_will_match(&test_ty, ty::Ty::Bool);
        assert_test_ty_will_match(&test_ty, ty::Ty::LitBool(false));
        assert_test_ty_wont_match(&test_ty, ty::Ty::Str);
    }

    #[test]
    fn num_test_ty() {
        let test_ty = TestTy::Num;

        assert_test_ty_will_match(&test_ty, ty::Ty::Num);
        assert_test_ty_will_match(&test_ty, ty::Ty::Int);
        assert_test_ty_will_match(&test_ty, ty::Ty::Float);
        assert_test_ty_wont_match(&test_ty, ty::Ty::Str);
    }

    #[test]
    fn int_test_ty() {
        let test_ty = TestTy::Int;

        assert_test_ty_will_match(&test_ty, ty::Ty::Int);
        assert_test_ty_may_match(&test_ty, ty::Ty::Num);
        assert_test_ty_wont_match(&test_ty, ty::Ty::Float);
    }

    #[test]
    fn float_test_ty() {
        let test_ty = TestTy::Float;

        assert_test_ty_will_match(&test_ty, ty::Ty::Float);
        assert_test_ty_may_match(&test_ty, ty::Ty::Num);
        assert_test_ty_wont_match(&test_ty, ty::Ty::Int);
    }

    #[test]
    fn char_test_ty() {
        assert_trivial_test_ty(ty::Ty::Char, TestTy::Char);
    }

    #[test]
    fn list_test_ty() {
        assert_trivial_test_ty(
            ty::List::new(Box::new([]), ty::Ty::Any.into()).into(),
            TestTy::List,
        );
    }

    #[test]
    fn vector_test_ty() {
        let test_ty = TestTy::Vector;

        assert_test_ty_will_match(&test_ty, ty::Ty::Vector(Box::new([])));
        assert_test_ty_will_match(&test_ty, ty::Ty::Vectorof(Box::new(ty::Ty::Any.into())));
        assert_test_ty_wont_match(&test_ty, ty::Ty::Int);
    }

    #[test]
    fn set_test_ty() {
        assert_trivial_test_ty(ty::Ty::Set(Box::new(ty::Ty::Any.into())), TestTy::Set);
    }

    #[test]
    fn map_test_ty() {
        assert_trivial_test_ty(
            ty::Map::new(ty::Ty::Any.into(), ty::Ty::Any.into()).into(),
            TestTy::Map,
        );
    }

    #[test]
    fn fun_test_ty() {
        let test_ty = TestTy::Fun;

        assert_test_ty_will_match(&test_ty, ty::TopFun::new_for_pred());
        assert_test_ty_will_match(&test_ty, ty::Fun::new_for_main());
        assert_test_ty_wont_match(&test_ty, ty::Ty::Str);
    }

    #[test]
    fn nil_test_ty() {
        let test_ty = TestTy::Nil;

        assert_test_ty_will_match(&test_ty, ty::List::empty());
        assert_test_ty_may_match(&test_ty, ty::List::new(Box::new([]), ty::Ty::Any.into()));
        assert_test_ty_wont_match(
            &test_ty,
            ty::List::new(Box::new([ty::Ty::Any.into()]), ty::Ty::never().into()),
        );
    }

    #[test]
    fn top_record_test_ty() {
        use crate::id_type::ArcId;
        use crate::ty::ty_args::TyArgs;

        let cons = ArcId::new(record::Cons::new(
            EMPTY_SPAN,
            "cons".into(),
            Box::new([]),
            Box::new([]),
        ));

        let other_cons = ArcId::new(record::Cons::new(
            EMPTY_SPAN,
            "other_cons".into(),
            Box::new([]),
            Box::new([]),
        ));

        let test_ty = TestTy::TopRecord(cons.clone());

        let test_instance_poly: ty::Ref<ty::Poly> =
            record::Instance::new(cons, TyArgs::empty()).into();
        let other_instance_poly: ty::Ref<ty::Poly> =
            record::Instance::new(other_cons, TyArgs::empty()).into();

        assert_test_ty_will_match(&test_ty, test_instance_poly);
        assert_test_ty_may_match(&test_ty, ty::Ty::Any);
        assert_test_ty_wont_match(&test_ty, other_instance_poly);
    }

    #[test]
    fn union_subject_ref() {
        let str_sym_union: ty::Ref<ty::Poly> =
            ty::Ty::Union(Box::new([ty::Ty::Str.into(), ty::Ty::Sym.into()])).into();

        assert_test_ty_may_match(&TestTy::Str, str_sym_union.clone());
        assert_test_ty_may_match(&TestTy::Sym, str_sym_union.clone());
        assert_test_ty_wont_match(&TestTy::Int, str_sym_union.clone());

        let list_false_union: ty::Ref<ty::Poly> = ty::Ty::Union(Box::new([
            ty::List::new(Box::new([]), ty::Ty::Any.into()).into(),
            ty::Ty::LitBool(false).into(),
        ]))
        .into();

        assert_test_ty_may_match(&TestTy::Nil, list_false_union);

        let never: ty::Ref<ty::Poly> = ty::Ty::never().into();
        assert_test_ty_will_match(&TestTy::Str, never.clone());
        assert_test_ty_will_match(&TestTy::Sym, never.clone());
    }

    #[test]
    fn intersect_subject_ref() {
        let str_num_intersect: ty::Ref<ty::Poly> =
            ty::Ty::Intersect(Box::new([ty::Ty::Str.into(), ty::Ty::Num.into()])).into();

        assert_test_ty_will_match(&TestTy::Str, str_num_intersect.clone());
        assert_test_ty_may_match(&TestTy::Int, str_num_intersect.clone());
        assert_test_ty_wont_match(&TestTy::Sym, str_num_intersect.clone());
    }
}
