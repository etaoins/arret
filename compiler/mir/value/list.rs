use std::vec;

use arret_syntax::span::Span;

use crate::mir::builder::{Builder, TryToBuilder};
use crate::mir::value;
use crate::mir::value::types::TypeHint;
use crate::mir::value::Value;

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum ListValueLen {
    Exact(usize),
    Min(usize),
}

impl ListValueLen {
    pub fn lower_bound(&self) -> usize {
        match self {
            ListValueLen::Exact(len) => *len,
            ListValueLen::Min(len) => *len,
        }
    }
}

impl std::ops::Add for ListValueLen {
    type Output = ListValueLen;

    fn add(self, other: ListValueLen) -> ListValueLen {
        match (self, other) {
            (ListValueLen::Exact(self_len), ListValueLen::Exact(other_len)) => {
                ListValueLen::Exact(self_len + other_len)
            }

            _ => ListValueLen::Min(self.lower_bound() + other.lower_bound()),
        }
    }
}

pub fn list_value_len(value: &Value) -> ListValueLen {
    use arret_runtime::boxed;

    match value {
        Value::List(fixed, rest) => {
            let fixed_len = ListValueLen::Exact(fixed.len());

            match rest {
                Some(rest) => fixed_len + list_value_len(rest),
                None => fixed_len,
            }
        }

        Value::Const(any_ref) => match any_ref.downcast_ref::<boxed::List<boxed::Any>>() {
            Some(list_ref) => ListValueLen::Exact(list_ref.len()),
            None => ListValueLen::Min(0),
        },

        Value::Reg(reg_value) => {
            if !reg_value.possible_type_tags.contains(boxed::TypeTag::Pair) {
                // Must be empty
                ListValueLen::Exact(0)
            } else if !reg_value.possible_type_tags.contains(boxed::TypeTag::Nil) {
                if let TypeHint::KnownListLen(len) = reg_value.type_hint {
                    ListValueLen::Exact(len)
                } else {
                    // Cannot be empty
                    ListValueLen::Min(1)
                }
            } else {
                ListValueLen::Min(0)
            }
        }

        _ => ListValueLen::Min(0),
    }
}

pub struct UnsizedListIterator {
    fixed: vec::IntoIter<Value>,
    rest: Option<Value>,
}

impl UnsizedListIterator {
    pub fn new(value: Value) -> Self {
        Self {
            fixed: Vec::new().into_iter(),
            rest: Some(value),
        }
    }

    /// Returns the next element in the list
    ///
    /// It is undefined if the list has no more elements. This function may panic, generate
    /// nonsense code, generate code that crashes at runtime, etc.
    #[must_use]
    pub fn next_unchecked(&mut self, b: &mut impl TryToBuilder, span: Span) -> Value {
        if let Some(next) = self.fixed.next() {
            return next;
        }

        let rest_value = self
            .rest
            .take()
            .expect("ran off the end of list with no rest argument");

        match rest_value {
            Value::List(fixed, rest) => {
                // Become our tail
                self.fixed = fixed.into_vec().into_iter();
                self.rest = rest.map(|rest| *rest);

                self.next_unchecked(b, span)
            }
            Value::Const(any_ref) => {
                use arret_runtime::boxed;

                let const_pair = any_ref
                    .downcast_ref::<boxed::Pair<boxed::Any>>()
                    .expect("tried to pop off non-pair constant");

                let tail = const_pair.rest();
                self.rest = if tail.is_empty() {
                    None
                } else {
                    Some(tail.into())
                };

                const_pair.head().into()
            }
            Value::Reg(reg_value) => {
                let b = b
                    .try_to_builder()
                    .expect("popping rest argument without builder");

                self.build_rest_next(b, span, &reg_value)
            }
            other => unimplemented!("popping rest argument off value {:?}", other),
        }
    }

    /// Returns a Value containing the rest of the iterator
    #[must_use]
    pub fn into_rest(self) -> Value {
        Value::List(self.fixed.collect(), self.rest.map(Box::new))
    }

    fn build_rest_next(
        &mut self,
        b: &mut Builder,
        span: Span,
        current_rest_value: &value::RegValue,
    ) -> Value {
        use crate::mir::ops::*;
        use crate::mir::value::build_reg::reg_to_boxed_reg;
        use arret_runtime::abitype;

        let needed_pair_type = abitype::BoxedAbiType::Pair(&abitype::BoxedAbiType::Any);
        let current_rest_reg = reg_to_boxed_reg(b, span, current_rest_value, &needed_pair_type);

        let head_reg = b.push_reg(span, OpKind::LoadBoxedPairHead, current_rest_reg.into());
        let rest_reg = b.push_reg(span, OpKind::LoadBoxedPairRest, current_rest_reg.into());

        self.rest =
            Some(value::RegValue::new(rest_reg, abitype::TOP_LIST_BOXED_ABI_TYPE.into()).into());

        value::RegValue::new(head_reg, abitype::BoxedAbiType::Any.into()).into()
    }
}

pub struct SizedListIterator {
    size: usize,
    unsized_list_iterator: UnsizedListIterator,
}

impl SizedListIterator {
    pub fn try_new(value: &Value) -> Option<Self> {
        match list_value_len(value) {
            ListValueLen::Exact(size) => Some(Self {
                size,
                unsized_list_iterator: UnsizedListIterator::new(value.clone()),
            }),
            _ => None,
        }
    }
}

impl SizedListIterator {
    pub fn next(&mut self, b: &mut impl TryToBuilder, span: Span) -> Option<Value> {
        if self.size == 0 {
            return None;
        }

        self.size -= 1;
        Some(self.unsized_list_iterator.next_unchecked(b, span))
    }

    #[must_use]
    pub fn into_rest(self) -> Value {
        self.unsized_list_iterator.into_rest()
    }

    pub fn len(&self) -> usize {
        self.size
    }

    pub fn is_empty(&self) -> bool {
        self.size == 0
    }
}

#[cfg(test)]
mod test {
    use super::*;

    use arret_runtime::boxed;
    use arret_runtime::boxed::prelude::*;

    use crate::source::EMPTY_SPAN;

    #[test]
    fn list_len() {
        use crate::mir::builder::BuiltReg;
        use crate::mir::ops::RegId;
        use arret_runtime::abitype;

        let mut heap = boxed::Heap::empty();
        let elements = &[1, 2, 3];

        // Start with three fixed values
        let fixed_values: Box<[Value]> = elements
            .iter()
            .map(|element| boxed::Int::new(&mut heap, *element).into())
            .collect();

        // Have a constant list tail
        let boxed_list_tail =
            boxed::List::from_values(&mut heap, elements.iter().cloned(), boxed::Int::new);

        let const_list_tail = Value::List(Box::new([]), Some(Box::new(boxed_list_tail.into())));

        // Add the fixed values (3 elements) to the constant tail (3 elements)
        let list_value = Value::List(fixed_values.clone(), Some(Box::new(const_list_tail)));

        // The length should be 6
        assert_eq!(ListValueLen::Exact(6), list_value_len(&list_value));

        // Try 3 fixed values with a completely unknown tail
        let list_with_unknown_tail = Value::List(
            fixed_values.clone(),
            Some(Box::new(
                value::RegValue::new(
                    BuiltReg::Local(RegId::alloc()),
                    abitype::BoxedAbiType::Any.into(),
                )
                .into(),
            )),
        );

        // Length should be at least 3
        assert_eq!(
            ListValueLen::Min(3),
            list_value_len(&list_with_unknown_tail)
        );

        // Try 3 fixed values with a pair tail
        let list_with_pair_tail = Value::List(
            fixed_values.clone(),
            Some(Box::new(
                value::RegValue::new(BuiltReg::Local(RegId::alloc()), boxed::TypeTag::Pair.into())
                    .into(),
            )),
        );

        // Length should be at least 4
        assert_eq!(ListValueLen::Min(4), list_value_len(&list_with_pair_tail));

        // Try 3 fixed values with a nil tail
        let list_with_nil_tail = Value::List(
            fixed_values,
            Some(Box::new(
                value::RegValue::new(BuiltReg::Local(RegId::alloc()), boxed::TypeTag::Nil.into())
                    .into(),
            )),
        );

        // Length should be at exactly 3
        assert_eq!(ListValueLen::Exact(3), list_value_len(&list_with_nil_tail));
    }

    #[test]
    fn const_unsized_list_iter() {
        let mut heap = boxed::Heap::empty();

        let elements = &[1, 2, 3];

        let boxed_list =
            boxed::List::from_values(&mut heap, elements.iter().cloned(), boxed::Int::new);

        let mut iter = UnsizedListIterator {
            fixed: Vec::new().into_iter(),
            rest: Some(boxed_list.into()),
        };

        for expected in elements {
            let next_value = iter.next_unchecked(&mut None, EMPTY_SPAN);

            if let Value::Const(next_ref) = next_value {
                let expected_ref = boxed::Int::new(&mut heap, *expected).as_any_ref();
                assert!(expected_ref.eq_in_heap(&heap, &next_ref));
            } else {
                panic!("expected const value, got {:?}", next_value);
            }
        }
    }

    #[test]
    fn fixed_list_value_unsized_iter() {
        let mut heap = boxed::Heap::empty();

        let elements = &[1, 2, 3];

        let element_values: Vec<Value> = elements
            .iter()
            .map(|element| boxed::Int::new(&mut heap, *element).into())
            .collect();

        let mut iter = UnsizedListIterator {
            fixed: element_values.into_iter(),
            rest: None,
        };

        for expected in elements {
            let next_value = iter.next_unchecked(&mut None, EMPTY_SPAN);

            if let Value::Const(next_ref) = next_value {
                let expected_ref = boxed::Int::new(&mut heap, *expected).as_any_ref();
                assert!(expected_ref.eq_in_heap(&heap, &next_ref));
            } else {
                panic!("expected const value, got {:?}", next_value);
            }
        }
    }
}
