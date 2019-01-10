use std::rc::Rc;
use std::vec;

use syntax::span::Span;

use crate::mir::builder::{Builder, TryToBuilder};
use crate::mir::value;
use crate::mir::value::Value;

pub fn list_value_length(value: &Value) -> Option<usize> {
    use runtime::boxed;

    match value {
        Value::List(fixed, rest) => {
            let fixed_len = fixed.len();

            match rest {
                Some(rest) => list_value_length(rest).map(|rest_len| fixed_len + rest_len),
                None => Some(fixed_len),
            }
        }
        Value::Const(any_ref) => any_ref
            .downcast_ref::<boxed::TopList>()
            .map(|top_list| top_list.as_list().len()),
        _ => None,
    }
}

pub struct ListIterator {
    fixed: vec::IntoIter<Value>,
    rest: Option<Value>,
}

impl<'list> ListIterator {
    pub fn new(fixed: Vec<Value>, rest: Option<Value>) -> ListIterator {
        ListIterator {
            fixed: fixed.into_iter(),
            rest,
        }
    }

    /// Returns the next element in the list
    ///
    /// It is undefined if the list has no more elements. This function may panic, generate
    /// nonsense code, generate code that crashes at runtime, etc.
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
                use runtime::boxed;

                let const_pair = any_ref
                    .downcast_ref::<boxed::TopPair>()
                    .expect("tried to pop off non-pair constant")
                    .as_pair();

                let tail = const_pair.rest();
                self.rest = if tail.is_empty() {
                    None
                } else {
                    Some(Value::Const(tail.as_any_ref()))
                };

                Value::Const(const_pair.head())
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
    pub fn into_rest(self) -> Value {
        Value::List(
            self.fixed.collect::<Vec<Value>>().into_boxed_slice(),
            self.rest.map(Box::new),
        )
    }

    fn build_rest_next(
        &mut self,
        b: &mut Builder,
        span: Span,
        current_rest_value: &value::RegValue,
    ) -> Value {
        use crate::mir::ops::*;
        use crate::mir::value::build_reg::reg_to_boxed_reg;
        use runtime::abitype;

        let needed_pair_type = abitype::BoxedABIType::Pair(&abitype::BoxedABIType::Any);
        let current_rest_reg = reg_to_boxed_reg(b, span, &current_rest_value, &needed_pair_type);

        let head_reg = b.push_reg(span, OpKind::LoadBoxedPairHead, current_rest_reg.into());
        let rest_reg = b.push_reg(span, OpKind::LoadBoxedPairRest, current_rest_reg.into());

        self.rest = Some(Value::Reg(Rc::new(value::RegValue::new(
            rest_reg,
            abitype::TOP_LIST_BOXED_ABI_TYPE.into(),
        ))));

        Value::Reg(Rc::new(value::RegValue::new(
            head_reg,
            abitype::BoxedABIType::Any.into(),
        )))
    }
}

#[cfg(test)]
mod test {
    use super::*;

    use runtime::boxed;
    use runtime::boxed::prelude::*;
    use syntax::span::EMPTY_SPAN;

    #[test]
    fn list_length() {
        let mut heap = boxed::Heap::new();
        let elements = &[1, 2, 3];

        // Start with three fixed values
        let fixed_values: Vec<Value> = elements
            .iter()
            .map(|element| {
                let element_ref = boxed::Int::new(&mut heap, *element).as_any_ref();
                Value::Const(element_ref)
            })
            .collect();

        // Have a constant list tail
        let boxed_list_tail =
            boxed::List::<boxed::Int>::from_values(&mut heap, elements.iter().cloned());

        let const_list_tail = Value::List(
            Box::new([]),
            Some(Box::new(Value::Const(boxed_list_tail.as_any_ref()))),
        );

        // Add the fixed values (3 elements) to the constant tail (3 elements)
        let list_value = Value::List(
            fixed_values.into_boxed_slice(),
            Some(Box::new(const_list_tail)),
        );

        // The length should be 6
        assert_eq!(Some(6), list_value_length(&list_value));
    }

    #[test]
    fn const_list_iter() {
        let mut heap = boxed::Heap::new();

        let elements = &[1, 2, 3];

        let boxed_list =
            boxed::List::<boxed::Int>::from_values(&mut heap, elements.iter().cloned());

        let mut iter = ListIterator::new(vec![], Some(Value::Const(boxed_list.as_any_ref())));

        for expected in elements {
            let next_value = iter.next_unchecked(&mut None, EMPTY_SPAN);

            if let Value::Const(next_ref) = next_value {
                let expected_ref = boxed::Int::new(&mut heap, *expected).as_any_ref();
                assert_eq!(expected_ref, next_ref);
            } else {
                panic!("expected const value, got {:?}", next_value);
            }
        }
    }

    #[test]
    fn fixed_list_value_iter() {
        let mut heap = boxed::Heap::new();

        let elements = &[1, 2, 3];

        let element_values: Vec<Value> = elements
            .iter()
            .map(|element| {
                let element_ref = boxed::Int::new(&mut heap, *element).as_any_ref();
                Value::Const(element_ref)
            })
            .collect();

        let mut iter = ListIterator::new(element_values, None);

        for expected in elements {
            let next_value = iter.next_unchecked(&mut None, EMPTY_SPAN);

            if let Value::Const(next_ref) = next_value {
                let expected_ref = boxed::Int::new(&mut heap, *expected).as_any_ref();
                assert_eq!(expected_ref, next_ref);
            } else {
                panic!("expected const value, got {:?}", next_value);
            }
        }
    }
}
