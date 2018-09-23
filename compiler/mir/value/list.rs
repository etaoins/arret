use std::rc::Rc;
use std::vec;

use syntax::span::Span;

use crate::mir::builder::{Builder, TryToBuilder};
use crate::mir::value;
use crate::mir::value::Value;

pub fn list_value_length(value: &Value) -> Option<usize> {
    match value {
        Value::List(fixed, rest) => {
            let fixed_len = fixed.len();

            match rest {
                Some(rest) => list_value_length(rest).map(|rest_len| fixed_len + rest_len),
                None => Some(fixed_len),
            }
        }
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

    pub fn next_unchecked(&mut self, b: &mut impl TryToBuilder, span: Span) -> Value {
        match self.fixed.next() {
            Some(next) => next,
            None => {
                match self.rest.take() {
                    Some(Value::List(fixed, rest)) => {
                        // Become our tail
                        self.fixed = fixed.into_vec().into_iter();
                        self.rest = rest.map(|rest| *rest);

                        self.next_unchecked(b, span)
                    }
                    Some(other_rest) => {
                        if let Some(b) = b.try_to_builder() {
                            self.build_rest_next(b, span, &other_rest)
                        } else {
                            panic!("Tried to iterate in to unknown list tail without builder")
                        }
                    }
                    _ => {
                        panic!("Ran off the end of list with no rest argument");
                    }
                }
            }
        }
    }

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
        current_rest_value: &Value,
    ) -> Value {
        use crate::mir::ops::*;
        use crate::mir::value::to_reg::value_to_reg;
        use runtime::abitype;

        let needed_pair_type = abitype::BoxedABIType::Pair(&abitype::BoxedABIType::Any).into();
        let current_rest_reg = value_to_reg(b, span, &current_rest_value, &needed_pair_type);

        let head_reg = b.push_reg(span, OpKind::LoadBoxedPairHead, current_rest_reg);
        let rest_reg = b.push_reg(span, OpKind::LoadBoxedPairRest, current_rest_reg);

        self.rest = Some(Value::Reg(Rc::new(value::RegValue {
            reg: rest_reg,
            abi_type: abitype::TOP_LIST_BOXED_ABI_TYPE.into(),
        })));

        Value::Reg(Rc::new(value::RegValue {
            reg: head_reg,
            abi_type: abitype::BoxedABIType::Any.into(),
        }))
    }
}
