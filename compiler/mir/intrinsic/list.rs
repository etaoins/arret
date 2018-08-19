use runtime::boxed;
use runtime::boxed::prelude::*;

use crate::mir::intrinsic::Intrinsic;
use crate::mir::partial_eval::PartialEvalCtx;
use crate::mir::value::ListIterator;
use crate::mir::Value;

pub struct Length {}

impl Length {
    fn value_len(value: &Value) -> Option<usize> {
        match value {
            Value::List(fixed, rest) => {
                let fixed_len = fixed.len();

                match rest {
                    Some(rest) => Self::value_len(rest).map(|rest_len| fixed_len + rest_len),
                    None => Some(fixed_len),
                }
            }
            _ => None,
        }
    }
}

impl Intrinsic for Length {
    fn eval_arg_list(pcx: &mut PartialEvalCtx, mut iter: ListIterator<'_>) -> Option<Value> {
        let single_arg = iter.next_unchecked();

        Self::value_len(single_arg).map(|known_length| {
            Value::Const(boxed::Int::new(pcx, known_length as i64).as_any_ref())
        })
    }
}
