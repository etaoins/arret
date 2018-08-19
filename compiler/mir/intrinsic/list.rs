use runtime::boxed;
use runtime::boxed::prelude::*;

use crate::mir::intrinsic::Intrinsic;
use crate::mir::partial_eval::PartialEvalCtx;
use crate::mir::value::ListIterator;
use crate::mir::Value;

pub struct Length {}

impl Intrinsic for Length {
    fn eval_arg_list(pcx: &mut PartialEvalCtx, mut iter: ListIterator<'_>) -> Option<Value> {
        let single_arg = iter.next_unchecked();

        match single_arg {
            Value::List(fixed, None) => {
                let known_length = fixed.len();

                Some(Value::Const(
                    boxed::Int::new(pcx, known_length as i64).as_any_ref(),
                ))
            }
            _ => None,
        }
    }
}
