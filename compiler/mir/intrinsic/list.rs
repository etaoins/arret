use syntax::span::Span;

use runtime::boxed;
use runtime::boxed::prelude::*;

use crate::mir::builder::Builder;
use crate::mir::error::Result;
use crate::mir::eval_hir::EvalHirCtx;
use crate::mir::intrinsic::Intrinsic;
use crate::mir::value::list::{list_value_length, ListIterator};
use crate::mir::Value;

pub struct Length {}

impl Intrinsic for Length {
    fn eval_arg_list(
        ehx: &mut EvalHirCtx,
        b: &mut Option<Builder>,
        span: Span,
        mut iter: ListIterator,
    ) -> Result<Option<Value>> {
        let single_arg = iter.next_unchecked(b, span);

        Ok(list_value_length(&single_arg).map(|known_length| {
            Value::Const(boxed::Int::new(ehx, known_length as i64).as_any_ref())
        }))
    }
}

pub struct Cons {}

impl Intrinsic for Cons {
    fn eval_arg_list(
        _ehx: &mut EvalHirCtx,
        b: &mut Option<Builder>,
        span: Span,
        mut iter: ListIterator,
    ) -> Result<Option<Value>> {
        let head = iter.next_unchecked(b, span);
        let rest = iter.next_unchecked(b, span);

        Ok(Some(Value::List(Box::new([head]), Some(Box::new(rest)))))
    }
}
