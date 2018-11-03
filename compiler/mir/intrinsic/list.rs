use std::rc::Rc;

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

        if let Some(known_length) = list_value_length(&single_arg) {
            return Ok(Some(Value::Const(
                boxed::Int::new(ehx, known_length as i64).as_any_ref(),
            )));
        }

        if let Some(b) = b {
            use crate::mir::ops::*;
            use crate::mir::value;
            use crate::mir::value::build_reg::value_to_reg;
            use runtime::abitype;

            let list_reg = value_to_reg(
                ehx,
                b,
                span,
                &single_arg,
                &abitype::TOP_LIST_BOXED_ABI_TYPE.into(),
            );

            let usize_length_reg = b.push_reg(span, OpKind::LoadBoxedListLength, list_reg.into());
            let i64_length_reg = b.push_reg(span, OpKind::UsizeToInt64, usize_length_reg);

            return Ok(Some(Value::Reg(Rc::new(value::RegValue {
                reg: i64_length_reg,
                abi_type: abitype::ABIType::Int,
            }))));
        }

        Ok(None)
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
