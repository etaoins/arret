mod list;
mod testing;

use syntax::span::Span;

use crate::mir::intrinsic::list::{Cons, Length};
use crate::mir::intrinsic::testing::FnOpCategories;

use crate::mir::builder::Builder;
use crate::mir::error::Result;
use crate::mir::eval_hir::EvalHirCtx;
use crate::mir::value::list::ListIterator;
use crate::mir::Value;

trait Intrinsic {
    fn eval_arg_list_value(
        ehx: &mut EvalHirCtx,
        b: &mut Option<Builder>,
        span: Span,
        arg_list_value: &Value,
    ) -> Result<Option<Value>> {
        let iter = arg_list_value.list_iter();
        Self::eval_arg_list(ehx, b, span, iter)
    }

    fn eval_arg_list(
        _ehx: &mut EvalHirCtx,
        _b: &mut Option<Builder>,
        _span: Span,
        _iter: ListIterator,
    ) -> Result<Option<Value>> {
        panic!("Intrinsic does not implement `eval_arg_list`")
    }
}

macro_rules! define_intrinsics {
    ( $($name:expr => $handler:ident),* ) => {
        pub fn try_eval(
            ehx: &mut EvalHirCtx,
            b: &mut Option<Builder>,
            span: Span,
            intrinsic_name: &'static str,
            arg_list_value: &Value,
        ) -> Result<Option<Value>> {
            match intrinsic_name {
                $(
                    $name => {
                        $handler::eval_arg_list_value(ehx, b, span, arg_list_value)
                    }
                ),*
                _ => Ok(None),
            }
        }
    };
}

define_intrinsics! {
    "length" => Length,
    "cons" => Cons,
    "fn-op-categories" => FnOpCategories
}
