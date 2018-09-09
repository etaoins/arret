mod list;

use syntax::span::Span;

use crate::mir::intrinsic::list::*;

use crate::mir::error::Result;
use crate::mir::partial_eval::{DefCtx, PartialEvalCtx};
use crate::mir::value::ListIterator;
use crate::mir::{Expr, Value};

trait Intrinsic {
    fn eval_exprs(
        pcx: &mut PartialEvalCtx,
        dcx: &mut DefCtx<'_>,
        span: Span,
        fixed_exprs: &[Expr],
        rest_expr: Option<&Expr>,
    ) -> Result<Option<Value>> {
        let fixed_values = fixed_exprs
            .iter()
            .map(|arg| pcx.eval_expr(dcx, arg))
            .collect::<Result<Vec<Value>>>()?;

        let rest_value = match rest_expr {
            Some(rest_arg) => Some(Box::new(pcx.eval_expr(dcx, rest_arg)?)),
            None => None,
        };

        let args_value = Value::List(fixed_values.into_boxed_slice(), rest_value);
        Self::eval_args_value(pcx, span, args_value)
    }

    fn eval_args_value(
        pcx: &mut PartialEvalCtx,
        span: Span,
        args_value: Value,
    ) -> Result<Option<Value>> {
        let iter = args_value.list_iter();
        Self::eval_arg_list(pcx, span, iter)
    }

    fn eval_arg_list(
        _pcx: &mut PartialEvalCtx,
        _span: Span,
        _iter: ListIterator<'_>,
    ) -> Result<Option<Value>> {
        panic!("Intrinsic does not implement `eval_arg_list`")
    }
}

macro_rules! define_intrinsics {
    ( $($name:expr => $handler:ident),* ) => {
        pub fn try_eval(
            pcx: &mut PartialEvalCtx,
            dcx: &mut DefCtx<'_>,
            span: Span,
            intrinsic_name: &'static str,
            fixed_exprs: &[Expr],
            rest_expr: Option<&Expr>,
        ) -> Result<Option<Value>> {
            match intrinsic_name {
                $(
                    $name => {
                        $handler::eval_exprs(pcx, dcx, span, fixed_exprs, rest_expr)
                    }
                ),*
                _ => Ok(None),
            }
        }
    };
}

define_intrinsics! {
    "length" => Length
}
