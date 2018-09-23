mod list;

use syntax::span::Span;

use crate::mir::intrinsic::list::{Cons, Length};

use crate::mir::builder::Builder;
use crate::mir::error::Result;
use crate::mir::eval_hir::{DefCtx, EvalHirCtx};
use crate::mir::value::list::ListIterator;
use crate::mir::{Expr, Value};

trait Intrinsic {
    fn eval_exprs(
        ehx: &mut EvalHirCtx,
        dcx: &mut DefCtx,
        b: &mut Option<Builder>,
        span: Span,
        fixed_exprs: &[Expr],
        rest_expr: Option<&Expr>,
    ) -> Result<Option<Value>> {
        let fixed_values = fixed_exprs
            .iter()
            .map(|arg| ehx.eval_expr(dcx, b, arg))
            .collect::<Result<Vec<Value>>>()?;

        let rest_value = match rest_expr {
            Some(rest_arg) => Some(Box::new(ehx.eval_expr(dcx, b, rest_arg)?)),
            None => None,
        };

        let args_value = Value::List(fixed_values.into_boxed_slice(), rest_value);
        Self::eval_args_value(ehx, b, span, args_value)
    }

    fn eval_args_value(
        ehx: &mut EvalHirCtx,
        b: &mut Option<Builder>,
        span: Span,
        args_value: Value,
    ) -> Result<Option<Value>> {
        let iter = args_value.list_iter();
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
            dcx: &mut DefCtx,
            b: &mut Option<Builder>,
            span: Span,
            intrinsic_name: &'static str,
            fixed_exprs: &[Expr],
            rest_expr: Option<&Expr>,
        ) -> Result<Option<Value>> {
            match intrinsic_name {
                $(
                    $name => {
                        $handler::eval_exprs(ehx, dcx, b, span, fixed_exprs, rest_expr)
                    }
                ),*
                _ => Ok(None),
            }
        }
    };
}

define_intrinsics! {
    "length" => Length,
    "cons" => Cons
}
