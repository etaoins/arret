mod list;
use crate::mir::intrinsic::list::*;

use crate::mir::partial_eval::PartialEvalCtx;
use crate::mir::value::ListIterator;
use crate::mir::{Expr, Value};

trait Intrinsic {
    fn eval_exprs(
        pcx: &mut PartialEvalCtx,
        fixed_exprs: &[Expr],
        rest_expr: Option<&Expr>,
    ) -> Option<Value> {
        let fixed_values: Vec<Value> = fixed_exprs.iter().map(|arg| pcx.eval_expr(arg)).collect();
        let rest_value = rest_expr.map(|rest_arg| Box::new(pcx.eval_expr(rest_arg)));

        let args_value = Value::List(fixed_values.into_boxed_slice(), rest_value);
        Self::eval_args_value(pcx, args_value)
    }

    fn eval_args_value(pcx: &mut PartialEvalCtx, args_value: Value) -> Option<Value> {
        let iter = args_value.list_iter();
        Self::eval_arg_list(pcx, iter)
    }

    fn eval_arg_list(_pcx: &mut PartialEvalCtx, _iter: ListIterator<'_>) -> Option<Value> {
        panic!("Intrinsic does not implement `eval_arg_list`")
    }
}

macro_rules! define_intrinsics {
    ( $($name:expr => $handler:ident),* ) => {
        pub fn try_eval(
            pcx: &mut PartialEvalCtx,
            intrinsic_name: &'static str,
            fixed_exprs: &[Expr],
            rest_expr: Option<&Expr>,
        ) -> Option<Value> {
            match intrinsic_name {
                $(
                    $name => {
                        $handler::eval_exprs(pcx, fixed_exprs, rest_expr)
                    }
                ),*
                _ => None,
            }
        }
    };
}

define_intrinsics! {
    "length" => Length
}
