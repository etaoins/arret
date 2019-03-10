mod list;
mod math;
mod testing;
mod number;

use syntax::span::Span;

use crate::mir::builder::Builder;
use crate::mir::error::Result;
use crate::mir::eval_hir::EvalHirCtx;
use crate::mir::Value;

macro_rules! define_intrinsics {
    ( $($name:expr => $handler:path),* ) => {
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
                        $handler(ehx, b, span, arg_list_value)
                    }
                ),*
                _ => Ok(None),
            }
        }
    };
}

define_intrinsics! {
    "length" => list::length,
    "cons" => list::cons,
    "fn-op-categories" => testing::fn_op_categories,
    "+" => math::add,
    "*" => math::mul,
    "int" => number::int,
    "float" => number::float
}
