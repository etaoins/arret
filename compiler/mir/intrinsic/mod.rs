mod bitwise;
mod list;
mod math;
mod num_utils;
mod number;
mod panics;
mod testing;
mod vector;

use arret_syntax::span::Span;

use crate::mir::builder::Builder;
use crate::mir::error::Result;
use crate::mir::eval_hir::EvalHirCtx;
use crate::mir::Value;

macro_rules! define_eval_intrinsics {
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

macro_rules! define_build_intrinsics {
    ( $($name:expr => $handler:path),* ) => {
        pub fn try_build(
            ehx: &mut EvalHirCtx,
            b: &mut Builder,
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

define_eval_intrinsics! {
    "length" => list::length,
    "cons" => list::cons,
    "repeat" => list::repeat,
    "fn-op-categories" => testing::fn_op_categories
}

define_build_intrinsics! {
    "+" => math::add,
    "*" => math::mul,
    "-" => math::sub,
    "/" => math::div,
    "quot" => math::quot,
    "rem" => math::rem,
    "sqrt" => math::sqrt,

    "int" => number::int,
    "float" => number::float,
    "<" => number::num_lt,
    "<=" => number::num_le,
    "==" => number::num_eq,
    ">" => number::num_gt,
    ">=" => number::num_ge,

    // Purity doesn't matter at the MIR level; these are both treated as impure so they're not
    // optimised away.
    "panic" => panics::panics,
    "panic!" => panics::panics,

    "vector-length" => vector::vector_length,
    "vector-ref" => vector::vector_ref,

    "bit-and" => bitwise::bit_and,
    "bit-or" => bitwise::bit_or,
    "bit-xor" => bitwise::bit_xor,
    "bit-not" => bitwise::bit_not,
    "bit-shift-left" => bitwise::bit_shift_left,
    "bit-shift-right" => bitwise::bit_shift_right,
    "unsigned-bit-shift-right" => bitwise::unsigned_bit_shift_right
}
