use std::hash::{Hash, Hasher};

use arret_syntax::span::Span;

use arret_runtime::boxed::prelude::*;
use arret_runtime::boxed::Heap;

use crate::mir::builder::Builder;
use crate::mir::costing::{cost_for_ops, OpCost, OpCostFactor};
use crate::mir::env_values::EnvValues;
use crate::mir::error::{Error, Result};
use crate::mir::eval_hir::ApplyArgs;
use crate::mir::eval_hir::EvalHirCtx;
use crate::mir::eval_hir::FunCtx;
use crate::mir::optimise::optimise_inlined_fun;
use crate::mir::value;
use crate::mir::value::Value;
use crate::ty;

/// Maximum number of consecutive inlinings in a call stack
const MAX_INLINE_DEPTH: usize = 16;

/// Opaque hash of an Arret fun application
///
/// This is used to heuristically detect recursion loops
#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub struct ApplyCookie {
    arret_fun_id: value::ArretFunId,
    arg_hash: u64,
}

impl ApplyCookie {
    pub fn new(heap: &Heap, arret_fun: &value::ArretFun, arg_list_value: &Value) -> Self {
        ApplyCookie {
            arret_fun_id: arret_fun.id(),
            arg_hash: hash_for_arg_list_value(heap, arg_list_value),
        }
    }
}

/// Tracks a stack of inline Arret fun applications
///
/// This is used to make inlining decisions
pub struct ApplyStack {
    entries: Vec<ApplyCookie>,
}

impl ApplyStack {
    pub fn new() -> ApplyStack {
        ApplyStack { entries: vec![] }
    }

    fn with_apply_cookie(&self, apply_cookie: ApplyCookie) -> ApplyStack {
        use std::iter;

        ApplyStack {
            entries: self
                .entries
                .iter()
                .cloned()
                .chain(iter::once(apply_cookie))
                .collect(),
        }
    }
}

/// Returns the scaling factor we should use to prefer inlining for a given value
///
/// This is used to identify values that lose significant information when converted to a reg and
/// back as part of a call. This is intended as a proxy for the "hidden" cost of calling to an
/// out-of-line function that has lost optimisation-related information.
///
/// These factors seem small but the we take the product of all args and captured free values.
/// This can cause the final factor to be quite large.
fn inline_preference_factor_for_value(arg_value: &Value) -> OpCostFactor {
    match arg_value {
        Value::Reg(_) => 1.0,
        // Consts allow for const evaling, const propagation, dead code elimination, etc
        Value::Const(_) => 1.1,
        // Lists and records can partially evaluate their type-specific operations
        Value::List(_, _) | Value::Record(_, _) => 1.2,
        // RustFuns can be const eval'ed
        Value::RustFun(_) => 1.5,
        // These can be const eval'ed or completely inlined
        Value::ArretFun(_)
        | Value::TyPred(_)
        | Value::EqPred
        | Value::RecordCons(_)
        | Value::FieldAccessor(_, _) => 2.0,
    }
}

/// Returns the product of the inline scaling factor for each of our arguments
fn inline_preference_factor_for_arg_list_value(arg_list_value: &Value) -> OpCostFactor {
    match arg_list_value {
        Value::List(fixed, rest) => fixed
            .iter()
            .chain(rest.iter().map(AsRef::as_ref))
            .map(inline_preference_factor_for_value)
            .product(),
        Value::Const(_) => 1.5,
        _ => 1.0,
    }
}

/// Returns the product of the inline scaling factor for each captured value
fn inline_preference_factor_for_env_values(env_values: &EnvValues) -> OpCostFactor {
    env_values
        .free_values
        .iter()
        .map(|(_, value)| inline_preference_factor_for_value(value))
        .product()
}

/// Returns the scaling factor we should use to prefer inlining for the given application
fn calc_inline_preference_factor(
    arret_fun: &value::ArretFun,
    arg_list_value: &Value,
) -> OpCostFactor {
    inline_preference_factor_for_arg_list_value(arg_list_value)
        * inline_preference_factor_for_env_values(arret_fun.env_values())
}

/// Hashes the passed value, poorly
///
/// This can only distinguish constants; regs hash to the same value. It's possible for constants
/// would compare as equal to receive different hashes depending on their representation.
fn hash_value<H: Hasher>(heap: &Heap, value: &Value, state: &mut H) {
    match value {
        Value::List(fixed, rest) => {
            state.write_u8(0);

            state.write_usize(fixed.len());
            for member in fixed.iter() {
                hash_value(heap, member, state);
            }

            state.write_u8(rest.is_some() as u8);
            if let Some(rest_value) = rest {
                hash_value(heap, rest_value, state);
            }
        }
        Value::Record(record_cons, fields) => {
            state.write_u8(1);
            record_cons.hash(state);
            for field in fields.iter() {
                hash_value(heap, field, state);
            }
        }
        Value::Const(any_ref) => {
            state.write_u8(2);
            any_ref.hash_in_heap(heap, state);
        }
        Value::EqPred => {
            state.write_u8(3);
        }
        Value::TyPred(test_ty) => {
            state.write_u8(4);
            test_ty.hash(state);
        }
        Value::RecordCons(record_cons) => {
            state.write_u8(5);
            record_cons.hash(state);
        }
        Value::FieldAccessor(record_cons, field_index) => {
            state.write_u8(6);
            record_cons.hash(state);
            field_index.hash(state);
        }
        Value::RustFun(rust_fun) => {
            state.write_u8(7);
            rust_fun.symbol().hash(state);
        }
        Value::ArretFun(arret_fun) => {
            state.write_u8(8);

            state.write_usize(arret_fun.env_values().const_values.len());
            for (_, const_value) in arret_fun.env_values().const_values.iter() {
                hash_value(heap, const_value, state);
            }
        }
        Value::Reg(_) => {
            state.write_u8(9);
        }
    };
}

/// Hashes the arg list, poorly
///
/// This is used to detect if a recursive loop is making forward progress. Collisions will cause us
/// to abort recursive inlining.
fn hash_for_arg_list_value(heap: &Heap, arg_list_value: &Value) -> u64 {
    use std::collections::hash_map::DefaultHasher;
    let mut state = DefaultHasher::new();

    hash_value(heap, arg_list_value, &mut state);
    state.finish()
}

/// Conditionally inlines an Arret fun
///
/// This makes an inlining decision based on four criteria:
///
/// 1. The approximate cost of performing a call versus inlining. This is calculated by attempting
///    both options and measuring the cost of the ops they build.
///
/// 2. The amount of knowledge lost by calling through a fun and arg regs. This is referred to as
///    the inlining preference factor. This is multiplied against the call cost.
///
/// 3. If the inlining limit has been reached. This is a basic fixed threshold.
///
/// 4. If we've seen this exact call before in our inlining call stack. In that case we use
///    `Err::AbortRecursion` to "unwind" back to the original inlining and replace it with a call.
///
/// Due to the amount of partial evaluation we support this is very eager to inline. Out-of-line
/// calls should only be used in extreme cases.
pub(super) fn cond_inline<'a>(
    ehx: &mut EvalHirCtx,
    fcx: &mut FunCtx<'_>,
    outer_b: &mut Builder,
    span: Span,
    ret_ty: &ty::Ref<ty::Mono>,
    arret_fun: &value::ArretFun,
    apply_args: ApplyArgs<'a>,
) -> Result<value::Value> {
    // We need to build an out-of-line call in every case
    let mut call_b = Builder::new();
    let call_result =
        ehx.build_arret_fun_app(fcx, &mut call_b, span, ret_ty, arret_fun, &apply_args);
    let call_ops = call_b.into_ops();

    let apply_stack = &fcx.inliner_stack;
    let apply_cookie = ApplyCookie::new(ehx.as_heap(), arret_fun, &apply_args.list_value);
    if apply_stack.entries.len() >= MAX_INLINE_DEPTH || apply_stack.entries.contains(&apply_cookie)
    {
        // Abort recursion all the way back to the original call of this function

        // This prevents us from doing a "partial unroll" where we recurse in to one iteration
        // of the fun application and then bail out to a call. This is a bit gnarly as we're
        // using errors for flow control but it's isolated to this function.
        let abort_to = apply_stack
            .entries
            .iter()
            .find(|apply_cookie| apply_cookie.arret_fun_id == arret_fun.id());

        if let Some(abort_to) = abort_to {
            return Err(Error::AbortRecursion(*abort_to));
        } else {
            // Inline limit reached; don't attempt another inline
            outer_b.append(call_ops.into_vec().into_iter());
            return call_result;
        }
    }

    let mut inline_b = Some(Builder::new());
    let apply_stack = apply_stack.with_apply_cookie(apply_cookie);

    // Figure out how much we should prefer an inline version
    let inline_preference_factor = calc_inline_preference_factor(arret_fun, &apply_args.list_value);

    // Build an inline version
    let inline_result =
        ehx.inline_arret_fun_app(fcx, &mut inline_b, span, arret_fun, apply_args, apply_stack);

    let inline_ops = inline_b.unwrap().into_ops();
    let inline_ops = if let Ok(ref return_value) = inline_result {
        // In order to cost the inline function accurately we need to optimise it first
        optimise_inlined_fun(inline_ops, return_value)
    } else {
        inline_ops
    };

    // Determine if calling is cheaper
    let call_cost = cost_for_ops(call_ops.iter());
    let inline_cost = cost_for_ops(inline_ops.iter());

    if ((call_cost as OpCostFactor * inline_preference_factor) as OpCost) < inline_cost {
        // Calling is cheaper than inlining
        outer_b.append(call_ops.into_vec().into_iter());
        return call_result;
    }

    match inline_result {
        Ok(_) | Err(Error::Diverged) => {
            // We either succeeded or hit a divergence - use the steps
            outer_b.append(inline_ops.into_vec().into_iter());
            inline_result
        }
        Err(Error::AbortRecursion(abort_to_cookie)) if abort_to_cookie == apply_cookie => {
            // We detected another application of this fun and requested recursion is aborted back
            // to this point
            outer_b.append(call_ops.into_vec().into_iter());
            call_result
        }
        Err(other) => Err(other),
    }
}
