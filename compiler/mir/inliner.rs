use syntax::span::Span;

use crate::mir::builder::Builder;
use crate::mir::closure::Closure;
use crate::mir::error::{Error, Result};
use crate::mir::eval_hir::ApplyArgs;
use crate::mir::eval_hir::EvalHirCtx;
use crate::mir::eval_hir::FunCtx;
use crate::mir::ops;
use crate::mir::value;
use crate::mir::value::Value;
use crate::ty;

/// Opaque hash of an Arret fun application
///
/// This is used to heuristically detect recursion loops
#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub struct ApplyCookie(u64);

impl ApplyCookie {
    pub fn new(arret_fun: &value::ArretFun) -> Self {
        ApplyCookie(arret_fun.id.to_usize() as u64)
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

/// Abstract unit for measuring the runtime cost of ops
type OpCost = u32;

/// Abstract unit for a multiplier of an `OpCost`
type OpCostFactor = f32;

/// Returns the approximate runtime cost of an operation in an abstract unit
fn cost_for_op(op: &ops::Op) -> OpCost {
    use crate::mir::ops::OpCategory::*;

    let inner_cost = if let ops::OpKind::Cond(cond_op) = op.kind() {
        cost_for_ops(cond_op.true_ops.iter().chain(cond_op.false_ops.iter()))
    } else {
        0
    };

    inner_cost
        + match op.kind().category() {
            Unreachable => 0,
            ConstReg => 1,
            RegCast => 1,
            RegOp => 2,
            ConstBox => 4,
            Cond => 5,
            MakeCallback => 5,
            MemLoad => 5,
            Ret => 5,
            Call => 10,
            // This is tricky. This could either do a stack allocation (which is cheap) or a heap
            // allocation (which is very expensive). This depends on the type and escape analysis
            // in codegen. We need to make use compromise between those two costs here.
            AllocBoxed => 15,
        }
}

/// Returns the cost for a sequence of ops
fn cost_for_ops<'o>(ops: impl Iterator<Item = &'o ops::Op>) -> OpCost {
    ops.map(cost_for_op).sum()
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
        // Lists can partially evaluate list operations
        Value::List(_, _) => 1.2,
        // RustFuns can be const eval'ed
        Value::RustFun(_) => 1.5,
        // These can be const eval'ed or completely inlined
        Value::ArretFun(_) | Value::TyPred(_) | Value::EqPred => 2.0,
    }
}

/// Returns the product of the inline scaling factor for each of our arguments
fn inline_preference_factor_for_arg_list_value(arg_list_value: &Value) -> OpCostFactor {
    match arg_list_value {
        Value::List(fixed, rest) => fixed
            .iter()
            .chain(rest.iter().map(|rest| rest.as_ref()))
            .map(inline_preference_factor_for_value)
            .product(),
        Value::Const(_) => 1.5,
        _ => 1.0,
    }
}

/// Returns the product of the inline scaling factor for each captured value
fn inline_preference_factor_for_closure(closure: &Closure) -> OpCostFactor {
    closure
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
        * inline_preference_factor_for_closure(&arret_fun.closure)
}

/// Conditionally inlines an Arret fun
///
/// This makes an inlining decision based on four criteria:
///
/// 1. The approximate cost of performing a call versus inlining. This is calculated by attempting
///    both options and measuring the cost of the ops they build.
///
/// 2. The amount of knowledge lost by calling through a closure and arg regs. This is referred to
///    as the inlining preference factor. This is multiplied against the call cost.
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
    fcx: &mut FunCtx,
    outer_b: &mut Builder,
    span: Span,
    ret_ty: &ty::Mono,
    arret_fun: &value::ArretFun,
    apply_args: ApplyArgs<'a>,
) -> Result<value::Value> {
    let apply_stack = &fcx.inliner_stack;
    const INLINE_LIMIT: usize = 16;

    // We need to build an out-of-line call in every case
    let mut call_b = Builder::new();
    let call_result = ehx.build_arret_fun_app(
        &mut call_b,
        span,
        ret_ty,
        arret_fun,
        apply_args.list_value.clone(),
    );
    let call_ops = call_b.into_ops();

    if apply_stack.entries.len() >= INLINE_LIMIT {
        // Inline limit reached; don't attempt another inline
        outer_b.append(call_ops.into_vec().into_iter());
        return call_result;
    }

    let apply_cookie = ApplyCookie::new(arret_fun);
    if apply_stack.entries.contains(&apply_cookie) {
        // Abort recursion all the way back to the original callsite

        // This prevents us from doing a "partial unroll" where we recurse in to one iteration
        // of the fun application and then bail out to a call. This is a bit gnarly as we're
        // using errors for flow control but it's isolated to this function.
        return Err(Error::AbortRecursion(apply_cookie));
    }

    let mut inline_b = Some(Builder::new());
    let apply_stack = apply_stack.with_apply_cookie(apply_cookie);

    // Figure out how much we should prefer an inline version
    let inline_preference_factor = calc_inline_preference_factor(arret_fun, &apply_args.list_value);

    // Build an inline version
    let inline_result =
        ehx.inline_arret_fun_app(fcx, &mut inline_b, span, arret_fun, apply_args, apply_stack);
    let inline_ops = inline_b.unwrap().into_ops();

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
