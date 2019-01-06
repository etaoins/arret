use crate::mir::error::{Error, Result};
use crate::mir::value;

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

/// Conditionally inlines an Arret fun using the passed `inliner`
///
/// If the inline limit is reached no inling will be attempted and `Ok(None)` will be returned. In
/// this case `eval_inline` will never be called.
///
/// However, it's possible for an inling to be attempted and then "rolled back" to this call site.
/// This occurs when a recursive loop is detected and we roll back to the beginning of loop. In this
/// case `eval_inline` will be called but `Ok(None)` will be returned anyway. For this reason it's
/// important that `inliner` has no side effects.
pub fn cond_inline<F>(
    apply_stack: &ApplyStack,
    arret_fun: &value::ArretFun,
    eval_inline: F,
) -> Result<Option<value::Value>>
where
    F: FnOnce(ApplyStack) -> Result<value::Value>,
{
    const INLINE_LIMIT: usize = 16;

    if apply_stack.entries.len() >= INLINE_LIMIT {
        // Inline limit reached; don't attempt another inline
        return Ok(None);
    }

    let apply_cookie = ApplyCookie::new(arret_fun);
    if apply_stack.entries.contains(&apply_cookie) {
        // Abort recursion all the way back to the original callsite

        // This prevents us from doing a "partial unroll" where we recurse in to one iteration
        // of the fun application and then bail out to a call. This is a bit gnarly as we're
        // using errors for flow control but it's isolated to this function.
        return Err(Error::AbortRecursion(apply_cookie));
    }

    let inline_result = eval_inline(apply_stack.with_apply_cookie(apply_cookie));

    match inline_result {
        Ok(value) => {
            // Success!
            Ok(Some(value))
        }
        Err(Error::AbortRecursion(abort_to_cookie)) if abort_to_cookie == apply_cookie => {
            // We detected another application of this fun and requested recursion is aborted back
            // to this point
            Ok(None)
        }
        Err(other) => Err(other),
    }
}
