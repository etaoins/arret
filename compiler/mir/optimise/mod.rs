use crate::mir::ops;
use crate::mir::value::Value;

mod duplicate_alloc_ops;
mod unused_ops;

pub fn optimise_fun(fun: ops::Fun) -> ops::Fun {
    let mut used_ops = unused_ops::remove_unused_fun_ops(fun.ops);
    duplicate_alloc_ops::remove_redundant_alloc_ops(&mut used_ops);

    ops::Fun {
        ops: unused_ops::remove_unused_fun_ops(used_ops),
        ..fun
    }
}

/// Optimise a function that has been inlined and returned the provided value
pub fn optimise_inlined_fun(ops: Box<[ops::Op]>, return_value: &Value) -> Box<[ops::Op]> {
    let mut used_ops = unused_ops::remove_unused_value_ops(ops, return_value);
    duplicate_alloc_ops::remove_redundant_alloc_ops(&mut used_ops);

    used_ops
}
