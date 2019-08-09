use crate::mir::ops;
use crate::mir::value::Value;

mod unused_ops;

pub fn optimise_fun(fun: ops::Fun) -> ops::Fun {
    ops::Fun {
        ops: unused_ops::remove_unused_fun_ops(fun.ops),
        ..fun
    }
}

/// Optimise a function that has been inlined and returned the provided value
pub fn optimise_inlined_fun(ops: Box<[ops::Op]>, return_value: &Value) -> Box<[ops::Op]> {
    unused_ops::remove_unused_value_ops(ops, return_value)
}
