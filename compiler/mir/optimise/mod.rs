use crate::mir::ops;

mod unused_ops;

pub fn optimise_fun(fun: ops::Fun) -> ops::Fun {
    ops::Fun {
        ops: unused_ops::remove_unused_fun_ops(fun.ops),
        ..fun
    }
}
