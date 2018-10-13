use crate::codegen::alloc::AllocAtom;
use crate::codegen::callee;
use crate::codegen::fun_gen::BuiltFun;
use crate::mir::ops;

fn op_needs_task(built_funs: &[BuiltFun], op: &ops::Op) -> bool {
    use crate::mir::ops::OpKind;

    match op.kind() {
        OpKind::Cond(_, cond_op) => cond_op
            .true_ops
            .iter()
            .chain(cond_op.false_ops.iter())
            .any(|branch_op| op_needs_task(built_funs, branch_op)),
        OpKind::Call(_, ops::CallOp { callee, .. }) => {
            callee::callee_takes_task(built_funs, callee)
        }
        _ => false,
    }
}

pub fn alloc_plan_needs_task(built_funs: &[BuiltFun], atoms: &[AllocAtom<'_>]) -> bool {
    use crate::codegen::alloc::BoxSource;

    atoms.iter().any(|atom| {
        atom.box_sources()
            .values()
            .any(|box_source| match box_source {
                BoxSource::Stack => false,
                BoxSource::Heap(_) => true,
            })
            || atom.ops().iter().any(|op| op_needs_task(built_funs, op))
    })
}
