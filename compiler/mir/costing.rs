use crate::mir::ops;

/// Abstract unit for measuring the runtime cost of ops
pub type OpCost = u32;

/// Abstract unit for a multiplier of an `OpCost`
pub type OpCostFactor = f32;

/// Returns the approximate runtime cost of an operation in an abstract unit
fn cost_for_op(op: &ops::Op) -> OpCost {
    use crate::mir::ops::OpCategory;

    let category_cost = match op.kind().category() {
        OpCategory::Unreachable => 0,
        OpCategory::ConstCastBoxed => 0,
        OpCategory::ConstReg => 1,
        OpCategory::RegCast => 1,
        OpCategory::RegOp => 2,
        OpCategory::ConstBox => 4,
        OpCategory::Cond => 5, // Adjusted below to include branches
        OpCategory::MakeCallback => 5,
        OpCategory::MemLoad => 5,
        OpCategory::Ret => 5,
        OpCategory::Call => 9, // Adjusted below based on the call purity
        // This is tricky. This could either do a stack allocation (which is cheap) or a heap
        // allocation (which is very expensive). This depends on the type and escape analysis
        // in codegen. We need to make use compromise between those two costs here.
        OpCategory::AllocBoxed => 15,
    };

    let op_adjustment = match op.kind() {
        ops::OpKind::Cond(cond_op) => {
            // Only one branch can be taken so the runtime cost is the average of the branches. On
            // the other hand, the code size (and thus icache footprint) is the sum of the
            // branches. Compromise by using the most expensive branch.
            std::cmp::max(
                cost_for_ops(cond_op.true_ops.iter()),
                cost_for_ops(cond_op.false_ops.iter()),
            )
        }
        ops::OpKind::Call(_, call_op) if call_op.impure => {
            // Impure calls are harder to optimise. Penalise them.
            2
        }
        _ => 0,
    };

    category_cost + op_adjustment
}

/// Returns the cost for a sequence of ops
pub fn cost_for_ops<'o>(ops: impl Iterator<Item = &'o ops::Op>) -> OpCost {
    ops.map(cost_for_op).sum()
}
