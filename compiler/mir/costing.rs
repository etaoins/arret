use crate::mir::ops;

/// Abstract unit for measuring the runtime cost of ops
pub type OpCost = u32;

/// Abstract unit for a multiplier of an `OpCost`
pub type OpCostFactor = f32;

/// Returns the approximate runtime cost of an operation category
///
/// This isn't adjusted for any specifics of a given op. `cost_for_ops` should be used when costing
/// a known sequence of ops.
pub fn cost_for_op_category(category: ops::OpCategory) -> OpCost {
    use crate::mir::ops::OpCategory;

    match category {
        OpCategory::Unreachable => 0,
        OpCategory::ConstCastBoxed | OpCategory::CastBoxed => 0,
        OpCategory::ConstReg => 1,
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
    }
}

/// Returns the approximate runtime cost of an operation in an abstract unit
fn cost_for_op(op: &ops::Op) -> OpCost {
    let category_cost = cost_for_op_category(op.kind().category());

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
        ops::OpKind::Call(_, call_op) => {
            // Impure calls are harder to optimise. Penalise them.
            let impure_penalty = if call_op.impure { 2 } else { 0 };

            let callee_penalty = match call_op.callee {
                // These cannot be inlined and need to use the standard calling convention
                ops::Callee::BoxedFunThunk(_) | ops::Callee::StaticSymbol(_) => 2,
                _ => 0,
            };

            impure_penalty + callee_penalty
        }
        _ => 0,
    };

    category_cost + op_adjustment
}

/// Returns the cost for a sequence of ops
pub fn cost_for_ops<'o>(ops: impl Iterator<Item = &'o ops::Op>) -> OpCost {
    ops.map(cost_for_op).sum()
}
