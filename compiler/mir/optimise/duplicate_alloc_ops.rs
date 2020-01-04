use std::collections::HashMap;

use crate::mir::ops;

fn visit_simple_alloc_op_kind(
    op_kind: &mut ops::OpKind,
    boxed_reg: ops::RegId,
    native_reg: ops::RegId,
    native_to_boxed: &mut HashMap<ops::RegId, ops::RegId>,
) {
    use std::collections::hash_map::Entry;

    match native_to_boxed.entry(native_reg) {
        Entry::Occupied(existing_output) => {
            *op_kind = ops::OpKind::Alias(boxed_reg, *existing_output.get())
        }
        Entry::Vacant(vacant_entry) => {
            vacant_entry.insert(boxed_reg);
        }
    }
}

fn remove_branch_redundant_alloc_ops(
    ops: &mut [ops::Op],
    // We can use a single `HashMap` because simple alloc ops all take distinct native reg types
    native_to_boxed: &mut HashMap<ops::RegId, ops::RegId>,
) {
    for op in ops.iter_mut() {
        match op.kind {
            ops::OpKind::AllocBoxedInt(boxed_reg, native_reg)
            | ops::OpKind::AllocBoxedFloat(boxed_reg, native_reg)
            | ops::OpKind::AllocBoxedChar(boxed_reg, native_reg)
            | ops::OpKind::AllocBoxedSym(boxed_reg, native_reg) => {
                visit_simple_alloc_op_kind(&mut op.kind, boxed_reg, native_reg, native_to_boxed);
            }
            ops::OpKind::LoadBoxedIntValue(native_reg, boxed_reg)
            | ops::OpKind::LoadBoxedFloatValue(native_reg, boxed_reg)
            | ops::OpKind::LoadBoxedCharValue(native_reg, boxed_reg)
            | ops::OpKind::LoadBoxedSymInterned(native_reg, boxed_reg) => {
                native_to_boxed.insert(native_reg, boxed_reg);
            }
            ops::OpKind::Cond(ref mut cond_op) => {
                remove_branch_redundant_alloc_ops(
                    &mut cond_op.true_ops,
                    &mut native_to_boxed.clone(),
                );
                remove_branch_redundant_alloc_ops(
                    &mut cond_op.false_ops,
                    &mut native_to_boxed.clone(),
                );
            }
            _ => {}
        }
    }
}

/// Updates `ops` in-place to replace allocs of the same native value with `OpKind::Alias`
pub fn remove_redundant_alloc_ops(ops: &mut [ops::Op]) {
    let mut native_to_boxed = HashMap::new();
    remove_branch_redundant_alloc_ops(ops, &mut native_to_boxed)
}

#[cfg(test)]
mod test {
    use super::*;

    use crate::source::empty_span;

    #[test]
    fn test_box_different_native_regs() {
        let native_reg1 = ops::RegId::alloc();
        let boxed_reg1 = ops::RegId::alloc();

        let native_reg2 = ops::RegId::alloc();
        let boxed_reg2 = ops::RegId::alloc();

        let ops = &mut [
            ops::Op::new(
                empty_span(),
                ops::OpKind::AllocBoxedInt(boxed_reg1, native_reg1),
            ),
            ops::Op::new(
                empty_span(),
                ops::OpKind::AllocBoxedInt(boxed_reg2, native_reg2),
            ),
        ];

        // Should be identical because we're boxing different native values
        let expected_ops = ops.clone();

        remove_redundant_alloc_ops(ops);
        assert_eq!(&expected_ops, ops);
    }

    #[test]
    fn test_box_same_native_regs() {
        let native_reg1 = ops::RegId::alloc();
        let boxed_reg1 = ops::RegId::alloc();

        let boxed_reg2 = ops::RegId::alloc();

        let ops = &mut [
            ops::Op::new(
                empty_span(),
                ops::OpKind::AllocBoxedFloat(boxed_reg1, native_reg1),
            ),
            ops::Op::new(
                empty_span(),
                ops::OpKind::AllocBoxedFloat(boxed_reg2, native_reg1),
            ),
        ];

        let expected_ops = &[
            ops::Op::new(
                empty_span(),
                ops::OpKind::AllocBoxedFloat(boxed_reg1, native_reg1),
            ),
            // Should remove the redundant alloc of the same native value
            ops::Op::new(empty_span(), ops::OpKind::Alias(boxed_reg2, boxed_reg1)),
        ];

        remove_redundant_alloc_ops(ops);
        assert_eq!(expected_ops, ops);
    }

    #[test]
    fn test_reboxing() {
        let native_reg1 = ops::RegId::alloc();
        let boxed_reg1 = ops::RegId::alloc();
        let boxed_reg2 = ops::RegId::alloc();

        let ops = &mut [
            ops::Op::new(
                empty_span(),
                ops::OpKind::LoadBoxedSymInterned(native_reg1, boxed_reg1),
            ),
            ops::Op::new(
                empty_span(),
                ops::OpKind::AllocBoxedSym(boxed_reg2, native_reg1),
            ),
        ];

        let expected_ops = &[
            ops::Op::new(
                empty_span(),
                ops::OpKind::LoadBoxedSymInterned(native_reg1, boxed_reg1),
            ),
            // Should re-use the original box we got the native value from
            ops::Op::new(empty_span(), ops::OpKind::Alias(boxed_reg2, boxed_reg1)),
        ];

        remove_redundant_alloc_ops(ops);
        assert_eq!(expected_ops, ops);
    }

    #[test]
    fn test_cond_branch() {
        let outer_native_reg1 = ops::RegId::alloc();
        let outer_boxed_reg1 = ops::RegId::alloc();

        let test_reg = ops::RegId::alloc();

        let branch_boxed_reg1 = ops::RegId::alloc();
        let branch_native_reg2 = ops::RegId::alloc();
        let branch_boxed_reg2 = ops::RegId::alloc();

        let outer_boxed_reg2 = ops::RegId::alloc();

        let ops = &mut [
            ops::Op::new(
                empty_span(),
                ops::OpKind::AllocBoxedChar(outer_boxed_reg1, outer_native_reg1),
            ),
            ops::Op::new(
                empty_span(),
                ops::OpKind::Cond(ops::CondOp {
                    reg_phi: None,
                    test_reg,
                    true_ops: Box::new([
                        ops::Op::new(
                            empty_span(),
                            ops::OpKind::AllocBoxedChar(branch_boxed_reg1, outer_native_reg1),
                        ),
                        ops::Op::new(
                            empty_span(),
                            ops::OpKind::AllocBoxedChar(branch_boxed_reg2, branch_native_reg2),
                        ),
                    ]),
                    false_ops: Box::new([]),
                }),
            ),
            ops::Op::new(
                empty_span(),
                ops::OpKind::AllocBoxedChar(outer_boxed_reg2, branch_native_reg2),
            ),
        ];

        let expected_ops = &[
            ops::Op::new(
                empty_span(),
                ops::OpKind::AllocBoxedChar(outer_boxed_reg1, outer_native_reg1),
            ),
            ops::Op::new(
                empty_span(),
                ops::OpKind::Cond(ops::CondOp {
                    reg_phi: None,
                    test_reg,
                    true_ops: Box::new([
                        ops::Op::new(
                            empty_span(),
                            // We can use the alloc from outside this branch
                            ops::OpKind::Alias(branch_boxed_reg1, outer_boxed_reg1),
                        ),
                        ops::Op::new(
                            empty_span(),
                            ops::OpKind::AllocBoxedChar(branch_boxed_reg2, branch_native_reg2),
                        ),
                    ]),
                    false_ops: Box::new([]),
                }),
            ),
            ops::Op::new(
                empty_span(),
                // We can't use an alloc from within the branch
                ops::OpKind::AllocBoxedChar(outer_boxed_reg2, branch_native_reg2),
            ),
        ];

        remove_redundant_alloc_ops(ops);
        assert_eq!(expected_ops, ops);
    }
}
