use std::collections::HashMap;

use crate::codegen::alloc::{AllocAtom, BoxSource};
use crate::mir::ops;

/// Determines if an op requires the heap to be in a consistent state before it's executed
///
/// Our `AllocAtom`s cannot span these operations
fn op_needs_heap_checkpoint(op: &ops::Op) -> bool {
    use crate::mir::ops::OpKind;

    match op.kind() {
        OpKind::Ret(_) | OpKind::RetVoid | OpKind::Unreachable | OpKind::Call(_, _) => true,
        OpKind::Cond(_, cond_op) => cond_op
            .true_ops
            .iter()
            .chain(cond_op.false_ops.iter())
            // We additionally need to make sure we don't allocate in our branches. Otherwise we
            // might need to plan an allocation of a dynamic size to cover each branch. Instead
            // just start a new atom for each branch.
            .any(|op| op_needs_heap_checkpoint(op) || op_alloc_output_reg(op).is_some()),
        _ => false,
    }
}

/// Returns the output reg for an allocating op, or `None` otherwise
fn op_alloc_output_reg(op: &ops::Op) -> Option<ops::RegId> {
    use crate::mir::ops::OpKind;

    match op.kind() {
        OpKind::AllocInt(reg_id, _) | OpKind::AllocBoxedPair(reg_id, _) => Some(*reg_id),
        _ => None,
    }
}

fn push_complete_atom<'op>(
    atoms: &mut Vec<AllocAtom<'op>>,
    box_sources: &mut HashMap<ops::RegId, BoxSource>,
    atom_ops: &mut Vec<&'op ops::Op>,
) {
    use std::mem;

    if !atom_ops.is_empty() {
        atoms.push(AllocAtom {
            box_sources: mem::replace(box_sources, HashMap::new()),
            ops: mem::replace(atom_ops, vec![]).into_boxed_slice(),
        });
    }
}

pub fn plan_allocs<'op>(ops: &'op [ops::Op]) -> Vec<AllocAtom<'op>> {
    let mut atoms = vec![];

    let mut box_sources = HashMap::new();
    let mut atom_ops = vec![];

    for op in ops {
        if op_needs_heap_checkpoint(op) {
            push_complete_atom(&mut atoms, &mut box_sources, &mut atom_ops);
            atoms.push(AllocAtom::with_unallocating_op(op));
            continue;
        }

        if let Some(reg_id) = op_alloc_output_reg(op) {
            box_sources.insert(reg_id, BoxSource::Stack);
        }

        atom_ops.push(op);
    }

    push_complete_atom(&mut atoms, &mut box_sources, &mut atom_ops);
    atoms
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn empty_ops() {
        let actual_atoms = plan_allocs(&[]);
        assert_eq!(0, actual_atoms.len());
    }

    #[test]
    fn condless_allocs() {
        let mut reg_counter = ops::RegIdCounter::new();

        let reg1 = reg_counter.alloc();
        let reg2 = reg_counter.alloc();
        let reg3 = reg_counter.alloc();
        let reg4 = reg_counter.alloc();

        let input_ops = [
            ops::OpKind::AllocInt(reg1, reg1).into(),
            ops::OpKind::ConstTrue(reg2, ()).into(),
            ops::OpKind::RetVoid.into(),
            ops::OpKind::AllocInt(reg3, reg3).into(),
            ops::OpKind::AllocInt(reg4, reg4).into(),
        ];

        let expected_atoms = vec![
            AllocAtom {
                box_sources: [(reg1, BoxSource::Stack)].iter().cloned().collect(),
                ops: Box::new([&input_ops[0], &input_ops[1]]),
            },
            AllocAtom::with_unallocating_op(&input_ops[2]),
            AllocAtom {
                box_sources: [(reg3, BoxSource::Stack), (reg4, BoxSource::Stack)]
                    .iter()
                    .cloned()
                    .collect(),
                ops: Box::new([&input_ops[3], &input_ops[4]]),
            },
        ];

        let actual_atoms = plan_allocs(&input_ops);

        assert_eq!(expected_atoms, actual_atoms);
    }

    #[test]
    fn non_allocating_cond() {
        let mut reg_counter = ops::RegIdCounter::new();

        let output_reg = reg_counter.alloc();
        let test_reg = reg_counter.alloc();
        let true_result_reg = reg_counter.alloc();
        let false_result_reg = reg_counter.alloc();

        let true_ops = Box::new([ops::OpKind::ConstNil(true_result_reg, ()).into()]);
        let false_ops = Box::new([ops::OpKind::ConstNil(false_result_reg, ()).into()]);

        let input_ops = [
            ops::OpKind::AllocInt(test_reg, test_reg).into(),
            ops::OpKind::Cond(
                output_reg,
                ops::CondOp {
                    test_reg,
                    true_ops,
                    true_result_reg,
                    false_ops,
                    false_result_reg,
                },
            )
            .into(),
        ];

        let actual_atoms = plan_allocs(&input_ops);
        // We should place the `AllocInt` and `Cond` in the same atom
        assert_eq!(1, actual_atoms.len());
    }

    #[test]
    fn allocating_cond() {
        let mut reg_counter = ops::RegIdCounter::new();

        let output_reg = reg_counter.alloc();
        let test_reg = reg_counter.alloc();
        let true_result_reg = reg_counter.alloc();
        let false_result_reg = reg_counter.alloc();

        let true_ops = Box::new([ops::OpKind::ConstNil(true_result_reg, ()).into()]);
        let false_ops =
            Box::new([ops::OpKind::AllocInt(false_result_reg, false_result_reg).into()]);

        let input_ops = [
            ops::OpKind::AllocInt(test_reg, test_reg).into(),
            ops::OpKind::Cond(
                output_reg,
                ops::CondOp {
                    test_reg,
                    true_ops,
                    true_result_reg,
                    false_ops,
                    false_result_reg,
                },
            )
            .into(),
        ];

        let actual_atoms = plan_allocs(&input_ops);
        // We should place the `AllocInt` and `Cond` in different atoms
        assert_eq!(2, actual_atoms.len());
    }
}
