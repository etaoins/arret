use runtime::boxed;

use crate::codegen::alloc::{AllocAtom, BoxSource, CondPlan};
use crate::codegen::analysis::escape::{CaptureKind, Captures};
use crate::mir::ops;

struct AllocInfo {
    output_reg: ops::RegId,
    box_size: boxed::BoxSize,
}

/// Determines if an op requires the heap to be in a consistent state before it's executed
///
/// Our `AllocAtom`s cannot span these operations
fn op_needs_heap_checkpoint(op: &ops::Op) -> bool {
    use crate::mir::ops::OpKind;

    match op.kind() {
        OpKind::Ret(_) | OpKind::RetVoid | OpKind::Unreachable | OpKind::Call(_, _) => true,
        OpKind::Cond(cond_op) => cond_op
            .true_ops
            .iter()
            .chain(cond_op.false_ops.iter())
            // We additionally need to make sure we don't allocate in our branches. Otherwise we
            // might need to plan an allocation of a dynamic size to cover each branch. Instead
            // just start a new atom for each branch.
            .any(|op| op_needs_heap_checkpoint(op) || op_alloc_info(op).is_some()),
        _ => false,
    }
}

/// Returns the output reg for an allocating op, or `None` otherwise
fn op_alloc_info(op: &ops::Op) -> Option<AllocInfo> {
    use crate::mir::ops::OpKind;

    match op.kind() {
        OpKind::AllocBoxedInt(output_reg, _) => Some(AllocInfo {
            output_reg: *output_reg,
            box_size: boxed::Int::size(),
        }),
        OpKind::AllocBoxedFloat(output_reg, _) => Some(AllocInfo {
            output_reg: *output_reg,
            box_size: boxed::Float::size(),
        }),
        OpKind::AllocBoxedPair(output_reg, _) => Some(AllocInfo {
            output_reg: *output_reg,
            box_size: boxed::TopPair::size(),
        }),
        OpKind::AllocBoxedFunThunk(output_reg, _) => Some(AllocInfo {
            output_reg: *output_reg,
            box_size: boxed::FunThunk::size(),
        }),
        _ => None,
    }
}

pub fn plan_allocs<'op>(captures: &Captures, ops: &'op [ops::Op]) -> Vec<AllocAtom<'op>> {
    use std::mem;

    let mut atoms = vec![];
    let mut current_atom = AllocAtom::new();

    for op in ops {
        let checkpointing_op = op_needs_heap_checkpoint(op);

        if checkpointing_op && !current_atom.is_empty() {
            atoms.push(mem::replace(&mut current_atom, AllocAtom::new()));
        }

        if let ops::OpKind::Cond(ops::CondOp {
            true_ops,
            false_ops,
            ..
        }) = op.kind()
        {
            current_atom.push_cond_plan(
                current_atom.ops().len(),
                CondPlan {
                    true_subplan: plan_allocs(captures, true_ops),
                    false_subplan: plan_allocs(captures, false_ops),
                },
            );
        } else if let Some(AllocInfo {
            output_reg,
            box_size,
        }) = op_alloc_info(op)
        {
            if captures.get(output_reg) == CaptureKind::Never {
                current_atom.push_box_source(output_reg, BoxSource::Stack);
            } else {
                current_atom.push_box_source(output_reg, BoxSource::Heap(box_size));
            }
        }

        current_atom.push_op(op);

        if checkpointing_op {
            atoms.push(mem::replace(&mut current_atom, AllocAtom::new()));
        }
    }

    if !current_atom.is_empty() {
        atoms.push(current_atom);
    }

    atoms
}

#[cfg(test)]
mod test {
    use super::*;
    use std::collections::HashMap;

    #[test]
    fn empty_ops() {
        let actual_atoms = plan_allocs(&Captures::new(), &[]);
        assert_eq!(0, actual_atoms.len());
    }

    #[test]
    fn condless_allocs() {
        let reg1 = ops::RegId::alloc();
        let reg2 = ops::RegId::alloc();
        let reg3 = ops::RegId::alloc();
        let reg4 = ops::RegId::alloc();

        let input_ops = [
            ops::OpKind::AllocBoxedInt(reg1, reg1).into(),
            ops::OpKind::ConstBoxedTrue(reg2, ()).into(),
            ops::OpKind::RetVoid.into(),
            ops::OpKind::AllocBoxedInt(reg3, reg3).into(),
            ops::OpKind::AllocBoxedInt(reg4, reg4).into(),
        ];

        let expected_atoms = vec![
            AllocAtom {
                box_sources: [(reg1, BoxSource::Stack)].iter().cloned().collect(),
                cond_plans: HashMap::new(),
                ops: vec![&input_ops[0], &input_ops[1]],
            },
            AllocAtom {
                box_sources: HashMap::new(),
                cond_plans: HashMap::new(),
                ops: vec![&input_ops[2]],
            },
            AllocAtom {
                box_sources: [(reg3, BoxSource::Stack), (reg4, BoxSource::Stack)]
                    .iter()
                    .cloned()
                    .collect(),
                cond_plans: HashMap::new(),
                ops: vec![&input_ops[3], &input_ops[4]],
            },
        ];

        let actual_atoms = plan_allocs(&Captures::new(), &input_ops);

        assert_eq!(expected_atoms, actual_atoms);
    }

    #[test]
    fn non_allocating_cond() {
        let output_reg = ops::RegId::alloc();
        let true_result_reg = ops::RegId::alloc();
        let false_result_reg = ops::RegId::alloc();

        let test_reg = ops::RegId::alloc();

        let true_ops = Box::new([ops::OpKind::ConstBoxedNil(true_result_reg, ()).into()]);
        let false_ops = Box::new([ops::OpKind::ConstBoxedNil(false_result_reg, ()).into()]);

        let input_ops = [
            ops::OpKind::AllocBoxedInt(test_reg, test_reg).into(),
            ops::OpKind::Cond(ops::CondOp {
                reg_phi: Some(ops::RegPhi {
                    output_reg,
                    true_result_reg,
                    false_result_reg,
                }),
                test_reg,
                true_ops,
                false_ops,
            })
            .into(),
        ];

        let actual_atoms = plan_allocs(&Captures::new(), &input_ops);
        // We should place the `AllocBoxedInt` and `Cond` in the same atom
        assert_eq!(1, actual_atoms.len());
    }

    #[test]
    fn allocating_cond() {
        let output_reg = ops::RegId::alloc();
        let test_reg = ops::RegId::alloc();
        let true_result_reg = ops::RegId::alloc();
        let false_result_reg = ops::RegId::alloc();

        let true_ops = Box::new([ops::OpKind::ConstBoxedNil(true_result_reg, ()).into()]);
        let false_ops =
            Box::new([ops::OpKind::AllocBoxedInt(false_result_reg, false_result_reg).into()]);

        let input_ops = [
            ops::OpKind::AllocBoxedInt(test_reg, test_reg).into(),
            ops::OpKind::Cond(ops::CondOp {
                reg_phi: Some(ops::RegPhi {
                    output_reg,
                    true_result_reg,
                    false_result_reg,
                }),
                test_reg,
                true_ops,
                false_ops,
            })
            .into(),
        ];

        let actual_atoms = plan_allocs(&Captures::new(), &input_ops);
        // We should place the `AllocBoxedInt` and `Cond` in different atoms
        assert_eq!(2, actual_atoms.len());
    }
}
