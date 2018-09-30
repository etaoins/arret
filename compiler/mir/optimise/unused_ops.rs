use std::collections::HashSet;

use crate::mir::ops;

fn remove_unused_cond_ops(
    output_reg: ops::RegId,
    cond_op: ops::CondOp,
    used_regs: &mut HashSet<ops::RegId>,
) -> Option<ops::CondOp> {
    let ops::CondOp {
        test_reg,
        true_ops,
        true_result_reg,
        false_ops,
        false_result_reg,
    } = cond_op;

    // Instead of cloning `used_regs` just rollback any changes we make do it
    let (rollback_true_result, rollback_false_result) = if used_regs.contains(&output_reg) {
        // These phi in to the output reg
        (
            !used_regs.insert(true_result_reg),
            !used_regs.insert(false_result_reg),
        )
    } else {
        (false, false)
    };

    let used_true_ops = remove_unused_branch_ops(true_ops, used_regs);
    let used_false_ops = remove_unused_branch_ops(false_ops, used_regs);

    if used_true_ops.is_empty() && used_false_ops.is_empty() && !used_regs.contains(&output_reg) {
        // We can disappear!
        if rollback_true_result {
            used_regs.remove(&true_result_reg);
        }
        if rollback_false_result {
            used_regs.remove(&false_result_reg);
        }

        return None;
    }

    used_regs.insert(test_reg);
    Some(ops::CondOp {
        test_reg,
        true_ops: used_true_ops,
        true_result_reg,
        false_ops: used_false_ops,
        false_result_reg,
    })
}

fn remove_unused_branch_ops(
    ops: Vec<ops::Op>,
    used_regs: &mut HashSet<ops::RegId>,
) -> Vec<ops::Op> {
    let mut reverse_ops = ops
        .into_iter()
        .rev()
        .filter_map(|op| {
            let ops::Op { span, kind } = op;

            match kind {
                ops::OpKind::Cond(output_reg, cond_op) => {
                    remove_unused_cond_ops(output_reg, cond_op, used_regs)
                        .map(|cond_op| ops::Op::new(span, ops::OpKind::Cond(output_reg, cond_op)))
                }
                _ => {
                    // Does this have no side effects and its output is unused?
                    if !kind.has_side_effects() && kind
                        .output_reg()
                        .map(|output_reg| !used_regs.contains(&output_reg))
                        .unwrap_or(true)
                    {
                        None
                    } else {
                        kind.add_input_regs(used_regs);
                        Some(ops::Op { span, kind })
                    }
                }
            }
        })
        .collect::<Vec<ops::Op>>();

    // If we do .rev() on the iterator before we collect it will change the order we iterate in
    reverse_ops.reverse();
    reverse_ops
}

pub fn remove_unused_fun_ops(ops: Vec<ops::Op>) -> Vec<ops::Op> {
    // Nothing is used at the beginning of a function
    let mut used_regs = HashSet::new();
    remove_unused_branch_ops(ops, &mut used_regs)
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn empty_ops() {
        let ops = remove_unused_fun_ops(vec![]);
        assert!(ops.is_empty());
    }

    #[test]
    fn simple_unused() {
        let mut reg_counter = ops::RegIdCounter::new();
        let reg1 = reg_counter.alloc();
        let reg2 = reg_counter.alloc();
        let reg3 = reg_counter.alloc();

        let input_ops = vec![
            ops::OpKind::ConstNil(reg1, ()).into(),
            ops::OpKind::ConstNil(reg2, ()).into(),
            ops::OpKind::ConstNil(reg3, ()).into(),
            ops::OpKind::Ret(reg2).into(),
        ];

        let output_ops = remove_unused_fun_ops(input_ops);

        let expected_ops: Vec<ops::Op> = vec![
            ops::OpKind::ConstNil(reg2, ()).into(),
            ops::OpKind::Ret(reg2).into(),
        ];

        assert_eq!(expected_ops, output_ops);
    }

    #[test]
    fn fully_used_cond() {
        let mut reg_counter = ops::RegIdCounter::new();

        let output_reg = reg_counter.alloc();
        let test_reg = reg_counter.alloc();
        let true_result_reg = reg_counter.alloc();
        let false_result_reg = reg_counter.alloc();

        let true_ops = vec![ops::OpKind::ConstNil(true_result_reg, ()).into()];

        let false_ops = vec![ops::OpKind::ConstNil(false_result_reg, ()).into()];

        let input_ops = vec![
            ops::OpKind::ConstNil(test_reg, ()).into(),
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
            ops::OpKind::Ret(output_reg).into(),
        ];

        let expected_ops = input_ops.clone();
        let output_ops = remove_unused_fun_ops(input_ops);

        assert_eq!(expected_ops, output_ops);
    }

    #[test]
    fn partially_used_cond() {
        let mut reg_counter = ops::RegIdCounter::new();

        let output_reg = reg_counter.alloc();
        let test_reg = reg_counter.alloc();
        let true_result_reg = reg_counter.alloc();
        let false_result_reg = reg_counter.alloc();

        let true_ops = vec![ops::OpKind::ConstNil(true_result_reg, ()).into()];

        let false_ops = vec![ops::OpKind::ConstNil(false_result_reg, ()).into()];

        let input_ops = vec![
            ops::OpKind::ConstNil(test_reg, ()).into(),
            ops::OpKind::Cond(
                output_reg,
                ops::CondOp {
                    test_reg,
                    true_ops,
                    true_result_reg: test_reg, // This makes the true branch unused
                    false_ops: false_ops.clone(),
                    false_result_reg,
                },
            )
            .into(),
            ops::OpKind::Ret(output_reg).into(),
        ];

        let expected_ops: Vec<ops::Op> = vec![
            ops::OpKind::ConstNil(test_reg, ()).into(),
            ops::OpKind::Cond(
                output_reg,
                ops::CondOp {
                    test_reg,
                    true_ops: vec![],
                    true_result_reg: test_reg,
                    false_ops,
                    false_result_reg,
                },
            )
            .into(),
            ops::OpKind::Ret(output_reg).into(),
        ];

        let output_ops = remove_unused_fun_ops(input_ops);

        assert_eq!(expected_ops, output_ops);
    }

    #[test]
    fn output_only_cond() {
        let mut reg_counter = ops::RegIdCounter::new();

        let output_reg = reg_counter.alloc();
        let test_reg = reg_counter.alloc();
        let true_result_reg = reg_counter.alloc();
        let false_result_reg = reg_counter.alloc();

        let true_ops = vec![ops::OpKind::ConstNil(true_result_reg, ()).into()];

        let false_ops = vec![ops::OpKind::ConstNil(false_result_reg, ()).into()];

        let input_ops = vec![
            ops::OpKind::ConstNil(test_reg, ()).into(),
            ops::OpKind::Cond(
                output_reg,
                ops::CondOp {
                    test_reg,
                    true_ops,
                    true_result_reg: test_reg, // This makes the true branch unused
                    false_ops,
                    false_result_reg: test_reg, // This makes the false branch unused
                },
            )
            .into(),
            // However, the output of the cond is still used
            ops::OpKind::Ret(output_reg).into(),
        ];

        let expected_ops: Vec<ops::Op> = vec![
            ops::OpKind::ConstNil(test_reg, ()).into(),
            ops::OpKind::Cond(
                output_reg,
                ops::CondOp {
                    test_reg,
                    true_ops: vec![],
                    true_result_reg: test_reg,
                    false_ops: vec![],
                    false_result_reg: test_reg,
                },
            )
            .into(),
            ops::OpKind::Ret(output_reg).into(),
        ];

        let output_ops = remove_unused_fun_ops(input_ops);

        assert_eq!(expected_ops, output_ops);
    }

    #[test]
    fn fully_unused_cond() {
        let mut reg_counter = ops::RegIdCounter::new();

        let output_reg = reg_counter.alloc();
        let test_reg = reg_counter.alloc();
        let true_result_reg = reg_counter.alloc();
        let false_result_reg = reg_counter.alloc();

        let true_ops = vec![ops::OpKind::ConstNil(true_result_reg, ()).into()];

        let false_ops = vec![ops::OpKind::ConstNil(false_result_reg, ()).into()];

        let input_ops = vec![
            ops::OpKind::ConstNil(test_reg, ()).into(),
            ops::OpKind::Cond(
                output_reg,
                ops::CondOp {
                    test_reg,
                    true_ops,
                    true_result_reg: test_reg, // This makes the true branch unused
                    false_ops,
                    false_result_reg: test_reg, // This makes the false branch unused
                },
            )
            .into(),
        ];

        let output_ops = remove_unused_fun_ops(input_ops);
        assert!(output_ops.is_empty());
    }
}
