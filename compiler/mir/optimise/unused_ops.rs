use std::collections::HashSet;

use crate::mir::ops;

fn remove_unused_cond_ops(
    cond_op: ops::CondOp,
    used_regs: &mut HashSet<ops::RegId>,
) -> Option<ops::CondOp> {
    let ops::CondOp {
        reg_phi,
        test_reg,
        true_ops,
        false_ops,
    } = cond_op;

    // Determine if our output is used
    let used_reg_phi = reg_phi.filter(|reg_phi| used_regs.contains(&reg_phi.output_reg));

    // Instead of cloning `used_regs` just rollback any changes we make do it
    let (rollback_true_reg, rollback_false_reg) = match used_reg_phi {
        Some(ops::RegPhi {
            true_result_reg,
            false_result_reg,
            ..
        }) => (
            Some(true_result_reg).filter(|_| !used_regs.insert(true_result_reg)),
            Some(false_result_reg).filter(|_| !used_regs.insert(false_result_reg)),
        ),
        _ => (None, None),
    };

    let used_true_ops = remove_unused_branch_ops(true_ops, used_regs);
    let used_false_ops = remove_unused_branch_ops(false_ops, used_regs);

    if used_true_ops.is_empty() && used_false_ops.is_empty() && used_reg_phi.is_none() {
        // We can disappear!
        if let Some(rollback_true_reg) = rollback_true_reg {
            used_regs.remove(&rollback_true_reg);
        }
        if let Some(rollback_false_reg) = rollback_false_reg {
            used_regs.remove(&rollback_false_reg);
        }

        None
    } else {
        used_regs.insert(test_reg);
        Some(ops::CondOp {
            reg_phi: used_reg_phi,
            test_reg,
            true_ops: used_true_ops,
            false_ops: used_false_ops,
        })
    }
}

fn remove_unused_branch_ops(
    ops: Box<[ops::Op]>,
    used_regs: &mut HashSet<ops::RegId>,
) -> Box<[ops::Op]> {
    let mut reverse_ops = ops
        .into_vec()
        .into_iter()
        .rev()
        .filter_map(|op| {
            let ops::Op { span, kind } = op;

            match kind {
                ops::OpKind::Cond(cond_op) => remove_unused_cond_ops(cond_op, used_regs)
                    .map(|cond_op| ops::Op::new(span, ops::OpKind::Cond(cond_op))),
                _ => {
                    // Does this have no side effects and its output is unused?
                    if !kind.has_side_effects()
                        && kind
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
    reverse_ops.into_boxed_slice()
}

pub fn remove_unused_fun_ops(ops: Box<[ops::Op]>) -> Box<[ops::Op]> {
    // Nothing is used at the beginning of a function
    let mut used_regs = HashSet::new();
    remove_unused_branch_ops(ops, &mut used_regs)
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn empty_ops() {
        let ops = remove_unused_fun_ops(Box::new([]));
        assert!(ops.is_empty());
    }

    #[test]
    fn simple_unused() {
        let reg1 = ops::RegId::alloc();
        let reg2 = ops::RegId::alloc();
        let reg3 = ops::RegId::alloc();

        let input_ops = Box::new([
            ops::OpKind::ConstBoxedNil(reg1, ()).into(),
            ops::OpKind::ConstBoxedNil(reg2, ()).into(),
            ops::OpKind::ConstBoxedNil(reg3, ()).into(),
            ops::OpKind::Ret(reg2).into(),
        ]);

        let output_ops = remove_unused_fun_ops(input_ops);

        let expected_ops: Box<[ops::Op]> = Box::new([
            ops::OpKind::ConstBoxedNil(reg2, ()).into(),
            ops::OpKind::Ret(reg2).into(),
        ]);

        assert_eq!(expected_ops, output_ops);
    }

    #[test]
    fn fully_used_cond() {
        let output_reg = ops::RegId::alloc();
        let test_reg = ops::RegId::alloc();
        let true_result_reg = ops::RegId::alloc();
        let false_result_reg = ops::RegId::alloc();

        let true_ops = Box::new([ops::OpKind::ConstBoxedNil(true_result_reg, ()).into()]);

        let false_ops = Box::new([ops::OpKind::ConstBoxedNil(false_result_reg, ()).into()]);

        let input_ops: Box<[ops::Op]> = Box::new([
            ops::OpKind::ConstBoxedNil(test_reg, ()).into(),
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
            ops::OpKind::Ret(output_reg).into(),
        ]);

        let expected_ops = input_ops.clone();
        let output_ops = remove_unused_fun_ops(input_ops);

        assert_eq!(expected_ops, output_ops);
    }

    #[test]
    fn partially_used_cond() {
        let output_reg = ops::RegId::alloc();
        let test_reg = ops::RegId::alloc();
        let true_result_reg = ops::RegId::alloc();
        let false_result_reg = ops::RegId::alloc();

        let true_ops = Box::new([ops::OpKind::ConstBoxedNil(true_result_reg, ()).into()]);

        let false_ops = Box::new([ops::OpKind::ConstBoxedNil(false_result_reg, ()).into()]);

        let input_ops = Box::new([
            ops::OpKind::ConstBoxedNil(test_reg, ()).into(),
            ops::OpKind::Cond(ops::CondOp {
                reg_phi: Some(ops::RegPhi {
                    output_reg,
                    true_result_reg: test_reg, // This makes the true branch unused
                    false_result_reg,
                }),
                test_reg,
                true_ops,
                false_ops: false_ops.clone(),
            })
            .into(),
            ops::OpKind::Ret(output_reg).into(),
        ]);

        let expected_ops: Box<[ops::Op]> = Box::new([
            ops::OpKind::ConstBoxedNil(test_reg, ()).into(),
            ops::OpKind::Cond(ops::CondOp {
                reg_phi: Some(ops::RegPhi {
                    output_reg,
                    true_result_reg: test_reg,
                    false_result_reg,
                }),
                test_reg,
                true_ops: Box::new([]),
                false_ops,
            })
            .into(),
            ops::OpKind::Ret(output_reg).into(),
        ]);

        let output_ops = remove_unused_fun_ops(input_ops);

        assert_eq!(expected_ops, output_ops);
    }

    #[test]
    fn output_only_cond() {
        let output_reg = ops::RegId::alloc();
        let test_reg = ops::RegId::alloc();
        let true_result_reg = ops::RegId::alloc();
        let false_result_reg = ops::RegId::alloc();

        let true_ops = Box::new([ops::OpKind::ConstBoxedNil(true_result_reg, ()).into()]);

        let false_ops = Box::new([ops::OpKind::ConstBoxedNil(false_result_reg, ()).into()]);

        let input_ops = Box::new([
            ops::OpKind::ConstBoxedNil(test_reg, ()).into(),
            ops::OpKind::Cond(ops::CondOp {
                reg_phi: Some(ops::RegPhi {
                    output_reg,
                    true_result_reg: test_reg, // This makes the true branch unused
                    false_result_reg: test_reg, // This makes the false branch unused
                }),
                test_reg,
                true_ops,
                false_ops,
            })
            .into(),
            // However, the output of the `Cond` is still used
            ops::OpKind::Ret(output_reg).into(),
        ]);

        let expected_ops: Box<[ops::Op]> = Box::new([
            ops::OpKind::ConstBoxedNil(test_reg, ()).into(),
            ops::OpKind::Cond(ops::CondOp {
                reg_phi: Some(ops::RegPhi {
                    output_reg,
                    true_result_reg: test_reg,
                    false_result_reg: test_reg,
                }),
                test_reg,
                true_ops: Box::new([]),
                false_ops: Box::new([]),
            })
            .into(),
            ops::OpKind::Ret(output_reg).into(),
        ]);

        let output_ops = remove_unused_fun_ops(input_ops);

        assert_eq!(expected_ops, output_ops);
    }

    #[test]
    fn output_unused_cond() {
        let output_reg = ops::RegId::alloc();
        let test_reg = ops::RegId::alloc();

        let true_ops = Box::new([ops::OpKind::RetVoid.into()]);
        let false_ops = Box::new([ops::OpKind::RetVoid.into()]);

        let input_ops = Box::new([
            ops::OpKind::ConstBoxedNil(test_reg, ()).into(),
            ops::OpKind::Cond(ops::CondOp {
                reg_phi: Some(ops::RegPhi {
                    output_reg,
                    true_result_reg: test_reg, // This makes the true result unused
                    false_result_reg: test_reg, // This makes the false result unused
                }),
                test_reg,
                true_ops: true_ops.clone(),
                false_ops: false_ops.clone(),
            })
            .into(),
        ]);

        let expected_ops: Box<[ops::Op]> = Box::new([
            ops::OpKind::ConstBoxedNil(test_reg, ()).into(),
            ops::OpKind::Cond(ops::CondOp {
                reg_phi: None,
                test_reg,
                true_ops,
                false_ops,
            })
            .into(),
        ]);

        let output_ops = remove_unused_fun_ops(input_ops);

        assert_eq!(expected_ops, output_ops);
    }

    #[test]
    fn fully_unused_cond() {
        let output_reg = ops::RegId::alloc();
        let test_reg = ops::RegId::alloc();
        let true_result_reg = ops::RegId::alloc();
        let false_result_reg = ops::RegId::alloc();

        let true_ops = Box::new([ops::OpKind::ConstBoxedNil(true_result_reg, ()).into()]);

        let false_ops = Box::new([ops::OpKind::ConstBoxedNil(false_result_reg, ()).into()]);

        let input_ops = Box::new([
            ops::OpKind::ConstBoxedNil(test_reg, ()).into(),
            ops::OpKind::Cond(ops::CondOp {
                reg_phi: Some(ops::RegPhi {
                    output_reg,
                    true_result_reg: test_reg, // This makes the true branch unused
                    false_result_reg: test_reg, // This makes the false branch unused
                }),
                test_reg,
                true_ops,
                false_ops,
            })
            .into(),
        ]);

        let output_ops = remove_unused_fun_ops(input_ops);
        assert!(output_ops.is_empty());
    }
}
