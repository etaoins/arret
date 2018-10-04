use runtime::abitype;

use syntax::span::Span;

new_indexing_id_type!(BuiltFunId, u32);
new_counting_id_type!(RegIdCounter, RegId);

#[derive(Debug, PartialEq, Clone)]
pub struct FunABI {
    pub takes_task: bool,
    pub takes_closure: bool,
    pub params: Box<[abitype::ABIType]>,
    pub ret: abitype::RetABIType,
}

impl FunABI {
    pub fn thunk_abi() -> FunABI {
        FunABI {
            takes_task: true,
            takes_closure: true,
            params: Box::new([abitype::TOP_LIST_BOXED_ABI_TYPE.into()]),
            ret: abitype::BoxedABIType::Any.into(),
        }
    }
}

#[derive(Debug, PartialEq, Clone)]
pub struct StaticSymbol {
    pub symbol: &'static str,
    pub abi: FunABI,
}

/// Represents a callable function
///
/// This is used instead of a `RegId` to make the ABI of the called function obvious for codegen's
/// analysis passes.
#[derive(Debug, PartialEq, Clone)]
pub enum Callee {
    BuiltFun(BuiltFunId),
    BoxedFunThunk(RegId),
    StaticSymbol(StaticSymbol),
}

#[derive(Debug, PartialEq, Clone)]
pub struct CallOp {
    pub callee: Callee,
    pub args: Box<[RegId]>,
}

#[derive(Debug, PartialEq, Clone)]
pub struct BoxPairOp {
    pub head_reg: RegId,
    pub rest_reg: RegId,
    pub length_reg: RegId,
}

#[derive(Debug, PartialEq, Clone)]
pub struct CastBoxedOp {
    pub from_reg: RegId,
    pub to_type: abitype::BoxedABIType,
}

#[derive(Debug, PartialEq, Clone)]
pub struct CondOp {
    pub test_reg: RegId,
    pub true_ops: Box<[Op]>,
    pub true_result_reg: RegId,
    pub false_ops: Box<[Op]>,
    pub false_result_reg: RegId,
}

#[derive(Debug, PartialEq, Clone)]
pub struct BinaryOp {
    pub lhs_reg: RegId,
    pub rhs_reg: RegId,
}

#[derive(Debug, PartialEq, Clone)]
pub enum OpKind {
    ConstNil(RegId, ()),
    ConstTrue(RegId, ()),
    ConstFalse(RegId, ()),
    ConstInt(RegId, i64),
    ConstBoxedInt(RegId, i64),
    ConstBoxedStr(RegId, Box<str>),
    ConstBoxedPair(RegId, BoxPairOp),
    ConstBoxedFunThunk(RegId, Callee),

    AllocInt(RegId, RegId),
    AllocBoxedPair(RegId, BoxPairOp),

    ConstCastBoxed(RegId, CastBoxedOp),
    CastBoxed(RegId, CastBoxedOp),

    CurrentTask(RegId, ()),

    Call(RegId, CallOp),
    LoadBoxedListLength(RegId, RegId),
    LoadBoxedPairHead(RegId, RegId),
    LoadBoxedPairRest(RegId, RegId),
    LoadBoxedIntValue(RegId, RegId),
    LoadBoxedFunThunkClosure(RegId, RegId),
    Cond(RegId, CondOp),

    Add(RegId, BinaryOp),

    Ret(RegId),
    RetVoid,
    Unreachable,
}

impl OpKind {
    pub fn output_reg(&self) -> Option<RegId> {
        use crate::mir::ops::OpKind::*;

        match self {
            ConstNil(reg_id, _)
            | ConstTrue(reg_id, _)
            | ConstFalse(reg_id, _)
            | ConstInt(reg_id, _)
            | ConstBoxedInt(reg_id, _)
            | ConstBoxedStr(reg_id, _)
            | ConstBoxedPair(reg_id, _)
            | ConstBoxedFunThunk(reg_id, _)
            | AllocInt(reg_id, _)
            | AllocBoxedPair(reg_id, _)
            | ConstCastBoxed(reg_id, _)
            | CastBoxed(reg_id, _)
            | CurrentTask(reg_id, _)
            | Call(reg_id, _)
            | LoadBoxedListLength(reg_id, _)
            | LoadBoxedPairHead(reg_id, _)
            | LoadBoxedPairRest(reg_id, _)
            | LoadBoxedIntValue(reg_id, _)
            | LoadBoxedFunThunkClosure(reg_id, _)
            | Cond(reg_id, _) => Some(*reg_id),
            Add(reg_id, _) => Some(*reg_id),
            Ret(_) | RetVoid | Unreachable => None,
        }
    }

    pub fn add_input_regs(&self, coll: &mut impl Extend<RegId>) {
        use crate::mir::ops::OpKind::*;
        use std::iter;

        match self {
            ConstNil(_, _)
            | ConstTrue(_, _)
            | ConstFalse(_, _)
            | ConstInt(_, _)
            | ConstBoxedInt(_, _)
            | ConstBoxedStr(_, _)
            | ConstBoxedFunThunk(_, _)
            | CurrentTask(_, _)
            | RetVoid
            | Unreachable => {}
            ConstBoxedPair(_, box_pair_op) | AllocBoxedPair(_, box_pair_op) => {
                coll.extend([box_pair_op.head_reg, box_pair_op.rest_reg].iter().cloned());
            }
            AllocInt(_, reg_id)
            | ConstCastBoxed(
                _,
                CastBoxedOp {
                    from_reg: reg_id, ..
                },
            )
            | CastBoxed(
                _,
                CastBoxedOp {
                    from_reg: reg_id, ..
                },
            )
            | Ret(reg_id)
            | LoadBoxedListLength(_, reg_id)
            | LoadBoxedPairHead(_, reg_id)
            | LoadBoxedPairRest(_, reg_id)
            | LoadBoxedIntValue(_, reg_id)
            | LoadBoxedFunThunkClosure(_, reg_id) => {
                coll.extend(iter::once(*reg_id));
            }
            Call(_, call_op) => {
                coll.extend(call_op.args.iter().cloned());
            }
            Cond(_, cond_op) => {
                coll.extend(
                    [
                        cond_op.test_reg,
                        cond_op.true_result_reg,
                        cond_op.false_result_reg,
                    ]
                        .iter()
                        .cloned(),
                );

                for op in cond_op.true_ops.iter().chain(cond_op.false_ops.iter()) {
                    op.kind().add_input_regs(coll);
                }
            }
            Add(_, binary_op) => {
                coll.extend([binary_op.lhs_reg, binary_op.rhs_reg].iter().cloned());
            }
        }
    }

    pub fn has_side_effects(&self) -> bool {
        use crate::mir::ops::OpKind::*;

        match self {
            Call(_, _) | Ret(_) | RetVoid | Unreachable => true,
            Cond(_, cond_op) => cond_op
                .true_ops
                .iter()
                .chain(cond_op.false_ops.iter())
                .any(|op| op.kind().has_side_effects()),
            _ => false,
        }
    }
}

#[derive(Debug, PartialEq, Clone)]
pub struct Op {
    pub span: Span,
    pub kind: OpKind,
}

impl Op {
    pub fn new(span: Span, kind: OpKind) -> Op {
        Op { span, kind }
    }

    pub fn span(&self) -> Span {
        self.span
    }

    pub fn kind(&self) -> &OpKind {
        &self.kind
    }
}

pub struct Fun {
    pub source_name: Option<String>,

    pub abi: FunABI,
    pub params: Box<[RegId]>,
    pub ops: Box<[Op]>,
}

#[cfg(test)]
mod test {
    use super::*;

    use std::collections::HashSet;

    impl From<OpKind> for Op {
        fn from(op_kind: OpKind) -> Self {
            use syntax::span::EMPTY_SPAN;
            Op::new(EMPTY_SPAN, op_kind)
        }
    }

    #[test]
    fn output_reg() {
        let mut reg_counter = RegIdCounter::new();
        let reg1 = reg_counter.alloc();

        assert_eq!(None, OpKind::RetVoid.output_reg());
        assert_eq!(None, OpKind::Ret(reg1).output_reg());
        assert_eq!(Some(reg1), OpKind::ConstInt(reg1, 14).output_reg());
    }

    #[test]
    fn has_side_effects() {
        let mut reg_counter = RegIdCounter::new();
        let reg1 = reg_counter.alloc();

        assert_eq!(true, OpKind::RetVoid.has_side_effects());
        assert_eq!(false, OpKind::ConstInt(reg1, 14).has_side_effects());

        let cond_op_with_no_side_effects = CondOp {
            test_reg: reg1,
            true_ops: Box::new([]),
            true_result_reg: reg1,
            false_ops: Box::new([]),
            false_result_reg: reg1,
        };

        assert_eq!(
            false,
            OpKind::Cond(reg1, cond_op_with_no_side_effects).has_side_effects()
        );

        let cond_op_with_true_side_effects = CondOp {
            test_reg: reg1,
            true_ops: Box::new([OpKind::RetVoid.into()]),
            true_result_reg: reg1,
            false_ops: Box::new([]),
            false_result_reg: reg1,
        };

        assert_eq!(
            true,
            OpKind::Cond(reg1, cond_op_with_true_side_effects).has_side_effects()
        );

        let cond_op_with_false_side_effects = CondOp {
            test_reg: reg1,
            true_ops: Box::new([]),
            true_result_reg: reg1,
            false_ops: Box::new([OpKind::RetVoid.into()]),
            false_result_reg: reg1,
        };

        assert_eq!(
            true,
            OpKind::Cond(reg1, cond_op_with_false_side_effects).has_side_effects()
        );
    }

    #[test]
    fn ret_input_regs() {
        let mut reg_counter = RegIdCounter::new();
        let mut used_regs = HashSet::<RegId>::new();

        let ret_reg = reg_counter.alloc();
        OpKind::Ret(ret_reg).add_input_regs(&mut used_regs);

        assert_eq!(1, used_regs.len());
        assert!(used_regs.contains(&ret_reg));
    }

    #[test]
    fn cond_input_regs() {
        let mut reg_counter = RegIdCounter::new();
        let mut used_regs = HashSet::<RegId>::new();

        let output_reg = reg_counter.alloc();
        let test_reg = reg_counter.alloc();

        let true_input_reg = reg_counter.alloc();
        let true_result_reg = reg_counter.alloc();

        let false_input_reg = reg_counter.alloc();
        let false_result_reg = reg_counter.alloc();

        let cond_op = CondOp {
            test_reg,
            true_ops: Box::new([OpKind::Ret(true_input_reg).into()]),
            true_result_reg,
            false_ops: Box::new([OpKind::Ret(false_input_reg).into()]),
            false_result_reg,
        };

        OpKind::Cond(output_reg, cond_op).add_input_regs(&mut used_regs);

        for used_reg in &[
            test_reg,
            true_input_reg,
            true_result_reg,
            false_input_reg,
            false_result_reg,
        ] {
            assert!(used_regs.contains(used_reg));
        }

        assert!(!used_regs.contains(&output_reg));
    }
}
