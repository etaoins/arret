use runtime::abitype;
use runtime::boxed;

use syntax::span::Span;

use crate::codegen::GenABI;
use crate::mir::tagset::TypeTagSet;

new_indexing_id_type!(BuiltFunId, u32);
new_global_id_type!(RegId);

#[derive(Debug, PartialEq, Clone)]
pub struct OpsABI {
    /// Indicates if this function must be externally callable
    ///
    /// This ensures the function takes both a task and closure even if they're unused.
    pub external_call_conv: bool,

    pub params: Box<[abitype::ABIType]>,
    pub ret: abitype::RetABIType,
}

impl OpsABI {
    pub fn thunk_abi() -> OpsABI {
        OpsABI {
            external_call_conv: true,
            params: Box::new([
                abitype::BoxedABIType::Any.into(),
                abitype::TOP_LIST_BOXED_ABI_TYPE.into(),
            ]),
            ret: abitype::BoxedABIType::Any.into(),
        }
    }
}

#[derive(Debug, PartialEq, Clone)]
pub struct StaticSymbol {
    pub symbol: &'static str,
    pub impure: bool,
    pub abi: GenABI,
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
    pub impure: bool,
    pub args: Box<[RegId]>,
}

#[derive(Debug, PartialEq, Clone)]
pub struct BoxPairOp {
    pub head_reg: RegId,
    pub rest_reg: RegId,
    pub length_reg: RegId,
}

#[derive(Debug, PartialEq, Clone)]
pub struct BoxFunThunkOp {
    pub closure_reg: RegId,
    pub callee: Callee,
}

#[derive(Debug, PartialEq, Clone)]
pub struct CastBoxedOp {
    pub from_reg: RegId,
    pub to_type: abitype::BoxedABIType,
}

#[derive(Debug, PartialEq, Clone)]
pub struct RegPhi {
    pub output_reg: RegId,
    pub true_result_reg: RegId,
    pub false_result_reg: RegId,
}

#[derive(Debug, PartialEq, Clone)]
pub struct CondOp {
    pub reg_phi: Option<RegPhi>,
    pub test_reg: RegId,
    pub true_ops: Box<[Op]>,
    pub false_ops: Box<[Op]>,
}

#[derive(Debug, PartialEq, Clone)]
pub struct BinaryOp {
    pub lhs_reg: RegId,
    pub rhs_reg: RegId,
}

#[derive(Debug, PartialEq, Clone)]
pub struct LoadBoxedTypeTagOp {
    pub subject_reg: RegId,
    pub possible_type_tags: TypeTagSet,
}

#[derive(Debug, PartialEq, Clone)]
pub enum OpKind {
    ConstInt64(RegId, i64),
    ConstUsize(RegId, usize),
    ConstBool(RegId, bool),
    ConstTypeTag(RegId, boxed::TypeTag),

    ConstBoxedNil(RegId, ()),
    ConstBoxedTrue(RegId, ()),
    ConstBoxedFalse(RegId, ()),
    ConstBoxedInt(RegId, i64),
    ConstBoxedStr(RegId, Box<str>),
    ConstBoxedPair(RegId, BoxPairOp),
    ConstBoxedFunThunk(RegId, BoxFunThunkOp),

    AllocBoxedInt(RegId, RegId),
    AllocBoxedPair(RegId, BoxPairOp),
    AllocBoxedFunThunk(RegId, BoxFunThunkOp),

    ConstCastBoxed(RegId, CastBoxedOp),
    CastBoxed(RegId, CastBoxedOp),

    Call(RegId, CallOp),
    LoadBoxedTypeTag(RegId, LoadBoxedTypeTagOp),
    LoadBoxedListLength(RegId, RegId),
    LoadBoxedPairHead(RegId, RegId),
    LoadBoxedPairRest(RegId, RegId),
    LoadBoxedIntValue(RegId, RegId),
    LoadBoxedFunThunkClosure(RegId, RegId),
    Cond(CondOp),

    Add(RegId, BinaryOp),
    IntEqual(RegId, BinaryOp),
    UsizeToInt64(RegId, RegId),

    Ret(RegId),
    RetVoid,
    Unreachable,
}

impl OpKind {
    pub fn output_reg(&self) -> Option<RegId> {
        use crate::mir::ops::OpKind::*;

        match self {
            ConstBoxedNil(reg_id, _)
            | ConstBoxedTrue(reg_id, _)
            | ConstBoxedFalse(reg_id, _)
            | ConstInt64(reg_id, _)
            | ConstUsize(reg_id, _)
            | ConstBool(reg_id, _)
            | ConstTypeTag(reg_id, _)
            | ConstBoxedInt(reg_id, _)
            | ConstBoxedStr(reg_id, _)
            | ConstBoxedPair(reg_id, _)
            | ConstBoxedFunThunk(reg_id, _)
            | AllocBoxedInt(reg_id, _)
            | AllocBoxedPair(reg_id, _)
            | AllocBoxedFunThunk(reg_id, _)
            | ConstCastBoxed(reg_id, _)
            | CastBoxed(reg_id, _)
            | Call(reg_id, _)
            | LoadBoxedTypeTag(reg_id, _)
            | LoadBoxedListLength(reg_id, _)
            | LoadBoxedPairHead(reg_id, _)
            | LoadBoxedPairRest(reg_id, _)
            | LoadBoxedIntValue(reg_id, _)
            | LoadBoxedFunThunkClosure(reg_id, _)
            | Add(reg_id, _) => Some(*reg_id),
            IntEqual(reg_id, _) | UsizeToInt64(reg_id, _) => Some(*reg_id),
            Cond(cond_op) => cond_op.reg_phi.clone().map(|reg_phi| reg_phi.output_reg),
            Ret(_) | RetVoid | Unreachable => None,
        }
    }

    pub fn add_input_regs(&self, coll: &mut impl Extend<RegId>) {
        use crate::mir::ops::OpKind::*;
        use std::iter;

        match self {
            ConstBoxedNil(_, _)
            | ConstBoxedTrue(_, _)
            | ConstBoxedFalse(_, _)
            | ConstInt64(_, _)
            | ConstUsize(_, _)
            | ConstBool(_, _)
            | ConstTypeTag(_, _)
            | ConstBoxedInt(_, _)
            | ConstBoxedStr(_, _)
            | RetVoid
            | Unreachable => {}
            ConstBoxedPair(_, box_pair_op) | AllocBoxedPair(_, box_pair_op) => {
                coll.extend(
                    [
                        box_pair_op.length_reg,
                        box_pair_op.head_reg,
                        box_pair_op.rest_reg,
                    ]
                    .iter()
                    .cloned(),
                );
            }
            ConstBoxedFunThunk(_, box_fun_thunk_op) | AllocBoxedFunThunk(_, box_fun_thunk_op) => {
                coll.extend(iter::once(box_fun_thunk_op.closure_reg));
            }
            AllocBoxedInt(_, reg_id)
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
            | LoadBoxedTypeTag(
                _,
                LoadBoxedTypeTagOp {
                    subject_reg: reg_id,
                    ..
                },
            )
            | LoadBoxedListLength(_, reg_id)
            | LoadBoxedPairHead(_, reg_id)
            | LoadBoxedPairRest(_, reg_id)
            | LoadBoxedIntValue(_, reg_id)
            | LoadBoxedFunThunkClosure(_, reg_id)
            | UsizeToInt64(_, reg_id) => {
                coll.extend(iter::once(*reg_id));
            }
            Call(_, call_op) => {
                coll.extend(call_op.args.iter().cloned());
            }
            Cond(cond_op) => {
                coll.extend(iter::once(cond_op.test_reg));

                if let Some(reg_phi) = &cond_op.reg_phi {
                    coll.extend(
                        [reg_phi.true_result_reg, reg_phi.false_result_reg]
                            .iter()
                            .cloned(),
                    );
                }

                for op in cond_op.true_ops.iter().chain(cond_op.false_ops.iter()) {
                    op.kind().add_input_regs(coll);
                }
            }
            Add(_, binary_op) | IntEqual(_, binary_op) => {
                coll.extend([binary_op.lhs_reg, binary_op.rhs_reg].iter().cloned());
            }
        }
    }

    pub fn has_side_effects(&self) -> bool {
        use crate::mir::ops::OpKind::*;

        match self {
            Ret(_) | RetVoid | Unreachable => true,
            Call(_, call_op) => call_op.impure,
            Cond(cond_op) => cond_op
                .true_ops
                .iter()
                .chain(cond_op.false_ops.iter())
                .any(|op| op.kind().has_side_effects()),
            _ => false,
        }
    }

    pub fn is_terminator(&self) -> bool {
        use crate::mir::ops::OpKind::*;

        match self {
            Ret(_) | RetVoid | Unreachable => true,
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
    pub span: Span,
    pub source_name: Option<String>,

    pub abi: OpsABI,
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
        let reg1 = RegId::alloc();

        assert_eq!(None, OpKind::RetVoid.output_reg());
        assert_eq!(None, OpKind::Ret(reg1).output_reg());
        assert_eq!(Some(reg1), OpKind::ConstInt64(reg1, 14).output_reg());
    }

    #[test]
    fn has_side_effects() {
        let reg1 = RegId::alloc();

        assert_eq!(true, OpKind::RetVoid.has_side_effects());
        assert_eq!(false, OpKind::ConstInt64(reg1, 14).has_side_effects());

        let cond_op_with_no_side_effects = CondOp {
            reg_phi: None,
            test_reg: reg1,
            true_ops: Box::new([]),
            false_ops: Box::new([]),
        };

        assert_eq!(
            false,
            OpKind::Cond(cond_op_with_no_side_effects).has_side_effects()
        );

        let cond_op_with_true_side_effects = CondOp {
            reg_phi: None,
            test_reg: reg1,
            true_ops: Box::new([OpKind::RetVoid.into()]),
            false_ops: Box::new([]),
        };

        assert_eq!(
            true,
            OpKind::Cond(cond_op_with_true_side_effects).has_side_effects()
        );

        let cond_op_with_false_side_effects = CondOp {
            reg_phi: None,
            test_reg: reg1,
            true_ops: Box::new([]),
            false_ops: Box::new([OpKind::RetVoid.into()]),
        };

        assert_eq!(
            true,
            OpKind::Cond(cond_op_with_false_side_effects).has_side_effects()
        );
    }

    #[test]
    fn ret_input_regs() {
        let mut used_regs = HashSet::<RegId>::new();

        let ret_reg = RegId::alloc();
        OpKind::Ret(ret_reg).add_input_regs(&mut used_regs);

        assert_eq!(1, used_regs.len());
        assert!(used_regs.contains(&ret_reg));
    }

    #[test]
    fn cond_input_regs() {
        let mut used_regs = HashSet::<RegId>::new();

        let output_reg = RegId::alloc();
        let test_reg = RegId::alloc();

        let true_input_reg = RegId::alloc();
        let true_result_reg = RegId::alloc();

        let false_input_reg = RegId::alloc();
        let false_result_reg = RegId::alloc();

        let cond_op = CondOp {
            reg_phi: Some(RegPhi {
                output_reg,
                true_result_reg,
                false_result_reg,
            }),
            test_reg,
            true_ops: Box::new([OpKind::Ret(true_input_reg).into()]),
            false_ops: Box::new([OpKind::Ret(false_input_reg).into()]),
        };

        OpKind::Cond(cond_op).add_input_regs(&mut used_regs);

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
