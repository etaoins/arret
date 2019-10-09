use arret_runtime::abitype;
use arret_runtime::boxed;
use arret_runtime::callback;

use arret_syntax::datum::DataStr;
use arret_syntax::span::Span;

use crate::codegen::GenABI;

use crate::id_type::ArcId;
use crate::mir::tagset::TypeTagSet;
new_counting_id_type!(PrivateFunIdCounter, PrivateFunId);
new_global_id_type!(RegId);

#[derive(Debug, PartialEq, Eq, Hash, Clone)]
pub enum CallConv {
    /// C calling convention
    ///
    /// This is required for thunks and callbacks because they can be called from Rust.
    CCC,

    /// Fast calling convention
    ///
    /// This supports tail recursion.
    FastCC,
}

#[derive(Debug, PartialEq, Eq, Hash, Clone)]
pub struct OpsABI {
    pub call_conv: CallConv,
    pub params: Box<[abitype::ABIType]>,
    pub ret: abitype::RetABIType,
}

impl OpsABI {
    pub fn thunk_abi() -> OpsABI {
        OpsABI {
            call_conv: CallConv::CCC,
            params: Box::new([
                // Closure
                abitype::BoxedABIType::Any.into(),
                // Rest argument
                abitype::TOP_LIST_BOXED_ABI_TYPE.into(),
            ]),
            ret: abitype::BoxedABIType::Any.into(),
        }
    }
}

impl From<callback::EntryPointABIType> for OpsABI {
    fn from(abi_type: callback::EntryPointABIType) -> Self {
        use std::iter;

        // Include our closure
        let params = iter::once(abitype::BoxedABIType::Any.into())
            .chain(abi_type.params.iter().cloned())
            .collect();

        OpsABI {
            call_conv: CallConv::CCC,
            params,
            ret: abi_type.ret,
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
    PrivateFun(PrivateFunId),
    BoxedFunThunk(RegId),
    StaticSymbol(StaticSymbol),
}

/// Represents a structure tagged with a class ID
///
/// This is the MIR analog of [`record::Cons`](crate::ty::record::Cons)
#[derive(Debug, PartialEq, Clone)]
pub struct RecordStruct {
    pub source_name: DataStr,
    pub field_abi_types: Box<[abitype::ABIType]>,
}

pub type RecordStructId = ArcId<RecordStruct>;

impl RecordStruct {
    pub fn new(source_name: DataStr, field_abi_types: Box<[abitype::ABIType]>) -> RecordStructId {
        ArcId::new(RecordStruct {
            source_name,
            field_abi_types,
        })
    }
}

#[derive(Debug, PartialEq, Clone)]
pub struct CallOp {
    pub callee: Callee,
    pub impure: bool,
    pub args: Box<[RegId]>,
}

#[derive(Debug, PartialEq, Clone)]
pub struct TailCallOp {
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
pub struct BoxRecordOp {
    pub record_struct: RecordStructId,
    pub field_regs: Box<[RegId]>,
}

#[derive(Debug, PartialEq, Clone)]
pub struct LoadBoxedRecordFieldOp {
    pub record_reg: RegId,
    pub record_struct: RecordStructId,
    pub field_index: usize,
}

#[derive(Debug, PartialEq, Clone)]
pub struct LoadBoxedVectorMemberOp {
    pub vector_reg: RegId,
    /// Exact known length of the vector we're loading from
    pub known_vector_length: usize,
    pub member_index: usize,
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
pub struct CompareOp {
    pub comparison: Comparison,
    pub lhs_reg: RegId,
    pub rhs_reg: RegId,
}

#[derive(Debug, PartialEq, Clone)]
pub struct LoadBoxedTypeTagOp {
    pub subject_reg: RegId,
    pub possible_type_tags: TypeTagSet,
}

#[derive(Debug, PartialEq, Clone)]
pub struct LoadBoxedListLengthOp {
    pub list_reg: RegId,
    pub min_length: usize,
}

#[derive(Debug, PartialEq, Clone)]
pub struct MakeCallbackOp {
    pub closure_reg: RegId,
    pub callee: Callee,
}

#[derive(Debug, PartialEq, Clone, Copy)]
pub enum Comparison {
    Lt,
    Le,
    Eq,
    Gt,
    Ge,
}

#[derive(Debug, PartialEq, Clone)]
pub enum OpKind {
    ConstInt64(RegId, i64),
    ConstFloat(RegId, f64),
    ConstChar(RegId, char),
    ConstBool(RegId, bool),
    ConstInternedSym(RegId, Box<str>),
    ConstTypeTag(RegId, boxed::TypeTag),
    ConstRecordClassId(RegId, RecordStructId),

    ConstBoxedNil(RegId, ()),
    ConstBoxedTrue(RegId, ()),
    ConstBoxedFalse(RegId, ()),
    ConstBoxedInt(RegId, i64),
    ConstBoxedFloat(RegId, f64),
    ConstBoxedChar(RegId, char),
    ConstBoxedStr(RegId, Box<str>),
    ConstBoxedSym(RegId, Box<str>),
    ConstBoxedPair(RegId, BoxPairOp),
    ConstBoxedFunThunk(RegId, BoxFunThunkOp),
    ConstBoxedVector(RegId, Box<[RegId]>),

    AllocBoxedInt(RegId, RegId),
    AllocBoxedFloat(RegId, RegId),
    AllocBoxedChar(RegId, RegId),
    AllocBoxedSym(RegId, RegId),
    AllocBoxedPair(RegId, BoxPairOp),
    AllocBoxedFunThunk(RegId, BoxFunThunkOp),

    ConstCastBoxed(RegId, CastBoxedOp),
    CastBoxed(RegId, CastBoxedOp),
    Alias(RegId, RegId), // TODO: This is a hack for `duplicate_alloc_ops`

    Call(RegId, CallOp),
    TailCall(RegId, TailCallOp),

    LoadBoxedTypeTag(RegId, LoadBoxedTypeTagOp),
    LoadBoxedListLength(RegId, LoadBoxedListLengthOp),
    LoadBoxedPairHead(RegId, RegId),
    LoadBoxedPairRest(RegId, RegId),
    LoadBoxedIntValue(RegId, RegId),
    LoadBoxedFloatValue(RegId, RegId),
    LoadBoxedCharValue(RegId, RegId),
    LoadBoxedSymInterned(RegId, RegId),
    LoadBoxedFunThunkClosure(RegId, RegId),
    LoadBoxedRecordClassId(RegId, RegId),
    LoadBoxedVectorMember(RegId, LoadBoxedVectorMemberOp),

    Cond(CondOp),

    MakeCallback(RegId, MakeCallbackOp),

    BoolEqual(RegId, BinaryOp),
    CharEqual(RegId, BinaryOp),
    InternedSymEqual(RegId, BinaryOp),
    TypeTagEqual(RegId, BinaryOp),
    RecordClassIdEqual(RegId, BinaryOp),
    BoxIdentical(RegId, BinaryOp),
    Int64ToFloat(RegId, RegId),

    IntCompare(RegId, CompareOp),
    FloatCompare(RegId, CompareOp),

    FloatAdd(RegId, BinaryOp),
    Int64Add(RegId, BinaryOp),
    Int64CheckedAdd(RegId, BinaryOp),
    FloatMul(RegId, BinaryOp),
    Int64CheckedMul(RegId, BinaryOp),
    FloatSub(RegId, BinaryOp),
    Int64CheckedSub(RegId, BinaryOp),
    FloatDiv(RegId, BinaryOp),
    Int64Div(RegId, BinaryOp),
    Int64CheckedDiv(RegId, BinaryOp),
    Int64Rem(RegId, BinaryOp),
    Int64CheckedRem(RegId, BinaryOp),

    ConstBoxedRecord(RegId, BoxRecordOp),
    AllocBoxedRecord(RegId, BoxRecordOp),
    LoadBoxedRecordField(RegId, LoadBoxedRecordFieldOp),

    Ret(RegId),
    RetVoid,
    Unreachable,
    Panic(String),
}

/// Indicates the high-level category of an op
#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Clone, Copy)]
pub enum OpCategory {
    ConstReg,
    ConstBox,
    ConstCastBoxed,
    AllocBoxed,
    Call,
    Cond,
    MemLoad,
    CastBoxed,
    RegOp,
    MakeCallback,
    Ret,
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
            | ConstFloat(reg_id, _)
            | ConstChar(reg_id, _)
            | ConstBool(reg_id, _)
            | ConstInternedSym(reg_id, _)
            | ConstTypeTag(reg_id, _)
            | ConstBoxedInt(reg_id, _)
            | ConstBoxedFloat(reg_id, _)
            | ConstBoxedChar(reg_id, _)
            | ConstBoxedStr(reg_id, _)
            | ConstBoxedSym(reg_id, _)
            | ConstBoxedPair(reg_id, _)
            | ConstBoxedFunThunk(reg_id, _)
            | ConstBoxedVector(reg_id, _)
            | ConstRecordClassId(reg_id, _)
            | AllocBoxedInt(reg_id, _)
            | AllocBoxedFloat(reg_id, _)
            | AllocBoxedChar(reg_id, _)
            | AllocBoxedSym(reg_id, _)
            | AllocBoxedPair(reg_id, _)
            | AllocBoxedFunThunk(reg_id, _)
            | ConstCastBoxed(reg_id, _)
            | CastBoxed(reg_id, _)
            | Alias(reg_id, _)
            | Call(reg_id, _)
            | TailCall(reg_id, _)
            | LoadBoxedTypeTag(reg_id, _)
            | LoadBoxedListLength(reg_id, _)
            | LoadBoxedPairHead(reg_id, _)
            | LoadBoxedPairRest(reg_id, _)
            | LoadBoxedIntValue(reg_id, _)
            | LoadBoxedSymInterned(reg_id, _)
            | LoadBoxedFloatValue(reg_id, _)
            | LoadBoxedCharValue(reg_id, _)
            | LoadBoxedFunThunkClosure(reg_id, _)
            | LoadBoxedRecordClassId(reg_id, _)
            | LoadBoxedRecordField(reg_id, _)
            | LoadBoxedVectorMember(reg_id, _)
            | FloatAdd(reg_id, _)
            | Int64Add(reg_id, _)
            | Int64CheckedAdd(reg_id, _)
            | FloatMul(reg_id, _)
            | Int64CheckedMul(reg_id, _)
            | FloatSub(reg_id, _)
            | Int64CheckedSub(reg_id, _)
            | FloatDiv(reg_id, _)
            | Int64Div(reg_id, _)
            | Int64CheckedDiv(reg_id, _)
            | Int64Rem(reg_id, _)
            | Int64CheckedRem(reg_id, _)
            | IntCompare(reg_id, _)
            | BoolEqual(reg_id, _)
            | CharEqual(reg_id, _)
            | InternedSymEqual(reg_id, _)
            | TypeTagEqual(reg_id, _)
            | RecordClassIdEqual(reg_id, _)
            | FloatCompare(reg_id, _)
            | BoxIdentical(reg_id, _)
            | Int64ToFloat(reg_id, _)
            | MakeCallback(reg_id, _)
            | ConstBoxedRecord(reg_id, _)
            | AllocBoxedRecord(reg_id, _) => Some(*reg_id),
            Cond(cond_op) => cond_op.reg_phi.clone().map(|reg_phi| reg_phi.output_reg),
            Ret(_) | RetVoid | Unreachable | Panic(_) => None,
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
            | ConstFloat(_, _)
            | ConstChar(_, _)
            | ConstBool(_, _)
            | ConstInternedSym(_, _)
            | ConstTypeTag(_, _)
            | ConstRecordClassId(_, _)
            | ConstBoxedInt(_, _)
            | ConstBoxedFloat(_, _)
            | ConstBoxedChar(_, _)
            | ConstBoxedStr(_, _)
            | ConstBoxedSym(_, _)
            | RetVoid
            | Unreachable
            | Panic(_) => {}
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
            ConstBoxedVector(_, element_regs) => coll.extend(element_regs.iter().copied()),
            AllocBoxedInt(_, reg_id)
            | AllocBoxedFloat(_, reg_id)
            | AllocBoxedChar(_, reg_id)
            | AllocBoxedSym(_, reg_id)
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
            | Alias(_, reg_id)
            | Ret(reg_id)
            | LoadBoxedTypeTag(
                _,
                LoadBoxedTypeTagOp {
                    subject_reg: reg_id,
                    ..
                },
            )
            | LoadBoxedListLength(
                _,
                LoadBoxedListLengthOp {
                    list_reg: reg_id, ..
                },
            )
            | LoadBoxedPairHead(_, reg_id)
            | LoadBoxedPairRest(_, reg_id)
            | LoadBoxedIntValue(_, reg_id)
            | LoadBoxedFloatValue(_, reg_id)
            | LoadBoxedCharValue(_, reg_id)
            | LoadBoxedSymInterned(_, reg_id)
            | LoadBoxedFunThunkClosure(_, reg_id)
            | LoadBoxedRecordClassId(_, reg_id)
            | LoadBoxedRecordField(
                _,
                LoadBoxedRecordFieldOp {
                    record_reg: reg_id, ..
                },
            )
            | LoadBoxedVectorMember(
                _,
                LoadBoxedVectorMemberOp {
                    vector_reg: reg_id, ..
                },
            )
            | Int64ToFloat(_, reg_id)
            | MakeCallback(
                _,
                MakeCallbackOp {
                    closure_reg: reg_id,
                    ..
                },
            ) => {
                coll.extend(iter::once(*reg_id));
            }
            Call(_, CallOp { args, .. }) | TailCall(_, TailCallOp { args, .. }) => {
                coll.extend(args.iter().cloned());
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
            FloatAdd(_, binary_op)
            | Int64Add(_, binary_op)
            | Int64CheckedAdd(_, binary_op)
            | FloatMul(_, binary_op)
            | Int64CheckedMul(_, binary_op)
            | FloatSub(_, binary_op)
            | Int64CheckedSub(_, binary_op)
            | FloatDiv(_, binary_op)
            | Int64Div(_, binary_op)
            | Int64CheckedDiv(_, binary_op)
            | Int64Rem(_, binary_op)
            | Int64CheckedRem(_, binary_op)
            | BoolEqual(_, binary_op)
            | CharEqual(_, binary_op)
            | InternedSymEqual(_, binary_op)
            | TypeTagEqual(_, binary_op)
            | RecordClassIdEqual(_, binary_op)
            | BoxIdentical(_, binary_op) => {
                coll.extend([binary_op.lhs_reg, binary_op.rhs_reg].iter().cloned());
            }
            IntCompare(_, compare_op) | FloatCompare(_, compare_op) => {
                coll.extend([compare_op.lhs_reg, compare_op.rhs_reg].iter().cloned());
            }
            ConstBoxedRecord(_, box_record_op) | AllocBoxedRecord(_, box_record_op) => {
                coll.extend(box_record_op.field_regs.iter().cloned());
            }
        }
    }

    /// Indicates if the output of this op is a constant
    pub fn const_output(&self) -> bool {
        [
            OpCategory::ConstBox,
            OpCategory::ConstReg,
            OpCategory::ConstCastBoxed,
        ]
        .contains(&self.category())
    }

    pub fn has_side_effects(&self) -> bool {
        use crate::mir::ops::OpKind::*;

        match self {
            Ret(_) | RetVoid | Unreachable | Panic(_) => true,
            Call(_, CallOp { impure, .. }) | TailCall(_, TailCallOp { impure, .. }) => *impure,
            Cond(cond_op) => cond_op
                .true_ops
                .iter()
                .chain(cond_op.false_ops.iter())
                .any(|op| op.kind().has_side_effects()),
            _ => false,
        }
    }

    pub fn is_terminator(&self) -> bool {
        let category = self.category();
        category == OpCategory::Ret || category == OpCategory::Unreachable
    }

    pub fn category(&self) -> OpCategory {
        use crate::mir::ops::OpKind::*;

        match self {
            ConstInt64(_, _)
            | ConstFloat(_, _)
            | ConstChar(_, _)
            | ConstBool(_, _)
            | ConstInternedSym(_, _)
            | ConstTypeTag(_, _)
            | ConstRecordClassId(_, _) => OpCategory::ConstReg,

            ConstBoxedNil(_, _)
            | ConstBoxedTrue(_, _)
            | ConstBoxedFalse(_, _)
            | ConstBoxedInt(_, _)
            | ConstBoxedFloat(_, _)
            | ConstBoxedChar(_, _)
            | ConstBoxedStr(_, _)
            | ConstBoxedSym(_, _)
            | ConstBoxedPair(_, _)
            | ConstBoxedFunThunk(_, _)
            | ConstBoxedRecord(_, _)
            | ConstBoxedVector(_, _) => OpCategory::ConstBox,

            AllocBoxedInt(_, _)
            | AllocBoxedFloat(_, _)
            | AllocBoxedChar(_, _)
            | AllocBoxedSym(_, _)
            | AllocBoxedPair(_, _)
            | AllocBoxedFunThunk(_, _)
            | AllocBoxedRecord(_, _) => OpCategory::AllocBoxed,

            LoadBoxedTypeTag(_, _)
            | LoadBoxedListLength(_, _)
            | LoadBoxedPairHead(_, _)
            | LoadBoxedPairRest(_, _)
            | LoadBoxedIntValue(_, _)
            | LoadBoxedFloatValue(_, _)
            | LoadBoxedCharValue(_, _)
            | LoadBoxedSymInterned(_, _)
            | LoadBoxedFunThunkClosure(_, _)
            | LoadBoxedRecordClassId(_, _)
            | LoadBoxedRecordField(_, _)
            | LoadBoxedVectorMember(_, _) => OpCategory::MemLoad,

            FloatAdd(_, _)
            | Int64Add(_, _)
            | Int64CheckedAdd(_, _)
            | FloatSub(_, _)
            | Int64CheckedSub(_, _)
            | FloatMul(_, _)
            | Int64CheckedMul(_, _)
            | FloatDiv(_, _)
            | Int64Div(_, _)
            | Int64CheckedDiv(_, _)
            | Int64Rem(_, _)
            | Int64CheckedRem(_, _)
            | IntCompare(_, _)
            | BoolEqual(_, _)
            | CharEqual(_, _)
            | InternedSymEqual(_, _)
            | TypeTagEqual(_, _)
            | RecordClassIdEqual(_, _)
            | FloatCompare(_, _)
            | BoxIdentical(_, _)
            | Int64ToFloat(_, _) => OpCategory::RegOp,

            Ret(_) | RetVoid => OpCategory::Ret,

            Cond(_) => OpCategory::Cond,
            MakeCallback(_, _) => OpCategory::MakeCallback,
            ConstCastBoxed(_, _) => OpCategory::ConstCastBoxed,
            CastBoxed(_, _) | Alias(_, _) => OpCategory::CastBoxed,

            Call(_, _) | TailCall(_, _) => OpCategory::Call,

            Unreachable | Panic(_) => OpCategory::Unreachable,
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
    pub source_name: Option<DataStr>,

    pub abi: OpsABI,
    pub param_regs: Box<[RegId]>,
    pub ops: Box<[Op]>,
}

#[cfg(test)]
mod test {
    use super::*;

    use std::collections::HashSet;

    impl From<OpKind> for Op {
        fn from(op_kind: OpKind) -> Self {
            use arret_syntax::span::EMPTY_SPAN;
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
    fn const_output() {
        assert_eq!(true, OpKind::ConstBool(RegId::alloc(), true).const_output());
        assert_eq!(
            true,
            OpKind::ConstBoxedFalse(RegId::alloc(), ()).const_output()
        );
        assert_eq!(
            true,
            OpKind::ConstCastBoxed(
                RegId::alloc(),
                CastBoxedOp {
                    from_reg: RegId::alloc(),
                    to_type: abitype::BoxedABIType::Any
                }
            )
            .const_output()
        );

        assert_eq!(
            false,
            OpKind::AllocBoxedInt(RegId::alloc(), RegId::alloc()).const_output()
        );
        assert_eq!(
            false,
            OpKind::LoadBoxedListLength(
                RegId::alloc(),
                LoadBoxedListLengthOp {
                    list_reg: RegId::alloc(),
                    min_length: 0
                }
            )
            .const_output()
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
