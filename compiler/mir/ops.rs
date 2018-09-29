use runtime::abitype;

use syntax::span::Span;

new_indexing_id_type!(BuiltFunId, u32);
new_counting_id_type!(RegIdCounter, RegId);

#[derive(Debug)]
pub struct FunABI {
    pub takes_task: bool,
    pub takes_captures: bool,
    pub params: Box<[abitype::ABIType]>,
    pub ret: abitype::RetABIType,
}

impl FunABI {
    pub fn thunk_abi() -> FunABI {
        FunABI {
            takes_task: true,
            takes_captures: true,
            params: Box::new([abitype::TOP_LIST_BOXED_ABI_TYPE.into()]),
            ret: abitype::BoxedABIType::Any.into(),
        }
    }
}

#[derive(Debug)]
pub struct CallOp {
    pub fun_reg: RegId,
    pub args: Box<[RegId]>,
}

#[derive(Debug)]
pub struct ConstEntryPointOp {
    pub symbol: &'static str,
    pub abi: FunABI,
}

#[derive(Debug)]
pub struct ConstBoxedPairOp {
    pub head_reg: RegId,
    pub rest_reg: RegId,
    pub length: usize,
}

#[derive(Debug)]
pub struct CastBoxedOp {
    pub from_reg: RegId,
    pub to_type: abitype::BoxedABIType,
}

#[derive(Debug)]
pub struct CondOp {
    pub test_reg: RegId,
    pub true_ops: Vec<Op>,
    pub true_result_reg: RegId,
    pub false_ops: Vec<Op>,
    pub false_result_reg: RegId,
}

#[derive(Debug)]
pub enum OpKind {
    ConstNil(RegId, ()),
    ConstTrue(RegId, ()),
    ConstFalse(RegId, ()),
    ConstInt(RegId, i64),
    ConstBoxedInt(RegId, i64),
    ConstBoxedStr(RegId, Box<str>),
    ConstBoxedPair(RegId, ConstBoxedPairOp),
    ConstEntryPoint(RegId, ConstEntryPointOp),
    ConstBuiltFunEntryPoint(RegId, BuiltFunId),
    ConstBoxedFunThunk(RegId, RegId),
    ConstCastBoxed(RegId, CastBoxedOp),
    CastBoxed(RegId, CastBoxedOp),
    CurrentTask(RegId, ()),
    Call(RegId, CallOp),
    Ret(RegId),
    RetVoid,
    Unreachable,
    LoadBoxedPairHead(RegId, RegId),
    LoadBoxedPairRest(RegId, RegId),
    LoadBoxedIntValue(RegId, RegId),
    Cond(RegId, CondOp),
}

#[derive(Debug)]
pub struct Op {
    span: Span,
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
    pub params: Vec<RegId>,
    pub ops: Vec<Op>,
}
