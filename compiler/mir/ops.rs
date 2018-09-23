use std::num::NonZeroUsize;

use runtime::abitype;

use syntax::span::Span;

#[derive(Debug, PartialEq, Eq, Clone, Copy, Hash, PartialOrd, Ord)]
pub struct RegId(NonZeroUsize);

#[derive(Copy, Clone)]
pub(super) struct RegIdCounter {
    curr_id: usize,
}

impl RegIdCounter {
    pub(super) fn new() -> RegIdCounter {
        RegIdCounter { curr_id: 0 }
    }

    pub(super) fn alloc(&mut self) -> RegId {
        self.curr_id += 1;
        RegId(NonZeroUsize::new(self.curr_id).unwrap())
    }
}

#[derive(Debug)]
pub struct EntryPointABI {
    pub takes_task: bool,
    pub params: Box<[abitype::ABIType]>,
    pub ret: abitype::RetABIType,
}

#[derive(Debug)]
pub struct CallOp {
    pub fun_reg: RegId,
    pub args: Box<[RegId]>,
}

#[derive(Debug)]
pub struct ConstEntryPointOp {
    pub symbol: &'static str,
    pub abi: EntryPointABI,
}

#[derive(Debug)]
pub struct ConstBoxedPairOp {
    pub car_reg: RegId,
    pub cdr_reg: RegId,
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
    CastBoxed(RegId, CastBoxedOp),
    CurrentTask(RegId, ()),
    Call(RegId, CallOp),
    Ret(RegId),
    LoadBoxedPairHead(RegId, RegId),
    LoadBoxedPairRest(RegId, RegId),
    LoadBoxedIntValue(RegId, RegId),
    Cond(RegId, CondOp),
}

#[derive(Debug)]
pub struct Op {
    _span: Span,
    pub kind: OpKind,
}

impl Op {
    pub fn new(span: Span, kind: OpKind) -> Op {
        Op { _span: span, kind }
    }
}

pub struct Fun {
    pub abi: EntryPointABI,
    pub params: Vec<RegId>,
    pub ops: Vec<Op>,
}
