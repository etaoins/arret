use runtime::abitype;

use syntax::span::Span;

new_counting_id_type!(RegIdCounter, RegId, u32);

pub struct EntryPointABI {
    pub takes_task: bool,
    pub params: Box<[abitype::ABIType]>,
    pub ret: abitype::RetABIType,
}

pub struct CallOp {
    pub fun_reg: RegId,
    pub args: Box<[RegId]>,
}

pub struct ConstEntryPointOp {
    pub symbol: &'static str,
    pub abi: EntryPointABI,
}

pub struct ConstBoxedPairOp {
    pub car_reg: RegId,
    pub cdr_reg: RegId,
    pub length: usize,
}

pub struct CastBoxedOp {
    pub from_reg: RegId,
    pub to_type: abitype::BoxedABIType,
}

pub enum OpKind {
    ConstNil(RegId, ()),
    ConstInt(RegId, i64),
    ConstBoxedInt(RegId, i64),
    ConstBoxedStr(RegId, Box<str>),
    ConstBoxedPair(RegId, ConstBoxedPairOp),
    ConstEntryPoint(RegId, ConstEntryPointOp),
    CastBoxed(RegId, CastBoxedOp),
    CurrentTask(RegId, ()),
    Call(RegId, CallOp),
}

pub struct Op {
    _span: Span,
    pub kind: OpKind,
}

impl Op {
    pub fn new(span: Span, kind: OpKind) -> Op {
        Op { _span: span, kind }
    }
}
