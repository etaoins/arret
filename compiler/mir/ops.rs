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

pub enum OpKind {
    ConstInt(RegId, i64),
    ConstBoxedInt(RegId, i64),
    ConstBoxedStr(RegId, Box<str>),
    ConstEntryPoint(RegId, ConstEntryPointOp),
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
