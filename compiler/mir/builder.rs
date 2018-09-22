use syntax::span::Span;

use crate::mir::ops::{Op, OpKind, RegId};

pub struct Builder {
    ops: Vec<Op>,
}

impl Builder {
    pub fn new() -> Builder {
        Builder { ops: vec![] }
    }

    pub fn push_reg<F, P>(&mut self, span: Span, kind_cons: F, kind_param: P) -> RegId
    where
        F: FnOnce(RegId, P) -> OpKind,
    {
        let reg_id = RegId::alloc();
        self.ops.push(Op::new(span, kind_cons(reg_id, kind_param)));
        reg_id
    }

    pub fn into_ops(self) -> Vec<Op> {
        self.ops
    }
}

impl Default for Builder {
    fn default() -> Builder {
        Builder::new()
    }
}
