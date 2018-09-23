use syntax::span::Span;

use runtime::abitype;

use crate::mir::ops::{CastBoxedOp, CondOp, Op, OpKind, RegId};

pub struct Builder {
    ops: Vec<Op>,
}

impl Builder {
    pub fn new() -> Builder {
        Builder { ops: vec![] }
    }

    pub fn alloc_reg(&mut self) -> RegId {
        RegId::alloc()
    }

    pub fn push_reg<F, P>(&mut self, span: Span, kind_cons: F, kind_param: P) -> RegId
    where
        F: FnOnce(RegId, P) -> OpKind,
    {
        let reg_id = RegId::alloc();
        self.push(span, kind_cons(reg_id, kind_param));
        reg_id
    }

    pub fn push(&mut self, span: Span, kind: OpKind) {
        self.ops.push(Op::new(span, kind));
    }

    pub fn into_ops(self) -> Vec<Op> {
        self.ops
    }

    pub fn push_cond<T, F>(
        &mut self,
        span: Span,
        test_reg: RegId,
        true_cons: T,
        false_cons: F,
    ) -> RegId
    where
        T: FnOnce(&mut Builder) -> RegId,
        F: FnOnce(&mut Builder) -> RegId,
    {
        let mut true_builder = Builder::new();
        let true_result_reg = true_cons(&mut true_builder);

        let mut false_builder = Builder::new();
        let false_result_reg = false_cons(&mut false_builder);

        self.push_reg(
            span,
            OpKind::Cond,
            CondOp {
                test_reg,
                true_ops: true_builder.into_ops(),
                true_result_reg,
                false_ops: false_builder.into_ops(),
                false_result_reg,
            },
        )
    }

    pub fn cast_boxed(
        &mut self,
        span: Span,
        from_reg: RegId,
        to_type: abitype::BoxedABIType,
    ) -> RegId {
        self.push_reg(span, OpKind::CastBoxed, CastBoxedOp { from_reg, to_type })
    }

    pub fn cast_boxed_cond(
        &mut self,
        span: Span,
        from_type: &abitype::BoxedABIType,
        from_reg: RegId,
        to_type: abitype::BoxedABIType,
    ) -> RegId {
        if from_type == &to_type {
            from_reg
        } else {
            self.cast_boxed(span, from_reg, to_type)
        }
    }
}

impl Default for Builder {
    fn default() -> Builder {
        Builder::new()
    }
}

pub trait TryToBuilder {
    fn try_to_builder(&mut self) -> Option<&mut Builder>;
}

impl TryToBuilder for Option<Builder> {
    fn try_to_builder(&mut self) -> Option<&mut Builder> {
        self.as_mut()
    }
}

impl TryToBuilder for Builder {
    fn try_to_builder(&mut self) -> Option<&mut Builder> {
        Some(self)
    }
}
