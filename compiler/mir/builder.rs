use std::fmt;

use crate::mir::ops::{CastBoxedOp, CondOp, Op, OpKind, RegId, RegPhi};
use arret_runtime::abitype;
use arret_syntax::span::Span;

pub struct Builder {
    ops: Vec<Op>,
}

#[derive(Clone, Copy)]
pub enum BuiltReg {
    Const(RegId),
    Local(RegId),
}

impl BuiltReg {
    pub fn into_reg_id(self) -> RegId {
        match self {
            BuiltReg::Const(reg_id) | BuiltReg::Local(reg_id) => reg_id,
        }
    }

    pub fn is_const(self) -> bool {
        match self {
            BuiltReg::Const(_) => true,
            BuiltReg::Local(_) => false,
        }
    }
}

impl From<BuiltReg> for RegId {
    fn from(built_reg: BuiltReg) -> RegId {
        built_reg.into_reg_id()
    }
}

impl fmt::Debug for BuiltReg {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        // This is by analogy with LLVM
        if self.is_const() {
            write!(f, "@{}", self.into_reg_id().get())
        } else {
            write!(f, "%{}", self.into_reg_id().get())
        }
    }
}

impl Builder {
    pub fn new() -> Builder {
        Builder { ops: vec![] }
    }

    pub fn alloc_local(&mut self) -> BuiltReg {
        BuiltReg::Local(RegId::alloc())
    }

    pub fn push_reg<F, P>(&mut self, span: Span, kind_cons: F, kind_param: P) -> BuiltReg
    where
        F: FnOnce(RegId, P) -> OpKind,
    {
        let reg_id = RegId::alloc();
        let kind = kind_cons(reg_id, kind_param);
        let output_reg_is_const = kind.const_output();

        self.push(span, kind);

        if output_reg_is_const {
            BuiltReg::Const(reg_id)
        } else {
            BuiltReg::Local(reg_id)
        }
    }

    pub fn push(&mut self, span: Span, kind: OpKind) {
        self.ops.push(Op::new(span, kind));
    }

    pub fn append(&mut self, ops: impl IntoIterator<Item = Op>) {
        self.ops.extend(ops);
    }

    pub fn into_ops(self) -> Box<[Op]> {
        self.ops.into_boxed_slice()
    }

    pub fn push_cond<T, F>(
        &mut self,
        span: Span,
        test_reg: RegId,
        true_cons: T,
        false_cons: F,
    ) -> BuiltReg
    where
        T: FnOnce(&mut Builder) -> RegId,
        F: FnOnce(&mut Builder) -> RegId,
    {
        let mut true_builder = Builder::new();
        let true_result_reg = true_cons(&mut true_builder);

        let mut false_builder = Builder::new();
        let false_result_reg = false_cons(&mut false_builder);

        let output_reg = RegId::alloc();
        self.push(
            span,
            OpKind::Cond(CondOp {
                reg_phi: Some(RegPhi {
                    output_reg,
                    true_result_reg,
                    false_result_reg,
                }),
                test_reg,
                true_ops: true_builder.into_ops(),
                false_ops: false_builder.into_ops(),
            }),
        );

        BuiltReg::Local(output_reg)
    }

    pub fn cast_boxed(
        &mut self,
        span: Span,
        from_reg: BuiltReg,
        to_type: abitype::BoxedABIType,
    ) -> BuiltReg {
        let kind_cons = if from_reg.is_const() {
            OpKind::ConstCastBoxed
        } else {
            OpKind::CastBoxed
        };

        let cast_boxed_op = CastBoxedOp {
            from_reg: from_reg.into(),
            to_type,
        };

        self.push_reg(span, kind_cons, cast_boxed_op)
    }

    pub fn cast_boxed_cond(
        &mut self,
        span: Span,
        from_type: &abitype::BoxedABIType,
        from_reg: BuiltReg,
        to_type: abitype::BoxedABIType,
    ) -> BuiltReg {
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
