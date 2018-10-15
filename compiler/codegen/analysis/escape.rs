use std::collections::HashMap;

use runtime::abitype::{ABIType, ParamABIType, ParamCapture, RetABIType};

use crate::codegen::fun_gen::BuiltFun;
use crate::codegen::GenABI;
use crate::mir::ops;

/// Describes the capture behaviour of a function parameter
#[derive(PartialEq, Eq, PartialOrd, Ord, Clone, Copy, Debug)]
pub enum CaptureKind {
    /// This reg is always captured
    Always = 2,
    /// This reg is captured if the function's return value is captured
    ViaRet = 1,
    /// This reg is never captured
    Never = 0,
}

impl CaptureKind {
    /// Calculates the capture of a passed parameter based on the the call's return capture
    fn capture_for_call_param(self, return_capture: CaptureKind) -> CaptureKind {
        match self {
            CaptureKind::Always => CaptureKind::Always,
            CaptureKind::ViaRet => return_capture,
            CaptureKind::Never => CaptureKind::Never,
        }
    }
}

/// Tracks the captures for all regs in a fucntion
#[derive(Debug)]
pub struct Captures {
    inner: HashMap<ops::RegId, CaptureKind>,
}

impl Captures {
    pub fn new() -> Captures {
        Captures {
            inner: HashMap::new(),
        }
    }

    /// Adds the capture of a reg to the capture state
    ///
    /// If there is an existing capture the "stronger" capture kind of the two will be used.
    pub fn add(&mut self, reg_id: ops::RegId, capture: CaptureKind) {
        use std::cmp::max;

        // It's not worthwhile to track never captures; that's the default capture type
        if capture != CaptureKind::Never {
            self.inner
                .entry(reg_id)
                .and_modify(|e| {
                    *e = max(*e, capture);
                })
                .or_insert(capture);
        }
    }

    pub fn get(&self, reg_id: ops::RegId) -> CaptureKind {
        self.inner
            .get(&reg_id)
            .cloned()
            .unwrap_or(CaptureKind::Never)
    }
}

/// Infers if a function can capture a parameter based on its return type
///
/// This is used for Rust functions where we don't have precise capture information. This uses a
/// very conservative algorithm where any function returning a box is assumed to capture all of its
/// arguments.
pub fn infer_param_capture_kind(
    ret_abi_type: &RetABIType,
    param_abi_type: &ParamABIType,
) -> CaptureKind {
    let returns_box = match ret_abi_type {
        RetABIType::Inhabited(ABIType::Boxed(_)) => true,
        _ => false,
    };

    match param_abi_type.capture {
        ParamCapture::Auto => {
            if returns_box {
                CaptureKind::ViaRet
            } else {
                CaptureKind::Never
            }
        }
        ParamCapture::Always => CaptureKind::Always,
        ParamCapture::Never => CaptureKind::Never,
    }
}

fn add_static_symbol_call_captures(
    captures: &mut Captures,
    return_capture: CaptureKind,
    static_symbol_abi: &GenABI,
    args: &[ops::RegId],
) {
    let mut arg_iter = args.iter();

    if static_symbol_abi.takes_closure {
        arg_iter.next();
    }

    assert_eq!(arg_iter.len(), static_symbol_abi.params.len());
    for (arg_reg, param_abi_type) in arg_iter.zip(static_symbol_abi.params.iter()) {
        let param_capture = infer_param_capture_kind(&static_symbol_abi.ret, param_abi_type);

        captures.add(
            *arg_reg,
            param_capture.capture_for_call_param(return_capture),
        );
    }
}

fn add_built_fun_call_captures(
    captures: &mut Captures,
    return_capture: CaptureKind,
    built_fun: &BuiltFun,
    args: &[ops::RegId],
) {
    assert_eq!(args.len(), built_fun.param_captures.len());
    for (arg_reg, param_capture) in args.iter().zip(built_fun.param_captures.iter()) {
        captures.add(
            *arg_reg,
            param_capture.capture_for_call_param(return_capture),
        );
    }
}

fn add_op_captures(
    built_funs: &[BuiltFun],
    captures: &mut Captures,
    ret_type: &RetABIType,
    op: &ops::Op,
) {
    use crate::mir::ops::OpKind;

    match op.kind() {
        OpKind::Ret(ret_reg) => {
            if let RetABIType::Inhabited(ABIType::Boxed(_)) = ret_type {
                // `Ret` captures boxes unconditionally
                captures.add(*ret_reg, CaptureKind::ViaRet);
            }
        }
        OpKind::CastBoxed(reg, ops::CastBoxedOp { from_reg, .. })
        | OpKind::LoadBoxedPairHead(reg, from_reg)
        | OpKind::LoadBoxedPairRest(reg, from_reg) => {
            captures.add(*from_reg, captures.get(*reg));
        }
        OpKind::AllocBoxedPair(
            reg,
            ops::BoxPairOp {
                head_reg, rest_reg, ..
            },
        ) => {
            let output_capture = captures.get(*reg);
            captures.add(*head_reg, output_capture);
            captures.add(*rest_reg, output_capture);
        }
        OpKind::Cond(ops::CondOp {
            reg_phi,
            true_ops,
            false_ops,
            ..
        }) => {
            if let Some(reg_phi) = reg_phi {
                let output_capture = captures.get(reg_phi.output_reg);

                // Propagate captures through the phi
                captures.add(reg_phi.true_result_reg, output_capture);
                captures.add(reg_phi.false_result_reg, output_capture);
            }

            for op in true_ops.iter().rev().chain(false_ops.iter().rev()) {
                add_op_captures(built_funs, captures, ret_type, op);
            }
        }
        OpKind::Call(reg, ops::CallOp { callee, args, .. }) => {
            let return_capture = captures.get(*reg);

            match callee {
                ops::Callee::StaticSymbol(ops::StaticSymbol { abi, .. }) => {
                    add_static_symbol_call_captures(captures, return_capture, abi, args);
                }
                ops::Callee::BuiltFun(built_fun_id) => {
                    let built_fun = &built_funs[built_fun_id.to_usize()];
                    add_built_fun_call_captures(captures, return_capture, built_fun, args);
                }
                ops::Callee::BoxedFunThunk(_) => {
                    // We know nothing about the actual captures. We need to assume the worst.
                    captures.add(args[1], CaptureKind::Always);
                }
            };
        }
        _ => {}
    }
}

/// Calculates all of the the captured regs for a function
pub fn calc_fun_captures(built_funs: &[BuiltFun], fun: &ops::Fun) -> Captures {
    let mut captures = Captures::new();

    for op in fun.ops.iter().rev() {
        add_op_captures(built_funs, &mut captures, &fun.abi.ret, op);
    }

    captures
}

#[cfg(test)]
mod test {
    use super::*;
    use runtime::boxed;
    use syntax::span::EMPTY_SPAN;

    #[test]
    fn infer_param_capture() {
        // Boxed return type can capture boxed parameter
        assert_eq!(
            CaptureKind::ViaRet,
            infer_param_capture_kind(&boxed::TypeTag::Int.into(), &boxed::TypeTag::Int.into())
        );

        // Unboxed return type cannot capture boxed parameter
        assert_eq!(
            CaptureKind::Never,
            infer_param_capture_kind(&ABIType::Bool.into(), &boxed::TypeTag::Int.into())
        );
    }

    #[test]
    fn empty_fun_captures() {
        let param_reg = ops::RegId::alloc();

        let test_fun = ops::Fun {
            span: EMPTY_SPAN,
            source_name: None,

            abi: ops::OpsABI {
                call_conv: ops::CallConv::FreeFunction,
                params: Box::new([boxed::TypeTag::Int.into()]),
                ret: RetABIType::Void,
            },
            params: Box::new([param_reg]),
            ops: Box::new([]),
        };

        let captures = calc_fun_captures(&[], &test_fun);
        assert_eq!(CaptureKind::Never, captures.get(param_reg));
    }

    #[test]
    fn capture_param_via_ret() {
        let capture_reg = ops::RegId::alloc();

        let test_fun = ops::Fun {
            span: EMPTY_SPAN,
            source_name: None,

            abi: ops::OpsABI {
                call_conv: ops::CallConv::FreeFunction,
                params: Box::new([boxed::TypeTag::Int.into()]),
                ret: boxed::TypeTag::Int.into(),
            },
            params: Box::new([capture_reg]),
            ops: Box::new([ops::OpKind::Ret(capture_reg).into()]),
        };

        let captures = calc_fun_captures(&[], &test_fun);
        assert_eq!(CaptureKind::ViaRet, captures.get(capture_reg));
    }

    #[test]
    fn capture_param_via_pair() {
        let param_reg = ops::RegId::alloc();
        let ret_reg = ops::RegId::alloc();

        let test_fun = ops::Fun {
            span: EMPTY_SPAN,
            source_name: None,

            abi: ops::OpsABI {
                call_conv: ops::CallConv::FreeFunction,
                params: Box::new([boxed::TypeTag::Int.into()]),
                ret: boxed::TypeTag::TopPair.into(),
            },
            params: Box::new([param_reg]),
            ops: Box::new([
                ops::OpKind::AllocBoxedPair(
                    ret_reg,
                    ops::BoxPairOp {
                        head_reg: param_reg,
                        rest_reg: param_reg,
                        length_reg: param_reg,
                    },
                )
                .into(),
                ops::OpKind::Ret(ret_reg).into(),
            ]),
        };

        let captures = calc_fun_captures(&[], &test_fun);
        assert_eq!(CaptureKind::ViaRet, captures.get(param_reg));
        assert_eq!(CaptureKind::ViaRet, captures.get(ret_reg));
    }

    #[test]
    fn capture_param_via_box_thunk_call() {
        let param_reg = ops::RegId::alloc();
        let ret_reg = ops::RegId::alloc();

        let test_fun = ops::Fun {
            span: EMPTY_SPAN,
            source_name: None,

            abi: ops::OpsABI {
                call_conv: ops::CallConv::FreeFunction,
                params: Box::new([boxed::TypeTag::Int.into()]),
                ret: boxed::TypeTag::TopPair.into(),
            },
            params: Box::new([param_reg]),
            ops: Box::new([
                ops::OpKind::Call(
                    ret_reg,
                    ops::CallOp {
                        callee: ops::Callee::BoxedFunThunk(param_reg),
                        impure: true,
                        args: Box::new([param_reg, param_reg, param_reg]),
                    },
                )
                .into(),
                ops::OpKind::Ret(ret_reg).into(),
            ]),
        };

        let captures = calc_fun_captures(&[], &test_fun);
        assert_eq!(CaptureKind::Always, captures.get(param_reg));
        assert_eq!(CaptureKind::ViaRet, captures.get(ret_reg));
    }

    #[test]
    fn capture_param_via_static_symbol_call() {
        // These are passed to the first call with an unused ret
        let param_reg1 = ops::RegId::alloc();
        let param_reg2 = ops::RegId::alloc();
        let param_reg3 = ops::RegId::alloc();

        // These are passed to the second call which does have its ret captured
        let param_reg4 = ops::RegId::alloc();
        let param_reg5 = ops::RegId::alloc();
        let param_reg6 = ops::RegId::alloc();

        let unused_reg = ops::RegId::alloc();
        let ret_reg = ops::RegId::alloc();

        let static_symbol_abi = GenABI {
            takes_task: false,
            takes_closure: false,
            params: Box::new([
                ParamABIType {
                    abi_type: boxed::TypeTag::Int.into(),
                    capture: ParamCapture::Never,
                },
                ParamABIType {
                    abi_type: boxed::TypeTag::Int.into(),
                    capture: ParamCapture::Auto,
                },
                ParamABIType {
                    abi_type: boxed::TypeTag::Int.into(),
                    capture: ParamCapture::Always,
                },
            ]),
            ret: boxed::TypeTag::Int.into(),
        };

        let static_symbol = ops::StaticSymbol {
            symbol: "test",
            impure: true,
            abi: static_symbol_abi,
        };

        let test_fun = ops::Fun {
            span: EMPTY_SPAN,
            source_name: None,

            abi: ops::OpsABI {
                call_conv: ops::CallConv::FreeFunction,
                params: Box::new([
                    boxed::TypeTag::Int.into(),
                    boxed::TypeTag::Int.into(),
                    boxed::TypeTag::Int.into(),
                    boxed::TypeTag::Int.into(),
                    boxed::TypeTag::Int.into(),
                    boxed::TypeTag::Int.into(),
                ]),
                ret: boxed::TypeTag::Int.into(),
            },
            params: Box::new([param_reg1]),
            ops: Box::new([
                ops::OpKind::Call(
                    unused_reg,
                    ops::CallOp {
                        callee: ops::Callee::StaticSymbol(static_symbol.clone()),
                        impure: true,
                        args: Box::new([param_reg1, param_reg2, param_reg3]),
                    },
                )
                .into(),
                ops::OpKind::Call(
                    ret_reg,
                    ops::CallOp {
                        callee: ops::Callee::StaticSymbol(static_symbol.clone()),
                        impure: true,
                        args: Box::new([param_reg4, param_reg5, param_reg6]),
                    },
                )
                .into(),
                ops::OpKind::Ret(ret_reg).into(),
            ]),
        };

        let captures = calc_fun_captures(&[], &test_fun);

        assert_eq!(CaptureKind::Never, captures.get(param_reg1));
        assert_eq!(CaptureKind::Never, captures.get(param_reg2));
        assert_eq!(CaptureKind::Always, captures.get(param_reg3));

        assert_eq!(CaptureKind::Never, captures.get(param_reg4));
        assert_eq!(CaptureKind::ViaRet, captures.get(param_reg5));
        assert_eq!(CaptureKind::Always, captures.get(param_reg6));

        assert_eq!(CaptureKind::Never, captures.get(unused_reg));
        assert_eq!(CaptureKind::ViaRet, captures.get(ret_reg));
    }

    #[test]
    fn capture_param_via_cond() {
        let param_reg = ops::RegId::alloc();
        let ret_reg = ops::RegId::alloc();

        let test_fun = ops::Fun {
            span: EMPTY_SPAN,
            source_name: None,

            abi: ops::OpsABI {
                call_conv: ops::CallConv::FreeFunction,
                params: Box::new([boxed::TypeTag::Int.into()]),
                ret: boxed::TypeTag::TopPair.into(),
            },
            params: Box::new([param_reg]),
            ops: Box::new([
                ops::OpKind::Cond(ops::CondOp {
                    reg_phi: Some(ops::RegPhi {
                        output_reg: ret_reg,
                        true_result_reg: param_reg,
                        false_result_reg: param_reg,
                    }),
                    test_reg: param_reg,
                    true_ops: Box::new([]),
                    false_ops: Box::new([]),
                })
                .into(),
                ops::OpKind::Ret(ret_reg).into(),
            ]),
        };

        let captures = calc_fun_captures(&[], &test_fun);
        assert_eq!(CaptureKind::ViaRet, captures.get(param_reg));
        assert_eq!(CaptureKind::ViaRet, captures.get(ret_reg));
    }
}
