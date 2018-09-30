use std::rc::Rc;

use syntax::span::Span;

use runtime::abitype;

use crate::hir;
use crate::mir::builder::Builder;
use crate::mir::eval_hir::EvalHirCtx;
use crate::mir::ops;
use crate::mir::value;
use crate::mir::value::Value;

pub fn build_rust_fun_app(
    ehx: &mut EvalHirCtx,
    b: &mut Builder,
    span: Span,
    rust_fun: &hir::rfi::Fun,
    arg_list_value: Value,
) -> Value {
    use crate::mir::ops::*;
    use crate::mir::value::to_reg::value_to_reg;
    use runtime::abitype::{BoxedABIType, RetABIType};

    let mut list_iter = arg_list_value.into_list_iter();
    let mut arg_regs = vec![];

    if rust_fun.takes_task() {
        arg_regs.push(b.push_reg(span, OpKind::CurrentTask, ()));
    }

    let mut rust_fixed_iter = rust_fun.params().iter();
    if rust_fun.has_rest() {
        rust_fixed_iter.next_back();
    }

    for abi_type in rust_fixed_iter {
        let fixed_value = list_iter.next_unchecked(b, span);
        let reg_id = value_to_reg(ehx, b, span, &fixed_value, abi_type);
        arg_regs.push(reg_id);
    }

    if rust_fun.has_rest() {
        let reg_id = value_to_reg(
            ehx,
            b,
            span,
            &list_iter.into_rest(),
            &BoxedABIType::List(&BoxedABIType::Any).into(),
        );
        arg_regs.push(reg_id);
    };

    let abi = FunABI {
        takes_task: rust_fun.takes_task(),
        takes_closure: false,
        params: rust_fun.params().to_owned().into(),
        ret: rust_fun.ret().clone(),
    };

    let fun_reg = b.push_reg(
        span,
        OpKind::ConstEntryPoint,
        ConstEntryPointOp {
            symbol: rust_fun.symbol(),
            abi,
        },
    );

    let ret_reg = b.push_reg(
        span,
        OpKind::Call,
        CallOp {
            fun_reg,
            args: arg_regs.into_boxed_slice(),
        },
    );

    match rust_fun.ret() {
        RetABIType::Void => Value::List(Box::new([]), None),
        RetABIType::Never => {
            b.push(span, OpKind::Unreachable);
            Value::Divergent
        }
        RetABIType::Inhabited(abi_type) => {
            let reg_value = value::RegValue {
                reg: ret_reg,
                abi_type: abi_type.clone(),
            };

            Value::Reg(Rc::new(reg_value))
        }
    }
}

pub fn ops_for_rust_fun_thunk(
    ehx: &mut EvalHirCtx,
    span: Span,
    rust_fun: &hir::rfi::Fun,
) -> ops::Fun {
    use crate::mir::ops::*;
    use crate::mir::value::to_reg::value_to_reg;

    let mut b = Builder::new();
    let fun_symbol = format!("{}_thunk", rust_fun.symbol());

    let rest_abi_type: abitype::ABIType = abitype::TOP_LIST_BOXED_ABI_TYPE.into();
    let ret_abi_type: abitype::ABIType = abitype::BoxedABIType::Any.into();

    let rest_reg = b.alloc_reg();
    let rest_value = Value::Reg(Rc::new(value::RegValue {
        reg: rest_reg,
        abi_type: rest_abi_type.clone(),
    }));

    let empty_list_value = Value::List(Box::new([]), Some(Box::new(rest_value)));
    let return_value = build_rust_fun_app(ehx, &mut b, span, rust_fun, empty_list_value);

    if !return_value.is_divergent() {
        let return_reg = value_to_reg(ehx, &mut b, span, &return_value, &ret_abi_type);
        b.push(span, OpKind::Ret(return_reg))
    }

    ops::Fun {
        source_name: Some(fun_symbol),
        abi: ops::FunABI::thunk_abi(),
        params: vec![rest_reg],
        ops: b.into_ops(),
    }
}
