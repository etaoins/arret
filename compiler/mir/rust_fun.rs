use syntax::span::Span;

use crate::codegen::GenABI;
use crate::hir;
use crate::mir::builder::Builder;
use crate::mir::eval_hir::EvalHirCtx;
use crate::mir::ops;
use crate::mir::value::Value;
use crate::ty;
use crate::ty::purity;

pub fn build_rust_fun_app(
    ehx: &mut EvalHirCtx,
    b: &mut Builder,
    span: Span,
    ret_ty: &ty::Mono,
    rust_fun: &hir::rfi::Fun,
    arg_list_value: Value,
) -> Value {
    use crate::mir::ops::*;
    use crate::mir::value::build_reg::value_to_reg;
    use crate::mir::value::from_reg::reg_to_value;
    use runtime::abitype::RetABIType;

    let mut list_iter = arg_list_value.into_list_iter();
    let mut arg_regs = vec![];

    let mut rust_fixed_iter = rust_fun.params().iter();

    let rest_abi_type = if rust_fun.has_rest() {
        rust_fixed_iter.next_back()
    } else {
        None
    };

    for param_abi_type in rust_fixed_iter {
        let fixed_value = list_iter.next_unchecked(b, span);
        let reg_id = value_to_reg(ehx, b, span, &fixed_value, &param_abi_type.abi_type);
        arg_regs.push(reg_id.into());
    }

    if let Some(rest_abi_type) = rest_abi_type {
        let reg_id = value_to_reg(
            ehx,
            b,
            span,
            &list_iter.into_rest(),
            &rest_abi_type.abi_type,
        );
        arg_regs.push(reg_id.into());
    };

    // TODO: Fix for polymorphism once it's supported
    // This will need to be split in to `always_impure` for the Symbol and a `call_impure` for this call site
    let impure = (rust_fun.arret_fun_type().purity() != &purity::Purity::Pure.into_poly())
        || rust_fun.arret_fun_type().ret() == &ty::Ty::never().into_poly();

    let abi = GenABI {
        takes_task: rust_fun.takes_task(),
        params: rust_fun.params().to_owned().into(),
        ret: rust_fun.ret().clone(),
    };

    ehx.register_rust_fun_with_jit(rust_fun);
    let callee = ops::Callee::StaticSymbol(ops::StaticSymbol {
        symbol: rust_fun.symbol(),
        impure,
        abi,
    });

    let ret_reg = b.push_reg(
        span,
        OpKind::Call,
        CallOp {
            callee,
            impure,
            args: arg_regs.into_boxed_slice(),
        },
    );

    match rust_fun.ret() {
        RetABIType::Void => Value::List(Box::new([]), None),
        RetABIType::Never => {
            b.push(span, OpKind::Unreachable);
            Value::Divergent
        }
        RetABIType::Inhabited(abi_type) => reg_to_value(ehx, ret_reg, abi_type, ret_ty),
    }
}

pub fn ops_for_rust_fun(
    ehx: &mut EvalHirCtx,
    span: Span,
    rust_fun: &hir::rfi::Fun,
    wanted_abi: ops::OpsABI,
    has_rest: bool,
) -> ops::Fun {
    use crate::mir::arg_list::{build_load_arg_list_value, LoadedArgList};
    use crate::mir::optimise::optimise_fun;
    use crate::mir::ret_value::build_ret_value;

    let mut b = Builder::new();
    let fun_symbol = format!("{}_adapter", rust_fun.symbol());

    let LoadedArgList {
        param_regs,
        arg_list_value,
        ..
    } = build_load_arg_list_value(&mut b, &wanted_abi, false, has_rest);

    let ret_ty = ty::Ty::Any.into_mono();
    let return_value = build_rust_fun_app(ehx, &mut b, span, &ret_ty, rust_fun, arg_list_value);

    build_ret_value(ehx, &mut b, span, &return_value, &wanted_abi.ret);

    optimise_fun(ops::Fun {
        span,
        source_name: Some(fun_symbol),

        abi: wanted_abi,
        params: param_regs,
        ops: b.into_ops(),
    })
}
