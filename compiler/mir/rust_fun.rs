use arret_syntax::span::Span;

use crate::codegen::GenABI;
use crate::mir::builder::Builder;
use crate::mir::error::{Error, Result};
use crate::mir::eval_hir::EvalHirCtx;
use crate::mir::ops;
use crate::mir::polymorph::PolymorphABI;
use crate::mir::value::Value;
use crate::rfi;
use crate::ty;
use crate::ty::purity::Purity;
use crate::ty::Ty;

/// Returns the upper bound on the purity for a Rust fun
pub fn rust_fun_purity_upper_bound(rust_fun: &rfi::Fun) -> Purity {
    let arret_fun_type = rust_fun.arret_fun_type();

    if arret_fun_type.ret().is_never() {
        Purity::Impure
    } else if arret_fun_type.purity() == &Purity::Pure.into() {
        Purity::Pure
    } else {
        Purity::Impure
    }
}

pub fn build_rust_fun_app(
    ehx: &mut EvalHirCtx,
    b: &mut Builder,
    span: Span,
    ret_ty: &ty::Ref<ty::Mono>,
    rust_fun: &rfi::Fun,
    call_purity: Purity,
    arg_list_value: Value,
) -> Result<Value> {
    use crate::mir::arg_list::build_save_arg_list_to_regs;
    use crate::mir::ops::*;
    use crate::mir::value::from_reg::reg_to_value;
    use arret_runtime::abitype::RetABIType;

    let mut arg_abi_types = rust_fun
        .params()
        .iter()
        .map(|param_abi_type| &param_abi_type.abi_type);

    let rest_abi_type = if rust_fun.has_rest() {
        arg_abi_types.next_back()
    } else {
        None
    };

    let arg_regs =
        build_save_arg_list_to_regs(ehx, b, span, arg_list_value, arg_abi_types, rest_abi_type);

    let purity_upper_bound = rust_fun_purity_upper_bound(rust_fun);

    let abi = GenABI {
        takes_task: rust_fun.takes_task(),
        params: rust_fun.params().to_owned().into(),
        ret: rust_fun.ret().clone(),
    };

    ehx.register_rust_fun_with_jit(rust_fun);
    let callee = ops::Callee::StaticSymbol(ops::StaticSymbol {
        symbol: rust_fun.symbol(),
        impure: purity_upper_bound == Purity::Impure,
        abi,
    });

    let ret_reg = b.push_reg(
        span,
        OpKind::Call,
        CallOp {
            callee,
            impure: call_purity == Purity::Impure,
            args: arg_regs.into_boxed_slice(),
        },
    );

    match rust_fun.ret() {
        RetABIType::Void => Ok(Value::List(Box::new([]), None)),
        RetABIType::Never => {
            b.push(span, OpKind::Unreachable);
            Err(Error::Diverged)
        }
        RetABIType::Inhabited(abi_type) => Ok(reg_to_value(ehx, ret_reg, abi_type, ret_ty)),
    }
}

pub fn ops_for_rust_fun(
    ehx: &mut EvalHirCtx,
    rust_fun: &rfi::Fun,
    wanted_abi: PolymorphABI,
) -> ops::Fun {
    use crate::mir::arg_list::{build_load_arg_list_value, LoadedArgList};
    use crate::mir::optimise::optimise_fun;
    use crate::mir::ret_value::build_value_ret;

    let mut b = Builder::new();
    let span = rust_fun.span();

    let fun_symbol = format!("{}_adapter", rust_fun.symbol());

    let LoadedArgList {
        param_regs,
        arg_list_value,
        ..
    } = build_load_arg_list_value(ehx, &mut b, &wanted_abi, rust_fun.arret_fun_type().params());

    let purity_upper_bound = rust_fun_purity_upper_bound(rust_fun);
    let ret_ty = Ty::Any.into();
    let app_result = build_rust_fun_app(
        ehx,
        &mut b,
        span,
        &ret_ty,
        rust_fun,
        purity_upper_bound,
        arg_list_value,
    );

    build_value_ret(ehx, &mut b, span, app_result, &wanted_abi.ret);

    optimise_fun(ops::Fun {
        span,
        source_name: Some(fun_symbol.into()),

        abi: wanted_abi.into(),
        param_regs,
        ops: b.into_ops(),
    })
}
