use std::collections::HashMap;

use syntax::span::Span;

use crate::codegen::GenABI;
use crate::hir;
use crate::mir::builder::Builder;
use crate::mir::eval_hir::EvalHirCtx;
use crate::mir::ops;
use crate::mir::value::Value;
use crate::ty;
use crate::ty::purity;
use crate::ty::purity::Purity;

/// Returns the purity for a Rust fun application
///
/// Rust funs cannot capture pvars so this only needs to look at the pvars from the apply
pub fn rust_fun_app_purity(
    apply_pvar_purities: &HashMap<purity::PVarId, purity::Poly>,
    rust_fun: &hir::rfi::Fun,
) -> Purity {
    use crate::ty::TyRef;
    let arret_fun_type = rust_fun.arret_fun_type();

    if arret_fun_type.ret().is_never() {
        // This is a hack for things like `panic`. Pure funs are allowed to panic but if they
        // return `(U)` they're likely only called to terminate the program. Without this `panic`
        // would be optimised away.
        return Purity::Impure;
    }

    match arret_fun_type.purity() {
        purity::Poly::Fixed(purity) => *purity,
        purity::Poly::Var(pvar_id) => {
            if let purity::Poly::Fixed(purity) = apply_pvar_purities
                .get(pvar_id)
                .expect("Unable to find PVar when monomorphising Rust fun apply")
            {
                *purity
            } else {
                panic!("found polymorphic purity during Rust fun apply");
            }
        }
    }
}

/// Returns the upper bound on the purity for a Rust fun
pub fn rust_fun_purity_upper_bound(rust_fun: &hir::rfi::Fun) -> Purity {
    use crate::ty::TyRef;
    let arret_fun_type = rust_fun.arret_fun_type();

    if arret_fun_type.ret().is_never() {
        Purity::Impure
    } else if arret_fun_type.purity() == &Purity::Pure.into_poly() {
        Purity::Pure
    } else {
        Purity::Impure
    }
}

pub fn build_rust_fun_app(
    ehx: &mut EvalHirCtx,
    b: &mut Builder,
    span: Span,
    ret_ty: &ty::Mono,
    rust_fun: &hir::rfi::Fun,
    call_purity: Purity,
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

    let purity_upper_bound = rust_fun_purity_upper_bound(rust_fun);
    let ret_ty = ty::Ty::Any.into_mono();
    let return_value = build_rust_fun_app(
        ehx,
        &mut b,
        span,
        &ret_ty,
        rust_fun,
        purity_upper_bound,
        arg_list_value,
    );

    build_ret_value(ehx, &mut b, span, &return_value, &wanted_abi.ret);

    optimise_fun(ops::Fun {
        span,
        source_name: Some(fun_symbol),

        abi: wanted_abi,
        params: param_regs,
        ops: b.into_ops(),
    })
}
