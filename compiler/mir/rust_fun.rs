use std::rc::Rc;

use syntax::span::Span;

use runtime::abitype;

use crate::codegen::GenABI;
use crate::hir;
use crate::mir::builder::Builder;
use crate::mir::eval_hir::EvalHirCtx;
use crate::mir::ops;
use crate::mir::value;
use crate::mir::value::Value;
use crate::ty;
use crate::ty::purity;

pub fn build_rust_fun_app(
    ehx: &mut EvalHirCtx,
    b: &mut Builder,
    span: Span,
    ret_ty: &ty::Poly,
    rust_fun: &hir::rfi::Fun,
    arg_list_value: Value,
) -> Value {
    use crate::mir::ops::*;
    use crate::mir::value::build_reg::value_to_reg;
    use crate::mir::value::from_reg::reg_to_value;
    use runtime::abitype::{BoxedABIType, RetABIType};

    let mut list_iter = arg_list_value.into_list_iter();
    let mut arg_regs = vec![];

    let mut rust_fixed_iter = rust_fun.params().iter();
    if rust_fun.has_rest() {
        rust_fixed_iter.next_back();
    }

    for param_abi_type in rust_fixed_iter {
        let fixed_value = list_iter.next_unchecked(b, span);
        let reg_id = value_to_reg(ehx, b, span, &fixed_value, &param_abi_type.abi_type);
        arg_regs.push(reg_id.into());
    }

    if rust_fun.has_rest() {
        let reg_id = value_to_reg(
            ehx,
            b,
            span,
            &list_iter.into_rest(),
            &BoxedABIType::List(&BoxedABIType::Any).into(),
        );
        arg_regs.push(reg_id.into());
    };

    // TODO: Fix for polymorphism once it's supported
    // This will need to be split in to `always_impure` for the Symbol and a `call_impure` for this call site
    let impure = (rust_fun.arret_fun_type().purity() != &purity::Purity::Pure.into_poly())
        || rust_fun.arret_fun_type().ret() == &ty::Ty::never().into_poly();

    let abi = GenABI {
        takes_task: rust_fun.takes_task(),
        takes_closure: false,
        params: rust_fun.params().to_owned().into(),
        ret: rust_fun.ret().clone(),
    };

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

pub fn ops_for_rust_fun_thunk(
    ehx: &mut EvalHirCtx,
    span: Span,
    rust_fun: &hir::rfi::Fun,
) -> ops::Fun {
    use crate::mir::ops::*;
    use crate::mir::optimise::optimise_fun;
    use crate::mir::value::build_reg::value_to_reg;

    let mut b = Builder::new();
    let fun_symbol = format!("{}_thunk", rust_fun.symbol());

    let rest_abi_type: abitype::ABIType = abitype::TOP_LIST_BOXED_ABI_TYPE.into();
    let ret_abi_type: abitype::ABIType = abitype::BoxedABIType::Any.into();

    let rest_reg = b.alloc_reg();
    let rest_value = Value::Reg(Rc::new(value::RegValue {
        reg: rest_reg,
        abi_type: rest_abi_type.clone(),
    }));

    let ret_ty = ty::Ty::Any.into_poly();
    let return_value = build_rust_fun_app(ehx, &mut b, span, &ret_ty, rust_fun, rest_value);

    if !return_value.is_divergent() {
        let return_reg = value_to_reg(ehx, &mut b, span, &return_value, &ret_abi_type);
        b.push(span, OpKind::Ret(return_reg.into()))
    }

    optimise_fun(ops::Fun {
        source_name: Some(fun_symbol),
        abi: ops::OpsABI::thunk_abi(),
        params: Box::new([rest_reg]),
        ops: b.into_ops(),
    })
}
