use std::ffi;
use std::rc::Rc;

use syntax::span::{Span, EMPTY_SPAN};

use runtime::abitype;
use runtime::boxed;

use crate::codegen;
use crate::hir;
use crate::mir::builder::Builder;
use crate::mir::ops;
use crate::mir::value;
use crate::mir::value::Value;

fn has_thunk_abi(rust_fun: &hir::rfi::Fun) -> bool {
    if !rust_fun.takes_task() || !rust_fun.has_rest() || rust_fun.params().len() != 1 {
        // Args not compatible
        false
    } else {
        match rust_fun.ret() {
            abitype::RetABIType::Never => true,
            abitype::RetABIType::Inhabited(abitype::ABIType::Boxed(_)) => true,
            _ => false,
        }
    }
}

pub fn build_rust_fun_app(
    b: &mut Builder,
    span: Span,
    rust_fun: &hir::rfi::Fun,
    fixed_values: &[Value],
    rest_value: Option<&Value>,
) -> Value {
    use crate::mir::ops::*;
    use crate::mir::value::list::ListIterator;
    use crate::mir::value::to_reg::value_to_reg;
    use runtime::abitype::{BoxedABIType, RetABIType};

    let mut list_iter = ListIterator::new(fixed_values.to_owned(), rest_value.cloned());
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
        let reg_id = value_to_reg(b, span, &fixed_value, abi_type);
        arg_regs.push(reg_id);
    }

    if rust_fun.has_rest() {
        let reg_id = value_to_reg(
            b,
            span,
            &list_iter.into_rest(),
            &BoxedABIType::List(&BoxedABIType::Any).into(),
        );
        arg_regs.push(reg_id);
    };

    let abi = EntryPointABI {
        takes_task: rust_fun.takes_task(),
        params: rust_fun.params().to_owned().into(),
        ret: rust_fun.ret().clone(),
    };

    let fun_reg = b.push_reg(
        span,
        OpKind::ConstEntryPoint,
        ConstEntryPointOp {
            symbol: rust_fun.symbol(),
            abi: abi,
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
        RetABIType::Never => Value::Divergent,
        RetABIType::Inhabited(abi_type) => {
            let reg_value = value::RegValue {
                reg: ret_reg,
                abi_type: abi_type.clone(),
            };

            Value::Reg(Rc::new(reg_value))
        }
    }
}

pub fn build_rust_fun_thunk(span: Span, rust_fun: &hir::rfi::Fun) -> ops::Fun {
    use crate::mir::ops::*;
    use crate::mir::value::to_reg::value_to_reg;

    let mut b = Builder::new();

    let rest_abi_type: abitype::ABIType = abitype::TOP_LIST_BOXED_ABI_TYPE.into();
    let ret_abi_type: abitype::ABIType = abitype::BoxedABIType::Any.into();

    let rest_reg = b.alloc_reg();
    let rest_value = Value::Reg(Rc::new(value::RegValue {
        reg: rest_reg,
        abi_type: rest_abi_type.clone(),
    }));

    let return_value = build_rust_fun_app(&mut b, span, rust_fun, &[], Some(&rest_value));

    if !return_value.is_divergent() {
        let return_reg = value_to_reg(&mut b, span, &return_value, &ret_abi_type);
        b.push(span, OpKind::Ret(return_reg))
    }

    ops::Fun {
        abi: ops::EntryPointABI {
            takes_task: true,
            params: Box::new([rest_abi_type]),
            ret: abitype::BoxedABIType::Any.into(),
        },
        params: vec![rest_reg],
        ops: b.into_ops(),
    }
}

pub fn jit_thunk_for_rust_fun(
    cgx: &mut codegen::CodegenCtx,
    jcx: &mut codegen::jit::JITCtx,
    rust_fun: &hir::rfi::Fun,
) -> boxed::ThunkEntry {
    unsafe {
        use std::mem;

        if has_thunk_abi(rust_fun) {
            // We can skip building the thunk
            return mem::transmute(rust_fun.entry_point());
        }

        // Create some names
        let inner_symbol = ffi::CString::new(rust_fun.symbol()).unwrap();
        let outer_symbol = ffi::CString::new(format!("{}_thunk", rust_fun.symbol())).unwrap();

        // Add the inner symbol
        jcx.add_symbol(
            inner_symbol.as_bytes_with_nul(),
            rust_fun.entry_point() as u64,
        );

        let ops_fun = build_rust_fun_thunk(EMPTY_SPAN, rust_fun);
        let address = jcx.compile_fun(cgx, &outer_symbol, &ops_fun);

        mem::transmute(address)
    }
}
