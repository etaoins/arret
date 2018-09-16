use std::collections::HashMap;
use std::ffi::CString;
use std::mem;

use llvm_sys::core::*;
use llvm_sys::prelude::*;

use runtime::boxed;

use crate::codegen::mod_gen::ModCtx;
use crate::codegen::CodegenCtx;
use crate::mir::ops::*;

pub(crate) struct FunCtx {
    pub builder: LLVMBuilderRef,
    regs: HashMap<RegId, LLVMValueRef>,
}

impl FunCtx {
    pub(crate) fn new(builder: LLVMBuilderRef) -> FunCtx {
        FunCtx {
            builder,
            regs: HashMap::new(),
        }
    }
}

fn push_box_header(
    cgx: &mut CodegenCtx,
    members: &mut Vec<LLVMValueRef>,
    type_tag: boxed::TypeTag,
) {
    unsafe {
        let llvm_i8 = LLVMInt8TypeInContext(cgx.llx);

        members.push(LLVMConstInt(llvm_i8, type_tag as u64, 0));
        members.push(LLVMConstInt(llvm_i8, 0, 0));
    }
}

pub(crate) fn gen_const_boxed_pair(
    cgx: &mut CodegenCtx,
    mcx: &mut ModCtx,
    llvm_car: LLVMValueRef,
    llvm_cdr: LLVMValueRef,
    list_length: usize,
) -> LLVMValueRef {
    unsafe {
        let type_tag = boxed::TypeTag::TopPair;
        let llvm_type = cgx.boxed_abi_to_llvm_struct_type(&type_tag.into());
        let llvm_i64 = LLVMInt64TypeInContext(cgx.llx);

        let mut members = vec![];
        push_box_header(cgx, &mut members, type_tag);
        members.push(llvm_car);
        members.push(llvm_cdr);
        members.push(LLVMConstInt(llvm_i64, list_length as u64, 0));

        let llvm_value =
            LLVMConstNamedStruct(llvm_type, members.as_mut_ptr(), members.len() as u32);

        let global = LLVMAddGlobal(mcx.module, llvm_type, "const_pair\0".as_ptr() as *const _);
        LLVMSetInitializer(global, llvm_value);
        LLVMSetAlignment(global, mem::align_of::<boxed::TopPair>() as u32);

        global
    }
}

pub(crate) fn gen_const_boxed_inline_str(
    cgx: &mut CodegenCtx,
    mcx: &mut ModCtx,
    value: &str,
) -> LLVMValueRef {
    unsafe {
        const MAX_INLINE_BYTES: usize = boxed::Str::MAX_INLINE_BYTES;

        let mut inline_buffer: [u8; MAX_INLINE_BYTES] = [0; MAX_INLINE_BYTES];
        inline_buffer[0..value.len()].copy_from_slice(value.as_bytes());

        let type_tag = boxed::TypeTag::Str;
        let inline_llvm_type = cgx.boxed_inline_str_llvm_type();
        let llvm_i8 = LLVMInt8TypeInContext(cgx.llx);

        let mut members = vec![];
        push_box_header(cgx, &mut members, type_tag);
        members.push(LLVMConstInt(llvm_i8, value.len() as u64, 0));
        members.push(LLVMConstString(
            inline_buffer.as_mut_ptr() as *mut _,
            MAX_INLINE_BYTES as u32,
            1,
        ));

        let inline_llvm_value =
            LLVMConstNamedStruct(inline_llvm_type, members.as_mut_ptr(), members.len() as u32);

        let global = LLVMAddGlobal(
            mcx.module,
            inline_llvm_type,
            "const_str\0".as_ptr() as *const _,
        );
        LLVMSetInitializer(global, inline_llvm_value);
        LLVMSetAlignment(global, mem::align_of::<boxed::Str>() as u32);

        let llvm_type = cgx.boxed_abi_to_llvm_struct_type(&type_tag.into());
        LLVMConstBitCast(global, LLVMPointerType(llvm_type, 0))
    }
}

pub(crate) fn gen_op(cgx: &mut CodegenCtx, mcx: &mut ModCtx, fcx: &mut FunCtx, op: &Op) {
    unsafe {
        match &op.kind {
            OpKind::CurrentTask(reg, _) => {
                // TODO: Properly track our current tasl
                let llvm_type = cgx.task_llvm_type();
                let llvm_value = LLVMConstNull(llvm_type);

                fcx.regs.insert(*reg, llvm_value);
            }
            OpKind::ConstNil(reg, _) => {
                let llvm_value = cgx.ptr_to_singleton_box(
                    mcx.module,
                    &boxed::TypeTag::Nil.into(),
                    b"ARRET_NIL\0",
                );
                fcx.regs.insert(*reg, llvm_value);
            }
            OpKind::ConstInt(reg, value) => {
                let llvm_val = LLVMConstInt(LLVMInt64TypeInContext(cgx.llx), *value as u64, 1);
                fcx.regs.insert(*reg, llvm_val);
            }
            OpKind::ConstBoxedInt(reg, value) => {
                let type_tag = boxed::TypeTag::Int;
                let llvm_type = cgx.boxed_abi_to_llvm_struct_type(&type_tag.into());
                let llvm_i64 = LLVMInt64TypeInContext(cgx.llx);

                let box_name = CString::new(format!("const_int_{}", value)).unwrap();

                let llvm_value = mcx.get_global_or_insert(llvm_type, &box_name, || {
                    let mut members = vec![];
                    push_box_header(cgx, &mut members, type_tag);
                    members.push(LLVMConstInt(llvm_i64, *value as u64, 1));

                    LLVMConstNamedStruct(llvm_type, members.as_mut_ptr(), members.len() as u32)
                });

                LLVMSetAlignment(llvm_value, mem::align_of::<boxed::Int>() as u32);

                fcx.regs.insert(*reg, llvm_value);
            }
            OpKind::ConstBoxedPair(
                reg,
                ConstBoxedPairOp {
                    car_reg,
                    cdr_reg,
                    length,
                },
            ) => {
                let llvm_car = fcx.regs[car_reg];
                let llvm_cdr = fcx.regs[cdr_reg];

                let llvm_value = gen_const_boxed_pair(cgx, mcx, llvm_car, llvm_cdr, *length);
                fcx.regs.insert(*reg, llvm_value);
            }
            OpKind::ConstBoxedStr(reg, value) => {
                if value.len() > boxed::Str::MAX_INLINE_BYTES {
                    unimplemented!("Non-inline strings");
                }

                let llvm_value = gen_const_boxed_inline_str(cgx, mcx, value.as_ref());
                fcx.regs.insert(*reg, llvm_value);
            }
            OpKind::CastBoxed(reg, CastBoxedOp { from_reg, to_type }) => {
                let from_llvm_value = fcx.regs[from_reg];

                let to_llvm_type = cgx.boxed_abi_to_llvm_ptr_type(to_type);
                let to_llvm_value = LLVMConstBitCast(from_llvm_value, to_llvm_type);
                fcx.regs.insert(*reg, to_llvm_value);
            }
            OpKind::ConstEntryPoint(reg, ConstEntryPointOp { symbol, abi }) => {
                let function_type =
                    cgx.function_to_llvm_type(abi.takes_task, &abi.params, &abi.ret);

                let function = LLVMAddGlobal(
                    mcx.module,
                    function_type,
                    CString::new(*symbol).unwrap().as_bytes_with_nul().as_ptr() as *const _,
                );

                fcx.regs.insert(*reg, function);
            }
            OpKind::Call(reg, CallOp { fun_reg, args }) => {
                let llvm_fun = fcx.regs[&fun_reg];
                let mut llvm_args = args
                    .iter()
                    .map(|param_reg| fcx.regs[&param_reg])
                    .collect::<Vec<LLVMValueRef>>();

                let llvm_ret = LLVMBuildCall(
                    fcx.builder,
                    llvm_fun,
                    llvm_args.as_mut_ptr(),
                    llvm_args.len() as u32,
                    b"\0".as_ptr() as *const _,
                );

                fcx.regs.insert(*reg, llvm_ret);
            }
        }
    }
}
