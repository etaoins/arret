use std::collections::HashMap;
use std::ffi::CString;
use std::mem;

use llvm_sys::core::*;
use llvm_sys::prelude::*;

use runtime::boxed;

use crate::codegen::mod_gen::ModCtx;
use crate::codegen::CodegenCtx;
use crate::mir::ops::*;

struct FunCtx {
    function: LLVMValueRef,
    builder: LLVMBuilderRef,

    regs: HashMap<RegId, LLVMValueRef>,
    current_task: Option<LLVMValueRef>,
}

impl FunCtx {
    pub(crate) fn new(function: LLVMValueRef, builder: LLVMBuilderRef) -> FunCtx {
        FunCtx {
            function,
            builder,
            regs: HashMap::new(),
            current_task: None,
        }
    }
}

impl Drop for FunCtx {
    fn drop(&mut self) {
        unsafe {
            LLVMDisposeBuilder(self.builder);
        }
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

        let members = &mut [
            cgx.const_box_header(type_tag),
            llvm_car,
            llvm_cdr,
            LLVMConstInt(llvm_i64, list_length as u64, 0),
        ];

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

        let members = &mut [
            cgx.const_box_header(type_tag),
            LLVMConstInt(llvm_i8, value.len() as u64, 0),
            LLVMConstString(
                inline_buffer.as_mut_ptr() as *mut _,
                MAX_INLINE_BYTES as u32,
                1,
            ),
        ];

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

fn gen_cond(
    cgx: &mut CodegenCtx,
    mcx: &mut ModCtx,
    fcx: &mut FunCtx,
    cond_op: &CondOp,
) -> LLVMValueRef {
    let CondOp {
        test_reg,
        true_ops,
        true_result_reg,
        false_ops,
        false_result_reg,
    } = cond_op;

    unsafe {
        let previous_block = LLVMGetInsertBlock(fcx.builder);

        let mut true_block = LLVMAppendBasicBlockInContext(
            cgx.llx,
            fcx.function,
            b"cond_true\0".as_ptr() as *const _,
        );
        let mut false_block = LLVMAppendBasicBlockInContext(
            cgx.llx,
            fcx.function,
            b"cond_false\0".as_ptr() as *const _,
        );
        let cont_block = LLVMAppendBasicBlockInContext(
            cgx.llx,
            fcx.function,
            b"cond_cont\0".as_ptr() as *const _,
        );

        for (block, ops) in &[(true_block, true_ops), (false_block, false_ops)] {
            LLVMPositionBuilderAtEnd(fcx.builder, *block);
            for op in ops.iter() {
                gen_op(cgx, mcx, fcx, op);
            }
            LLVMBuildBr(fcx.builder, cont_block);
        }
        let mut true_result_llvm = fcx.regs[true_result_reg];
        let mut false_result_llvm = fcx.regs[false_result_reg];

        let test_llvm = fcx.regs[test_reg];
        LLVMPositionBuilderAtEnd(fcx.builder, previous_block);
        LLVMBuildCondBr(fcx.builder, test_llvm, true_block, false_block);

        LLVMPositionBuilderAtEnd(fcx.builder, cont_block);
        let phi_value = LLVMBuildPhi(
            fcx.builder,
            LLVMTypeOf(true_result_llvm),
            b"cond_phi\0".as_ptr() as *const _,
        );

        LLVMAddIncoming(
            phi_value,
            &mut true_result_llvm as *mut _,
            &mut true_block as *mut _,
            1,
        );
        LLVMAddIncoming(
            phi_value,
            &mut false_result_llvm as *mut _,
            &mut false_block as *mut _,
            1,
        );

        phi_value
    }
}

fn gen_op(cgx: &mut CodegenCtx, mcx: &mut ModCtx, fcx: &mut FunCtx, op: &Op) {
    unsafe {
        match &op.kind {
            OpKind::CurrentTask(reg, _) => {
                fcx.regs.insert(
                    *reg,
                    fcx.current_task
                        .expect("attempted to get current task in function without task param"),
                );
            }
            OpKind::ConstNil(reg, _) => {
                let llvm_value = cgx.ptr_to_singleton_box(
                    mcx.module,
                    &boxed::TypeTag::Nil.into(),
                    b"ARRET_NIL\0",
                );
                fcx.regs.insert(*reg, llvm_value);
            }
            OpKind::ConstTrue(reg, _) => {
                let llvm_value = cgx.ptr_to_singleton_box(
                    mcx.module,
                    &boxed::TypeTag::True.into(),
                    b"ARRET_TRUE\0",
                );
                fcx.regs.insert(*reg, llvm_value);
            }
            OpKind::ConstFalse(reg, _) => {
                let llvm_value = cgx.ptr_to_singleton_box(
                    mcx.module,
                    &boxed::TypeTag::False.into(),
                    b"ARRET_FALSE\0",
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
                    let members = &mut [
                        cgx.const_box_header(type_tag),
                        LLVMConstInt(llvm_i64, *value as u64, 1),
                    ];

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

                let to_llvm_value = LLVMBuildBitCast(
                    fcx.builder,
                    from_llvm_value,
                    to_llvm_type,
                    "box_bitcast\0".as_ptr() as *const _,
                );
                fcx.regs.insert(*reg, to_llvm_value);
            }
            OpKind::ConstEntryPoint(reg, ConstEntryPointOp { symbol, abi }) => {
                let function_type =
                    cgx.function_to_llvm_type(abi.takes_task, &abi.params, &abi.ret);
                let function_name = CString::new(*symbol).unwrap();

                let global = LLVMGetNamedGlobal(
                    mcx.module,
                    function_name.as_bytes_with_nul().as_ptr() as *const _,
                );

                let function = if global.is_null() {
                    LLVMAddGlobal(
                        mcx.module,
                        function_type,
                        function_name.as_bytes_with_nul().as_ptr() as *const _,
                    )
                } else {
                    global
                };

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
            OpKind::Ret(reg) => {
                let llvm_value = fcx.regs[reg];
                LLVMBuildRet(fcx.builder, llvm_value);
            }
            OpKind::RetVoid => {
                LLVMBuildRetVoid(fcx.builder);
            }
            OpKind::LoadBoxedPairHead(reg, pair_reg) => {
                let llvm_pair = fcx.regs[pair_reg];
                let head_ptr = LLVMBuildStructGEP(
                    fcx.builder,
                    llvm_pair,
                    1,
                    b"head_ptr\0".as_ptr() as *const _,
                );

                let llvm_head = LLVMBuildLoad(fcx.builder, head_ptr, "head\0".as_ptr() as *const _);
                fcx.regs.insert(*reg, llvm_head);
            }
            OpKind::LoadBoxedPairRest(reg, pair_reg) => {
                let llvm_pair = fcx.regs[pair_reg];
                let head_ptr = LLVMBuildStructGEP(
                    fcx.builder,
                    llvm_pair,
                    2,
                    b"rest_ptr\0".as_ptr() as *const _,
                );

                let llvm_rest = LLVMBuildLoad(fcx.builder, head_ptr, "rest\0".as_ptr() as *const _);
                fcx.regs.insert(*reg, llvm_rest);
            }
            OpKind::LoadBoxedIntValue(reg, boxed_int_reg) => {
                let llvm_boxed_int = fcx.regs[boxed_int_reg];
                let head_ptr = LLVMBuildStructGEP(
                    fcx.builder,
                    llvm_boxed_int,
                    1,
                    b"int_value_ptr\0".as_ptr() as *const _,
                );

                let llvm_value =
                    LLVMBuildLoad(fcx.builder, head_ptr, "int_value\0".as_ptr() as *const _);
                fcx.regs.insert(*reg, llvm_value);
            }
            OpKind::Cond(reg, cond_op) => {
                let llvm_value = gen_cond(cgx, mcx, fcx, cond_op);
                fcx.regs.insert(*reg, llvm_value);
            }
        }
    }
}

pub(crate) fn gen_fun(
    cgx: &mut CodegenCtx,
    mcx: &mut ModCtx,
    outer_symbol: &CString,
    fun: &Fun,
) -> LLVMValueRef {
    unsafe {
        let builder = LLVMCreateBuilderInContext(cgx.llx);

        let function_type =
            cgx.function_to_llvm_type(fun.abi.takes_task, &fun.abi.params, &fun.abi.ret);

        let function =
            LLVMAddFunction(mcx.module, outer_symbol.as_ptr() as *const _, function_type);

        let bb = LLVMAppendBasicBlockInContext(cgx.llx, function, b"entry\0".as_ptr() as *const _);
        LLVMPositionBuilderAtEnd(builder, bb);

        let mut fcx = FunCtx::new(function, builder);

        if fun.abi.takes_task {
            fcx.current_task = Some(LLVMGetParam(function, 0));
        }

        for (offset, reg) in fun.params.iter().enumerate() {
            let llvm_offset = (offset + (fun.abi.takes_task as usize)) as u32;
            fcx.regs.insert(*reg, LLVMGetParam(function, llvm_offset));
        }

        for op in &fun.ops {
            gen_op(cgx, mcx, &mut fcx, &op);
        }

        function
    }
}
