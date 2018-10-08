use std::collections::HashMap;
use std::ffi;

use llvm_sys::core::*;
use llvm_sys::prelude::*;

use runtime::boxed;

use crate::codegen::alloc;
use crate::codegen::const_gen;
use crate::codegen::escape_analysis::{CaptureKind, Captures};
use crate::codegen::mod_gen::ModCtx;
use crate::codegen::CodegenCtx;
use crate::mir::ops::*;

pub struct BuiltFun {
    pub llvm_value: LLVMValueRef,
    // This includes the task and closure
    pub param_captures: Box<[CaptureKind]>,
}

struct FunCtx<'captures> {
    regs: HashMap<RegId, LLVMValueRef>,
    current_task: Option<LLVMValueRef>,
    captures: &'captures Captures,

    function: LLVMValueRef,
    builder: LLVMBuilderRef,
}

impl<'captures> FunCtx<'captures> {
    pub(crate) fn new(
        captures: &'captures Captures,
        function: LLVMValueRef,
        builder: LLVMBuilderRef,
    ) -> FunCtx<'captures> {
        FunCtx {
            regs: HashMap::new(),
            current_task: None,
            captures,

            function,
            builder,
        }
    }
}

impl<'captures> Drop for FunCtx<'captures> {
    fn drop(&mut self) {
        unsafe {
            LLVMDisposeBuilder(self.builder);
        }
    }
}

fn gen_cond(
    cgx: &mut CodegenCtx,
    mcx: &mut ModCtx,
    fcx: &mut FunCtx<'_>,
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
            gen_op_sequence(cgx, mcx, fcx, ops);
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

fn gen_callee(
    cgx: &mut CodegenCtx,
    mcx: &mut ModCtx,
    fcx: &mut FunCtx<'_>,
    callee: &Callee,
) -> LLVMValueRef {
    use crate::codegen::callee_gen::*;

    match callee {
        Callee::BuiltFun(built_fun_id) => gen_built_fun_callee(mcx, *built_fun_id),
        Callee::BoxedFunThunk(fun_thunk_reg) => {
            let llvm_fun_thunk = fcx.regs[fun_thunk_reg];
            gen_boxed_fun_thunk_callee(fcx.builder, llvm_fun_thunk)
        }
        Callee::StaticSymbol(static_symbol) => gen_static_symbol_callee(cgx, mcx, static_symbol),
    }
}

fn gen_op(
    cgx: &mut CodegenCtx,
    mcx: &mut ModCtx,
    fcx: &mut FunCtx<'_>,
    active_alloc: &mut alloc::gen::ActiveAlloc,
    box_sources: &HashMap<RegId, alloc::BoxSource>,
    op: &Op,
) {
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
                let llvm_value =
                    cgx.ptr_to_singleton_box(mcx.module, boxed::TypeTag::Nil, b"ARRET_NIL\0");
                fcx.regs.insert(*reg, llvm_value);
            }
            OpKind::ConstTrue(reg, _) => {
                let llvm_value =
                    cgx.ptr_to_singleton_box(mcx.module, boxed::TypeTag::True, b"ARRET_TRUE\0");
                fcx.regs.insert(*reg, llvm_value);
            }
            OpKind::ConstFalse(reg, _) => {
                let llvm_value =
                    cgx.ptr_to_singleton_box(mcx.module, boxed::TypeTag::False, b"ARRET_FALSE\0");
                fcx.regs.insert(*reg, llvm_value);
            }
            OpKind::ConstInt(reg, value) => {
                let llvm_value = LLVMConstInt(LLVMInt64TypeInContext(cgx.llx), *value as u64, 1);
                fcx.regs.insert(*reg, llvm_value);
            }
            OpKind::ConstBoxedInt(reg, value) => {
                let llvm_value = const_gen::gen_boxed_int(cgx, mcx, *value);
                fcx.regs.insert(*reg, llvm_value);
            }
            OpKind::ConstBoxedFunThunk(reg, callee) => {
                let llvm_entry_point = gen_callee(cgx, mcx, fcx, callee);
                let llvm_value = const_gen::gen_boxed_fun_thunk(cgx, mcx, llvm_entry_point);
                fcx.regs.insert(*reg, llvm_value);
            }
            OpKind::ConstBoxedPair(
                reg,
                BoxPairOp {
                    head_reg,
                    rest_reg,
                    length_reg,
                },
            ) => {
                let llvm_head = fcx.regs[head_reg];
                let llvm_rest = fcx.regs[rest_reg];
                let llvm_length = fcx.regs[length_reg];

                let llvm_value =
                    const_gen::gen_boxed_pair(cgx, mcx, llvm_head, llvm_rest, llvm_length);
                fcx.regs.insert(*reg, llvm_value);
            }
            OpKind::ConstBoxedStr(reg, value) => {
                if value.len() > boxed::Str::MAX_INLINE_BYTES {
                    unimplemented!("Non-inline strings");
                }

                let llvm_value = const_gen::gen_boxed_inline_str(cgx, mcx, value.as_ref());
                fcx.regs.insert(*reg, llvm_value);
            }
            OpKind::ConstCastBoxed(reg, CastBoxedOp { from_reg, to_type }) => {
                let from_llvm_value = fcx.regs[from_reg];
                let to_llvm_type = cgx.boxed_abi_to_llvm_ptr_type(to_type);
                let to_llvm_value = LLVMConstBitCast(from_llvm_value, to_llvm_type);

                fcx.regs.insert(*reg, to_llvm_value);
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
            OpKind::Call(reg, CallOp { callee, args }) => {
                let llvm_fun = gen_callee(cgx, mcx, fcx, callee);

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
            OpKind::Unreachable => {
                LLVMBuildUnreachable(fcx.builder);
            }
            OpKind::LoadBoxedListLength(reg, list_reg) => {
                let llvm_list = fcx.regs[list_reg];
                let length_ptr = LLVMBuildStructGEP(
                    fcx.builder,
                    llvm_list,
                    1,
                    b"length_ptr\0".as_ptr() as *const _,
                );

                let llvm_head =
                    LLVMBuildLoad(fcx.builder, length_ptr, "length\0".as_ptr() as *const _);
                fcx.regs.insert(*reg, llvm_head);
            }
            OpKind::LoadBoxedPairHead(reg, pair_reg) => {
                let llvm_pair = fcx.regs[pair_reg];
                let head_ptr = LLVMBuildStructGEP(
                    fcx.builder,
                    llvm_pair,
                    2,
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
                    3,
                    b"rest_ptr\0".as_ptr() as *const _,
                );

                let llvm_rest = LLVMBuildLoad(fcx.builder, head_ptr, "rest\0".as_ptr() as *const _);
                fcx.regs.insert(*reg, llvm_rest);
            }
            OpKind::LoadBoxedIntValue(reg, boxed_int_reg) => {
                let llvm_boxed_int = fcx.regs[boxed_int_reg];
                let value_ptr = LLVMBuildStructGEP(
                    fcx.builder,
                    llvm_boxed_int,
                    1,
                    b"int_value_ptr\0".as_ptr() as *const _,
                );

                let llvm_value =
                    LLVMBuildLoad(fcx.builder, value_ptr, "int_value\0".as_ptr() as *const _);
                fcx.regs.insert(*reg, llvm_value);
            }
            OpKind::LoadBoxedFunThunkClosure(reg, boxed_fun_thunk_reg) => {
                let llvm_boxed_fun_thunk = fcx.regs[boxed_fun_thunk_reg];

                let closure_ptr = LLVMBuildStructGEP(
                    fcx.builder,
                    llvm_boxed_fun_thunk,
                    1,
                    b"boxed_fun_thunk_closure_ptr\0".as_ptr() as *const _,
                );
                let llvm_value = LLVMBuildLoad(
                    fcx.builder,
                    closure_ptr,
                    "boxed_fun_thunk_closure\0".as_ptr() as *const _,
                );

                fcx.regs.insert(*reg, llvm_value);
            }
            OpKind::Cond(reg, cond_op) => {
                let llvm_value = gen_cond(cgx, mcx, fcx, cond_op);
                fcx.regs.insert(*reg, llvm_value);
            }
            OpKind::AllocInt(reg, int_reg) => {
                let box_source = box_sources[reg];

                let llvm_int = fcx.regs[int_reg];
                let llvm_alloced =
                    alloc::gen::gen_alloc_int(cgx, fcx.builder, active_alloc, box_source, llvm_int);

                fcx.regs.insert(*reg, llvm_alloced);
            }
            OpKind::AllocBoxedPair(
                reg,
                BoxPairOp {
                    head_reg,
                    rest_reg,
                    length_reg,
                },
            ) => {
                let box_source = box_sources[reg];

                let input = alloc::gen::PairInput {
                    llvm_head: fcx.regs[head_reg],
                    llvm_rest: fcx.regs[rest_reg],
                    llvm_length: fcx.regs[length_reg],
                };

                let llvm_value = alloc::gen::gen_alloc_boxed_pair(
                    cgx,
                    fcx.builder,
                    active_alloc,
                    box_source,
                    &input,
                );
                fcx.regs.insert(*reg, llvm_value);
            }
            OpKind::Add(reg, BinaryOp { lhs_reg, rhs_reg }) => {
                let llvm_lhs = fcx.regs[lhs_reg];
                let llvm_rhs = fcx.regs[rhs_reg];

                let llvm_value = LLVMBuildNUWAdd(
                    fcx.builder,
                    llvm_lhs,
                    llvm_rhs,
                    "sum\0".as_ptr() as *const _,
                );
                fcx.regs.insert(*reg, llvm_value);
            }
        }
    }
}

fn gen_op_sequence(cgx: &mut CodegenCtx, mcx: &mut ModCtx, fcx: &mut FunCtx<'_>, ops: &[Op]) {
    use crate::codegen::alloc::plan::plan_allocs;
    let alloc_atoms = plan_allocs(fcx.captures, ops);

    for alloc_atom in alloc_atoms {
        let mut active_alloc = if let Some(llvm_task) = fcx.current_task {
            alloc::gen::gen_active_alloc_for_atom(cgx, mcx, fcx.builder, llvm_task, &alloc_atom)
        } else {
            alloc::gen::ActiveAlloc::empty()
        };

        for op in alloc_atom.ops() {
            gen_op(
                cgx,
                mcx,
                fcx,
                &mut active_alloc,
                alloc_atom.box_sources(),
                &op,
            );
        }

        assert!(
            active_alloc.is_empty(),
            "did not consume entire active heap allocation"
        );
    }
}

pub(crate) fn gen_fun(cgx: &mut CodegenCtx, mcx: &mut ModCtx, fun: &Fun) -> BuiltFun {
    use crate::codegen::escape_analysis;
    use runtime::abitype::{ABIType, RetABIType};

    let captures = escape_analysis::calc_fun_captures(mcx.built_funs(), fun);

    // Determine which params we captured
    let mut param_captures = vec![];
    if fun.abi.takes_task {
        param_captures.push(CaptureKind::Never);
    }
    if fun.abi.takes_closure {
        param_captures.push(CaptureKind::Never);
    }
    for param_reg in fun.params.iter() {
        param_captures.push(captures.get(*param_reg));
    }

    unsafe {
        let builder = LLVMCreateBuilderInContext(cgx.llx);

        let function_type = cgx.fun_abi_to_llvm_type(&fun.abi);

        let fun_symbol = fun
            .source_name
            .as_ref()
            .map(|source_name| ffi::CString::new(source_name.as_bytes()).unwrap())
            .unwrap_or_else(|| ffi::CString::new("anon_fun").unwrap());

        let function = LLVMAddFunction(mcx.module, fun_symbol.as_ptr() as *const _, function_type);

        let bb = LLVMAppendBasicBlockInContext(cgx.llx, function, b"entry\0".as_ptr() as *const _);
        LLVMPositionBuilderAtEnd(builder, bb);

        let mut fcx = FunCtx::new(&captures, function, builder);

        if fun.abi.takes_task {
            fcx.current_task = Some(LLVMGetParam(function, 0));
        }

        let params_offset = fun.abi.takes_task as usize + fun.abi.takes_closure as usize;
        for (param_index, reg) in fun.params.iter().enumerate() {
            let llvm_offset = (params_offset + param_index) as u32;
            fcx.regs.insert(*reg, LLVMGetParam(function, llvm_offset));
        }

        for (param_index, param_abi_type) in fun.abi.params.iter().enumerate() {
            if let ABIType::Boxed(_) = param_abi_type.abi_type {
                let no_capture = param_captures[params_offset + param_index] == CaptureKind::Never;

                cgx.add_boxed_param_attrs(
                    function,
                    (params_offset + param_index + 1) as u32,
                    no_capture,
                );
            }
        }

        if let RetABIType::Inhabited(ABIType::Boxed(_)) = fun.abi.ret {
            cgx.add_boxed_return_attrs(function);
        }

        gen_op_sequence(cgx, mcx, &mut fcx, fun.ops.as_ref());

        BuiltFun {
            llvm_value: function,
            param_captures: param_captures.into_boxed_slice(),
        }
    }
}
