use std::collections::HashMap;
use std::ffi;

use llvm_sys::core::*;
use llvm_sys::prelude::*;
use llvm_sys::LLVMIntPredicate;

use runtime::boxed;

use crate::codegen::analysis::escape::CaptureKind;
use crate::codegen::context::CodegenCtx;
use crate::codegen::mod_gen::ModCtx;
use crate::codegen::GenABI;
use crate::codegen::{alloc, const_gen};
use crate::mir::ops::*;

pub struct BuiltFun {
    pub llvm_value: LLVMValueRef,
    pub takes_task: bool,
    // This includes the closure
    pub param_captures: Box<[CaptureKind]>,
}

struct FunCtx {
    regs: HashMap<RegId, LLVMValueRef>,
    current_task: Option<LLVMValueRef>,

    function: LLVMValueRef,
    builder: LLVMBuilderRef,
}

impl FunCtx {
    pub(crate) fn new(function: LLVMValueRef, builder: LLVMBuilderRef) -> FunCtx {
        FunCtx {
            regs: HashMap::new(),
            current_task: None,

            function,
            builder,
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

fn gen_cond_branch(
    cgx: &mut CodegenCtx,
    mcx: &mut ModCtx,
    fcx: &mut FunCtx,
    block: LLVMBasicBlockRef,
    alloc_plan: &[alloc::AllocAtom<'_>],
    cont_block: LLVMBasicBlockRef,
) {
    unsafe {
        LLVMPositionBuilderAtEnd(fcx.builder, block);

        for alloc_atom in alloc_plan {
            gen_alloc_atom(cgx, mcx, fcx, alloc_atom);
        }

        // We can't branch if we terminated
        if alloc_plan
            .last()
            .and_then(|alloc_atom| alloc_atom.ops().last())
            .filter(|op| op.kind().is_terminator())
            .is_none()
        {
            LLVMBuildBr(fcx.builder, cont_block);
        }
    }
}

fn gen_cond(
    cgx: &mut CodegenCtx,
    mcx: &mut ModCtx,
    fcx: &mut FunCtx,
    cond_op: &CondOp,
    cond_alloc_plan: &alloc::CondPlan<'_>,
) {
    let CondOp {
        reg_phi, test_reg, ..
    } = cond_op;

    unsafe {
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

        let test_llvm = fcx.regs[test_reg];
        LLVMBuildCondBr(fcx.builder, test_llvm, true_block, false_block);

        gen_cond_branch(
            cgx,
            mcx,
            fcx,
            true_block,
            cond_alloc_plan.true_subplan(),
            cont_block,
        );
        gen_cond_branch(
            cgx,
            mcx,
            fcx,
            false_block,
            cond_alloc_plan.false_subplan(),
            cont_block,
        );
        LLVMPositionBuilderAtEnd(fcx.builder, cont_block);

        if let Some(RegPhi {
            output_reg,
            true_result_reg,
            false_result_reg,
        }) = reg_phi
        {
            let mut true_result_llvm = fcx.regs[true_result_reg];
            let mut false_result_llvm = fcx.regs[false_result_reg];

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

            fcx.regs.insert(*output_reg, phi_value);
        }
    }
}

fn gen_callee_entry_point(
    cgx: &mut CodegenCtx,
    mcx: &mut ModCtx,
    fcx: &mut FunCtx,
    callee: &Callee,
) -> LLVMValueRef {
    use crate::codegen::callee::*;

    match callee {
        Callee::BuiltFun(built_fun_id) => {
            gen_built_fun_entry_point(mcx.built_funs(), *built_fun_id)
        }
        Callee::BoxedFunThunk(fun_thunk_reg) => {
            let llvm_fun_thunk = fcx.regs[fun_thunk_reg];
            gen_boxed_fun_thunk_entry_point(fcx.builder, llvm_fun_thunk)
        }
        Callee::StaticSymbol(static_symbol) => {
            gen_static_symbol_entry_point(cgx, mcx, static_symbol)
        }
    }
}

fn gen_op(
    cgx: &mut CodegenCtx,
    mcx: &mut ModCtx,
    fcx: &mut FunCtx,
    alloc_atom: &alloc::AllocAtom<'_>,
    active_alloc: &mut alloc::ActiveAlloc,
    op_index: usize,
    op: &Op,
) {
    unsafe {
        match &op.kind {
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
            OpKind::ConstTypeTag(reg, type_tag) => {
                let llvm_value = LLVMConstInt(LLVMInt8TypeInContext(cgx.llx), *type_tag as u64, 1);
                fcx.regs.insert(*reg, llvm_value);
            }
            OpKind::ConstBoxedInt(reg, value) => {
                let llvm_value = const_gen::gen_boxed_int(cgx, mcx, *value);
                fcx.regs.insert(*reg, llvm_value);
            }
            OpKind::ConstBoxedFunThunk(reg, callee) => {
                let llvm_entry_point = gen_callee_entry_point(cgx, mcx, fcx, callee);
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
            OpKind::Call(reg, CallOp { callee, args, .. }) => {
                use crate::codegen::callee;

                let llvm_fun = gen_callee_entry_point(cgx, mcx, fcx, callee);
                let takes_task = callee::callee_takes_task(mcx.built_funs(), callee);

                let task_reg_iter = fcx.current_task.filter(|_| takes_task).into_iter();
                let mut llvm_args = task_reg_iter
                    .chain(args.iter().map(|param_reg| fcx.regs[&param_reg]))
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
            OpKind::LoadBoxedTypeTag(
                reg,
                LoadBoxedTypeTagOp {
                    subject_reg,
                    possible_type_tags,
                },
            ) => {
                use crate::codegen::range_md::int_range_metadata_node;

                let llvm_any = fcx.regs[subject_reg];
                let gep_indices = &mut [
                    LLVMConstInt(LLVMInt32TypeInContext(cgx.llx), 0, 0),
                    LLVMConstInt(LLVMInt32TypeInContext(cgx.llx), 0, 0),
                    LLVMConstInt(LLVMInt32TypeInContext(cgx.llx), 0, 0),
                ];

                let llvm_type_tag_ptr = LLVMBuildInBoundsGEP(
                    fcx.builder,
                    llvm_any,
                    gep_indices.as_mut_ptr(),
                    gep_indices.len() as u32,
                    b"type_tag_ptr\0".as_ptr() as *const _,
                );

                let llvm_type_tag = LLVMBuildLoad(
                    fcx.builder,
                    llvm_type_tag_ptr,
                    "type_tag\0".as_ptr() as *const _,
                );

                let llvm_i8 = LLVMInt8TypeInContext(cgx.llx);
                let possible_type_tag_metadata = int_range_metadata_node(
                    cgx.llx,
                    llvm_i8,
                    possible_type_tags
                        .into_iter()
                        .map(|type_tag| type_tag as i64),
                );

                let range_name = "range";
                let range_kind_id = LLVMGetMDKindIDInContext(
                    cgx.llx,
                    range_name.as_ptr() as *const _,
                    range_name.len() as u32,
                );
                LLVMSetMetadata(llvm_type_tag, range_kind_id, possible_type_tag_metadata);

                cgx.add_invariant_load_metadata(llvm_type_tag);
                fcx.regs.insert(*reg, llvm_type_tag);
            }
            OpKind::LoadBoxedListLength(reg, list_reg) => {
                let llvm_list = fcx.regs[list_reg];
                let length_ptr = LLVMBuildStructGEP(
                    fcx.builder,
                    llvm_list,
                    1,
                    b"length_ptr\0".as_ptr() as *const _,
                );

                let llvm_length =
                    LLVMBuildLoad(fcx.builder, length_ptr, "length\0".as_ptr() as *const _);
                cgx.add_invariant_load_metadata(llvm_length);

                fcx.regs.insert(*reg, llvm_length);
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
                cgx.add_invariant_load_metadata(llvm_head);

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
                cgx.add_invariant_load_metadata(llvm_rest);

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
                cgx.add_invariant_load_metadata(llvm_value);

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
            OpKind::Cond(cond_op) => {
                let cond_alloc_plan = &alloc_atom.cond_plans()[&op_index];
                gen_cond(cgx, mcx, fcx, cond_op, cond_alloc_plan);
            }
            OpKind::AllocInt(reg, int_reg) => {
                let box_source = alloc_atom.box_sources()[reg];

                let llvm_int = fcx.regs[int_reg];
                let llvm_alloced = alloc::types::gen_alloc_int(
                    cgx,
                    fcx.builder,
                    active_alloc,
                    box_source,
                    llvm_int,
                );

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
                let box_source = alloc_atom.box_sources()[reg];

                let input = alloc::types::PairInput {
                    llvm_head: fcx.regs[head_reg],
                    llvm_rest: fcx.regs[rest_reg],
                    llvm_length: fcx.regs[length_reg],
                };

                let llvm_value = alloc::types::gen_alloc_boxed_pair(
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
            OpKind::IntEqual(reg, BinaryOp { lhs_reg, rhs_reg }) => {
                let llvm_lhs = fcx.regs[lhs_reg];
                let llvm_rhs = fcx.regs[rhs_reg];

                let llvm_value = LLVMBuildICmp(
                    fcx.builder,
                    LLVMIntPredicate::LLVMIntEQ,
                    llvm_lhs,
                    llvm_rhs,
                    "int_equal\0".as_ptr() as *const _,
                );
                fcx.regs.insert(*reg, llvm_value);
            }
        }
    }
}

fn gen_alloc_atom(
    cgx: &mut CodegenCtx,
    mcx: &mut ModCtx,
    fcx: &mut FunCtx,
    alloc_atom: &alloc::AllocAtom<'_>,
) {
    let mut active_alloc = if let Some(llvm_task) = fcx.current_task {
        alloc::core::gen_active_alloc_for_atom(cgx, mcx, fcx.builder, llvm_task, &alloc_atom)
    } else {
        alloc::ActiveAlloc::empty()
    };

    for (op_index, op) in alloc_atom.ops().iter().enumerate() {
        gen_op(cgx, mcx, fcx, alloc_atom, &mut active_alloc, op_index, &op);
    }

    assert!(
        active_alloc.is_empty(),
        "did not consume entire active heap allocation"
    );
}

pub(crate) fn gen_fun(cgx: &mut CodegenCtx, mcx: &mut ModCtx, fun: &Fun) -> BuiltFun {
    use crate::codegen::alloc::plan::plan_allocs;
    use crate::codegen::analysis;
    use crate::mir::ops;
    use runtime::abitype::{ABIType, ParamABIType, RetABIType};

    // Determine which values are captured
    let captures = analysis::escape::calc_fun_captures(mcx.built_funs(), fun);

    // Use the capture information to plan our allocations
    let alloc_plan = plan_allocs(&captures, &fun.ops);

    // Use the allocation plan to determine if we need a task parameter
    let takes_task = (fun.abi.call_conv == ops::CallConv::External)
        | analysis::needs_task::alloc_plan_needs_task(mcx.built_funs(), &alloc_plan);

    // Determine which params we captured
    let takes_closure = fun.abi.call_conv.takes_closure();
    let mut param_captures = vec![];
    if takes_closure {
        param_captures.push(CaptureKind::Never);
    }
    for param_reg in fun.params.iter() {
        param_captures.push(captures.get(*param_reg));
    }

    let gen_abi = GenABI {
        takes_task,
        takes_closure,
        params: fun
            .abi
            .params
            .iter()
            .map(|abi_type| abi_type.clone().into())
            .collect::<Vec<ParamABIType>>()
            .into_boxed_slice(),
        ret: fun.abi.ret.clone(),
    };

    unsafe {
        let builder = LLVMCreateBuilderInContext(cgx.llx);

        let function_type = cgx.fun_abi_to_llvm_type(&gen_abi);

        let fun_symbol = fun
            .source_name
            .as_ref()
            .map(|source_name| ffi::CString::new(source_name.as_bytes()).unwrap())
            .unwrap_or_else(|| ffi::CString::new("anon_fun").unwrap());

        let function = LLVMAddFunction(mcx.module, fun_symbol.as_ptr() as *const _, function_type);

        let bb = LLVMAppendBasicBlockInContext(cgx.llx, function, b"entry\0".as_ptr() as *const _);
        LLVMPositionBuilderAtEnd(builder, bb);

        let mut fcx = FunCtx::new(function, builder);

        if takes_task {
            fcx.current_task = Some(LLVMGetParam(function, 0));
        }

        let llvm_params_offset = takes_task as usize + takes_closure as usize;
        for (param_index, reg) in fun.params.iter().enumerate() {
            let llvm_offset = (llvm_params_offset + param_index) as u32;
            fcx.regs.insert(*reg, LLVMGetParam(function, llvm_offset));
        }

        // Our task is an implicit parameter
        let captures_offset = takes_closure as usize;
        for (param_index, param_abi_type) in fun.abi.params.iter().enumerate() {
            if let ABIType::Boxed(_) = param_abi_type {
                let no_capture =
                    param_captures[captures_offset + param_index] == CaptureKind::Never;

                cgx.add_boxed_param_attrs(
                    function,
                    (llvm_params_offset + param_index + 1) as u32,
                    no_capture,
                );
            }
        }

        if let RetABIType::Inhabited(ABIType::Boxed(_)) = fun.abi.ret {
            cgx.add_boxed_return_attrs(function);
        }

        for alloc_atom in alloc_plan {
            gen_alloc_atom(cgx, mcx, &mut fcx, &alloc_atom);
        }

        mcx.optimise_function(function);

        BuiltFun {
            llvm_value: function,
            takes_task,
            param_captures: param_captures.into_boxed_slice(),
        }
    }
}
