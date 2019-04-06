use llvm_sys::core::*;
use llvm_sys::prelude::*;
use llvm_sys::{LLVMIntPredicate, LLVMRealPredicate};

use runtime::boxed;

use crate::mir::ops::*;

use crate::codegen::fun_gen::FunCtx;
use crate::codegen::mod_gen::ModCtx;
use crate::codegen::target_gen::TargetCtx;
use crate::codegen::{alloc, const_gen};

fn gen_op(
    tcx: &mut TargetCtx,
    mcx: &mut ModCtx<'_, '_, '_>,
    fcx: &mut FunCtx,
    alloc_atom: &alloc::AllocAtom<'_>,
    active_alloc: &mut alloc::ActiveAlloc,
    op: &Op,
) {
    unsafe {
        match &op.kind {
            OpKind::ConstBoxedNil(reg, _) => {
                let llvm_value = const_gen::gen_boxed_nil(tcx, mcx);
                fcx.regs.insert(*reg, llvm_value);
            }
            OpKind::ConstBoxedTrue(reg, _) => {
                let llvm_value =
                    tcx.ptr_to_singleton_box(mcx.module, boxed::TypeTag::True, b"ARRET_TRUE\0");
                fcx.regs.insert(*reg, llvm_value);
            }
            OpKind::ConstBoxedFalse(reg, _) => {
                let llvm_value =
                    tcx.ptr_to_singleton_box(mcx.module, boxed::TypeTag::False, b"ARRET_FALSE\0");
                fcx.regs.insert(*reg, llvm_value);
            }
            OpKind::ConstInt64(reg, value) => {
                let llvm_value = LLVMConstInt(LLVMInt64TypeInContext(tcx.llx), *value as u64, 1);
                fcx.regs.insert(*reg, llvm_value);
            }
            OpKind::ConstFloat(reg, value) => {
                let llvm_value = LLVMConstReal(LLVMDoubleTypeInContext(tcx.llx), *value);
                fcx.regs.insert(*reg, llvm_value);
            }
            OpKind::ConstUsize(reg, value) => {
                let llvm_value = LLVMConstInt(tcx.usize_llvm_type(), *value as u64, 1);
                fcx.regs.insert(*reg, llvm_value);
            }
            OpKind::ConstChar(reg, value) => {
                let llvm_value = LLVMConstInt(LLVMInt32TypeInContext(tcx.llx), *value as u64, 1);
                fcx.regs.insert(*reg, llvm_value);
            }
            OpKind::ConstBool(reg, value) => {
                let llvm_value = LLVMConstInt(LLVMInt1TypeInContext(tcx.llx), *value as u64, 1);
                fcx.regs.insert(*reg, llvm_value);
            }
            OpKind::ConstTypeTag(reg, type_tag) => {
                let llvm_value = LLVMConstInt(LLVMInt8TypeInContext(tcx.llx), *type_tag as u64, 1);
                fcx.regs.insert(*reg, llvm_value);
            }
            OpKind::ConstBoxedInt(reg, value) => {
                let llvm_value = const_gen::gen_boxed_int(tcx, mcx, *value);
                fcx.regs.insert(*reg, llvm_value);
            }
            OpKind::ConstBoxedFloat(reg, value) => {
                let llvm_value = const_gen::gen_boxed_float(tcx, mcx, *value);
                fcx.regs.insert(*reg, llvm_value);
            }
            OpKind::ConstBoxedChar(reg, value) => {
                let llvm_value = const_gen::gen_boxed_char(tcx, mcx, *value);
                fcx.regs.insert(*reg, llvm_value);
            }
            OpKind::ConstBoxedFunThunk(
                reg,
                BoxFunThunkOp {
                    closure_reg,
                    callee,
                },
            ) => {
                let llvm_entry_point = gen_callee_entry_point(tcx, mcx, fcx, callee);
                let llvm_closure = fcx.regs[closure_reg];

                let llvm_value =
                    const_gen::gen_boxed_fun_thunk(tcx, mcx, llvm_closure, llvm_entry_point);
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
                    const_gen::gen_boxed_pair(tcx, mcx, llvm_head, llvm_rest, llvm_length);
                fcx.regs.insert(*reg, llvm_value);
            }
            OpKind::ConstBoxedStr(reg, value) => {
                if value.len() > boxed::Str::MAX_INLINE_BYTES {
                    unimplemented!("Non-inline strings");
                }

                let llvm_value = const_gen::gen_boxed_inline_str(tcx, mcx, value.as_ref());
                fcx.regs.insert(*reg, llvm_value);
            }
            OpKind::ConstBoxedSym(reg, value) => {
                let llvm_value = const_gen::gen_boxed_sym(tcx, mcx, value.as_ref());
                fcx.regs.insert(*reg, llvm_value);
            }
            OpKind::ConstCastBoxed(reg, CastBoxedOp { from_reg, to_type }) => {
                let from_llvm_value = fcx.regs[from_reg];
                let to_llvm_type = tcx.boxed_abi_to_llvm_ptr_type(to_type);
                let to_llvm_value = LLVMConstBitCast(from_llvm_value, to_llvm_type);

                fcx.regs.insert(*reg, to_llvm_value);
            }
            OpKind::CastBoxed(reg, CastBoxedOp { from_reg, to_type }) => {
                let from_llvm_value = fcx.regs[from_reg];
                let to_llvm_type = tcx.boxed_abi_to_llvm_ptr_type(to_type);

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

                let llvm_fun = gen_callee_entry_point(tcx, mcx, fcx, callee);
                let takes_task = callee::callee_takes_task(callee);

                let task_reg_iter = Some(fcx.current_task).filter(|_| takes_task).into_iter();
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
                use crate::codegen::range_md::int_range_md_node;

                let llvm_any = fcx.regs[subject_reg];
                let gep_indices = &mut [
                    LLVMConstInt(LLVMInt32TypeInContext(tcx.llx), 0, 0),
                    LLVMConstInt(LLVMInt32TypeInContext(tcx.llx), 0, 0),
                    LLVMConstInt(LLVMInt32TypeInContext(tcx.llx), 0, 0),
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

                let llvm_i8 = LLVMInt8TypeInContext(tcx.llx);
                let possible_type_tag_metadata = int_range_md_node(
                    tcx.llx,
                    llvm_i8,
                    possible_type_tags
                        .into_iter()
                        .map(|type_tag| type_tag as i64),
                );

                let range_md_kind_id = tcx.llvm_md_kind_id_for_name(b"range");
                LLVMSetMetadata(llvm_type_tag, range_md_kind_id, possible_type_tag_metadata);

                tcx.add_invariant_load_metadata(llvm_type_tag);
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
                tcx.add_invariant_load_metadata(llvm_length);

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
                tcx.add_invariant_load_metadata(llvm_head);
                tcx.add_boxed_load_metadata(llvm_head);

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
                tcx.add_invariant_load_metadata(llvm_rest);
                tcx.add_boxed_load_metadata(llvm_rest);

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
                tcx.add_invariant_load_metadata(llvm_value);

                fcx.regs.insert(*reg, llvm_value);
            }
            OpKind::LoadBoxedFloatValue(reg, boxed_float_reg) => {
                let llvm_boxed_float = fcx.regs[boxed_float_reg];
                let value_ptr = LLVMBuildStructGEP(
                    fcx.builder,
                    llvm_boxed_float,
                    1,
                    b"float_value_ptr\0".as_ptr() as *const _,
                );

                let llvm_value =
                    LLVMBuildLoad(fcx.builder, value_ptr, "float_value\0".as_ptr() as *const _);
                tcx.add_invariant_load_metadata(llvm_value);

                fcx.regs.insert(*reg, llvm_value);
            }
            OpKind::LoadBoxedCharValue(reg, boxed_char_reg) => {
                let llvm_boxed_char = fcx.regs[boxed_char_reg];
                let value_ptr = LLVMBuildStructGEP(
                    fcx.builder,
                    llvm_boxed_char,
                    1,
                    b"char_value_ptr\0".as_ptr() as *const _,
                );

                let llvm_value =
                    LLVMBuildLoad(fcx.builder, value_ptr, "char_value\0".as_ptr() as *const _);

                tcx.add_invariant_load_metadata(llvm_value);
                tcx.add_char_codepoint_range_metadata(llvm_value);

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
                let cond_alloc_plan = active_alloc.next_cond_plan(alloc_atom);
                gen_cond(tcx, mcx, fcx, cond_op, cond_alloc_plan);
            }
            OpKind::AllocBoxedInt(reg, int_reg) => {
                let box_source = alloc_atom.box_sources()[reg];

                let llvm_int = fcx.regs[int_reg];
                let llvm_alloced = alloc::types::gen_alloc_int(
                    tcx,
                    fcx.builder,
                    active_alloc,
                    box_source,
                    llvm_int,
                );

                fcx.regs.insert(*reg, llvm_alloced);
            }
            OpKind::AllocBoxedFloat(reg, float_reg) => {
                let box_source = alloc_atom.box_sources()[reg];

                let llvm_float = fcx.regs[float_reg];
                let llvm_alloced = alloc::types::gen_alloc_float(
                    tcx,
                    fcx.builder,
                    active_alloc,
                    box_source,
                    llvm_float,
                );

                fcx.regs.insert(*reg, llvm_alloced);
            }
            OpKind::AllocBoxedChar(reg, char_reg) => {
                let box_source = alloc_atom.box_sources()[reg];

                let llvm_char = fcx.regs[char_reg];
                let llvm_alloced = alloc::types::gen_alloc_char(
                    tcx,
                    fcx.builder,
                    active_alloc,
                    box_source,
                    llvm_char,
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
                    tcx,
                    fcx.builder,
                    active_alloc,
                    box_source,
                    &input,
                );
                fcx.regs.insert(*reg, llvm_value);
            }
            OpKind::AllocBoxedFunThunk(
                reg,
                BoxFunThunkOp {
                    closure_reg,
                    callee,
                },
            ) => {
                let box_source = alloc_atom.box_sources()[reg];

                let input = alloc::types::FunThunkInput {
                    llvm_closure: fcx.regs[closure_reg],
                    llvm_entry_point: gen_callee_entry_point(tcx, mcx, fcx, callee),
                };

                let llvm_value = alloc::types::gen_alloc_boxed_fun_thunk(
                    tcx,
                    fcx.builder,
                    active_alloc,
                    box_source,
                    &input,
                );
                fcx.regs.insert(*reg, llvm_value);
            }
            OpKind::IntEqual(reg, BinaryOp { lhs_reg, rhs_reg })
            | OpKind::CharEqual(reg, BinaryOp { lhs_reg, rhs_reg }) => {
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
            OpKind::FloatEqual(reg, BinaryOp { lhs_reg, rhs_reg }) => {
                let llvm_lhs = fcx.regs[lhs_reg];
                let llvm_rhs = fcx.regs[rhs_reg];

                let llvm_value = LLVMBuildFCmp(
                    fcx.builder,
                    LLVMRealPredicate::LLVMRealOLE,
                    llvm_lhs,
                    llvm_rhs,
                    "float_equal\0".as_ptr() as *const _,
                );
                fcx.regs.insert(*reg, llvm_value);
            }
            OpKind::BoxIdentical(reg, BinaryOp { lhs_reg, rhs_reg }) => {
                let llvm_lhs = fcx.regs[lhs_reg];
                let llvm_rhs = fcx.regs[rhs_reg];
                let llvm_i64 = LLVMInt64TypeInContext(tcx.llx);

                let i64_lhs = LLVMBuildPtrToInt(
                    fcx.builder,
                    llvm_lhs,
                    llvm_i64,
                    "lhs_as_int\0".as_ptr() as *const _,
                );
                let i64_rhs = LLVMBuildPtrToInt(
                    fcx.builder,
                    llvm_rhs,
                    llvm_i64,
                    "rhs_as_int\0".as_ptr() as *const _,
                );

                let llvm_value = LLVMBuildICmp(
                    fcx.builder,
                    LLVMIntPredicate::LLVMIntEQ,
                    i64_lhs,
                    i64_rhs,
                    "box_identical\0".as_ptr() as *const _,
                );
                fcx.regs.insert(*reg, llvm_value);
            }
            OpKind::UsizeToInt64(reg, usize_reg) => {
                let llvm_usize = fcx.regs[usize_reg];

                let llvm_i64 = if tcx.pointer_bits() == 64 {
                    llvm_usize
                } else {
                    LLVMBuildZExt(
                        fcx.builder,
                        llvm_usize,
                        LLVMInt64TypeInContext(tcx.llx),
                        "usize_as_i64\0".as_ptr() as *const _,
                    )
                };

                fcx.regs.insert(*reg, llvm_i64);
            }
            OpKind::Int64ToFloat(reg, int64_reg) => {
                let llvm_i64 = fcx.regs[int64_reg];

                let llvm_double = LLVMBuildSIToFP(
                    fcx.builder,
                    llvm_i64,
                    LLVMDoubleTypeInContext(tcx.llx),
                    "i64_as_double\0".as_ptr() as *const _,
                );

                fcx.regs.insert(*reg, llvm_double);
            }
            OpKind::FloatAdd(reg, BinaryOp { lhs_reg, rhs_reg }) => {
                let llvm_lhs = fcx.regs[lhs_reg];
                let llvm_rhs = fcx.regs[rhs_reg];

                let llvm_value = LLVMBuildFAdd(
                    fcx.builder,
                    llvm_lhs,
                    llvm_rhs,
                    "float_sum\0".as_ptr() as *const _,
                );
                fcx.regs.insert(*reg, llvm_value);
            }
            OpKind::UsizeAdd(reg, BinaryOp { lhs_reg, rhs_reg }) => {
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
            OpKind::Int64CheckedAdd(reg, BinaryOp { lhs_reg, rhs_reg }) => {
                let llvm_lhs = fcx.regs[lhs_reg];
                let llvm_rhs = fcx.regs[rhs_reg];

                // TODO: Check for overflow
                let llvm_value = LLVMBuildAdd(
                    fcx.builder,
                    llvm_lhs,
                    llvm_rhs,
                    "sum\0".as_ptr() as *const _,
                );
                fcx.regs.insert(*reg, llvm_value);
            }
            OpKind::FloatMul(reg, BinaryOp { lhs_reg, rhs_reg }) => {
                let llvm_lhs = fcx.regs[lhs_reg];
                let llvm_rhs = fcx.regs[rhs_reg];

                let llvm_value = LLVMBuildFMul(
                    fcx.builder,
                    llvm_lhs,
                    llvm_rhs,
                    "float_product\0".as_ptr() as *const _,
                );
                fcx.regs.insert(*reg, llvm_value);
            }
            OpKind::Int64CheckedMul(reg, BinaryOp { lhs_reg, rhs_reg }) => {
                let llvm_lhs = fcx.regs[lhs_reg];
                let llvm_rhs = fcx.regs[rhs_reg];

                // TODO: Check for overflow
                let llvm_value = LLVMBuildMul(
                    fcx.builder,
                    llvm_lhs,
                    llvm_rhs,
                    "product\0".as_ptr() as *const _,
                );
                fcx.regs.insert(*reg, llvm_value);
            }
            OpKind::FloatSub(reg, BinaryOp { lhs_reg, rhs_reg }) => {
                let llvm_lhs = fcx.regs[lhs_reg];
                let llvm_rhs = fcx.regs[rhs_reg];

                let llvm_value = LLVMBuildFSub(
                    fcx.builder,
                    llvm_lhs,
                    llvm_rhs,
                    "float_difference\0".as_ptr() as *const _,
                );
                fcx.regs.insert(*reg, llvm_value);
            }
            OpKind::Int64CheckedSub(reg, BinaryOp { lhs_reg, rhs_reg }) => {
                let llvm_lhs = fcx.regs[lhs_reg];
                let llvm_rhs = fcx.regs[rhs_reg];

                // TODO: Check for overflow
                let llvm_value = LLVMBuildSub(
                    fcx.builder,
                    llvm_lhs,
                    llvm_rhs,
                    "difference\0".as_ptr() as *const _,
                );
                fcx.regs.insert(*reg, llvm_value);
            }
            OpKind::FloatDiv(reg, BinaryOp { lhs_reg, rhs_reg }) => {
                let llvm_lhs = fcx.regs[lhs_reg];
                let llvm_rhs = fcx.regs[rhs_reg];

                let llvm_value = LLVMBuildFDiv(
                    fcx.builder,
                    llvm_lhs,
                    llvm_rhs,
                    "float_quotient\0".as_ptr() as *const _,
                );
                fcx.regs.insert(*reg, llvm_value);
            }
            OpKind::Int64CheckedDiv(reg, BinaryOp { lhs_reg, rhs_reg }) => {
                let llvm_lhs = fcx.regs[lhs_reg];
                let llvm_rhs = fcx.regs[rhs_reg];

                // TODO: Check for overflow
                let llvm_value = LLVMBuildSDiv(
                    fcx.builder,
                    llvm_lhs,
                    llvm_rhs,
                    "quot\0".as_ptr() as *const _,
                );
                fcx.regs.insert(*reg, llvm_value);
            }
            OpKind::Int64CheckedRem(reg, BinaryOp { lhs_reg, rhs_reg }) => {
                let llvm_lhs = fcx.regs[lhs_reg];
                let llvm_rhs = fcx.regs[rhs_reg];

                // TODO: Check for overflow
                let llvm_value = LLVMBuildSRem(
                    fcx.builder,
                    llvm_lhs,
                    llvm_rhs,
                    "rem\0".as_ptr() as *const _,
                );
                fcx.regs.insert(*reg, llvm_value);
            }
            OpKind::MakeCallback(
                reg,
                MakeCallbackOp {
                    callee,
                    closure_reg,
                },
            ) => {
                let llvm_closure = fcx.regs[closure_reg];
                let llvm_entry_point = gen_callee_entry_point(tcx, mcx, fcx, callee);
                let entry_point_llvm_type = LLVMTypeOf(llvm_entry_point);

                let callback_type = tcx.callback_llvm_type(entry_point_llvm_type);

                let llvm_undef = LLVMGetUndef(callback_type);
                let llvm_with_closure = LLVMBuildInsertValue(
                    fcx.builder,
                    llvm_undef,
                    llvm_closure,
                    0,
                    b"\0".as_ptr() as *const _,
                );
                let llvm_callback = LLVMBuildInsertValue(
                    fcx.builder,
                    llvm_with_closure,
                    llvm_entry_point,
                    1,
                    b"callback\0".as_ptr() as *const _,
                );

                fcx.regs.insert(*reg, llvm_callback);
            }
        }
    }
}

fn gen_cond_branch(
    tcx: &mut TargetCtx,
    mcx: &mut ModCtx<'_, '_, '_>,
    fcx: &mut FunCtx,
    block: LLVMBasicBlockRef,
    alloc_plan: &[alloc::AllocAtom<'_>],
    cont_block: LLVMBasicBlockRef,
) {
    unsafe {
        LLVMPositionBuilderAtEnd(fcx.builder, block);

        for alloc_atom in alloc_plan {
            gen_alloc_atom(tcx, mcx, fcx, alloc_atom);
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
    tcx: &mut TargetCtx,
    mcx: &mut ModCtx<'_, '_, '_>,
    fcx: &mut FunCtx,
    cond_op: &CondOp,
    cond_alloc_plan: &alloc::CondPlan<'_>,
) {
    let CondOp {
        reg_phi, test_reg, ..
    } = cond_op;

    unsafe {
        let true_block = LLVMAppendBasicBlockInContext(
            tcx.llx,
            fcx.function,
            b"cond_true\0".as_ptr() as *const _,
        );
        let false_block = LLVMAppendBasicBlockInContext(
            tcx.llx,
            fcx.function,
            b"cond_false\0".as_ptr() as *const _,
        );
        let cont_block = LLVMAppendBasicBlockInContext(
            tcx.llx,
            fcx.function,
            b"cond_cont\0".as_ptr() as *const _,
        );

        let test_llvm = fcx.regs[test_reg];
        LLVMBuildCondBr(fcx.builder, test_llvm, true_block, false_block);

        gen_cond_branch(
            tcx,
            mcx,
            fcx,
            true_block,
            cond_alloc_plan.true_subplan(),
            cont_block,
        );
        let mut final_true_block = LLVMGetInsertBlock(fcx.builder);

        gen_cond_branch(
            tcx,
            mcx,
            fcx,
            false_block,
            cond_alloc_plan.false_subplan(),
            cont_block,
        );
        let mut final_false_block = LLVMGetInsertBlock(fcx.builder);

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
                &mut final_true_block as *mut _,
                1,
            );
            LLVMAddIncoming(
                phi_value,
                &mut false_result_llvm as *mut _,
                &mut final_false_block as *mut _,
                1,
            );

            fcx.regs.insert(*output_reg, phi_value);
        }
    }
}

fn gen_callee_entry_point(
    tcx: &mut TargetCtx,
    mcx: &mut ModCtx<'_, '_, '_>,
    fcx: &mut FunCtx,
    callee: &Callee,
) -> LLVMValueRef {
    use crate::codegen::callee::*;

    match callee {
        Callee::PrivateFun(private_fun_id) => mcx.llvm_private_fun(*private_fun_id),
        Callee::BoxedFunThunk(fun_thunk_reg) => {
            let llvm_fun_thunk = fcx.regs[fun_thunk_reg];
            gen_boxed_fun_thunk_entry_point(fcx.builder, llvm_fun_thunk)
        }
        Callee::StaticSymbol(static_symbol) => {
            gen_static_symbol_entry_point(tcx, mcx, static_symbol)
        }
    }
}

pub(crate) fn gen_alloc_atom(
    tcx: &mut TargetCtx,
    mcx: &mut ModCtx<'_, '_, '_>,
    fcx: &mut FunCtx,
    alloc_atom: &alloc::AllocAtom<'_>,
) {
    let mut active_alloc = alloc::core::gen_active_alloc_for_atom(
        tcx,
        mcx,
        fcx.builder,
        fcx.current_task,
        &alloc_atom,
    );

    for op in alloc_atom.ops() {
        gen_op(tcx, mcx, fcx, alloc_atom, &mut active_alloc, &op);
    }

    assert!(
        active_alloc.is_empty(),
        "did not consume entire active heap allocation"
    );
}
