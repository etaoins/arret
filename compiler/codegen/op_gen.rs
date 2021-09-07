use llvm_sys::core::*;
use llvm_sys::prelude::*;
use llvm_sys::{LLVMCallConv, LLVMIntPredicate, LLVMRealPredicate};

use arret_runtime::boxed;

use crate::mir::ops::*;

use crate::codegen::fun_gen::FunCtx;
use crate::codegen::math_gen;
use crate::codegen::mod_gen::ModCtx;
use crate::codegen::panic_gen::gen_panic;
use crate::codegen::record_struct;
use crate::codegen::target_gen::TargetCtx;
use crate::codegen::{alloc, const_gen};
use crate::libcstr;

fn comparison_to_llvm_int_pred(comparison: Comparison) -> LLVMIntPredicate {
    match comparison {
        Comparison::Lt => LLVMIntPredicate::LLVMIntSLT,
        Comparison::Le => LLVMIntPredicate::LLVMIntSLE,
        Comparison::Eq => LLVMIntPredicate::LLVMIntEQ,
        Comparison::Gt => LLVMIntPredicate::LLVMIntSGT,
        Comparison::Ge => LLVMIntPredicate::LLVMIntSGE,
    }
}

fn comparison_to_llvm_real_pred(comparison: Comparison) -> LLVMRealPredicate {
    match comparison {
        Comparison::Lt => LLVMRealPredicate::LLVMRealOLT,
        Comparison::Le => LLVMRealPredicate::LLVMRealOLE,
        Comparison::Eq => LLVMRealPredicate::LLVMRealOEQ,
        Comparison::Gt => LLVMRealPredicate::LLVMRealOGT,
        Comparison::Ge => LLVMRealPredicate::LLVMRealOGE,
    }
}

fn gen_int_compare(
    fcx: &mut FunCtx,
    reg: RegId,
    comparison: Comparison,
    lhs_reg: RegId,
    rhs_reg: RegId,
    reg_name: &str,
) {
    unsafe {
        fcx.regs.insert(
            reg,
            LLVMBuildICmp(
                fcx.builder,
                comparison_to_llvm_int_pred(comparison),
                fcx.regs[&lhs_reg],
                fcx.regs[&rhs_reg],
                reg_name.as_ptr() as *const _,
            ),
        );
    }
}

fn gen_op(
    tcx: &mut TargetCtx,
    mcx: &mut ModCtx<'_, '_, '_>,
    fcx: &mut FunCtx,
    active_alloc: &mut alloc::ActiveAlloc<'_>,
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
            OpKind::ConstChar(reg, value) => {
                let llvm_value = LLVMConstInt(LLVMInt32TypeInContext(tcx.llx), *value as u64, 1);
                fcx.regs.insert(*reg, llvm_value);
            }
            OpKind::ConstBool(reg, value) => {
                let llvm_value = LLVMConstInt(LLVMInt1TypeInContext(tcx.llx), *value as u64, 1);
                fcx.regs.insert(*reg, llvm_value);
            }
            OpKind::ConstInternedSym(reg, value) => {
                let interned_sym = mcx.intern_name(value);
                let llvm_value = LLVMConstInt(
                    LLVMInt64TypeInContext(tcx.llx),
                    interned_sym.to_raw_u64(),
                    1,
                );
                fcx.regs.insert(*reg, llvm_value);
            }
            OpKind::ConstTypeTag(reg, type_tag) => {
                let llvm_value = LLVMConstInt(LLVMInt8TypeInContext(tcx.llx), *type_tag as u64, 1);
                fcx.regs.insert(*reg, llvm_value);
            }
            OpKind::ConstRecordClassId(reg, record_struct) => {
                let record_class_id = mcx.record_class_id_for_struct(record_struct);
                let llvm_value = LLVMConstInt(
                    tcx.record_class_id_llvm_type(),
                    u64::from(record_class_id),
                    1,
                );
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
                    captures_reg,
                    callee,
                },
            ) => {
                let llvm_entry_point = gen_callee_entry_point(tcx, mcx, fcx, callee);
                let llvm_env = fcx.regs[captures_reg];

                let llvm_value =
                    const_gen::gen_boxed_fun_thunk(tcx, mcx, llvm_env, llvm_entry_point);
                fcx.regs.insert(*reg, llvm_value);
            }
            OpKind::ConstBoxedPair(
                reg,
                BoxPairOp {
                    head_reg,
                    rest_reg,
                    list_len_reg,
                },
            ) => {
                let llvm_head = fcx.regs[head_reg];
                let llvm_rest = fcx.regs[rest_reg];
                let llvm_list_len = fcx.regs[list_len_reg];

                let llvm_value =
                    const_gen::gen_boxed_pair(tcx, mcx, llvm_head, llvm_rest, llvm_list_len);
                fcx.regs.insert(*reg, llvm_value);
            }
            OpKind::ConstBoxedStr(reg, value) => {
                let llvm_value = const_gen::gen_boxed_str(tcx, mcx, value.as_ref());
                fcx.regs.insert(*reg, llvm_value);
            }
            OpKind::ConstBoxedSym(reg, value) => {
                let llvm_value = const_gen::gen_boxed_sym(tcx, mcx, value.as_ref());
                fcx.regs.insert(*reg, llvm_value);
            }
            OpKind::ConstBoxedVector(reg, elements) => {
                let llvm_value = const_gen::gen_boxed_vector(
                    tcx,
                    mcx,
                    elements.iter().map(|element| fcx.regs[element]),
                );

                fcx.regs.insert(*reg, llvm_value);
            }
            OpKind::ConstBoxedSet(reg, elements) => {
                let llvm_value = const_gen::gen_boxed_set(
                    tcx,
                    mcx,
                    elements.iter().map(|element| fcx.regs[element]),
                );

                fcx.regs.insert(*reg, llvm_value);
            }
            OpKind::ConstBoxedMap(reg, entries) => {
                let llvm_value = const_gen::gen_boxed_map(
                    tcx,
                    mcx,
                    entries
                        .iter()
                        .map(|(key, value)| (fcx.regs[key], fcx.regs[value])),
                );

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
                    libcstr!("box_bitcast"),
                );
                fcx.regs.insert(*reg, to_llvm_value);
            }
            OpKind::Alias(reg, from_reg) => {
                let from_llvm_value = fcx.regs[from_reg];
                fcx.regs.insert(*reg, from_llvm_value);
            }
            OpKind::Call(reg, CallOp { callee, args, .. }) => {
                use crate::codegen::callee;

                let llvm_fun = gen_callee_entry_point(tcx, mcx, fcx, callee);
                let takes_task = callee::callee_takes_task(callee);

                let task_reg_iter = Some(fcx.current_task).filter(|_| takes_task).into_iter();
                let mut llvm_args = task_reg_iter
                    .chain(args.iter().map(|param_reg| fcx.regs[param_reg]))
                    .collect::<Vec<LLVMValueRef>>();

                let llvm_ret = LLVMBuildCall(
                    fcx.builder,
                    llvm_fun,
                    llvm_args.as_mut_ptr(),
                    llvm_args.len() as u32,
                    libcstr!(""),
                );

                let call_conv = callee::callee_call_conv(mcx, callee);
                LLVMSetInstructionCallConv(llvm_ret, call_conv);

                fcx.regs.insert(*reg, llvm_ret);
            }
            OpKind::TailCall(reg, TailCallOp { args, .. }) => {
                let mut llvm_args = std::iter::once(fcx.current_task)
                    .chain(args.iter().map(|param_reg| fcx.regs[param_reg]))
                    .collect::<Vec<LLVMValueRef>>();

                let llvm_ret = LLVMBuildCall(
                    fcx.builder,
                    fcx.function,
                    llvm_args.as_mut_ptr(),
                    llvm_args.len() as u32,
                    libcstr!(""),
                );

                LLVMSetTailCall(llvm_ret, 1);
                LLVMSetInstructionCallConv(llvm_ret, LLVMCallConv::LLVMFastCallConv as u32);

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
            OpKind::Panic(message) => {
                gen_panic(tcx, mcx, fcx, message);
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
                    libcstr!("type_tag_ptr"),
                );

                let llvm_type_tag =
                    LLVMBuildLoad(fcx.builder, llvm_type_tag_ptr, libcstr!("type_tag"));

                let llvm_i8 = LLVMInt8TypeInContext(tcx.llx);
                let possible_type_tag_md = int_range_md_node(
                    tcx.llx,
                    llvm_i8,
                    possible_type_tags
                        .into_iter()
                        .map(|type_tag| type_tag as i64),
                );

                let range_md_kind_id = tcx.llvm_md_kind_id_for_name("range");
                LLVMSetMetadata(
                    llvm_type_tag,
                    range_md_kind_id,
                    LLVMMetadataAsValue(tcx.llx, possible_type_tag_md),
                );

                tcx.add_invariant_load_metadata(llvm_type_tag);
                fcx.regs.insert(*reg, llvm_type_tag);
            }
            OpKind::LoadBoxedListLen(
                reg,
                LoadBoxedListLenOp {
                    list_reg,
                    min_list_len,
                },
            ) => {
                let llvm_i64 = LLVMInt64TypeInContext(tcx.llx);

                let llvm_list = fcx.regs[list_reg];
                let list_len_ptr =
                    LLVMBuildStructGEP(fcx.builder, llvm_list, 1, libcstr!("list_len_ptr"));

                let llvm_list_len = LLVMBuildLoad(fcx.builder, list_len_ptr, libcstr!("list_len"));
                tcx.add_invariant_load_metadata(llvm_list_len);

                // Every list element needs at least one pair. This means there's a maximum list
                // length that can fit in our address space.
                let max_list_len = std::u64::MAX / std::mem::size_of::<boxed::Pair>() as u64;

                let mut llvm_range_values = [
                    LLVMValueAsMetadata(LLVMConstInt(llvm_i64, *min_list_len as u64, 0)),
                    LLVMValueAsMetadata(LLVMConstInt(llvm_i64, max_list_len + 1, 0)),
                ];

                let range_md_kind_id = tcx.llvm_md_kind_id_for_name("list_len_range");
                let list_len_range_md = LLVMMDNodeInContext2(
                    tcx.llx,
                    llvm_range_values.as_mut_ptr(),
                    llvm_range_values.len(),
                );
                LLVMSetMetadata(
                    llvm_list_len,
                    range_md_kind_id,
                    LLVMMetadataAsValue(tcx.llx, list_len_range_md),
                );

                fcx.regs.insert(*reg, llvm_list_len);
            }
            OpKind::LoadBoxedPairHead(reg, pair_reg) => {
                let llvm_pair = fcx.regs[pair_reg];
                let head_ptr = LLVMBuildStructGEP(fcx.builder, llvm_pair, 2, libcstr!("head_ptr"));

                let llvm_head = LLVMBuildLoad(fcx.builder, head_ptr, libcstr!("head"));
                tcx.add_invariant_load_metadata(llvm_head);
                tcx.add_boxed_load_metadata(llvm_head);

                fcx.regs.insert(*reg, llvm_head);
            }
            OpKind::LoadBoxedPairRest(reg, pair_reg) => {
                let llvm_pair = fcx.regs[pair_reg];
                let head_ptr = LLVMBuildStructGEP(fcx.builder, llvm_pair, 3, libcstr!("rest_ptr"));

                let llvm_rest = LLVMBuildLoad(fcx.builder, head_ptr, libcstr!("rest"));
                tcx.add_invariant_load_metadata(llvm_rest);
                tcx.add_boxed_load_metadata(llvm_rest);

                fcx.regs.insert(*reg, llvm_rest);
            }
            OpKind::LoadBoxedIntValue(reg, boxed_int_reg) => {
                let llvm_boxed_int = fcx.regs[boxed_int_reg];
                let value_ptr =
                    LLVMBuildStructGEP(fcx.builder, llvm_boxed_int, 1, libcstr!("int_value_ptr"));

                let llvm_value = LLVMBuildLoad(fcx.builder, value_ptr, libcstr!("int_value"));
                tcx.add_invariant_load_metadata(llvm_value);

                fcx.regs.insert(*reg, llvm_value);
            }
            OpKind::LoadBoxedSymInterned(reg, boxed_sym_reg) => {
                let llvm_boxed_sym = fcx.regs[boxed_sym_reg];
                let value_ptr = LLVMBuildStructGEP(
                    fcx.builder,
                    llvm_boxed_sym,
                    1,
                    libcstr!("interned_sym_ptr"),
                );

                let llvm_value = LLVMBuildLoad(fcx.builder, value_ptr, libcstr!("interned_sym"));
                tcx.add_invariant_load_metadata(llvm_value);

                fcx.regs.insert(*reg, llvm_value);
            }
            OpKind::LoadBoxedFloatValue(reg, boxed_float_reg) => {
                let llvm_boxed_float = fcx.regs[boxed_float_reg];
                let value_ptr = LLVMBuildStructGEP(
                    fcx.builder,
                    llvm_boxed_float,
                    1,
                    libcstr!("float_value_ptr"),
                );

                let llvm_value = LLVMBuildLoad(fcx.builder, value_ptr, libcstr!("float_value"));
                tcx.add_invariant_load_metadata(llvm_value);

                fcx.regs.insert(*reg, llvm_value);
            }
            OpKind::LoadBoxedCharValue(reg, boxed_char_reg) => {
                let llvm_boxed_char = fcx.regs[boxed_char_reg];
                let value_ptr =
                    LLVMBuildStructGEP(fcx.builder, llvm_boxed_char, 1, libcstr!("char_value_ptr"));

                let llvm_value = LLVMBuildLoad(fcx.builder, value_ptr, libcstr!("char_value"));

                tcx.add_invariant_load_metadata(llvm_value);
                tcx.add_char_codepoint_range_metadata(llvm_value);

                fcx.regs.insert(*reg, llvm_value);
            }
            OpKind::LoadBoxedFunThunkCaptures(reg, boxed_fun_thunk_reg) => {
                let llvm_boxed_fun_thunk = fcx.regs[boxed_fun_thunk_reg];

                let captures_ptr = LLVMBuildStructGEP(
                    fcx.builder,
                    llvm_boxed_fun_thunk,
                    1,
                    libcstr!("boxed_fun_thunk_captures_ptr"),
                );
                let llvm_value = LLVMBuildLoad(
                    fcx.builder,
                    captures_ptr,
                    libcstr!("boxed_fun_thunk_captures"),
                );

                fcx.regs.insert(*reg, llvm_value);
            }
            OpKind::LoadBoxedRecordClassId(reg, boxed_record_reg) => {
                let llvm_boxed_record = fcx.regs[boxed_record_reg];
                let value_ptr = LLVMBuildStructGEP(
                    fcx.builder,
                    llvm_boxed_record,
                    record_struct::RECORD_CLASS_ID_INDEX,
                    libcstr!("record_class_id_ptr"),
                );

                let llvm_value = LLVMBuildLoad(fcx.builder, value_ptr, libcstr!("record_class_id"));

                mcx.add_record_class_id_range_metadata(llvm_value);
                tcx.add_invariant_load_metadata(llvm_value);

                fcx.regs.insert(*reg, llvm_value);
            }
            OpKind::LoadBoxedRecordField(reg, load_boxed_record_field_op) => {
                let LoadBoxedRecordFieldOp {
                    record_reg,
                    record_struct,
                    field_index,
                } = load_boxed_record_field_op;

                let record_struct::TargetRecordStruct { record_storage, .. } =
                    *tcx.target_record_struct(record_struct);

                let boxed_record_name = format!("boxed_{}_record\0", record_struct.source_name);

                let boxed_record_ptr_type =
                    LLVMPointerType(tcx.record_struct_llvm_box_type(record_struct), 0);

                let llvm_boxed_record = LLVMBuildBitCast(
                    fcx.builder,
                    fcx.regs[record_reg],
                    boxed_record_ptr_type,
                    boxed_record_name.as_ptr() as *const _,
                );

                let field_ptr = record_struct::gen_record_field_ptr(
                    tcx,
                    fcx.builder,
                    record_storage,
                    llvm_boxed_record,
                    *field_index,
                    b"record_field_ptr\0",
                );

                let llvm_value =
                    LLVMBuildLoad(fcx.builder, field_ptr, libcstr!("record_field_value"));
                tcx.add_invariant_load_metadata(llvm_value);

                fcx.regs.insert(*reg, llvm_value);
            }
            OpKind::LoadBoxedVectorLen(reg, vector_reg) => {
                use crate::codegen::vector_gen::load_boxed_vector_len;

                let llvm_boxed_vector = fcx.regs[vector_reg];
                let llvm_vector_len = load_boxed_vector_len(tcx, fcx, llvm_boxed_vector);

                fcx.regs.insert(*reg, llvm_vector_len);
            }
            OpKind::LoadBoxedVectorMember(
                reg,
                LoadBoxedVectorMemberOp {
                    vector_reg,
                    known_vector_len,
                    member_index,
                },
            ) => {
                use crate::codegen::vector_gen::load_boxed_vector_member;

                let llvm_boxed_vector = fcx.regs[vector_reg];

                let llvm_vector_member = load_boxed_vector_member(
                    tcx,
                    fcx,
                    llvm_boxed_vector,
                    *known_vector_len,
                    *member_index,
                );

                fcx.regs.insert(*reg, llvm_vector_member);
            }
            OpKind::Cond(cond_op) => {
                let cond_alloc_plan = active_alloc.next_cond_plan();
                gen_cond(tcx, mcx, fcx, cond_op, cond_alloc_plan);
            }
            OpKind::AllocBoxedInt(reg, int_reg) => {
                let box_source = active_alloc.next_box_source();

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
                let box_source = active_alloc.next_box_source();

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
                let box_source = active_alloc.next_box_source();

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
            OpKind::AllocBoxedSym(reg, interned_sym_reg) => {
                let box_source = active_alloc.next_box_source();

                let llvm_interned_sym = fcx.regs[interned_sym_reg];
                let llvm_alloced = alloc::types::gen_alloc_sym(
                    tcx,
                    fcx.builder,
                    active_alloc,
                    box_source,
                    llvm_interned_sym,
                );

                fcx.regs.insert(*reg, llvm_alloced);
            }
            OpKind::AllocBoxedPair(
                reg,
                BoxPairOp {
                    head_reg,
                    rest_reg,
                    list_len_reg,
                },
            ) => {
                let box_source = active_alloc.next_box_source();

                let input = alloc::types::PairInput {
                    llvm_head: fcx.regs[head_reg],
                    llvm_rest: fcx.regs[rest_reg],
                    llvm_list_len: fcx.regs[list_len_reg],
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
                    captures_reg,
                    callee,
                },
            ) => {
                let box_source = active_alloc.next_box_source();

                let input = alloc::types::FunThunkInput {
                    llvm_captures: fcx.regs[captures_reg],
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
            OpKind::IntCompare(
                reg,
                CompareOp {
                    comparison,
                    lhs_reg,
                    rhs_reg,
                },
            ) => {
                let reg_name = if comparison == &Comparison::Eq {
                    "int_equal\0"
                } else {
                    "int_compare\0"
                };

                gen_int_compare(fcx, *reg, *comparison, *lhs_reg, *rhs_reg, reg_name)
            }
            OpKind::BoolEqual(reg, BinaryOp { lhs_reg, rhs_reg }) => gen_int_compare(
                fcx,
                *reg,
                Comparison::Eq,
                *lhs_reg,
                *rhs_reg,
                "bool_equal\0",
            ),
            OpKind::CharEqual(reg, BinaryOp { lhs_reg, rhs_reg }) => gen_int_compare(
                fcx,
                *reg,
                Comparison::Eq,
                *lhs_reg,
                *rhs_reg,
                "char_equal\0",
            ),
            OpKind::InternedSymEqual(reg, BinaryOp { lhs_reg, rhs_reg }) => gen_int_compare(
                fcx,
                *reg,
                Comparison::Eq,
                *lhs_reg,
                *rhs_reg,
                "interned_sym_equal\0",
            ),
            OpKind::TypeTagEqual(reg, BinaryOp { lhs_reg, rhs_reg }) => gen_int_compare(
                fcx,
                *reg,
                Comparison::Eq,
                *lhs_reg,
                *rhs_reg,
                "type_tag_equal\0",
            ),
            OpKind::RecordClassIdEqual(reg, BinaryOp { lhs_reg, rhs_reg }) => gen_int_compare(
                fcx,
                *reg,
                Comparison::Eq,
                *lhs_reg,
                *rhs_reg,
                "record_class_id_equal\0",
            ),
            OpKind::FloatCompare(
                reg,
                CompareOp {
                    comparison,
                    lhs_reg,
                    rhs_reg,
                },
            ) => {
                let llvm_lhs = fcx.regs[lhs_reg];
                let llvm_rhs = fcx.regs[rhs_reg];

                let reg_name = if comparison == &Comparison::Eq {
                    "float_equal\0"
                } else {
                    "float_compare\0"
                };

                let llvm_value = LLVMBuildFCmp(
                    fcx.builder,
                    comparison_to_llvm_real_pred(*comparison),
                    llvm_lhs,
                    llvm_rhs,
                    reg_name.as_ptr() as *const _,
                );
                fcx.regs.insert(*reg, llvm_value);
            }
            OpKind::BoxIdentical(reg, BinaryOp { lhs_reg, rhs_reg }) => {
                let llvm_lhs = fcx.regs[lhs_reg];
                let llvm_rhs = fcx.regs[rhs_reg];
                let llvm_i64 = LLVMInt64TypeInContext(tcx.llx);

                let i64_lhs =
                    LLVMBuildPtrToInt(fcx.builder, llvm_lhs, llvm_i64, libcstr!("lhs_as_int"));

                let i64_rhs =
                    LLVMBuildPtrToInt(fcx.builder, llvm_rhs, llvm_i64, libcstr!("rhs_as_int"));

                let llvm_value = LLVMBuildICmp(
                    fcx.builder,
                    LLVMIntPredicate::LLVMIntEQ,
                    i64_lhs,
                    i64_rhs,
                    libcstr!("box_identical"),
                );
                fcx.regs.insert(*reg, llvm_value);
            }
            OpKind::Int64ToFloat(reg, int64_reg) => {
                let llvm_i64 = fcx.regs[int64_reg];

                let llvm_double = LLVMBuildSIToFP(
                    fcx.builder,
                    llvm_i64,
                    LLVMDoubleTypeInContext(tcx.llx),
                    libcstr!("i64_as_double"),
                );

                fcx.regs.insert(*reg, llvm_double);
            }
            OpKind::FloatAdd(reg, BinaryOp { lhs_reg, rhs_reg }) => {
                let llvm_lhs = fcx.regs[lhs_reg];
                let llvm_rhs = fcx.regs[rhs_reg];

                let llvm_value =
                    LLVMBuildFAdd(fcx.builder, llvm_lhs, llvm_rhs, libcstr!("float_sum"));
                fcx.regs.insert(*reg, llvm_value);
            }
            OpKind::Int64Add(reg, BinaryOp { lhs_reg, rhs_reg }) => {
                let llvm_lhs = fcx.regs[lhs_reg];
                let llvm_rhs = fcx.regs[rhs_reg];

                let llvm_value = LLVMBuildNUWAdd(fcx.builder, llvm_lhs, llvm_rhs, libcstr!("sum"));
                fcx.regs.insert(*reg, llvm_value);
            }
            OpKind::Int64CheckedAdd(reg, BinaryOp { lhs_reg, rhs_reg }) => {
                let llvm_lhs = fcx.regs[lhs_reg];
                let llvm_rhs = fcx.regs[rhs_reg];

                let llvm_value = math_gen::gen_checked_int_math(
                    tcx,
                    mcx,
                    fcx,
                    &math_gen::CHECKED_ADD,
                    llvm_lhs,
                    llvm_rhs,
                );
                fcx.regs.insert(*reg, llvm_value);
            }
            OpKind::FloatMul(reg, BinaryOp { lhs_reg, rhs_reg }) => {
                let llvm_lhs = fcx.regs[lhs_reg];
                let llvm_rhs = fcx.regs[rhs_reg];

                let llvm_value =
                    LLVMBuildFMul(fcx.builder, llvm_lhs, llvm_rhs, libcstr!("float_product"));
                fcx.regs.insert(*reg, llvm_value);
            }
            OpKind::Int64CheckedMul(reg, BinaryOp { lhs_reg, rhs_reg }) => {
                let llvm_lhs = fcx.regs[lhs_reg];
                let llvm_rhs = fcx.regs[rhs_reg];

                let llvm_value = math_gen::gen_checked_int_math(
                    tcx,
                    mcx,
                    fcx,
                    &math_gen::CHECKED_MUL,
                    llvm_lhs,
                    llvm_rhs,
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
                    libcstr!("float_difference"),
                );
                fcx.regs.insert(*reg, llvm_value);
            }
            OpKind::Int64CheckedSub(reg, BinaryOp { lhs_reg, rhs_reg }) => {
                let llvm_lhs = fcx.regs[lhs_reg];
                let llvm_rhs = fcx.regs[rhs_reg];

                let llvm_value = math_gen::gen_checked_int_math(
                    tcx,
                    mcx,
                    fcx,
                    &math_gen::CHECKED_SUB,
                    llvm_lhs,
                    llvm_rhs,
                );
                fcx.regs.insert(*reg, llvm_value);
            }
            OpKind::FloatDiv(reg, BinaryOp { lhs_reg, rhs_reg }) => {
                let llvm_lhs = fcx.regs[lhs_reg];
                let llvm_rhs = fcx.regs[rhs_reg];

                let llvm_value =
                    LLVMBuildFDiv(fcx.builder, llvm_lhs, llvm_rhs, libcstr!("float_quotient"));
                fcx.regs.insert(*reg, llvm_value);
            }
            OpKind::Int64Div(reg, BinaryOp { lhs_reg, rhs_reg }) => {
                let llvm_numer = fcx.regs[lhs_reg];
                let llvm_denom = fcx.regs[rhs_reg];

                let llvm_value =
                    LLVMBuildSDiv(fcx.builder, llvm_numer, llvm_denom, libcstr!("quot"));
                fcx.regs.insert(*reg, llvm_value);
            }
            OpKind::Int64CheckedDiv(reg, BinaryOp { lhs_reg, rhs_reg }) => {
                let llvm_numer = fcx.regs[lhs_reg];
                let llvm_denom = fcx.regs[rhs_reg];

                let llvm_value =
                    math_gen::gen_checked_int_div(tcx, mcx, fcx, llvm_numer, llvm_denom);

                fcx.regs.insert(*reg, llvm_value);
            }
            OpKind::Int64Rem(reg, BinaryOp { lhs_reg, rhs_reg }) => {
                let llvm_numer = fcx.regs[lhs_reg];
                let llvm_denom = fcx.regs[rhs_reg];

                let llvm_value =
                    LLVMBuildSRem(fcx.builder, llvm_numer, llvm_denom, libcstr!("rem"));
                fcx.regs.insert(*reg, llvm_value);
            }
            OpKind::Int64CheckedRem(reg, BinaryOp { lhs_reg, rhs_reg }) => {
                let llvm_numer = fcx.regs[lhs_reg];
                let llvm_denom = fcx.regs[rhs_reg];

                let llvm_value =
                    math_gen::gen_checked_int_rem(tcx, mcx, fcx, llvm_numer, llvm_denom);

                fcx.regs.insert(*reg, llvm_value);
            }
            OpKind::FloatSqrt(reg, radicand) => {
                let llvm_radicand = fcx.regs[radicand];

                let llvm_value = math_gen::gen_float_sqrt(tcx, mcx, fcx, llvm_radicand);
                fcx.regs.insert(*reg, llvm_value);
            }
            OpKind::Int64BitwiseAnd(reg, BinaryOp { lhs_reg, rhs_reg }) => {
                let llvm_lhs = fcx.regs[lhs_reg];
                let llvm_rhs = fcx.regs[rhs_reg];

                let llvm_value = LLVMBuildAnd(fcx.builder, llvm_lhs, llvm_rhs, libcstr!("int_and"));
                fcx.regs.insert(*reg, llvm_value);
            }
            OpKind::Int64BitwiseOr(reg, BinaryOp { lhs_reg, rhs_reg }) => {
                let llvm_lhs = fcx.regs[lhs_reg];
                let llvm_rhs = fcx.regs[rhs_reg];

                let llvm_value = LLVMBuildOr(fcx.builder, llvm_lhs, llvm_rhs, libcstr!("int_or"));
                fcx.regs.insert(*reg, llvm_value);
            }
            OpKind::Int64BitwiseXor(reg, BinaryOp { lhs_reg, rhs_reg }) => {
                let llvm_lhs = fcx.regs[lhs_reg];
                let llvm_rhs = fcx.regs[rhs_reg];

                let llvm_value = LLVMBuildXor(fcx.builder, llvm_lhs, llvm_rhs, libcstr!("int_xor"));
                fcx.regs.insert(*reg, llvm_value);
            }
            OpKind::Int64BitwiseNot(reg, int_reg) => {
                let llvm_int = fcx.regs[int_reg];

                let llvm_value = LLVMBuildNot(fcx.builder, llvm_int, libcstr!("int_not"));
                fcx.regs.insert(*reg, llvm_value);
            }
            OpKind::Int64ShiftLeft(reg, ShiftOp { int_reg, bit_count }) => {
                let llvm_int = fcx.regs[int_reg];

                let llvm_value = LLVMBuildShl(
                    fcx.builder,
                    llvm_int,
                    LLVMConstInt(LLVMInt64TypeInContext(tcx.llx), *bit_count as u64, 0),
                    libcstr!("int_shl"),
                );

                fcx.regs.insert(*reg, llvm_value);
            }
            OpKind::Int64ArithmeticShiftRight(reg, ShiftOp { int_reg, bit_count }) => {
                let llvm_int = fcx.regs[int_reg];

                let llvm_value = LLVMBuildAShr(
                    fcx.builder,
                    llvm_int,
                    LLVMConstInt(LLVMInt64TypeInContext(tcx.llx), *bit_count as u64, 0),
                    libcstr!("int_ashr"),
                );

                fcx.regs.insert(*reg, llvm_value);
            }
            OpKind::Int64LogicalShiftRight(reg, ShiftOp { int_reg, bit_count }) => {
                let llvm_int = fcx.regs[int_reg];

                let llvm_value = LLVMBuildLShr(
                    fcx.builder,
                    llvm_int,
                    LLVMConstInt(LLVMInt64TypeInContext(tcx.llx), *bit_count as u64, 0),
                    libcstr!("int_lshr"),
                );

                fcx.regs.insert(*reg, llvm_value);
            }
            OpKind::MakeCallback(
                reg,
                MakeCallbackOp {
                    callee,
                    captures_reg,
                },
            ) => {
                let llvm_captures = fcx.regs[captures_reg];
                let llvm_entry_point = gen_callee_entry_point(tcx, mcx, fcx, callee);
                let entry_point_llvm_type = LLVMTypeOf(llvm_entry_point);

                let callback_type = tcx.callback_llvm_type(entry_point_llvm_type);

                let llvm_undef = LLVMGetUndef(callback_type);
                let llvm_with_captures = LLVMBuildInsertValue(
                    fcx.builder,
                    llvm_undef,
                    llvm_captures,
                    0,
                    libcstr!("captures"),
                );
                let llvm_callback = LLVMBuildInsertValue(
                    fcx.builder,
                    llvm_with_captures,
                    llvm_entry_point,
                    1,
                    libcstr!("callback"),
                );

                fcx.regs.insert(*reg, llvm_callback);
            }
            OpKind::ConstBoxedRecord(
                reg,
                BoxRecordOp {
                    record_struct,
                    field_regs,
                },
            ) => {
                let llvm_fields: Box<[LLVMValueRef]> = field_regs
                    .iter()
                    .map(|field_reg| fcx.regs[field_reg])
                    .collect();

                let llvm_value = const_gen::gen_boxed_record(tcx, mcx, record_struct, &llvm_fields);
                fcx.regs.insert(*reg, llvm_value);
            }
            OpKind::AllocBoxedRecord(
                reg,
                BoxRecordOp {
                    record_struct,
                    field_regs,
                },
            ) => {
                let box_source = active_alloc.next_box_source();

                let llvm_fields = field_regs
                    .iter()
                    .map(|field_reg| fcx.regs[field_reg])
                    .collect();

                let input = alloc::types::RecordInput {
                    record_struct,
                    llvm_fields,
                };

                let llvm_value = alloc::types::gen_alloc_boxed_record(
                    tcx,
                    mcx,
                    fcx.builder,
                    active_alloc,
                    box_source,
                    &input,
                );
                fcx.regs.insert(*reg, llvm_value);
            }
        }
    }
}

fn gen_cond_branch(
    tcx: &mut TargetCtx,
    mcx: &mut ModCtx<'_, '_, '_>,
    fcx: &mut FunCtx,
    block: LLVMBasicBlockRef,
    alloc_plan: Vec<alloc::AllocAtom<'_>>,
    cont_block: LLVMBasicBlockRef,
) {
    unsafe {
        LLVMPositionBuilderAtEnd(fcx.builder, block);

        // We can't branch if we terminated
        let will_terminate = alloc_plan
            .last()
            .and_then(|alloc_atom| alloc_atom.ops().last())
            .filter(|op| op.kind().is_terminator())
            .is_some();

        for alloc_atom in alloc_plan {
            gen_alloc_atom(tcx, mcx, fcx, alloc_atom);
        }

        if !will_terminate {
            LLVMBuildBr(fcx.builder, cont_block);
        }
    }
}

fn gen_cond(
    tcx: &mut TargetCtx,
    mcx: &mut ModCtx<'_, '_, '_>,
    fcx: &mut FunCtx,
    cond_op: &CondOp,
    cond_alloc_plan: alloc::CondPlan<'_>,
) {
    let CondOp {
        reg_phi, test_reg, ..
    } = cond_op;

    let alloc::CondPlan {
        true_subplan: true_alloc_subplan,
        false_subplan: false_alloc_subplan,
    } = cond_alloc_plan;

    unsafe {
        let true_block =
            LLVMAppendBasicBlockInContext(tcx.llx, fcx.function, libcstr!("cond_true"));

        let false_block =
            LLVMAppendBasicBlockInContext(tcx.llx, fcx.function, libcstr!("cond_false"));

        let cont_block =
            LLVMAppendBasicBlockInContext(tcx.llx, fcx.function, libcstr!("cond_cont"));

        let test_llvm = fcx.regs[test_reg];
        LLVMBuildCondBr(fcx.builder, test_llvm, true_block, false_block);

        gen_cond_branch(tcx, mcx, fcx, true_block, true_alloc_subplan, cont_block);
        let mut final_true_block = LLVMGetInsertBlock(fcx.builder);

        gen_cond_branch(tcx, mcx, fcx, false_block, false_alloc_subplan, cont_block);
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
                libcstr!("cond_phi"),
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
    alloc_atom: alloc::AllocAtom<'_>,
) {
    let ops = alloc_atom.ops();
    let mut active_alloc =
        alloc::core::atom_into_active_alloc(tcx, mcx, fcx.builder, fcx.current_task, alloc_atom);

    for op in ops {
        gen_op(tcx, mcx, fcx, &mut active_alloc, op);
    }

    assert!(
        active_alloc.is_empty(),
        "did not consume entire active heap allocation"
    );
}
