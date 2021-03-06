use llvm_sys::core::*;
use llvm_sys::prelude::*;
use llvm_sys::LLVMIntPredicate;

use crate::codegen::fun_gen::FunCtx;
use crate::codegen::mod_gen::ModCtx;
use crate::codegen::panic_gen::gen_panic;
use crate::codegen::target_gen::TargetCtx;
use crate::libcstr;

pub struct CheckedIntOp {
    math_intrinsic_name: &'static [u8],
    result_name: &'static [u8],
    panic_message: &'static str,
}

pub const CHECKED_ADD: CheckedIntOp = CheckedIntOp {
    math_intrinsic_name: b"llvm.sadd.with.overflow.i64\0",
    result_name: b"sum\0",
    panic_message: "attempt to add with overflow",
};

pub const CHECKED_SUB: CheckedIntOp = CheckedIntOp {
    math_intrinsic_name: b"llvm.ssub.with.overflow.i64\0",
    result_name: b"difference\0",
    panic_message: "attempt to subtract with overflow",
};

pub const CHECKED_MUL: CheckedIntOp = CheckedIntOp {
    math_intrinsic_name: b"llvm.smul.with.overflow.i64\0",
    result_name: b"product\0",
    panic_message: "attempt to multiply with overflow",
};

pub(crate) fn gen_checked_int_math(
    tcx: &mut TargetCtx,
    mcx: &mut ModCtx<'_, '_, '_>,
    fcx: &mut FunCtx,
    int_op: &'static CheckedIntOp,
    llvm_lhs: LLVMValueRef,
    llvm_rhs: LLVMValueRef,
) -> LLVMValueRef {
    let CheckedIntOp {
        math_intrinsic_name,
        result_name,
        panic_message,
    } = int_op;

    unsafe {
        let llvm_i1 = LLVMInt1TypeInContext(tcx.llx);
        let llvm_i64 = LLVMInt64TypeInContext(tcx.llx);

        let mut return_type_members = [llvm_i64, llvm_i1];

        let llvm_return_type = LLVMStructTypeInContext(
            tcx.llx,
            return_type_members.as_mut_ptr(),
            return_type_members.len() as u32,
            0,
        );

        let llvm_param_types = &mut [llvm_i64, llvm_i64];

        let math_intrinsic_llvm_type = LLVMFunctionType(
            llvm_return_type,
            llvm_param_types.as_mut_ptr(),
            llvm_param_types.len() as u32,
            0,
        );

        let math_intrinsic_fun =
            mcx.get_function_or_insert(math_intrinsic_llvm_type, math_intrinsic_name, |_| {});

        let math_intrinsic_args = &mut [llvm_lhs, llvm_rhs];

        let llvm_result_with_overflow = LLVMBuildCall(
            fcx.builder,
            math_intrinsic_fun,
            math_intrinsic_args.as_mut_ptr(),
            math_intrinsic_args.len() as u32,
            libcstr!("result_with_overflow"),
        );

        let llvm_math_result = LLVMBuildExtractValue(
            fcx.builder,
            llvm_result_with_overflow,
            0,
            result_name.as_ptr() as *const _,
        );

        let llvm_overflow = LLVMBuildExtractValue(
            fcx.builder,
            llvm_result_with_overflow,
            1,
            libcstr!("overflow_flag"),
        );

        let overflow_block =
            LLVMAppendBasicBlockInContext(tcx.llx, fcx.function, libcstr!("overflow"));

        let cont_block =
            LLVMAppendBasicBlockInContext(tcx.llx, fcx.function, libcstr!("no_overflow"));

        LLVMBuildCondBr(fcx.builder, llvm_overflow, overflow_block, cont_block);

        LLVMPositionBuilderAtEnd(fcx.builder, overflow_block);
        gen_panic(tcx, mcx, fcx, panic_message);

        LLVMPositionBuilderAtEnd(fcx.builder, cont_block);
        llvm_math_result
    }
}

pub(crate) fn gen_checked_int_rem(
    tcx: &mut TargetCtx,
    mcx: &mut ModCtx<'_, '_, '_>,
    fcx: &mut FunCtx,
    llvm_numer: LLVMValueRef,
    llvm_denom: LLVMValueRef,
) -> LLVMValueRef {
    unsafe {
        let llvm_i64 = LLVMInt64TypeInContext(tcx.llx);

        let denom_is_zero = LLVMBuildICmp(
            fcx.builder,
            LLVMIntPredicate::LLVMIntEQ,
            llvm_denom,
            LLVMConstInt(llvm_i64, 0, 0),
            libcstr!("denom_is_zero"),
        );

        let rem_by_zero_block =
            LLVMAppendBasicBlockInContext(tcx.llx, fcx.function, libcstr!("rem_by_zero"));

        let valid_rem_block =
            LLVMAppendBasicBlockInContext(tcx.llx, fcx.function, libcstr!("valid_rem"));

        LLVMBuildCondBr(
            fcx.builder,
            denom_is_zero,
            rem_by_zero_block,
            valid_rem_block,
        );

        LLVMPositionBuilderAtEnd(fcx.builder, rem_by_zero_block);
        gen_panic(tcx, mcx, fcx, "division by zero");

        LLVMPositionBuilderAtEnd(fcx.builder, valid_rem_block);
        LLVMBuildSRem(fcx.builder, llvm_numer, llvm_denom, libcstr!("rem"))
    }
}

pub(crate) fn gen_checked_int_div(
    tcx: &mut TargetCtx,
    mcx: &mut ModCtx<'_, '_, '_>,
    fcx: &mut FunCtx,
    llvm_numer: LLVMValueRef,
    llvm_denom: LLVMValueRef,
) -> LLVMValueRef {
    unsafe {
        let llvm_i64 = LLVMInt64TypeInContext(tcx.llx);

        // Build our blocks
        let div_by_zero_block =
            LLVMAppendBasicBlockInContext(tcx.llx, fcx.function, libcstr!("div_by_zero"));

        let non_zero_denom_block =
            LLVMAppendBasicBlockInContext(tcx.llx, fcx.function, libcstr!("non_zero_denom"));

        let neg_one_denom_block =
            LLVMAppendBasicBlockInContext(tcx.llx, fcx.function, libcstr!("neg_one_denom"));

        let valid_div_block =
            LLVMAppendBasicBlockInContext(tcx.llx, fcx.function, libcstr!("valid_div_block"));

        // Test if the denominator is 0
        let denom_is_zero = LLVMBuildICmp(
            fcx.builder,
            LLVMIntPredicate::LLVMIntEQ,
            llvm_denom,
            LLVMConstInt(llvm_i64, 0, 0),
            libcstr!("denom_is_zero"),
        );

        // If the denominator 0 then raise a divide by zero error
        LLVMBuildCondBr(
            fcx.builder,
            denom_is_zero,
            div_by_zero_block,
            non_zero_denom_block,
        );

        // Test if the denominator is -1
        LLVMPositionBuilderAtEnd(fcx.builder, non_zero_denom_block);

        let denom_is_neg_one = LLVMBuildICmp(
            fcx.builder,
            LLVMIntPredicate::LLVMIntEQ,
            llvm_denom,
            LLVMConstInt(llvm_i64, std::mem::transmute(-1i64), 0),
            libcstr!("denom_is_neg_one"),
        );

        // If the denominator in -1 then we need to test the numerator
        LLVMBuildCondBr(
            fcx.builder,
            denom_is_neg_one,
            neg_one_denom_block,
            valid_div_block,
        );

        // Test if the numerator is i64::MIN
        LLVMPositionBuilderAtEnd(fcx.builder, neg_one_denom_block);

        let numer_is_int_min = LLVMBuildICmp(
            fcx.builder,
            LLVMIntPredicate::LLVMIntEQ,
            llvm_numer,
            LLVMConstInt(llvm_i64, std::mem::transmute(std::i64::MIN), 0),
            libcstr!("numer_is_int_min"),
        );

        // If denominator is -1 and numerator is i64::MIN then raise a divide by zero error
        LLVMBuildCondBr(
            fcx.builder,
            numer_is_int_min,
            div_by_zero_block,
            valid_div_block,
        );

        // Build the common panic block
        LLVMPositionBuilderAtEnd(fcx.builder, div_by_zero_block);
        gen_panic(tcx, mcx, fcx, "division by zero");

        LLVMPositionBuilderAtEnd(fcx.builder, valid_div_block);
        LLVMBuildSDiv(fcx.builder, llvm_numer, llvm_denom, libcstr!("quot"))
    }
}

pub(crate) fn gen_float_sqrt(
    tcx: &mut TargetCtx,
    mcx: &mut ModCtx<'_, '_, '_>,
    fcx: &mut FunCtx,
    llvm_radicand: LLVMValueRef,
) -> LLVMValueRef {
    unsafe {
        let llvm_double = LLVMDoubleTypeInContext(tcx.llx);

        let llvm_param_types = &mut [llvm_double];

        let double_sqrt_llvm_type = LLVMFunctionType(
            llvm_double,
            llvm_param_types.as_mut_ptr(),
            llvm_param_types.len() as u32,
            0,
        );

        let double_sqrt_fun =
            mcx.get_function_or_insert(double_sqrt_llvm_type, b"llvm.sqrt.f64\0", |_| {});

        let llvm_sqrt_args = &mut [llvm_radicand];

        LLVMBuildCall(
            fcx.builder,
            double_sqrt_fun,
            llvm_sqrt_args.as_mut_ptr(),
            llvm_sqrt_args.len() as u32,
            libcstr!("sqrt"),
        )
    }
}
