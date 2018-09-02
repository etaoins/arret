pub mod convert;
pub mod portal;

use std::collections::HashMap;

use llvm_sys::core::*;
use llvm_sys::execution_engine::*;
use llvm_sys::prelude::*;
use llvm_sys::target::*;

use runtime::abitype;

pub struct CodegenCtx {
    llx: LLVMContextRef,
    task_type: Option<LLVMTypeRef>,
    boxed_abi_types: HashMap<abitype::BoxedABIType, LLVMTypeRef>,
}

impl CodegenCtx {
    pub fn new() -> CodegenCtx {
        unsafe {
            let llx = LLVMContextCreate();

            LLVMLinkInMCJIT();
            LLVM_InitializeNativeTarget();
            LLVM_InitializeNativeAsmPrinter();

            CodegenCtx {
                llx,
                task_type: None,
                boxed_abi_types: HashMap::new(),
            }
        }
    }

    fn task_llvm_type(&mut self) -> LLVMTypeRef {
        let llx = self.llx;
        *self.task_type.get_or_insert_with(|| unsafe {
            LLVMPointerType(
                LLVMStructCreateNamed(llx, b"task\0".as_ptr() as *const _),
                0,
            )
        })
    }

    fn boxed_abi_to_llvm_type(&mut self, boxed_abi_type: &abitype::BoxedABIType) -> LLVMTypeRef {
        if let Some(llvm_type) = self.boxed_abi_types.get(boxed_abi_type) {
            return *llvm_type;
        }

        unsafe {
            let llvm_i8 = LLVMInt8TypeInContext(self.llx);
            let mut members = vec![llvm_i8, llvm_i8];

            let type_name = if let abitype::BoxedABIType::Pair(member) = boxed_abi_type {
                let llvm_any_ptr = self.boxed_abi_to_llvm_type(&abitype::BoxedABIType::Any);

                members.push(self.boxed_abi_to_llvm_type(member));
                members.push(llvm_any_ptr);
                members.push(LLVMInt64TypeInContext(self.llx));

                b"pair\0".as_ptr()
            } else {
                b"boxed\0".as_ptr()
            };

            let box_header = LLVMStructCreateNamed(self.llx, type_name as *const _);
            LLVMStructSetBody(box_header, members.as_mut_ptr(), members.len() as u32, 0);

            let llvm_type = LLVMPointerType(box_header, 0);

            self.boxed_abi_types
                .insert(boxed_abi_type.clone(), llvm_type);

            llvm_type
        }
    }

    fn abi_to_llvm_type(&mut self, abi_type: &abitype::ABIType) -> LLVMTypeRef {
        unsafe {
            match abi_type {
                abitype::ABIType::Bool => LLVMInt8TypeInContext(self.llx),
                abitype::ABIType::Int => LLVMInt64TypeInContext(self.llx),
                abitype::ABIType::Boxed(boxed) => self.boxed_abi_to_llvm_type(boxed),
                other => {
                    unimplemented!("ABI type: {:?}", other);
                }
            }
        }
    }

    fn ret_abi_to_llvm_type(&mut self, ret_abi_type: &abitype::RetABIType) -> LLVMTypeRef {
        match ret_abi_type {
            abitype::RetABIType::Inhabited(abi_type) => self.abi_to_llvm_type(abi_type),
            abitype::RetABIType::Void | abitype::RetABIType::Never => unsafe { LLVMVoidType() },
        }
    }

    fn function_to_llvm_type(
        &mut self,
        takes_task: bool,
        arg_types: &[abitype::ABIType],
        ret_type: &abitype::RetABIType,
    ) -> LLVMTypeRef {
        let mut llvm_arg_types = vec![];

        if takes_task {
            llvm_arg_types.push(self.task_llvm_type());
        }

        llvm_arg_types.extend(
            arg_types
                .iter()
                .map(|abi_type| self.abi_to_llvm_type(abi_type)),
        );

        let llvm_ret_type = self.ret_abi_to_llvm_type(ret_type);

        unsafe {
            LLVMFunctionType(
                llvm_ret_type,
                llvm_arg_types.as_mut_ptr(),
                llvm_arg_types.len() as u32,
                0,
            )
        }
    }
}

impl Drop for CodegenCtx {
    fn drop(&mut self) {
        unsafe {
            LLVMContextDispose(self.llx);
        }
    }
}
