mod convert;
pub mod fun_abi;
mod fun_gen;
pub mod jit;
pub mod portal;
pub(crate) mod program;
mod target;

use std::collections::HashMap;
use std::ffi::CStr;

use llvm_sys::core::*;
use llvm_sys::prelude::*;
use llvm_sys::LLVMLinkage;

use runtime::abitype::{ABIType, BoxedABIType, RetABIType};
use runtime::boxed;

pub struct CodegenCtx {
    llx: LLVMContextRef,

    task_type: Option<LLVMTypeRef>,
    boxed_abi_types: HashMap<BoxedABIType, LLVMTypeRef>,
}

impl CodegenCtx {
    pub fn new() -> CodegenCtx {
        unsafe {
            let llx = LLVMContextCreate();

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

    fn boxed_abi_to_llvm_struct_type(&mut self, boxed_abi_type: &BoxedABIType) -> LLVMTypeRef {
        if let Some(llvm_struct) = self.boxed_abi_types.get(boxed_abi_type) {
            return *llvm_struct;
        }

        unsafe {
            let llvm_i8 = LLVMInt8TypeInContext(self.llx);
            let mut members = vec![llvm_i8, llvm_i8];

            let type_name = match boxed_abi_type {
                BoxedABIType::DirectTagged(boxed::TypeTag::Int) => {
                    members.push(LLVMInt64TypeInContext(self.llx));
                    b"boxed_int\0".as_ptr()
                }
                BoxedABIType::Pair(member) => {
                    let llvm_any_ptr = self.boxed_abi_to_llvm_ptr_type(&BoxedABIType::Any);

                    members.push(self.boxed_abi_to_llvm_ptr_type(member));
                    members.push(llvm_any_ptr);
                    members.push(LLVMInt64TypeInContext(self.llx));

                    b"pair\0".as_ptr()
                }
                _ => b"boxed\0".as_ptr(),
            };

            let llvm_type = LLVMStructCreateNamed(self.llx, type_name as *const _);
            LLVMStructSetBody(llvm_type, members.as_mut_ptr(), members.len() as u32, 0);

            self.boxed_abi_types
                .insert(boxed_abi_type.clone(), llvm_type);

            llvm_type
        }
    }

    fn boxed_abi_to_llvm_ptr_type(&mut self, boxed_abi_type: &BoxedABIType) -> LLVMTypeRef {
        unsafe { LLVMPointerType(self.boxed_abi_to_llvm_struct_type(boxed_abi_type), 0) }
    }

    fn abi_to_llvm_type(&mut self, abi_type: &ABIType) -> LLVMTypeRef {
        unsafe {
            match abi_type {
                ABIType::Bool => LLVMInt8TypeInContext(self.llx),
                ABIType::Int => LLVMInt64TypeInContext(self.llx),
                ABIType::Boxed(boxed) => self.boxed_abi_to_llvm_ptr_type(boxed),
                other => {
                    unimplemented!("ABI type: {:?}", other);
                }
            }
        }
    }

    fn ret_abi_to_llvm_type(&mut self, ret_abi_type: &RetABIType) -> LLVMTypeRef {
        match ret_abi_type {
            RetABIType::Inhabited(abi_type) => self.abi_to_llvm_type(abi_type),
            RetABIType::Void | RetABIType::Never => unsafe { LLVMVoidType() },
        }
    }

    fn function_to_llvm_type(
        &mut self,
        takes_task: bool,
        arg_types: &[ABIType],
        ret_type: &RetABIType,
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

    fn ptr_to_singleton_box(
        &mut self,
        module: LLVMModuleRef,
        boxed_abi_type: &BoxedABIType,
        name: &[u8],
    ) -> LLVMValueRef {
        unsafe {
            let global = LLVMGetNamedGlobal(module, name.as_ptr() as *const _);
            if !global.is_null() {
                return global;
            }

            let llvm_any = self.boxed_abi_to_llvm_struct_type(boxed_abi_type);
            LLVMAddGlobal(module, llvm_any, name.as_ptr() as *const _)
        }
    }

    fn get_global_or_insert<F>(
        &mut self,
        module: LLVMModuleRef,
        llvm_type: LLVMTypeRef,
        name: &CStr,
        mut initial_value: F,
    ) -> LLVMValueRef
    where
        F: FnMut(&mut CodegenCtx) -> LLVMValueRef,
    {
        unsafe {
            let global = LLVMGetNamedGlobal(module, name.as_ptr() as *const _);

            if !global.is_null() {
                return global;
            }

            let global = LLVMAddGlobal(module, llvm_type, name.as_ptr() as *const _);
            LLVMSetInitializer(global, initial_value(self));

            LLVMSetUnnamedAddr(global, 1);
            LLVMSetLinkage(global, LLVMLinkage::LLVMPrivateLinkage);

            global
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
