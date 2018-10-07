mod alloc;
mod callee_gen;
mod const_gen;
mod escape_analysis;
mod fun_gen;
pub mod jit;
mod mod_gen;
pub(crate) mod program;
mod target;

use std::collections::HashMap;

use llvm_sys::core::*;
use llvm_sys::prelude::*;
use llvm_sys::{LLVMAttributeReturnIndex, LLVMLinkage};

use runtime::abitype::{ABIType, BoxedABIType, RetABIType, TOP_LIST_BOXED_ABI_TYPE};
use runtime::boxed;

use crate::mir::ops;

fn llvm_enum_attr_for_name(
    llx: LLVMContextRef,
    attr_name: &[u8],
    attr_value: u64,
) -> LLVMAttributeRef {
    unsafe {
        let kind_id =
            LLVMGetEnumAttributeKindForName(attr_name.as_ptr() as *const _, attr_name.len());
        LLVMCreateEnumAttribute(llx, kind_id, attr_value)
    }
}

pub struct CodegenCtx {
    llx: LLVMContextRef,

    task_type: Option<LLVMTypeRef>,
    record_type: Option<LLVMTypeRef>,
    box_header_type: Option<LLVMTypeRef>,
    boxed_inline_str_type: Option<LLVMTypeRef>,
    boxed_abi_types: HashMap<BoxedABIType, LLVMTypeRef>,

    boxed_dereferenceable_attr: LLVMAttributeRef,
    boxed_align_attr: LLVMAttributeRef,
    readonly_attr: LLVMAttributeRef,
    noalias_attr: LLVMAttributeRef,
    nocapture_attr: LLVMAttributeRef,
}

impl CodegenCtx {
    pub fn new() -> CodegenCtx {
        use std::mem;

        unsafe {
            let llx = LLVMContextCreate();

            CodegenCtx {
                llx,

                task_type: None,
                record_type: None,
                box_header_type: None,
                boxed_inline_str_type: None,
                boxed_abi_types: HashMap::new(),

                boxed_dereferenceable_attr: llvm_enum_attr_for_name(
                    llx,
                    b"dereferenceable",
                    mem::size_of::<boxed::Any>() as u64,
                ),
                boxed_align_attr: llvm_enum_attr_for_name(
                    llx,
                    b"align",
                    mem::align_of::<boxed::Any>() as u64,
                ),
                readonly_attr: llvm_enum_attr_for_name(llx, b"readonly", 0),
                noalias_attr: llvm_enum_attr_for_name(llx, b"noalias", 0),
                nocapture_attr: llvm_enum_attr_for_name(llx, b"nocapture", 0),
            }
        }
    }

    fn task_llvm_type(&mut self) -> LLVMTypeRef {
        let llvm_any_ptr = self.boxed_abi_to_llvm_ptr_type(&BoxedABIType::Any);
        let llx = self.llx;
        *self.task_type.get_or_insert_with(|| unsafe {
            let members = &mut [llvm_any_ptr, llvm_any_ptr];

            let llvm_type = LLVMStructCreateNamed(llx, b"task\0".as_ptr() as *const _);
            LLVMStructSetBody(llvm_type, members.as_mut_ptr(), members.len() as u32, 0);

            LLVMPointerType(llvm_type, 0)
        })
    }

    fn record_llvm_type(&mut self) -> LLVMTypeRef {
        let llx = self.llx;
        *self.record_type.get_or_insert_with(|| unsafe {
            LLVMPointerType(
                LLVMStructCreateNamed(llx, b"record\0".as_ptr() as *const _),
                0,
            )
        })
    }

    fn box_header_llvm_type(&mut self) -> LLVMTypeRef {
        let llx = self.llx;
        *self.box_header_type.get_or_insert_with(|| unsafe {
            let llvm_i8 = LLVMInt8TypeInContext(llx);
            let members = &mut [llvm_i8, llvm_i8];

            let llvm_type = LLVMStructCreateNamed(llx, b"box_header\0".as_ptr() as *const _);
            LLVMStructSetBody(llvm_type, members.as_mut_ptr(), members.len() as u32, 0);

            llvm_type
        })
    }

    fn boxed_inline_str_llvm_type(&mut self) -> LLVMTypeRef {
        let llx = self.llx;
        let llvm_header = self.box_header_llvm_type();

        *self.boxed_inline_str_type.get_or_insert_with(|| unsafe {
            let llvm_i8 = LLVMInt8TypeInContext(llx);
            let members = &mut [
                llvm_header,
                llvm_i8,
                LLVMArrayType(llvm_i8, boxed::Str::MAX_INLINE_BYTES as u32),
            ];

            let llvm_type = LLVMStructCreateNamed(llx, b"boxed_inline_str\0".as_ptr() as *const _);
            LLVMStructSetBody(llvm_type, members.as_mut_ptr(), members.len() as u32, 0);

            llvm_type
        })
    }

    fn boxed_abi_to_llvm_struct_type(&mut self, boxed_abi_type: &BoxedABIType) -> LLVMTypeRef {
        if let Some(llvm_struct) = self.boxed_abi_types.get(boxed_abi_type) {
            return *llvm_struct;
        }

        unsafe {
            let llvm_header = self.box_header_llvm_type();
            let mut members = vec![llvm_header];

            let type_name = match boxed_abi_type {
                BoxedABIType::Any => b"any\0".as_ptr(),
                BoxedABIType::DirectTagged(boxed::TypeTag::Nil) => b"nil\0".as_ptr(),
                BoxedABIType::DirectTagged(boxed::TypeTag::Int) => {
                    members.push(LLVMInt64TypeInContext(self.llx));
                    b"boxed_int\0".as_ptr()
                }
                BoxedABIType::DirectTagged(boxed::TypeTag::Str) => {
                    members.push(LLVMInt8TypeInContext(self.llx));
                    b"boxed_str\0".as_ptr()
                }
                BoxedABIType::DirectTagged(boxed::TypeTag::FunThunk) => {
                    members.push(self.record_llvm_type());
                    members.push(LLVMPointerType(
                        self.fun_abi_to_llvm_type(&ops::FunABI::thunk_abi()),
                        0,
                    ));
                    b"boxed_fun_thunk\0".as_ptr()
                }
                BoxedABIType::Pair(_) | BoxedABIType::DirectTagged(boxed::TypeTag::TopPair) => {
                    let llvm_any_ptr = self.boxed_abi_to_llvm_ptr_type(&BoxedABIType::Any);
                    let llvm_any_list_ptr =
                        self.boxed_abi_to_llvm_ptr_type(&TOP_LIST_BOXED_ABI_TYPE);

                    members.push(LLVMInt64TypeInContext(self.llx));
                    members.push(llvm_any_ptr);
                    members.push(llvm_any_list_ptr);

                    b"pair\0".as_ptr()
                }
                BoxedABIType::List(_) => {
                    members.push(LLVMInt64TypeInContext(self.llx));
                    b"list\0".as_ptr()
                }
                _ => b"opaque_boxed\0".as_ptr(),
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
                ABIType::Bool => LLVMInt1TypeInContext(self.llx),
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
            RetABIType::Void | RetABIType::Never => unsafe { LLVMVoidTypeInContext(self.llx) },
        }
    }

    fn fun_abi_to_llvm_type(&mut self, fun_abi: &ops::FunABI) -> LLVMTypeRef {
        let mut llvm_param_types = vec![];

        if fun_abi.takes_task {
            llvm_param_types.push(self.task_llvm_type());
        }

        if fun_abi.takes_closure {
            llvm_param_types.push(self.record_llvm_type());
        }

        llvm_param_types.extend(
            fun_abi
                .params
                .iter()
                .map(|abi_type| self.abi_to_llvm_type(abi_type)),
        );

        let llvm_ret_type = self.ret_abi_to_llvm_type(&fun_abi.ret);

        unsafe {
            LLVMFunctionType(
                llvm_ret_type,
                llvm_param_types.as_mut_ptr(),
                llvm_param_types.len() as u32,
                0,
            )
        }
    }

    fn ptr_to_singleton_box(
        &mut self,
        module: LLVMModuleRef,
        type_tag: boxed::TypeTag,
        name: &[u8],
    ) -> LLVMValueRef {
        use std::mem;

        unsafe {
            let global = LLVMGetNamedGlobal(module, name.as_ptr() as *const _);
            if !global.is_null() {
                return global;
            }

            let llvm_type = self.boxed_abi_to_llvm_struct_type(&type_tag.into());
            let global = LLVMAddGlobal(module, llvm_type, name.as_ptr() as *const _);

            let members = &mut [self.llvm_box_header(type_tag.into_const_header())];

            let llvm_value =
                LLVMConstNamedStruct(llvm_type, members.as_mut_ptr(), members.len() as u32);

            LLVMSetInitializer(global, llvm_value);
            LLVMSetAlignment(global, mem::align_of::<boxed::Any>() as u32);
            LLVMSetGlobalConstant(global, 1 as i32);
            LLVMSetLinkage(global, LLVMLinkage::LLVMAvailableExternallyLinkage);

            global
        }
    }

    fn llvm_box_header(&mut self, header: boxed::Header) -> LLVMValueRef {
        unsafe {
            let llvm_i8 = LLVMInt8TypeInContext(self.llx);
            let llvm_type = self.box_header_llvm_type();

            let members = &mut [
                LLVMConstInt(llvm_i8, header.type_tag() as u64, 0),
                LLVMConstInt(llvm_i8, header.alloc_type() as u64, 0),
            ];

            LLVMConstNamedStruct(llvm_type, members.as_mut_ptr(), members.len() as u32)
        }
    }

    fn llvm_enum_attr_for_name(&mut self, attr_name: &[u8], attr_value: u64) -> LLVMAttributeRef {
        llvm_enum_attr_for_name(self.llx, attr_name, attr_value)
    }

    fn add_boxed_param_attrs(
        &mut self,
        function: LLVMValueRef,
        param_index: u32,
        no_capture: bool,
    ) {
        unsafe {
            for &common_attr in &[
                self.boxed_dereferenceable_attr,
                self.boxed_align_attr,
                self.readonly_attr,
                self.noalias_attr,
            ] {
                LLVMAddAttributeAtIndex(function, param_index, common_attr);
            }

            if no_capture {
                LLVMAddAttributeAtIndex(function, param_index, self.nocapture_attr);
            }
        }
    }

    fn add_boxed_return_attrs(&mut self, function: LLVMValueRef) {
        unsafe {
            for &common_attr in &[
                self.boxed_dereferenceable_attr,
                self.boxed_align_attr,
                self.noalias_attr,
            ] {
                LLVMAddAttributeAtIndex(function, LLVMAttributeReturnIndex, common_attr);
            }
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
