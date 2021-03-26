use std::collections::HashMap;

use llvm_sys::core::*;
use llvm_sys::prelude::*;
use llvm_sys::target::*;
use llvm_sys::target_machine::*;
use llvm_sys::{LLVMAttributeReturnIndex, LLVMLinkage};

use arret_runtime::abitype::{AbiType, BoxedAbiType, RetAbiType};
use arret_runtime::boxed;
use arret_runtime::callback::EntryPointAbiType as CallbackEntryPointAbiType;

use crate::codegen::box_layout::BoxLayout;
use crate::codegen::record_struct;
use crate::codegen::GenAbi;
use crate::libcstr;
use crate::mir::ops;

fn llvm_enum_attr_for_name(
    llx: LLVMContextRef,
    attr_name: &str,
    attr_value: u64,
) -> LLVMAttributeRef {
    unsafe {
        let kind_id =
            LLVMGetEnumAttributeKindForName(attr_name.as_ptr() as *const _, attr_name.len());
        LLVMCreateEnumAttribute(llx, kind_id, attr_value)
    }
}

fn llvm_md_kind_id_for_name(llx: LLVMContextRef, md_name: &str) -> u32 {
    unsafe { LLVMGetMDKindIDInContext(llx, md_name.as_ptr() as *const _, md_name.len() as u32) }
}

fn llvm_i64_md_node(llx: LLVMContextRef, values: &[u64]) -> LLVMMetadataRef {
    unsafe {
        let llvm_i64 = LLVMInt64TypeInContext(llx);

        let mut node_values: Vec<LLVMMetadataRef> = values
            .iter()
            .map(|value| LLVMConstInt(llvm_i64, *value as u64, 0))
            .map(|value| LLVMValueAsMetadata(value))
            .collect();

        LLVMMDNodeInContext2(llx, node_values.as_mut_ptr(), node_values.len())
    }
}

#[derive(Default)]
struct CachedTypes {
    task: Option<LLVMTypeRef>,
    box_header: Option<LLVMTypeRef>,

    boxed: HashMap<BoxLayout, LLVMTypeRef>,

    shared_str: Option<LLVMTypeRef>,
    boxed_inline_str: Option<LLVMTypeRef>,
    boxed_external_str: Option<LLVMTypeRef>,

    persistent_vector_leaf: Option<LLVMTypeRef>,
    boxed_inline_vector: Option<LLVMTypeRef>,
    boxed_external_vector: Option<LLVMTypeRef>,

    global_interned_name: Option<LLVMTypeRef>,

    record_struct_box: HashMap<ops::RecordStructId, LLVMTypeRef>,
    classmap_field: Option<LLVMTypeRef>,
}

/// Context for building against a given target machine
///
/// During compilation there will typically be two instances of `TargetCtx`: one for the eval JIT
/// and one for generating the program.
///
/// This has a number of responsibilities:
///
/// 1. Storing information about the target machine and its data layout
/// 2. Wrapping the global `LLVMContextRef`
/// 3. Caching complex types, attributes and metadata nodes
/// 4. Optimising modules
///
/// These are only vaguely related; this is a bit of a God Object.
pub struct TargetCtx {
    pub llx: LLVMContextRef,
    target_machine: LLVMTargetMachineRef,
    target_data: LLVMTargetDataRef,

    optimising: bool,
    module_pass_manager: LLVMPassManagerRef,

    boxed_dereferenceable_attr: LLVMAttributeRef,
    boxed_align_attr: LLVMAttributeRef,
    readonly_attr: LLVMAttributeRef,
    noalias_attr: LLVMAttributeRef,
    nocapture_attr: LLVMAttributeRef,

    invariant_load_md_kind_id: u32,
    dereferenceable_md_kind_id: u32,
    align_md_kind_id: u32,

    empty_md_node: LLVMMetadataRef,
    boxed_dereferenceable_md_node: LLVMMetadataRef,
    boxed_align_md_node: LLVMMetadataRef,

    cached_types: CachedTypes,
    target_record_structs: HashMap<ops::RecordStructId, record_struct::TargetRecordStruct>,
}

impl TargetCtx {
    /// Construct a new `TargetCtx`
    ///
    /// `target_machine` remains owned by the caller and must outlive this instance.
    pub fn new(target_machine: LLVMTargetMachineRef, optimising: bool) -> TargetCtx {
        use llvm_sys::transforms::pass_manager_builder::*;
        use std::mem;

        unsafe {
            let llx = LLVMContextCreate();
            let module_pass_manager = LLVMCreatePassManager();
            let target_data = LLVMCreateTargetDataLayout(target_machine);

            if optimising {
                let fpmb = LLVMPassManagerBuilderCreate();
                LLVMPassManagerBuilderSetOptLevel(fpmb, 2);
                LLVMPassManagerBuilderPopulateModulePassManager(fpmb, module_pass_manager);
                LLVMPassManagerBuilderDispose(fpmb);
            }

            TargetCtx {
                llx,
                target_machine,
                target_data,

                optimising,
                module_pass_manager,

                boxed_dereferenceable_attr: llvm_enum_attr_for_name(
                    llx,
                    "dereferenceable",
                    mem::size_of::<boxed::Any>() as u64,
                ),
                boxed_align_attr: llvm_enum_attr_for_name(
                    llx,
                    "align",
                    mem::align_of::<boxed::Any>() as u64,
                ),
                readonly_attr: llvm_enum_attr_for_name(llx, "readonly", 0),
                noalias_attr: llvm_enum_attr_for_name(llx, "noalias", 0),
                nocapture_attr: llvm_enum_attr_for_name(llx, "nocapture", 0),

                invariant_load_md_kind_id: llvm_md_kind_id_for_name(llx, "invariant.load"),
                dereferenceable_md_kind_id: llvm_md_kind_id_for_name(llx, "dereferenceable"),
                align_md_kind_id: llvm_md_kind_id_for_name(llx, "align"),

                empty_md_node: llvm_i64_md_node(llx, &[]),
                boxed_dereferenceable_md_node: llvm_i64_md_node(
                    llx,
                    &[mem::size_of::<boxed::Any>() as u64],
                ),
                boxed_align_md_node: llvm_i64_md_node(llx, &[mem::align_of::<boxed::Any>() as u64]),

                cached_types: Default::default(),
                target_record_structs: HashMap::new(),
            }
        }
    }

    pub fn optimising(&self) -> bool {
        self.optimising
    }

    pub fn target_machine(&self) -> LLVMTargetMachineRef {
        self.target_machine
    }

    pub fn target_data(&self) -> LLVMTargetDataRef {
        self.target_data
    }

    pub fn task_llvm_ptr_type(&mut self) -> LLVMTypeRef {
        let llvm_any_ptr = self.boxed_abi_to_llvm_ptr_type(&BoxedAbiType::Any);
        let llx = self.llx;
        *self.cached_types.task.get_or_insert_with(|| unsafe {
            let members = &mut [llvm_any_ptr, llvm_any_ptr];

            let llvm_type = LLVMStructCreateNamed(llx, libcstr!("task"));
            LLVMStructSetBody(llvm_type, members.as_mut_ptr(), members.len() as u32, 0);

            LLVMPointerType(llvm_type, 0)
        })
    }

    pub fn global_interned_name_llvm_type(&mut self) -> LLVMTypeRef {
        let llx = self.llx;

        *self
            .cached_types
            .global_interned_name
            .get_or_insert_with(|| unsafe {
                let llvm_i64 = LLVMInt64TypeInContext(llx);
                let llvm_i8 = LLVMInt8TypeInContext(llx);
                let members = &mut [llvm_i64, LLVMPointerType(llvm_i8, 0)];

                let llvm_type = LLVMStructCreateNamed(llx, libcstr!("global_interned_name"));
                LLVMStructSetBody(llvm_type, members.as_mut_ptr(), members.len() as u32, 0);

                llvm_type
            })
    }

    pub fn classmap_field_llvm_type(&mut self) -> LLVMTypeRef {
        let llx = self.llx;

        *self
            .cached_types
            .classmap_field
            .get_or_insert_with(|| unsafe {
                let llvm_i32 = LLVMInt32TypeInContext(llx);
                let llvm_i8 = LLVMInt8TypeInContext(llx);
                let members = &mut [llvm_i32, llvm_i8, llvm_i8];

                let llvm_type = LLVMStructCreateNamed(llx, libcstr!("classmap_field"));
                LLVMStructSetBody(llvm_type, members.as_mut_ptr(), members.len() as u32, 0);

                llvm_type
            })
    }

    pub fn classmap_class_llvm_type(&mut self) -> LLVMTypeRef {
        unsafe { LLVMPointerType(self.classmap_field_llvm_type(), 0) }
    }

    pub fn captures_llvm_type(&mut self) -> LLVMTypeRef {
        self.boxed_abi_to_llvm_ptr_type(&BoxedAbiType::Any)
    }

    pub fn record_class_id_llvm_type(&self) -> LLVMTypeRef {
        unsafe { LLVMInt32TypeInContext(self.llx) }
    }

    fn box_header_llvm_type(&mut self) -> LLVMTypeRef {
        let llx = self.llx;
        *self.cached_types.box_header.get_or_insert_with(|| unsafe {
            let llvm_i8 = LLVMInt8TypeInContext(llx);
            let members = &mut [llvm_i8, llvm_i8];

            let llvm_type = LLVMStructCreateNamed(llx, libcstr!("box_header"));
            LLVMStructSetBody(llvm_type, members.as_mut_ptr(), members.len() as u32, 0);

            llvm_type
        })
    }

    pub fn shared_str_llvm_type(&mut self) -> LLVMTypeRef {
        let llx = self.llx;

        *self.cached_types.shared_str.get_or_insert_with(|| unsafe {
            let llvm_i8 = LLVMInt8TypeInContext(llx);
            let llvm_i64 = LLVMInt64TypeInContext(llx);

            let members = &mut [
                // ref_count
                llvm_i64,
                // len
                llvm_i64,
                // data
                LLVMArrayType(llvm_i8, 0),
            ];

            let llvm_type = LLVMStructCreateNamed(llx, libcstr!("shared_str"));
            LLVMStructSetBody(llvm_type, members.as_mut_ptr(), members.len() as u32, 0);

            llvm_type
        })
    }

    pub fn boxed_external_str_llvm_type(&mut self) -> LLVMTypeRef {
        let llx = self.llx;
        let llvm_header = self.box_header_llvm_type();
        let shared_str_llvm_type = self.shared_str_llvm_type();

        *self
            .cached_types
            .boxed_external_str
            .get_or_insert_with(|| unsafe {
                let llvm_i8 = LLVMInt8TypeInContext(llx);
                let members = &mut [
                    llvm_header,
                    llvm_i8,
                    LLVMPointerType(shared_str_llvm_type, 0),
                ];

                let llvm_type = LLVMStructCreateNamed(llx, libcstr!("boxed_external_str"));
                LLVMStructSetBody(llvm_type, members.as_mut_ptr(), members.len() as u32, 0);

                llvm_type
            })
    }

    pub fn boxed_inline_str_llvm_type(&mut self) -> LLVMTypeRef {
        let llx = self.llx;
        let llvm_header = self.box_header_llvm_type();

        *self
            .cached_types
            .boxed_inline_str
            .get_or_insert_with(|| unsafe {
                let llvm_i8 = LLVMInt8TypeInContext(llx);
                let members = &mut [
                    llvm_header,
                    llvm_i8,
                    LLVMArrayType(llvm_i8, boxed::Str::MAX_INLINE_BYTES as u32),
                ];

                let llvm_type = LLVMStructCreateNamed(llx, libcstr!("boxed_inline_str"));
                LLVMStructSetBody(llvm_type, members.as_mut_ptr(), members.len() as u32, 0);

                llvm_type
            })
    }

    pub fn persistent_vector_leaf_llvm_type(&mut self) -> LLVMTypeRef {
        use arret_runtime::persistent::vector::NODE_SIZE;

        let llx = self.llx;
        let llvm_any_ptr = self.boxed_abi_to_llvm_ptr_type(&BoxedAbiType::Any);

        *self
            .cached_types
            .persistent_vector_leaf
            .get_or_insert_with(|| unsafe {
                let llvm_i64 = LLVMInt64TypeInContext(llx);

                let mut members = [llvm_i64, LLVMArrayType(llvm_any_ptr, NODE_SIZE as u32)];

                let llvm_type = LLVMStructCreateNamed(llx, libcstr!("persistent_vector_leaf"));
                LLVMStructSetBody(llvm_type, members.as_mut_ptr(), members.len() as u32, 0);

                llvm_type
            })
    }

    pub fn boxed_external_vector_llvm_type(&mut self) -> LLVMTypeRef {
        let llx = self.llx;
        let llvm_header = self.box_header_llvm_type();
        let persistent_vector_leaf_type = self.persistent_vector_leaf_llvm_type();

        *self
            .cached_types
            .boxed_external_vector
            .get_or_insert_with(|| unsafe {
                let llvm_i32 = LLVMInt32TypeInContext(llx);
                let llvm_i64 = LLVMInt64TypeInContext(llx);
                let persistent_vector_leaf_ptr = LLVMPointerType(persistent_vector_leaf_type, 0);

                let members = &mut [
                    llvm_header,
                    llvm_i32,
                    llvm_i64,
                    persistent_vector_leaf_ptr,
                    persistent_vector_leaf_ptr,
                ];

                let llvm_type = LLVMStructCreateNamed(llx, libcstr!("boxed_external_vector"));
                LLVMStructSetBody(llvm_type, members.as_mut_ptr(), members.len() as u32, 0);

                llvm_type
            })
    }

    pub fn boxed_inline_vector_llvm_type(&mut self) -> LLVMTypeRef {
        let llx = self.llx;
        let llvm_header = self.box_header_llvm_type();
        let llvm_any_ptr = self.boxed_abi_to_llvm_ptr_type(&BoxedAbiType::Any);

        *self
            .cached_types
            .boxed_inline_vector
            .get_or_insert_with(|| unsafe {
                let llvm_i32 = LLVMInt32TypeInContext(llx);

                let members = &mut [
                    llvm_header,
                    llvm_i32,
                    llvm_any_ptr,
                    llvm_any_ptr,
                    llvm_any_ptr,
                ];

                let llvm_type = LLVMStructCreateNamed(llx, libcstr!("boxed_inline_vector"));
                LLVMStructSetBody(llvm_type, members.as_mut_ptr(), members.len() as u32, 0);

                llvm_type
            })
    }

    pub fn boxed_abi_to_llvm_struct_type(&mut self, boxed_abi_type: &BoxedAbiType) -> LLVMTypeRef {
        let box_layout: BoxLayout = boxed_abi_type.into();

        if let Some(llvm_struct) = self.cached_types.boxed.get(&box_layout) {
            return *llvm_struct;
        }

        unsafe {
            let llvm_header = self.box_header_llvm_type();
            let mut members = vec![llvm_header];

            box_layout.append_members(self, &mut members);

            let llvm_type =
                LLVMStructCreateNamed(self.llx, box_layout.type_name().as_ptr() as *const _);
            LLVMStructSetBody(llvm_type, members.as_mut_ptr(), members.len() as u32, 0);

            self.cached_types.boxed.insert(box_layout, llvm_type);
            llvm_type
        }
    }

    fn callback_entry_point_llvm_type(
        &mut self,
        entry_point_abi_type: &CallbackEntryPointAbiType,
    ) -> LLVMTypeRef {
        let mut llvm_param_types = vec![
            self.task_llvm_ptr_type(),
            self.boxed_abi_to_llvm_ptr_type(&BoxedAbiType::Any),
        ];

        llvm_param_types.extend(
            entry_point_abi_type
                .params
                .iter()
                .map(|abi_type| self.abi_to_llvm_type(abi_type)),
        );

        let llvm_ret_type = self.ret_abi_to_llvm_type(&entry_point_abi_type.ret);

        unsafe {
            LLVMPointerType(
                LLVMFunctionType(
                    llvm_ret_type,
                    llvm_param_types.as_mut_ptr(),
                    llvm_param_types.len() as u32,
                    0,
                ),
                0,
            )
        }
    }

    pub fn callback_llvm_type(&mut self, entry_point_llvm_type: LLVMTypeRef) -> LLVMTypeRef {
        let mut members = [
            self.boxed_abi_to_llvm_ptr_type(&BoxedAbiType::Any),
            entry_point_llvm_type,
        ];

        unsafe { LLVMStructTypeInContext(self.llx, members.as_mut_ptr(), members.len() as u32, 0) }
    }

    pub fn boxed_abi_to_llvm_ptr_type(&mut self, boxed_abi_type: &BoxedAbiType) -> LLVMTypeRef {
        unsafe { LLVMPointerType(self.boxed_abi_to_llvm_struct_type(boxed_abi_type), 0) }
    }

    pub fn abi_to_llvm_type(&mut self, abi_type: &AbiType) -> LLVMTypeRef {
        unsafe {
            match abi_type {
                AbiType::Bool => LLVMInt1TypeInContext(self.llx),
                AbiType::Int => LLVMInt64TypeInContext(self.llx),
                AbiType::Char => LLVMInt32TypeInContext(self.llx),
                AbiType::Float => LLVMDoubleTypeInContext(self.llx),
                AbiType::InternedSym => LLVMInt64TypeInContext(self.llx),
                AbiType::Boxed(boxed) => self.boxed_abi_to_llvm_ptr_type(boxed),
                AbiType::Callback(entry_point_abi_type) => {
                    let entry_point_llvm_type =
                        self.callback_entry_point_llvm_type(entry_point_abi_type);
                    self.callback_llvm_type(entry_point_llvm_type)
                }
            }
        }
    }

    fn ret_abi_to_llvm_type(&mut self, ret_abi_type: &RetAbiType) -> LLVMTypeRef {
        match ret_abi_type {
            RetAbiType::Inhabited(abi_type) => self.abi_to_llvm_type(abi_type),
            RetAbiType::Void | RetAbiType::Never => unsafe { LLVMVoidTypeInContext(self.llx) },
        }
    }

    pub fn fun_abi_to_llvm_type(&mut self, fun_abi: &GenAbi) -> LLVMTypeRef {
        let mut llvm_param_types = vec![];

        if fun_abi.takes_task {
            llvm_param_types.push(self.task_llvm_ptr_type());
        }

        llvm_param_types.extend(
            fun_abi
                .params
                .iter()
                .map(|param_abi_type| self.abi_to_llvm_type(&param_abi_type.abi_type)),
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

    pub fn target_record_struct<'a>(
        &'a mut self,
        mir_record_struct: &ops::RecordStructId,
    ) -> &'a record_struct::TargetRecordStruct {
        if self.target_record_structs.contains_key(mir_record_struct) {
            return &self.target_record_structs[mir_record_struct];
        }

        let target_record_struct =
            record_struct::TargetRecordStruct::from_mir_record_struct(self, mir_record_struct);

        self.target_record_structs
            .entry(mir_record_struct.clone())
            .or_insert(target_record_struct)
    }

    pub fn record_struct_llvm_box_type(
        &mut self,
        record_struct: &ops::RecordStructId,
    ) -> LLVMTypeRef {
        if let Some(record_struct_box_type) = self.cached_types.record_struct_box.get(record_struct)
        {
            return *record_struct_box_type;
        }

        let record_struct::TargetRecordStruct {
            llvm_data_type,
            record_storage,
            ..
        } = *self.target_record_struct(record_struct);

        let llvm_header = self.box_header_llvm_type();

        let mut members = vec![llvm_header];
        record_struct::append_common_internal_members(self, &mut members);

        match record_storage {
            boxed::RecordStorage::Inline(_) => {
                members.push(llvm_data_type);
            }
            boxed::RecordStorage::External => unsafe {
                members.extend(&[
                    LLVMPointerType(llvm_data_type, 0),
                    LLVMInt64TypeInContext(self.llx),
                ]);
            },
        }

        let box_name = format!("boxed_{}\0", record_struct.source_name);

        let record_struct_box_type = unsafe {
            let record_struct_box_type =
                LLVMStructCreateNamed(self.llx, box_name.as_ptr() as *const _);

            LLVMStructSetBody(
                record_struct_box_type,
                members.as_mut_ptr(),
                members.len() as u32,
                0,
            );

            record_struct_box_type
        };

        self.cached_types
            .record_struct_box
            .insert(record_struct.clone(), record_struct_box_type);

        record_struct_box_type
    }

    pub fn ptr_to_singleton_box(
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

            let members = &mut [self.llvm_box_header(type_tag.to_const_header())];

            let llvm_value =
                LLVMConstNamedStruct(llvm_type, members.as_mut_ptr(), members.len() as u32);

            LLVMSetInitializer(global, llvm_value);
            LLVMSetAlignment(global, mem::align_of::<boxed::Any>() as u32);
            LLVMSetGlobalConstant(global, 1);
            LLVMSetLinkage(global, LLVMLinkage::LLVMAvailableExternallyLinkage);

            global
        }
    }

    pub fn llvm_box_header(&mut self, header: boxed::Header) -> LLVMValueRef {
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

    pub fn llvm_enum_attr_for_name(
        &mut self,
        attr_name: &str,
        attr_value: u64,
    ) -> LLVMAttributeRef {
        llvm_enum_attr_for_name(self.llx, attr_name, attr_value)
    }

    pub fn llvm_md_kind_id_for_name(&mut self, md_name: &str) -> u32 {
        llvm_md_kind_id_for_name(self.llx, md_name)
    }

    pub fn llvm_boxed_align_attr(&self) -> LLVMAttributeRef {
        self.boxed_align_attr
    }

    pub fn llvm_noalias_attr(&self) -> LLVMAttributeRef {
        self.noalias_attr
    }

    pub fn add_invariant_load_metadata(&self, loaded_value: LLVMValueRef) {
        unsafe {
            LLVMSetMetadata(
                loaded_value,
                self.invariant_load_md_kind_id,
                LLVMMetadataAsValue(self.llx, self.empty_md_node),
            );
        }
    }

    /// Adds range metadata to annotate our native `Char` type with valid Unicode codepoint ranges
    pub fn add_char_codepoint_range_metadata(&mut self, llvm_value: LLVMValueRef) {
        unsafe {
            // Valid Unicode codepoints are effectively 21bit values with a hole in the middle
            let llvm_char_type = LLVMInt32TypeInContext(self.llx);
            let mut llvm_range_values: Vec<LLVMMetadataRef> = [0x0000, 0xD800, 0xE000, 0x11_0000]
                .iter()
                .map(|value| LLVMConstInt(llvm_char_type, *value as u64, 0))
                .map(|value| LLVMValueAsMetadata(value))
                .collect();

            let codepoint_range_md = LLVMMDNodeInContext2(
                self.llx,
                llvm_range_values.as_mut_ptr(),
                llvm_range_values.len(),
            );
            let range_md_kind_id = self.llvm_md_kind_id_for_name("range");

            LLVMSetMetadata(
                llvm_value,
                range_md_kind_id,
                LLVMMetadataAsValue(self.llx, codepoint_range_md),
            );
        }
    }

    pub fn add_boxed_param_attrs(
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
                // Parameters are offset by 1
                LLVMAddAttributeAtIndex(function, param_index + 1, common_attr);
            }

            if no_capture {
                LLVMAddAttributeAtIndex(function, param_index + 1, self.nocapture_attr);
            }
        }
    }

    pub fn add_boxed_return_attrs(&mut self, function: LLVMValueRef) {
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

    pub fn add_boxed_load_metadata(&mut self, loaded_value: LLVMValueRef) {
        unsafe {
            for &(kind_id, md_node) in &[
                (
                    self.dereferenceable_md_kind_id,
                    self.boxed_dereferenceable_md_node,
                ),
                (self.align_md_kind_id, self.boxed_align_md_node),
            ] {
                LLVMSetMetadata(
                    loaded_value,
                    kind_id,
                    LLVMMetadataAsValue(self.llx, md_node),
                );
            }
        }
    }

    pub fn finish_module(&mut self, module: LLVMModuleRef) {
        use llvm_sys::analysis::*;
        use std::{env, ptr};

        unsafe {
            let mut error: *mut libc::c_char = ptr::null_mut();

            // Dump
            if env::var_os("ARRET_DUMP_LLVM").is_some() {
                LLVMDumpModule(module);
            }

            // Verify
            LLVMVerifyModule(
                module,
                LLVMVerifierFailureAction::LLVMAbortProcessAction,
                &mut error as *mut _,
            );
            LLVMDisposeMessage(error);

            // Optimise
            LLVMRunPassManager(self.module_pass_manager, module);
        }
    }
}

impl Drop for TargetCtx {
    fn drop(&mut self) {
        unsafe {
            LLVMDisposeTargetData(self.target_data);
            LLVMDisposePassManager(self.module_pass_manager);
            LLVMContextDispose(self.llx);
        }
    }
}
