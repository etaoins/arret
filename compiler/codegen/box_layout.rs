use llvm_sys::core::*;
use llvm_sys::prelude::*;

use arret_runtime::abitype::{BoxedAbiType, EncodeBoxedAbiType, TOP_LIST_BOXED_ABI_TYPE};
use arret_runtime::boxed;
use arret_runtime::boxed::TypeTag;

use crate::codegen::record_struct;
use crate::codegen::target_gen::TargetCtx;
use crate::codegen::GenAbi;

/// Represents the runtime layout of a boxed data structure
///
/// There are many boxed ABI types that can correspond to the same type name and layout. For
/// example, all boxed pair types currently share a layout.
#[derive(Clone, Hash, PartialEq, Eq)]
pub enum BoxLayout {
    Any,
    Bool,
    Num,
    List,
    Union,
    ConstTagged(boxed::TypeTag),
}

impl BoxLayout {
    /// Returns a NULL terminated type name for the box layout
    ///
    /// This is used to make the LLVM IR more descriptive. Some boxes with identical layouts have
    /// distinct enum values and names for the purposes of making LLVM IR more readable.
    pub fn type_name(&self) -> &'static [u8] {
        match self {
            BoxLayout::Any => b"boxed_any\0",
            BoxLayout::Bool => b"boxed_bool\0",
            BoxLayout::Num => b"boxed_num\0",
            BoxLayout::List => b"boxed_list\0",
            BoxLayout::Union => b"boxed_union\0",
            BoxLayout::ConstTagged(TypeTag::Nil) => b"boxed_nil\0",
            BoxLayout::ConstTagged(TypeTag::True) => b"boxed_true\0",
            BoxLayout::ConstTagged(TypeTag::False) => b"boxed_false\0",
            BoxLayout::ConstTagged(TypeTag::Int) => b"boxed_int\0",
            BoxLayout::ConstTagged(TypeTag::Float) => b"boxed_float\0",
            BoxLayout::ConstTagged(TypeTag::Char) => b"boxed_char\0",
            BoxLayout::ConstTagged(TypeTag::Set) => b"boxed_set\0",
            BoxLayout::ConstTagged(TypeTag::Str) => b"boxed_str\0",
            BoxLayout::ConstTagged(TypeTag::Sym) => b"boxed_sym\0",
            BoxLayout::ConstTagged(TypeTag::FunThunk) => b"boxed_fun_thunk\0",
            BoxLayout::ConstTagged(TypeTag::Pair) => b"boxed_pair\0",
            BoxLayout::ConstTagged(TypeTag::Vector) => b"boxed_vector\0",
            BoxLayout::ConstTagged(TypeTag::Record) => b"boxed_record\0",
            BoxLayout::ConstTagged(TypeTag::Map) => b"boxed_map\0",
        }
    }

    /// Appends member types to the passed `Vec`
    ///
    /// This presumes `members` already contains the box header
    pub fn append_members(&self, tcx: &mut TargetCtx, members: &mut Vec<LLVMTypeRef>) {
        unsafe {
            match self {
                BoxLayout::Any => {
                    use std::mem;

                    let llvm_byte = LLVMInt8TypeInContext(tcx.llx);
                    let padding_bytes =
                        mem::size_of::<boxed::Any>() - mem::size_of::<boxed::Header>();

                    members.push(LLVMArrayType(llvm_byte, padding_bytes as u32));
                }
                BoxLayout::ConstTagged(TypeTag::Int) => {
                    members.push(LLVMInt64TypeInContext(tcx.llx));
                }
                BoxLayout::ConstTagged(TypeTag::Float) => {
                    members.push(LLVMDoubleTypeInContext(tcx.llx));
                }
                BoxLayout::ConstTagged(TypeTag::Char) => {
                    members.push(LLVMInt32TypeInContext(tcx.llx));
                }
                BoxLayout::ConstTagged(TypeTag::Str) => {
                    members.push(LLVMInt8TypeInContext(tcx.llx));
                }
                BoxLayout::ConstTagged(TypeTag::Sym) => {
                    members.push(LLVMInt64TypeInContext(tcx.llx));
                }
                BoxLayout::ConstTagged(TypeTag::FunThunk) => {
                    members.extend_from_slice(&[
                        tcx.captures_llvm_type(),
                        LLVMPointerType(tcx.fun_abi_to_llvm_type(&GenAbi::thunk_abi()), 0),
                    ]);
                }
                BoxLayout::ConstTagged(TypeTag::Pair) => {
                    let llvm_i64 = LLVMInt64TypeInContext(tcx.llx);
                    let llvm_any_ptr = tcx.boxed_abi_to_llvm_ptr_type(&BoxedAbiType::Any);
                    let llvm_any_list_ptr =
                        tcx.boxed_abi_to_llvm_ptr_type(&TOP_LIST_BOXED_ABI_TYPE);

                    members.extend_from_slice(&[llvm_i64, llvm_any_ptr, llvm_any_list_ptr]);
                }
                BoxLayout::ConstTagged(TypeTag::Record) => {
                    record_struct::append_common_internal_members(tcx, members);
                }
                BoxLayout::List => {
                    members.push(LLVMInt64TypeInContext(tcx.llx));
                }
                BoxLayout::ConstTagged(TypeTag::Set) => {
                    let llvm_i32 = LLVMInt32TypeInContext(tcx.llx);
                    let llvm_any_ptr = tcx.boxed_abi_to_llvm_ptr_type(&BoxedAbiType::Any);

                    members.extend_from_slice(&[llvm_i32, llvm_any_ptr, llvm_any_ptr, llvm_any_ptr])
                }
                BoxLayout::ConstTagged(TypeTag::Vector) => {
                    // inline_len
                    members.push(LLVMInt32TypeInContext(tcx.llx));
                }
                BoxLayout::ConstTagged(TypeTag::Nil)
                | BoxLayout::ConstTagged(TypeTag::True)
                | BoxLayout::ConstTagged(TypeTag::False)
                | BoxLayout::ConstTagged(TypeTag::Map)
                | BoxLayout::Bool
                | BoxLayout::Num
                | BoxLayout::Union => {}
            };
        }
    }
}

impl From<&BoxedAbiType> for BoxLayout {
    fn from(boxed_abi_type: &BoxedAbiType) -> BoxLayout {
        match boxed_abi_type {
            BoxedAbiType::Any => BoxLayout::Any,

            BoxedAbiType::List(_) => BoxLayout::List,
            &boxed::Num::BOXED_ABI_TYPE => BoxLayout::Num,
            &boxed::Bool::BOXED_ABI_TYPE => BoxLayout::Bool,
            BoxedAbiType::Union(_, _) => BoxLayout::Union,

            BoxedAbiType::UniqueTagged(type_tag) => BoxLayout::ConstTagged(*type_tag),
            BoxedAbiType::Pair(_) => BoxLayout::ConstTagged(TypeTag::Pair),
            BoxedAbiType::Set(_) => BoxLayout::ConstTagged(TypeTag::Set),
            BoxedAbiType::Vector(_) => BoxLayout::ConstTagged(TypeTag::Vector),
            BoxedAbiType::Map(_, _) => BoxLayout::ConstTagged(TypeTag::Map),
        }
    }
}
