use llvm_sys::core::*;
use llvm_sys::prelude::*;

use runtime::abitype::{BoxedABIType, EncodeBoxedABIType, TOP_LIST_BOXED_ABI_TYPE};
use runtime::boxed;
use runtime::boxed::TypeTag;

use crate::codegen::target_gen::TargetCtx;
use crate::codegen::GenABI;

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
    DirectTagged(boxed::TypeTag),
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
            BoxLayout::DirectTagged(TypeTag::Nil) => b"boxed_nil\0",
            BoxLayout::DirectTagged(TypeTag::True) => b"boxed_true\0",
            BoxLayout::DirectTagged(TypeTag::False) => b"boxed_false\0",
            BoxLayout::DirectTagged(TypeTag::Int) => b"boxed_int\0",
            BoxLayout::DirectTagged(TypeTag::Float) => b"boxed_float\0",
            BoxLayout::DirectTagged(TypeTag::Char) => b"boxed_char\0",
            BoxLayout::DirectTagged(TypeTag::Str) => b"boxed_str\0",
            BoxLayout::DirectTagged(TypeTag::Sym) => b"boxed_sym\0",
            BoxLayout::DirectTagged(TypeTag::FunThunk) => b"boxed_fun_thunk\0",
            BoxLayout::DirectTagged(TypeTag::TopPair) => b"boxed_pair\0",
            BoxLayout::DirectTagged(TypeTag::TopVector) => b"boxed_vector\0",
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
                BoxLayout::DirectTagged(TypeTag::Int) => {
                    members.push(LLVMInt64TypeInContext(tcx.llx));
                }
                BoxLayout::DirectTagged(TypeTag::Float) => {
                    members.push(LLVMDoubleTypeInContext(tcx.llx));
                }
                BoxLayout::DirectTagged(TypeTag::Char) => {
                    members.push(LLVMInt32TypeInContext(tcx.llx));
                }
                BoxLayout::DirectTagged(TypeTag::Str) => {
                    members.push(LLVMInt8TypeInContext(tcx.llx));
                }
                BoxLayout::DirectTagged(TypeTag::Sym) => {
                    members.push(LLVMInt64TypeInContext(tcx.llx));
                }
                BoxLayout::DirectTagged(TypeTag::FunThunk) => {
                    members.extend_from_slice(&[
                        tcx.closure_llvm_type(),
                        LLVMPointerType(tcx.fun_abi_to_llvm_type(&GenABI::thunk_abi()), 0),
                    ]);
                }
                BoxLayout::DirectTagged(TypeTag::TopPair) => {
                    let llvm_any_ptr = tcx.boxed_abi_to_llvm_ptr_type(&BoxedABIType::Any);
                    let llvm_any_list_ptr =
                        tcx.boxed_abi_to_llvm_ptr_type(&TOP_LIST_BOXED_ABI_TYPE);

                    members.extend_from_slice(&[
                        tcx.usize_llvm_type(),
                        llvm_any_ptr,
                        llvm_any_list_ptr,
                    ]);
                }
                BoxLayout::List => {
                    members.push(tcx.usize_llvm_type());
                }
                BoxLayout::DirectTagged(TypeTag::Nil)
                | BoxLayout::DirectTagged(TypeTag::True)
                | BoxLayout::DirectTagged(TypeTag::False)
                | BoxLayout::DirectTagged(TypeTag::TopVector)
                | BoxLayout::Bool
                | BoxLayout::Num
                | BoxLayout::Union => {}
            };
        }
    }
}

impl From<&BoxedABIType> for BoxLayout {
    fn from(boxed_abi_type: &BoxedABIType) -> BoxLayout {
        match boxed_abi_type {
            BoxedABIType::Any => BoxLayout::Any,

            BoxedABIType::List(_) => BoxLayout::List,
            &boxed::Num::BOXED_ABI_TYPE => BoxLayout::Num,
            &boxed::Bool::BOXED_ABI_TYPE => BoxLayout::Bool,
            BoxedABIType::Union(_, _) => BoxLayout::Union,

            BoxedABIType::DirectTagged(type_tag) => BoxLayout::DirectTagged(*type_tag),
            BoxedABIType::Pair(_) => BoxLayout::DirectTagged(TypeTag::TopPair),
            BoxedABIType::Vector(_) => BoxLayout::DirectTagged(TypeTag::TopVector),
        }
    }
}
