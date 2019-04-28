mod alloc;
mod analysis;
mod box_layout;
mod callee;
mod const_gen;
mod debug_info;
mod fun_gen;
pub(crate) mod jit;
mod mod_gen;
mod op_gen;
pub(crate) mod program;
mod range_md;
pub(crate) mod target_gen;
mod target_machine;

use arret_runtime::abitype;

#[derive(Debug, PartialEq, Clone)]
pub struct GenABI {
    pub takes_task: bool,
    pub params: Box<[abitype::ParamABIType]>,
    pub ret: abitype::RetABIType,
}

impl GenABI {
    pub fn thunk_abi() -> GenABI {
        GenABI {
            takes_task: true,
            params: Box::new([
                abitype::BoxedABIType::Any.into(),
                abitype::TOP_LIST_BOXED_ABI_TYPE.into(),
            ]),
            ret: abitype::BoxedABIType::Any.into(),
        }
    }
}

/// Initialises LLVM
///
/// This must be called before anything else in this module. It can only be called from a single
/// thread at once.
pub fn initialise_llvm(support_cross_compilation: bool) {
    use llvm_sys::target::*;

    unsafe {
        if support_cross_compilation {
            LLVM_InitializeAllTargetInfos();
            LLVM_InitializeAllTargets();
            LLVM_InitializeAllTargetMCs();
            LLVM_InitializeAllAsmPrinters();
        } else {
            LLVM_InitializeNativeTarget();
            LLVM_InitializeNativeAsmPrinter();
        }
    }
}
