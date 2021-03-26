mod alloc;
mod analysis;
mod box_layout;
mod callee;
mod const_gen;
mod debug_info;
mod fun_gen;
pub(crate) mod jit;
mod libcstr;
mod math_gen;
mod mod_gen;
mod op_gen;
mod panic_gen;
pub(crate) mod program;
mod range_md;
mod record_struct;
pub(crate) mod target_gen;
mod target_machine;
mod vector_gen;

use crate::mir::ops::OpsAbi;
use arret_runtime::abitype;

#[derive(Debug, PartialEq, Clone)]
pub struct GenAbi {
    pub takes_task: bool,
    pub params: Box<[abitype::ParamAbiType]>,
    pub ret: abitype::RetAbiType,
}

impl GenAbi {
    pub fn thunk_abi() -> GenAbi {
        GenAbi {
            takes_task: true,
            params: Box::new([
                abitype::BoxedAbiType::Any.into(),
                abitype::TOP_LIST_BOXED_ABI_TYPE.into(),
            ]),
            ret: abitype::BoxedAbiType::Any.into(),
        }
    }
}

impl<'a> From<&'a OpsAbi> for GenAbi {
    fn from(ops_abi: &'a OpsAbi) -> GenAbi {
        GenAbi {
            takes_task: true,
            params: ops_abi
                .params
                .iter()
                .map(|abi_type| abi_type.clone().into())
                .collect(),
            ret: ops_abi.ret.clone(),
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

#[cfg(test)]
pub(crate) mod test {
    use super::*;
    use std::sync::Once;

    static INITIALISE_TEST_LLVM: Once = Once::new();

    pub fn initialise_test_llvm() {
        INITIALISE_TEST_LLVM.call_once(|| {
            initialise_llvm(false);
        });
    }
}
