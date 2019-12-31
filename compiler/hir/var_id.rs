use std::num::NonZeroU32;
use std::sync::atomic::AtomicU32;
use std::sync::atomic::Ordering;

use crate::context::ModuleId;

/// Identifier for a local variable within a module
///
/// This is not globally unique; it must be combined with a `ModuleId`
#[derive(Debug, PartialEq, Eq, Clone, Copy, Hash, PartialOrd, Ord)]
pub struct LocalId(NonZeroU32);

/// Globally unique identifier for a variable
#[derive(Debug, PartialEq, Eq, Clone, Copy, Hash, PartialOrd, Ord)]
pub struct VarId(ModuleId, LocalId);

impl VarId {
    pub fn module_id(self) -> ModuleId {
        self.0
    }

    pub fn local_id(self) -> LocalId {
        self.1
    }

    pub fn to_module_local_id(self, module_id: ModuleId) -> Option<LocalId> {
        if self.module_id() == module_id {
            Some(self.local_id())
        } else {
            None
        }
    }
}

pub struct VarIdAlloc {
    module_id: ModuleId,
    local_id_counter: AtomicU32,
}

impl VarIdAlloc {
    pub fn new(module_id: ModuleId) -> Self {
        Self {
            module_id,
            local_id_counter: AtomicU32::new(1),
        }
    }

    pub fn module_id(&self) -> ModuleId {
        self.module_id
    }

    pub fn alloc(&self) -> VarId {
        let raw_id = self.local_id_counter.fetch_add(1, Ordering::Relaxed);
        VarId(self.module_id, LocalId(NonZeroU32::new(raw_id).unwrap()))
    }
}
