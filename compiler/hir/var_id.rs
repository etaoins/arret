use std::num::NonZeroU32;
use std::sync::atomic::{AtomicU32, Ordering};

use crate::context::ModuleId;

/// Identifier for a local variable within a module
///
/// This is not globally unique; it must be combined with a `ModuleId`
#[derive(Debug, PartialEq, Eq, Clone, Copy, Hash, PartialOrd, Ord)]
pub struct LocalId(NonZeroU32);

/// Identifier for a variable exported from another module
#[derive(Debug, PartialEq, Eq, Clone, Copy, Hash, PartialOrd, Ord)]
pub struct ExportId(ModuleId, LocalId);

impl ExportId {
    pub fn new(module_id: ModuleId, local_id: LocalId) -> Self {
        Self(module_id, local_id)
    }

    pub fn module_id(self) -> ModuleId {
        self.0
    }

    pub fn local_id(self) -> LocalId {
        self.1
    }
}

pub struct LocalIdAlloc {
    local_id_counter: AtomicU32,
}

impl LocalIdAlloc {
    pub fn new() -> Self {
        Self {
            local_id_counter: AtomicU32::new(1),
        }
    }

    /// Allocates a `LocalId` using atomic operations on a shared instance
    pub fn alloc(&self) -> LocalId {
        LocalId(NonZeroU32::new(self.local_id_counter.fetch_add(1, Ordering::Relaxed)).unwrap())
    }

    /// Allocates a `LocalId` using non-atomic operations on an exclusive instance
    pub fn alloc_mut(&mut self) -> LocalId {
        let local_id_counter = self.local_id_counter.get_mut();

        let raw_id = *local_id_counter;
        *local_id_counter += 1;

        LocalId(NonZeroU32::new(raw_id).unwrap())
    }
}
