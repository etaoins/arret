use std::collections::HashMap;
use std::ptr;

use llvm_sys::prelude::*;

use runtime::boxed;

use crate::mir::ops;

pub mod core;
pub mod plan;
pub mod types;

/// Indicates where memory for a box allocation should come from
#[derive(PartialEq, Debug, Clone, Copy)]
pub enum BoxSource {
    Stack,
    Heap(boxed::BoxSize),
}

/// Represents a sequence of MIR ops that begin and end with the heap in a consistent state
#[derive(PartialEq, Debug)]
pub struct AllocAtom<'op> {
    box_sources: HashMap<ops::RegId, BoxSource>,
    ops: Box<[&'op ops::Op]>,
}

impl<'op> AllocAtom<'op> {
    fn with_unallocating_op(op: &'op ops::Op) -> AllocAtom<'op> {
        AllocAtom {
            box_sources: HashMap::new(),
            ops: Box::new([op]),
        }
    }

    pub fn box_sources(&self) -> &HashMap<ops::RegId, BoxSource> {
        &self.box_sources
    }

    pub fn ops(&self) -> &[&'op ops::Op] {
        self.ops.as_ref()
    }
}

pub struct ActiveAlloc {
    box_slots: LLVMValueRef,
    total_cells: usize,
    used_cells: usize,
}

impl ActiveAlloc {
    pub fn empty() -> ActiveAlloc {
        ActiveAlloc {
            box_slots: ptr::null_mut(),
            total_cells: 0,
            used_cells: 0,
        }
    }

    pub fn is_empty(&self) -> bool {
        self.total_cells == self.used_cells
    }
}
