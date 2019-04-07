use std::vec;

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

/// Contains the sub-plans for a conditional branch
#[derive(PartialEq, Debug)]
pub struct CondPlan<'op> {
    pub true_subplan: Vec<AllocAtom<'op>>,
    pub false_subplan: Vec<AllocAtom<'op>>,
}

/// Represents a sequence of MIR ops that begin and end with the heap in a consistent state
#[derive(PartialEq, Debug, Default)]
pub struct AllocAtom<'op> {
    box_sources: Vec<BoxSource>,
    cond_plans: Vec<CondPlan<'op>>,

    ops_base: &'op [ops::Op],
    ops_count: usize,
}

impl<'op> AllocAtom<'op> {
    /// Creates a new `AllocAtom` with its ops starting at the specified slice
    fn new(ops_base: &'op [ops::Op]) -> Self {
        Self {
            ops_base,
            ..Default::default()
        }
    }

    pub fn ops(&self) -> &'op [ops::Op] {
        &self.ops_base[0..self.ops_count]
    }

    /// Increments the used size of our ops by one
    fn push_op(&mut self) {
        self.ops_count += 1
    }

    fn is_empty(&self) -> bool {
        self.ops_count == 0
    }
}

pub struct ActiveAlloc<'op> {
    box_slots: LLVMValueRef,
    total_cells: usize,
    used_cells: usize,

    box_source_iter: vec::IntoIter<BoxSource>,
    cond_plan_iter: vec::IntoIter<CondPlan<'op>>,
}

impl<'op> ActiveAlloc<'op> {
    pub fn is_empty(&self) -> bool {
        self.total_cells == self.used_cells
    }

    pub fn next_box_source(&mut self) -> BoxSource {
        self.box_source_iter.next().unwrap()
    }

    pub fn next_cond_plan(&mut self) -> CondPlan<'op> {
        self.cond_plan_iter.next().unwrap()
    }
}
