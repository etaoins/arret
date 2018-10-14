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

/// Contains the sub-plans for a condiditional branch
#[derive(PartialEq, Debug)]
pub struct CondPlan<'op> {
    true_subplan: Vec<AllocAtom<'op>>,
    false_subplan: Vec<AllocAtom<'op>>,
}

impl<'op> CondPlan<'op> {
    pub fn true_subplan(&self) -> &[AllocAtom<'op>] {
        &self.true_subplan
    }

    pub fn false_subplan(&self) -> &[AllocAtom<'op>] {
        &self.false_subplan
    }
}

/// Represents a sequence of MIR ops that begin and end with the heap in a consistent state
#[derive(PartialEq, Debug)]
pub struct AllocAtom<'op> {
    box_sources: HashMap<ops::RegId, BoxSource>,
    cond_plans: HashMap<ops::RegId, CondPlan<'op>>,
    ops: Vec<&'op ops::Op>,
}

impl<'op> AllocAtom<'op> {
    fn new() -> AllocAtom<'op> {
        AllocAtom {
            box_sources: HashMap::new(),
            cond_plans: HashMap::new(),
            ops: vec![],
        }
    }

    fn with_unallocating_op(op: &'op ops::Op) -> AllocAtom<'op> {
        AllocAtom {
            box_sources: HashMap::new(),
            cond_plans: HashMap::new(),
            ops: vec![op],
        }
    }

    pub fn box_sources(&self) -> &HashMap<ops::RegId, BoxSource> {
        &self.box_sources
    }

    pub fn cond_plans(&self) -> &HashMap<ops::RegId, CondPlan<'op>> {
        &self.cond_plans
    }

    pub fn ops(&self) -> &[&'op ops::Op] {
        self.ops.as_ref()
    }

    fn is_empty(&self) -> bool {
        self.ops.is_empty()
    }

    fn push_box_source(&mut self, output_reg: ops::RegId, box_source: BoxSource) {
        self.box_sources.insert(output_reg, box_source);
    }

    fn push_cond_plan(&mut self, output_reg: ops::RegId, cond_plan: CondPlan<'op>) {
        self.cond_plans.insert(output_reg, cond_plan);
    }

    fn push_op(&mut self, op: &'op ops::Op) {
        self.ops.push(op);
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