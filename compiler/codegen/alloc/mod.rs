use std::collections::HashMap;

use runtime::boxed;

use crate::mir::ops;

pub mod gen;
pub mod plan;

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
