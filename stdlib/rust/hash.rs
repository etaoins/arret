use std::collections::hash_map::DefaultHasher;
use std::hash::Hasher as _;

use arret_runtime::binding::*;

use arret_runtime::boxed;
use arret_runtime::boxed::prelude::*;
use arret_runtime::boxed::refs::Gc;
use arret_runtime::task::Task;

#[arret_rfi_derive::rust_fun("(Any -> Int)")]
pub fn stdlib_hash(task: &mut Task, input: Gc<boxed::Any>) -> i64 {
    let mut state = DefaultHasher::new();

    input.hash_in_heap(task.heap(), &mut state);
    state.finish() as i64
}
