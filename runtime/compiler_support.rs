use std::{panic, process};

use crate::boxed;
use crate::boxed::refs::Gc;
use crate::task::Task;

type TaskEntry = extern "C" fn(&mut Task);

#[export_name = "arret_runtime_launch_task"]
pub extern "C" fn launch_task(entry: TaskEntry) {
    let mut task = Task::new();

    if let Err(err) = panic::catch_unwind(panic::AssertUnwindSafe(|| entry(&mut task))) {
        if let Some(message) = err.downcast_ref::<String>() {
            eprintln!("{}", message);
        } else {
            eprintln!("Unexpected panic type");
        };

        process::exit(1);
    };
}

#[export_name = "arret_runtime_alloc_cells"]
pub extern "C" fn alloc_cells(task: &mut Task, count: u32) -> *mut boxed::Any {
    task.heap_mut().alloc_cells(count as usize)
}

#[export_name = "arret_runtime_equals"]
pub extern "C" fn equals(lhs: Gc<boxed::Any>, rhs: Gc<boxed::Any>) -> bool {
    lhs == rhs
}
