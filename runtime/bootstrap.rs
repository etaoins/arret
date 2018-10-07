use crate::boxed;
use crate::task::Task;

type TaskEntry = extern "C" fn(&mut Task);

#[export_name = "arret_runtime_launch_task"]
pub fn launch_task(entry: TaskEntry) {
    let mut task = Task::new();
    entry(&mut task);
}

#[export_name = "arret_runtime_alloc_cells"]
pub fn alloc_cells(task: &mut Task, count: u32) -> *mut boxed::Any {
    task.heap_mut().alloc_cells(count as usize)
}
