use crate::task::Task;

type TaskEntry = extern "C" fn(&mut Task);

#[export_name = "arret_runtime_launch_task"]
pub fn launch_task(entry: TaskEntry) {
    let mut task = Task::new();
    entry(&mut task);
}
