use arret_runtime::binding::*;
use arret_runtime::boxed;
use arret_runtime::boxed::refs::Gc;
use arret_runtime::task::Task;

#[arret_rfi_derive::rust_fun("(All #{T} (Vectorof T) Int -> T)")]
pub fn stdlib_vector_ref(
    task: &mut Task,
    vector: Gc<boxed::Vector<boxed::Any>>,
    index: i64,
) -> Gc<boxed::Any> {
    let values = vector.values();

    let usize_index = if index < 0 {
        task.panic(format!("index {} is negative", index));
        unreachable!("returned from panic")
    } else {
        index as usize
    };

    if usize_index >= values.len() {
        task.panic(format!(
            "index {} out of bounds for vector of length {}",
            usize_index,
            values.len()
        ));
        unreachable!("returned from panic")
    }

    values[usize_index]
}
