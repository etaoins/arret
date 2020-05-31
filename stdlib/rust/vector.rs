use arret_runtime::binding::*;
use arret_runtime::boxed;
use arret_runtime::boxed::prelude::*;
use arret_runtime::boxed::refs::Gc;
use arret_runtime::task::Task;

#[arret_rfi_derive::rust_fun("(All #{T} & T -> (Vectorof T))")]
pub fn stdlib_vector(
    task: &mut Task,
    values: Gc<boxed::List<boxed::Any>>,
) -> Gc<boxed::Vector<boxed::Any>> {
    boxed::Vector::new(task, values.iter())
}

#[arret_rfi_derive::rust_fun("(All #{T} (Vectorof T) Int -> T)")]
pub fn stdlib_vector_ref(
    task: &mut Task,
    vector: Gc<boxed::Vector<boxed::Any>>,
    index: i64,
) -> Gc<boxed::Any> {
    let usize_index = if index < 0 {
        task.panic(format!("index {} is negative", index));
        unreachable!("returned from panic")
    } else {
        index as usize
    };

    match vector.get(usize_index) {
        Some(value) => value,
        None => {
            task.panic(format!(
                "index {} out of bounds for vector of length {}",
                usize_index,
                vector.len()
            ));
            unreachable!("returned from panic")
        }
    }
}

#[arret_rfi_derive::rust_fun("((Vectorof Any) -> Int)")]
pub fn stdlib_vector_length(vector: Gc<boxed::Vector<boxed::Any>>) -> i64 {
    vector.len() as i64
}

#[arret_rfi_derive::rust_fun("(All #{T} (Vectorof T) -> (List & T))")]
pub fn stdlib_vector_to_list(
    task: &mut Task,
    vector: Gc<boxed::Vector<boxed::Any>>,
) -> Gc<boxed::List<boxed::Any>> {
    boxed::List::new(task, vector.iter())
}

#[arret_rfi_derive::rust_fun("(All #{T} (Vectorof T) Int T -> (Vectorof T))")]
pub fn stdlib_vector_assoc(
    task: &mut Task,
    vector: Gc<boxed::Vector<boxed::Any>>,
    index: i64,
    value: Gc<boxed::Any>,
) -> Gc<boxed::Vector<boxed::Any>> {
    let usize_index = if index < 0 {
        task.panic(format!("index {} is negative", index));
        unreachable!("returned from panic")
    } else {
        index as usize
    };

    if usize_index >= vector.len() {
        task.panic(format!(
            "index {} out of bounds for vector of length {}",
            usize_index,
            vector.len()
        ));
        unreachable!("returned from panic")
    }

    vector.assoc(task, usize_index, value)
}

#[arret_rfi_derive::rust_fun("(All #{T} (Vectorof T) T -> (Vectorof T))")]
pub fn stdlib_vector_push(
    task: &mut Task,
    vector: Gc<boxed::Vector<boxed::Any>>,
    value: Gc<boxed::Any>,
) -> Gc<boxed::Vector<boxed::Any>> {
    vector.push(task, value)
}

#[arret_rfi_derive::rust_fun("(All #{T} (Vectorof T) -> (List (Vectorof T) T))")]
pub fn stdlib_vector_pop(
    task: &mut Task,
    vector: Gc<boxed::Vector<boxed::Any>>,
) -> Gc<boxed::List<boxed::Any>> {
    if let Some((new_vector, element)) = vector.pop(task) {
        let tuple_boxes: [Gc<boxed::Any>; 2] = [new_vector.as_any_ref(), element];

        boxed::List::new(task, tuple_boxes.iter().cloned())
    } else {
        task.panic("attempted to pop from an empty vector".to_owned());
        unreachable!("returned from panic")
    }
}

#[arret_rfi_derive::rust_fun("(All #{T} (Vectorof T) (Vectorof T) -> (Vectorof T))")]
pub fn stdlib_vector_append(
    task: &mut Task,
    vector1: Gc<boxed::Vector<boxed::Any>>,
    vector2: Gc<boxed::Vector<boxed::Any>>,
) -> Gc<boxed::Vector<boxed::Any>> {
    vector1.append(task, vector2)
}
