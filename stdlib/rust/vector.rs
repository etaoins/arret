use arret_runtime::binding::*;
use arret_runtime::boxed;
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

#[arret_rfi_derive::rust_fun("(All #{T} & (Vectorof T) -> (Vectorof T))")]
pub fn stdlib_vector_append(
    task: &mut Task,
    vectors: Gc<boxed::List<boxed::Vector<boxed::Any>>>,
) -> Gc<boxed::Vector<boxed::Any>> {
    let mut vectors_iter = vectors.iter();

    let first_vector = if let Some(first_vector) = vectors_iter.next() {
        first_vector
    } else {
        return boxed::Vector::new(task, std::iter::empty());
    };

    vectors_iter.fold(first_vector, |v1, v2| v1.append(task, v2))
}

#[arret_rfi_derive::rust_fun("(All #{T} (Vectorof T) & T -> (Vectorof T))")]
pub fn stdlib_vector_extend(
    task: &mut Task,
    vector: Gc<boxed::Vector<boxed::Any>>,
    new_values: Gc<boxed::List<boxed::Any>>,
) -> Gc<boxed::Vector<boxed::Any>> {
    vector.extend(task, new_values.iter())
}

#[arret_rfi_derive::rust_fun("(All #{T} Int (Vectorof T) -> (Vectorof T))")]
pub fn stdlib_vector_take(
    task: &mut Task,
    count: i64,
    input: Gc<boxed::Vector<boxed::Any>>,
) -> Gc<boxed::Vector<boxed::Any>> {
    let usize_count = if count < 0 { 0 } else { count as usize };
    input.take(task, usize_count)
}
