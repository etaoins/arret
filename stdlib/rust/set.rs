use arret_runtime::binding::*;
use arret_runtime::boxed;
use arret_runtime::boxed::refs::Gc;
use arret_runtime::task::Task;

#[arret_rfi_derive::rust_fun("(All #{T} & T -> (Setof T))")]
pub fn stdlib_set(
    task: &mut Task,
    values: Gc<boxed::List<boxed::Any>>,
) -> Gc<boxed::Set<boxed::Any>> {
    boxed::Set::new(task, values.iter())
}

#[arret_rfi_derive::rust_fun("(All #{T} (Setof T) T -> Bool)")]
pub fn stdlib_set_contains_p(
    task: &mut Task,
    set: Gc<boxed::Set<boxed::Any>>,
    needle: Gc<boxed::Any>,
) -> bool {
    set.contains(task.heap(), &needle)
}

#[arret_rfi_derive::rust_fun("((Setof Any) -> Int)")]
pub fn stdlib_set_length(set: Gc<boxed::Set<boxed::Any>>) -> i64 {
    set.len() as i64
}

#[arret_rfi_derive::rust_fun("(All #{T} (Setof T) -> (List & T))")]
pub fn stdlib_set_to_list(
    task: &mut Task,
    set: Gc<boxed::Set<boxed::Any>>,
) -> Gc<boxed::List<boxed::Any>> {
    boxed::List::new(task, set.iter().cloned())
}

#[arret_rfi_derive::rust_fun("(All #{T} (Setof T) (Setof T) -> Bool)")]
pub fn stdlib_subset_p(
    task: &mut Task,
    subset: Gc<boxed::Set<boxed::Any>>,
    superset: Gc<boxed::Set<boxed::Any>>,
) -> bool {
    subset.is_subset(task.heap(), &superset)
}
