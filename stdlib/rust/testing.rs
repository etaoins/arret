use arret_runtime::binding::*;

use arret_runtime::boxed;
use arret_runtime::boxed::prelude::*;
use arret_runtime::boxed::refs::Gc;
use arret_runtime::callback;
use arret_runtime::task::Task;

#[arret_rfi_derive::rust_fun("(All #{T} T -> T)")]
pub fn stdlib_black_box(value: Gc<boxed::Any>) -> Gc<boxed::Any> {
    value
}

#[arret_rfi_derive::rust_fun("(All #{T} T ->! T)")]
pub fn stdlib_black_box_impure(value: Gc<boxed::Any>) -> Gc<boxed::Any> {
    value
}

#[arret_rfi_derive::rust_fun("(All #{[->_ ->!] T} (->_ T) ->_ (List Int T))")]
pub fn stdlib_heap_alloc_count(
    task: &mut Task,
    block: callback::Callback<extern "C" fn(&mut Task, boxed::Closure) -> Gc<boxed::Any>>,
) -> Gc<boxed::List<boxed::Any>> {
    let before_len = task.heap().len();
    let ret = block.apply(task);
    let after_len = task.heap().len();

    let alloc_count = boxed::Int::new(task, (after_len - before_len) as i64);
    boxed::List::new(task, [alloc_count.as_any_ref(), ret].iter().cloned())
}

// TODO: This should return a `Set` once they're better supported
#[arret_rfi_derive::rust_fun("((... ->! Any) -> (List & Sym))")]
pub fn stdlib_fn_op_categories(_value: Gc<boxed::FunThunk>) -> Gc<boxed::List<boxed::Sym>> {
    panic!("cannot call `(fn-op-categories)` at runtime")
}
