use runtime::binding::*;

use runtime::boxed;
use runtime::boxed::prelude::*;
use runtime::boxed::refs::Gc;
use runtime::callback;
use runtime::task::Task;

use rfi_derive;

#[rfi_derive::rust_fun("(All #{T} T -> T)")]
pub fn stdlib_black_box(value: Gc<boxed::Any>) -> Gc<boxed::Any> {
    value
}

#[rfi_derive::rust_fun("(All #{T} T ->! T)")]
pub fn stdlib_black_box_impure(value: Gc<boxed::Any>) -> Gc<boxed::Any> {
    value
}

#[rfi_derive::rust_fun("(All #{[->_ ->!] T} (->_ T) ->_ (List Int T))")]
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
