use runtime::binding::*;

use runtime::boxed;
use runtime::boxed::prelude::*;
use runtime::boxed::refs::Gc;
use runtime::task::Task;

use rfi_derive;

#[rfi_derive::rust_fun("((Listof Any) -> Int)")]
pub fn stdlib_length(input: Gc<boxed::List<boxed::Any>>) -> i64 {
    input.len() as i64
}

#[rfi_derive::rust_fun("(All #{H T} H (Listof T) -> (List H T ...))")]
pub fn stdlib_cons(
    task: &mut Task,
    head: Gc<boxed::Any>,
    tail: Gc<boxed::List<boxed::Any>>,
) -> Gc<boxed::TopPair> {
    boxed::Pair::new(task, (head, tail)).as_top_pair()
}
