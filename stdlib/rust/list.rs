use runtime::binding::*;

use runtime::boxed;
use runtime::boxed::prelude::*;
use runtime::boxed::refs::Gc;
use runtime::callback;
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

#[rfi_derive::rust_fun("(All #{I O [->_ : ->!]} (Listof I) (I ->_ O) -> (Listof O))")]
pub fn stdlib_map(
    task: &mut Task,
    input: Gc<boxed::List<boxed::Any>>,
    mapper: callback::Callback<
        extern "C" fn(&mut Task, boxed::Closure, Gc<boxed::Any>) -> Gc<boxed::Any>,
    >,
) -> Gc<boxed::List<boxed::Any>> {
    // TODO: List::new needs a DoubleEndedIterator which List::iter doesn't implement
    let output_vec: Vec<Gc<boxed::Any>> =
        input.iter().map(|elem| mapper.apply(task, elem)).collect();

    boxed::List::new(task, output_vec.into_iter())
}
