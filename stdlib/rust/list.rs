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

#[rfi_derive::rust_fun("(All #{I O [->_ ->!]} (I ->_ O) (Listof I) ->_ (Listof O))")]
pub fn stdlib_map(
    task: &mut Task,
    mapper: callback::Callback<
        extern "C" fn(&mut Task, boxed::Closure, Gc<boxed::Any>) -> Gc<boxed::Any>,
    >,
    input: Gc<boxed::List<boxed::Any>>,
) -> Gc<boxed::List<boxed::Any>> {
    // TODO: List::new needs a DoubleEndedIterator which List::iter doesn't implement
    let output_vec: Vec<Gc<boxed::Any>> =
        input.iter().map(|elem| mapper.apply(task, elem)).collect();

    boxed::List::new(task, output_vec.into_iter())
}

#[rfi_derive::rust_fun("(All #{T [->_ ->!]} (T ->_ Bool) (Listof T) ->_ (Listof T))")]
pub fn stdlib_filter(
    task: &mut Task,
    filter: callback::Callback<extern "C" fn(&mut Task, boxed::Closure, Gc<boxed::Any>) -> bool>,
    input: Gc<boxed::List<boxed::Any>>,
) -> Gc<boxed::List<boxed::Any>> {
    let output_vec: Vec<Gc<boxed::Any>> = input
        .iter()
        .filter(|elem| filter.apply(task, *elem))
        .collect();

    boxed::List::new(task, output_vec.into_iter())
}

#[rfi_derive::rust_fun("(All #{T} (Listof T) ... -> (Listof T))")]
pub fn stdlib_concat(
    task: &mut Task,
    lists: Gc<boxed::List<boxed::List<boxed::Any>>>,
) -> Gc<boxed::List<boxed::Any>> {
    if lists.is_empty() {
        return boxed::List::empty();
    }

    let mut head_values = vec![];
    let mut list_iter = lists.iter();
    while list_iter.len() > 1 {
        head_values.extend(list_iter.next().unwrap().iter());
    }

    // We don't need to rebuild our tail
    boxed::List::new_with_tail(task, head_values.into_iter(), list_iter.next().unwrap())
}

#[rfi_derive::rust_fun("(Any (Listof Any) -> Bool)")]
pub fn stdlib_member_p(needle: Gc<boxed::Any>, haystack: Gc<boxed::List<boxed::Any>>) -> bool {
    haystack.iter().any(|member| member == needle)
}
