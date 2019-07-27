use arret_runtime::binding::*;

use arret_runtime::boxed;
use arret_runtime::boxed::prelude::*;
use arret_runtime::boxed::refs::Gc;
use arret_runtime::callback;
use arret_runtime::task::Task;

#[arret_rfi_derive::rust_fun("((List & Any) -> Int)")]
pub fn stdlib_length(input: Gc<boxed::List<boxed::Any>>) -> i64 {
    input.len() as i64
}

#[arret_rfi_derive::rust_fun("(All #{H T} H (List & T) -> (List H & T))")]
pub fn stdlib_cons(
    task: &mut Task,
    head: Gc<boxed::Any>,
    tail: Gc<boxed::List<boxed::Any>>,
) -> Gc<boxed::Pair<boxed::Any>> {
    boxed::Pair::new(task, head, tail)
}

#[arret_rfi_derive::rust_fun("(All #{I O [->_ ->!]} (I ->_ O) (List & I) ->_ (List & O))")]
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

#[arret_rfi_derive::rust_fun("(All #{T [->_ ->!]} (T ->_ Bool) (List & T) ->_ (List & T))")]
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

#[arret_rfi_derive::rust_fun("(All #{T [->_ ->!]} (T ->_ Bool) (List & T) ->_ Bool)")]
pub fn stdlib_some_p(
    task: &mut Task,
    pred: callback::Callback<extern "C" fn(&mut Task, boxed::Closure, Gc<boxed::Any>) -> bool>,
    input: Gc<boxed::List<boxed::Any>>,
) -> bool {
    input.iter().any(|elem| pred.apply(task, elem))
}

#[arret_rfi_derive::rust_fun("(All #{T [->_ ->!]} (T ->_ Bool) (List & T) ->_ Bool)")]
pub fn stdlib_every_p(
    task: &mut Task,
    pred: callback::Callback<extern "C" fn(&mut Task, boxed::Closure, Gc<boxed::Any>) -> bool>,
    input: Gc<boxed::List<boxed::Any>>,
) -> bool {
    input.iter().all(|elem| pred.apply(task, elem))
}

#[arret_rfi_derive::rust_fun("(All #{I O [->_ ->!]} (O I ->_ O) O (List & I) ->_ O)")]
pub fn stdlib_fold(
    task: &mut Task,
    folder: callback::Callback<
        extern "C" fn(&mut Task, boxed::Closure, Gc<boxed::Any>, Gc<boxed::Any>) -> Gc<boxed::Any>,
    >,
    initial: Gc<boxed::Any>,
    input: Gc<boxed::List<boxed::Any>>,
) -> Gc<boxed::Any> {
    input
        .iter()
        .fold(initial, |acc, elem| folder.apply(task, acc, elem))
}

#[arret_rfi_derive::rust_fun("(All #{T} & (List & T) -> (List & T))")]
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

#[arret_rfi_derive::rust_fun("(Any (List & Any) -> Bool)")]
pub fn stdlib_member_p(
    task: &Task,
    needle: Gc<boxed::Any>,
    haystack: Gc<boxed::List<boxed::Any>>,
) -> bool {
    haystack
        .iter()
        .any(|member| member.eq_in_heap(task.as_heap(), &needle))
}

#[arret_rfi_derive::rust_fun("(All #{T} Int (List & T) -> (List & T))")]
pub fn stdlib_take(
    task: &mut Task,
    count: i64,
    input: Gc<boxed::List<boxed::Any>>,
) -> Gc<boxed::List<boxed::Any>> {
    let usize_count = if count < 0 { 0 } else { count as usize };
    let output_vec: Vec<Gc<boxed::Any>> = input.iter().take(usize_count).collect();

    boxed::List::new(task, output_vec.into_iter())
}

#[arret_rfi_derive::rust_fun("(All #{T} Int (List & T) -> (List & T))")]
pub fn stdlib_drop(
    task: &mut Task,
    count: i64,
    input: Gc<boxed::List<boxed::Any>>,
) -> Gc<boxed::List<boxed::Any>> {
    let usize_count = if count < 0 { 0 } else { count as usize };
    let output_vec: Vec<Gc<boxed::Any>> = input.iter().skip(usize_count).collect();

    boxed::List::new(task, output_vec.into_iter())
}
