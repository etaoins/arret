use runtime::abitype::*;
use runtime::binding::*;

use runtime::boxed;
use runtime::boxed::prelude::*;
use runtime::boxed::refs::Gc;
use runtime::task::Task;

define_rust_fn! {
    #[arret_type="((Listof Any) -> Int)"]
    pub LENGTH = fn arret_stdlib_length(input: Gc<boxed::List<boxed::Any>>) -> i64 {
        input.len() as i64
    }
}

define_rust_fn! {
    #[arret_type="(All #{H T} H (Listof T) -> (List H T ...))"]
    pub CONS = fn arret_stdlib_cons(task: &mut Task, head: Gc<boxed::Any>, tail: Gc<boxed::List<boxed::Any>>) -> Gc<boxed::TopPair> {
        boxed::Pair::new(task, (head, tail)).as_top_pair()
    }
}
