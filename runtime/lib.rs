#![allow(dead_code)]
#![cfg_attr(feature = "cargo-clippy", warn(clippy))]

use std::collections::HashMap;

mod boxed;
mod intern;
use boxed::prelude::*;
use boxed::refs::Gc;

mod abitype;
use abitype::{ABIType, EncodeABIType};

pub struct Task {
    heap: boxed::Heap,
}

impl Task {
    pub fn heap(&mut self) -> &mut boxed::Heap {
        &mut self.heap
    }
}

impl Default for Task {
    fn default() -> Task {
        Task {
            heap: boxed::Heap::with_capacity(32),
        }
    }
}

impl AsHeap for Task {
    fn as_heap(&mut self) -> &mut boxed::Heap {
        &mut self.heap
    }
}

#[derive(Debug)]
pub struct ExternFun {
    arret_type: &'static str,
    takes_task: bool,
    params: &'static [ABIType],
    ret: ABIType,
    entry_point: &'static str,
}

macro_rules! define_extern_fn {
    (#[arret-type=$type:expr] $desc_name:ident = $func_name:ident($task_name:ident : &mut Task, $($param_name:ident : $rust_ty:ty),*) -> $ret:ty $body:block) => {
        #[no_mangle]
        pub extern "C" fn $func_name($task_name: &'static mut Task, $($param_name: $rust_ty),*) -> $ret {
            $body
        }

        const $desc_name: ExternFun = ExternFun {
            arret_type: $type,
            takes_task: true,
            params: &[
                $(<$rust_ty>::ABI_TYPE),*
            ],
            ret: <$ret>::ABI_TYPE,
            entry_point: stringify!($func_name),
        };
    };

    (#[arret-type=$type:expr] $desc_name:ident = $func_name:ident($($param_name:ident : $rust_ty:ty),*) -> $ret:ty $body:block) => {
        #[no_mangle]
        pub extern "C" fn $func_name($($param_name: $rust_ty),*) -> $ret {
            $body
        }

        const $desc_name: ExternFun = ExternFun {
            arret_type: $type,
            takes_task: false,
            params: &[
                $(<$rust_ty>::ABI_TYPE),*
            ],
            ret: <$ret>::ABI_TYPE,
            entry_point: stringify!($func_name),
        };
    };
}

define_extern_fn! {
    #[arret-type="(String -> Int)"]
    HELLO_WORLD = hello_world(param1: Gc<boxed::Str>) -> i64 {
        println!("Hello, {}!", param1.as_str());
        42
    }
}

define_extern_fn! {
    #[arret-type="((Vector Int) -> Num)"]
    TAKES_TASK = takes_task(task: &mut Task, _param1: Gc<boxed::Vector<boxed::Int>>) -> Gc<boxed::Num> {
        boxed::Float::new(task, 64.0).as_num_ref()
    }
}

define_extern_fn! {
     #[arret-type="(Num -> ())"]
     PRINT_NUM = print_num(number: i64) -> () {
         println!("Number is {}", number)
     }
}

define_extern_fn! {
    #[arret-type="((Listof Any) -> Int)"]
    LENGTH = length(input: Gc<boxed::List<boxed::Any>>) -> i64 {
        input.len() as i64
    }
}

fn main() {
    let mut task = Task::default();

    let sailor_str = boxed::Str::new(&mut task, "sailor");
    let number = hello_world(sailor_str);
    print_num(number);

    println!("Number entry point: '{}'", PRINT_NUM.entry_point);
    println!("TAKES_TASK takes task: '{}'", TAKES_TASK.takes_task);

    let mut exports = HashMap::<&'static str, ExternFun>::new();
    exports.insert("hello-world", HELLO_WORLD);
    exports.insert("print-num", PRINT_NUM);
}
