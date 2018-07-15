#![allow(dead_code)]
#![cfg_attr(feature = "cargo-clippy", warn(clippy))]

use std::collections::HashMap;

mod boxed;

#[derive(Debug)]
pub enum ABIType {
    Bool,
    Char,
    Float,
    Int,
    Boxed(boxed::TypeTag),
}

pub struct Task {}

impl Task {
    fn alloc_boxed_float(&mut self, _value: f64) -> &boxed::Float {
        unimplemented!("PLACEHOLDER");
    }
}

#[derive(Debug)]
pub struct ExternFun {
    takes_task: bool,
    params: &'static [ABIType],
    ret: ABIType,
    entry_point: &'static str,
}

trait EncodeABIType {
    const ABI_TYPE: ABIType;
}

impl EncodeABIType for f64 {
    const ABI_TYPE: ABIType = ABIType::Float;
}

impl EncodeABIType for i64 {
    const ABI_TYPE: ABIType = ABIType::Int;
}

impl EncodeABIType for () {
    const ABI_TYPE: ABIType = ABIType::Boxed(boxed::TypeTag::Nil);
}

impl<'a> EncodeABIType for &'a str {
    const ABI_TYPE: ABIType = ABIType::Boxed(boxed::TypeTag::Str);
}

impl<'a> EncodeABIType for &'a boxed::Float {
    const ABI_TYPE: ABIType = ABIType::Boxed(boxed::TypeTag::Float);
}

impl<'a> EncodeABIType for &'a boxed::Int {
    const ABI_TYPE: ABIType = ABIType::Boxed(boxed::TypeTag::Int);
}

macro_rules! define_extern_fn {
    ($desc_name:ident = $func_name:ident($task_name:ident : &mut Task, $($param_name:ident : $rust_ty:ty),*) -> $ret:ty $body:block) => {
        #[no_mangle]
        pub extern "C" fn $func_name($task_name: &'static mut Task, $($param_name: $rust_ty),*) -> $ret {
            $body
        }

        const $desc_name: ExternFun = ExternFun {
            takes_task: true,
            params: &[
                $(<$rust_ty>::ABI_TYPE),*
            ],
            ret: <$ret>::ABI_TYPE,
            entry_point: stringify!($func_name),
        };
    };

    ($desc_name:ident = $func_name:ident($($param_name:ident : $rust_ty:ty),*) -> $ret:ty $body:block) => {
        #[no_mangle]
        pub extern "C" fn $func_name($($param_name: $rust_ty),*) -> $ret {
            $body
        }

        const $desc_name: ExternFun = ExternFun {
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
    HELLO_WORLD = hello_world(param1: &str) -> i64 {
        println!("Hello, {}!", param1);
        42
    }
}

define_extern_fn! {
    TAKES_TASK = takes_task(task: &mut Task, _param1: &boxed::Int) -> &'static boxed::Float {
        task.alloc_boxed_float(64.0)
    }
}

define_extern_fn! {
     PRINT_NUM = print_num(number: i64) -> () {
         println!("Number is {}", number)
     }
}

fn main() {
    let number = hello_world("sailor");
    print_num(number);

    println!("Number entry point: '{}'", PRINT_NUM.entry_point);
    println!("TAKES_TASK takes task: '{}'", TAKES_TASK.takes_task);

    let mut exports = HashMap::<&'static str, ExternFun>::new();
    exports.insert("hello-world", HELLO_WORLD);
    exports.insert("print-num", PRINT_NUM);
}
