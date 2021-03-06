#![warn(clippy::all)]
#![warn(rust_2018_idioms)]

#[macro_use]
extern crate arret_runtime;

pub mod list;
use crate::list::*;

pub mod math;
use crate::math::*;

pub mod number;
use crate::number::*;

pub mod testing;
use crate::testing::*;

pub mod vector;
use crate::vector::*;

pub mod write;
use crate::write::*;

pub mod read;
use crate::read::*;

pub mod hash;
use crate::hash::*;

pub mod set;
use crate::set::*;

pub mod bitwise;
use crate::bitwise::*;

use arret_runtime_syntax::writer::pretty_print_boxed;

use arret_runtime::binding::*;
use arret_runtime::boxed;
use arret_runtime::boxed::refs::Gc;
use arret_runtime::task::Task;

pub fn panic_common(task: &mut Task, values: Gc<boxed::List<boxed::Any>>) -> Never {
    use std::str;

    let mut output = Vec::<u8>::new();
    for value in values.iter() {
        pretty_print_boxed(&mut output, task, value)
    }

    let message = str::from_utf8(output.as_slice()).unwrap().to_owned();
    task.panic(message)
}

#[arret_rfi_derive::rust_fun("(& Any -> (U))")]
pub fn stdlib_panic(task: &mut Task, values: Gc<boxed::List<boxed::Any>>) -> Never {
    panic_common(task, values)
}

#[arret_rfi_derive::rust_fun("(& Any ->! (U))")]
pub fn stdlib_panic_impure(task: &mut Task, values: Gc<boxed::List<boxed::Any>>) -> Never {
    panic_common(task, values)
}

#[arret_rfi_derive::rust_fun("(Int ->! (U))")]
pub fn stdlib_exit(exit_code: i64) {
    use std::process::exit;
    exit(exit_code as i32);
}

define_rust_module!(ARRET_STDLIB_RUST_EXPORTS, {
    "panic" => stdlib_panic,
    "panic!" => stdlib_panic_impure,
    "exit!" => stdlib_exit,

    "print!" => stdlib_print,
    "println!" => stdlib_println,
    "print-str" => stdlib_print_str,
    "write!" => stdlib_write,
    "writeln!" => stdlib_writeln,
    "write-str" => stdlib_write_str,

    "read-str" => stdlib_read_str,

    "length" => stdlib_length,
    "map" => stdlib_map,
    "filter" => stdlib_filter,
    "some?" => stdlib_some_p,
    "every?" => stdlib_every_p,
    "fold" => stdlib_fold,
    "cons" => stdlib_cons,
    "concat" => stdlib_concat,
    "take" => stdlib_take,
    "reverse" => stdlib_reverse,
    "repeat" => stdlib_repeat,

    "float" => stdlib_float,
    "int" => stdlib_int,
    "<" => stdlib_num_lt,
    "<=" => stdlib_num_le,
    "==" => stdlib_num_eq,
    ">" => stdlib_num_gt,
    ">=" => stdlib_num_ge,

    "+" => stdlib_add,
    "*" => stdlib_mul,
    "-" => stdlib_sub,
    "/" => stdlib_div,
    "quot" => stdlib_quot,
    "rem" => stdlib_rem,
    "sqrt" => stdlib_sqrt,

    "black-box" => stdlib_black_box,
    "black-box!" => stdlib_black_box_impure,
    "heap-alloc-count" => stdlib_heap_alloc_count,
    "fn-op-categories" => stdlib_fn_op_categories,

    "vector" => stdlib_vector,
    "vector-length" => stdlib_vector_length,
    "vector->list" => stdlib_vector_to_list,
    "vector-ref" => stdlib_vector_ref,
    "vector-assoc" => stdlib_vector_assoc,
    "vector-extend" => stdlib_vector_extend,
    "vector-append" => stdlib_vector_append,
    "vector-take" => stdlib_vector_take,

    "hash" => stdlib_hash,

    "set" => stdlib_set,
    "set-length" => stdlib_set_length,
    "set->list" => stdlib_set_to_list,
    "set-contains?" => stdlib_set_contains_p,
    "subset?" => stdlib_subset_p,

    "bit-and" => stdlib_bit_and,
    "bit-or" => stdlib_bit_or,
    "bit-xor" => stdlib_bit_xor,
    "bit-not" => stdlib_bit_not,
    "bit-shift-left" => stdlib_bit_shift_left,
    "bit-shift-right" => stdlib_bit_shift_right,
    "unsigned-bit-shift-right" => stdlib_unsigned_bit_shift_right
});
