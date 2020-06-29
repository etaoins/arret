use arret_runtime::binding::*;
use arret_runtime::boxed;
use arret_runtime::boxed::refs::Gc;
use arret_runtime::task::Task;

#[arret_rfi_derive::rust_fun("(Int Int & Int -> Int)")]
pub fn stdlib_bit_and(lhs: i64, rhs: i64, rest: Gc<boxed::List<boxed::Int>>) -> i64 {
    rest.iter().fold(lhs & rhs, |acc, i| acc & i.value())
}

#[arret_rfi_derive::rust_fun("(Int Int & Int -> Int)")]
pub fn stdlib_bit_or(lhs: i64, rhs: i64, rest: Gc<boxed::List<boxed::Int>>) -> i64 {
    rest.iter().fold(lhs | rhs, |acc, i| acc | i.value())
}

#[arret_rfi_derive::rust_fun("(Int Int & Int -> Int)")]
pub fn stdlib_bit_xor(lhs: i64, rhs: i64, rest: Gc<boxed::List<boxed::Int>>) -> i64 {
    rest.iter().fold(lhs ^ rhs, |acc, i| acc ^ i.value())
}

#[arret_rfi_derive::rust_fun("(Int -> Int)")]
pub fn stdlib_bit_not(val: i64) -> i64 {
    !val
}

#[arret_rfi_derive::rust_fun("(Int Int -> Int)")]
pub fn stdlib_bit_shift_left(task: &mut Task, val: i64, bit_count: i64) -> i64 {
    if bit_count < 0 {
        task.panic(format!("shift left by negative bit count {}", bit_count));
    } else if bit_count > 64 {
        task.panic(format!("shift left by {} bits exceeds 64 bits", bit_count));
    }

    val << (bit_count as u32)
}

#[arret_rfi_derive::rust_fun("(Int Int -> Int)")]
pub fn stdlib_bit_shift_right(task: &mut Task, val: i64, bit_count: i64) -> i64 {
    if bit_count < 0 {
        task.panic(format!("shift right by negative bit count {}", bit_count));
    } else if bit_count > 64 {
        task.panic(format!("shift right by {} bits exceeds 64 bits", bit_count));
    }

    val >> (bit_count as u32)
}

#[arret_rfi_derive::rust_fun("(Int Int -> Int)")]
pub fn stdlib_unsigned_bit_shift_right(task: &mut Task, val: i64, bit_count: i64) -> i64 {
    if bit_count < 0 {
        task.panic(format!("shift right by negative bit count {}", bit_count));
    } else if bit_count > 64 {
        task.panic(format!("shift right by {} bits exceeds 64 bits", bit_count));
    }

    (val as u64 >> (bit_count as u32)) as i64
}
