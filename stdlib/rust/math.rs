use arret_runtime::binding::*;

use arret_runtime::boxed;
use arret_runtime::boxed::refs::Gc;
use arret_runtime::task::Task;

fn fold_float_op<FR>(
    task: &mut Task,
    operands_iter: impl Iterator<Item = Gc<boxed::Num>>,
    initial_value: f64,
    float_reduce: FR,
) -> Gc<boxed::Float>
where
    FR: Fn(f64, f64) -> f64,
{
    let mut float_acc = initial_value;

    for operand in operands_iter {
        match operand.as_subtype() {
            boxed::NumSubtype::Int(int_ref) => {
                float_acc = float_reduce(float_acc, int_ref.value() as f64);
            }
            boxed::NumSubtype::Float(float_ref) => {
                // Convert to float and break
                float_acc = float_reduce(float_acc, float_ref.value());
            }
        }
    }

    boxed::Float::new(task, float_acc)
}

fn fold_num_op<IR, FR>(
    task: &mut Task,
    op_name: &'static str,
    mut operands_iter: impl Iterator<Item = Gc<boxed::Num>>,
    initial_value: i64,
    int_reduce: IR,
    float_reduce: FR,
) -> Gc<boxed::Num>
where
    IR: Fn(i64, i64) -> Option<i64>,
    FR: Fn(f64, f64) -> f64,
{
    // Accumulate as an integer for as long as possible
    let mut int_acc = initial_value;

    while let Some(operand) = operands_iter.next() {
        match operand.as_subtype() {
            boxed::NumSubtype::Int(int_ref) => {
                if let Some(reduced_int) = int_reduce(int_acc, int_ref.value()) {
                    int_acc = reduced_int;
                } else {
                    task.panic(format!("attempt to {} with overflow", op_name));
                }
            }
            boxed::NumSubtype::Float(float_ref) => {
                // Switch to float
                let float_acc = float_reduce(int_acc as f64, float_ref.value());
                return fold_float_op(task, operands_iter, float_acc, float_reduce).as_num_ref();
            }
        }
    }

    boxed::Int::new(task, int_acc).as_num_ref()
}

#[arret_rfi_derive::rust_fun("(All #{[N Num]} N & N -> N)")]
pub fn stdlib_add(
    task: &mut Task,
    initial_num: Gc<boxed::Num>,
    rest: Gc<boxed::List<boxed::Num>>,
) -> Gc<boxed::Num> {
    use std::iter;
    use std::ops::Add;

    fold_num_op(
        task,
        "add",
        iter::once(initial_num).chain(rest.iter()),
        0,
        i64::checked_add,
        f64::add,
    )
}

#[arret_rfi_derive::rust_fun("(All #{[N Num]} N & N -> N)")]
pub fn stdlib_mul(
    task: &mut Task,
    initial_num: Gc<boxed::Num>,
    rest: Gc<boxed::List<boxed::Num>>,
) -> Gc<boxed::Num> {
    use std::iter;
    use std::ops::Mul;

    fold_num_op(
        task,
        "multiply",
        iter::once(initial_num).chain(rest.iter()),
        1,
        i64::checked_mul,
        f64::mul,
    )
}

#[arret_rfi_derive::rust_fun("(All #{[N Num]} N & N -> N)")]
pub fn stdlib_sub(
    task: &mut Task,
    initial_num: Gc<boxed::Num>,
    rest: Gc<boxed::List<boxed::Num>>,
) -> Gc<boxed::Num> {
    use std::ops::Sub;

    match initial_num.as_subtype() {
        boxed::NumSubtype::Int(int_ref) => {
            if rest.is_empty() {
                boxed::Int::new(task, -int_ref.value()).as_num_ref()
            } else {
                fold_num_op(
                    task,
                    "subtract",
                    rest.iter(),
                    int_ref.value(),
                    i64::checked_sub,
                    f64::sub,
                )
            }
        }
        boxed::NumSubtype::Float(float_ref) => {
            if rest.is_empty() {
                boxed::Float::new(task, -float_ref.value()).as_num_ref()
            } else {
                fold_float_op(task, rest.iter(), float_ref.value(), f64::sub).as_num_ref()
            }
        }
    }
}

#[arret_rfi_derive::rust_fun("(Float & Float -> Float)")]
pub fn stdlib_div(initial_float: f64, rest: Gc<boxed::List<boxed::Float>>) -> f64 {
    if rest.is_empty() {
        initial_float.recip()
    } else {
        let mut acc = initial_float;
        for operand in rest.iter() {
            acc /= operand.value()
        }

        acc
    }
}

#[arret_rfi_derive::rust_fun("(Int Int -> Int)")]
pub fn stdlib_quot(task: &mut Task, numerator: i64, denominator: i64) -> i64 {
    match numerator.checked_div(denominator) {
        Some(result) => result,
        None => {
            task.panic("division by zero".to_owned());
            unreachable!("returned from panic")
        }
    }
}

#[arret_rfi_derive::rust_fun("(Int Int -> Int)")]
pub fn stdlib_rem(task: &mut Task, numerator: i64, denominator: i64) -> i64 {
    match numerator.checked_rem(denominator) {
        Some(result) => result,
        None => {
            task.panic("division by zero".to_owned());
            unreachable!("returned from panic")
        }
    }
}

#[arret_rfi_derive::rust_fun("(Float -> Float)")]
pub fn stdlib_sqrt(radicand: f64) -> f64 {
    radicand.sqrt()
}
