use runtime::binding::*;
use runtime::boxed;
use runtime::boxed::refs::Gc;
use runtime::task::Task;

use rfi_derive;

#[rfi_derive::rust_fun("(Num -> Float)")]
pub fn stdlib_float(input: Gc<boxed::Num>) -> f64 {
    match input.as_subtype() {
        boxed::NumSubtype::Int(int_ref) => int_ref.value() as f64,
        boxed::NumSubtype::Float(float_ref) => float_ref.value(),
    }
}

#[rfi_derive::rust_fun("(Num -> Int)")]
pub fn stdlib_int(task: &mut Task, input: Gc<boxed::Num>) -> i64 {
    match input.as_subtype() {
        boxed::NumSubtype::Int(int_ref) => int_ref.value(),
        boxed::NumSubtype::Float(float_ref) => {
            let float_val = float_ref.value();

            if float_val.is_nan() {
                task.panic(format!(
                    "Float value `{}` is not a number; cannot convert to Int",
                    float_val
                ));
            } else if float_val.is_infinite() {
                task.panic(format!(
                    "Float value `{}` is infinite; cannot convert to Int",
                    float_val
                ));
            }

            float_val as i64
        }
    }
}
