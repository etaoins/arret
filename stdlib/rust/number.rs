use arret_runtime::binding::*;
use arret_runtime::boxed;
use arret_runtime::boxed::refs::Gc;
use arret_runtime::task::Task;

fn compare_nums<IC, FC>(
    initial: Gc<boxed::Num>,
    rest: Gc<boxed::List<boxed::Num>>,
    int_comparator: IC,
    float_comparator: FC,
) -> bool
where
    IC: Fn(&i64, &i64) -> bool,
    FC: Fn(&f64, &f64) -> bool,
{
    let mut left = initial;

    for right in rest.iter() {
        use boxed::NumSubtype;

        let result = match (left.as_subtype(), right.as_subtype()) {
            (NumSubtype::Int(left_ref), NumSubtype::Int(right_ref)) => {
                int_comparator(&left_ref.value(), &right_ref.value())
            }
            (NumSubtype::Float(left_ref), NumSubtype::Int(right_ref)) => {
                float_comparator(&left_ref.value(), &(right_ref.value() as f64))
            }
            (NumSubtype::Int(left_ref), NumSubtype::Float(right_ref)) => {
                float_comparator(&(left_ref.value() as f64), &right_ref.value())
            }
            (NumSubtype::Float(left_ref), NumSubtype::Float(right_ref)) => {
                float_comparator(&left_ref.value(), &right_ref.value())
            }
        };

        if !result {
            return false;
        }

        left = right;
    }

    true
}

#[arret_rfi_derive::rust_fun("(Num -> Float)")]
pub fn stdlib_float(input: Gc<boxed::Num>) -> f64 {
    match input.as_subtype() {
        boxed::NumSubtype::Int(int_ref) => int_ref.value() as f64,
        boxed::NumSubtype::Float(float_ref) => float_ref.value(),
    }
}

#[arret_rfi_derive::rust_fun("(Num -> Int)")]
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

#[arret_rfi_derive::rust_fun("(Num & Num -> Bool)")]
pub fn stdlib_num_lt(initial: Gc<boxed::Num>, rest: Gc<boxed::List<boxed::Num>>) -> bool {
    compare_nums(initial, rest, i64::lt, f64::lt)
}

#[arret_rfi_derive::rust_fun("(Num & Num -> Bool)")]
pub fn stdlib_num_le(initial: Gc<boxed::Num>, rest: Gc<boxed::List<boxed::Num>>) -> bool {
    compare_nums(initial, rest, i64::le, f64::le)
}

#[arret_rfi_derive::rust_fun("(Num & Num -> Bool)")]
pub fn stdlib_num_eq(initial: Gc<boxed::Num>, rest: Gc<boxed::List<boxed::Num>>) -> bool {
    compare_nums(initial, rest, i64::eq, f64::eq)
}

#[arret_rfi_derive::rust_fun("(Num & Num -> Bool)")]
pub fn stdlib_num_gt(initial: Gc<boxed::Num>, rest: Gc<boxed::List<boxed::Num>>) -> bool {
    compare_nums(initial, rest, i64::gt, f64::gt)
}

#[arret_rfi_derive::rust_fun("(Num & Num -> Bool)")]
pub fn stdlib_num_ge(initial: Gc<boxed::Num>, rest: Gc<boxed::List<boxed::Num>>) -> bool {
    compare_nums(initial, rest, i64::ge, f64::ge)
}
