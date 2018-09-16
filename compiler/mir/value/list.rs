use crate::mir::value::Value;

pub fn list_value_length(value: &Value) -> Option<usize> {
    match value {
        Value::List(fixed, rest) => {
            let fixed_len = fixed.len();

            match rest {
                Some(rest) => list_value_length(rest).map(|rest_len| fixed_len + rest_len),
                None => Some(fixed_len),
            }
        }
        _ => None,
    }
}
