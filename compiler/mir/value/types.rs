use runtime::boxed;

use crate::mir::tagset::TypeTagSet;
use crate::mir::value::Value;

/// Returns a TypeTagSet containing the possible type tags for a given value
pub fn possible_type_tags_for_value(value: &Value) -> TypeTagSet {
    match value {
        Value::Const(any_ref) => any_ref.header().type_tag().into(),
        Value::ArretFun(_) | Value::RustFun(_) | Value::TyPred(_) | Value::EqPred => {
            boxed::TypeTag::FunThunk.into()
        }
        Value::List(fixed, rest) => {
            if fixed.is_empty() && rest.is_none() {
                boxed::TypeTag::Nil.into()
            } else {
                [boxed::TypeTag::Nil, boxed::TypeTag::TopPair]
                    .iter()
                    .collect()
            }
        }
        Value::Reg(reg_value) => (&reg_value.abi_type).into(),
        Value::Divergent => TypeTagSet::new(),
    }
}
