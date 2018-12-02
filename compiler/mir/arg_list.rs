use std::rc::Rc;

use crate::mir::builder::{Builder, BuiltReg};
use crate::mir::ops;
use crate::mir::value;
use crate::mir::value::Value;

pub struct LoadedArgList {
    /// Reg holding the closure parameter
    pub closure_reg: Option<BuiltReg>,

    /// All regs the function takes including the closure
    pub param_regs: Box<[ops::RegId]>,

    /// Built list value of the arguments
    pub arg_list_value: Value,
}

/// Builds the regs and ops for loading the argument list of a function
///
/// This results in an argument list value which contains all arguments passed to the function.
pub fn build_load_arg_list_value(
    b: &mut Builder,
    wanted_abi: &ops::OpsABI,
    has_closure: bool,
    has_rest: bool,
) -> LoadedArgList {
    let closure_reg = if wanted_abi.external_call_conv || has_closure {
        Some(b.alloc_local())
    } else {
        None
    };

    let mut abi_params_iter = wanted_abi.params.iter();

    if closure_reg.is_some() {
        abi_params_iter.next();
    }

    let rest_reg_value = if has_rest {
        let abi_type = abi_params_iter.next_back().unwrap();

        Some(Rc::new(value::RegValue::new(
            b.alloc_local(),
            abi_type.clone(),
        )))
    } else {
        None
    };

    let fixed_reg_values = abi_params_iter
        .map(|abi_type| {
            Rc::new(value::RegValue::new(
                b.alloc_local(),
                abi_type.clone(),
            ))
        })
        .collect::<Vec<Rc<value::RegValue>>>();

    let param_regs = closure_reg
        .into_iter()
        .map(|built_reg| built_reg.into())
        .chain(
            fixed_reg_values
                .iter()
                .map(|reg_value| reg_value.reg.into()),
        )
        .chain(rest_reg_value.iter().map(|reg_value| reg_value.reg.into()))
        .collect::<Vec<ops::RegId>>()
        .into_boxed_slice();

    let arg_list_value = Value::List(
        fixed_reg_values
            .into_iter()
            .map(Value::Reg)
            .collect::<Vec<Value>>()
            .into_boxed_slice(),
        rest_reg_value.map(|reg_value| Box::new(Value::Reg(reg_value))),
    );

    LoadedArgList {
        closure_reg,
        param_regs,
        arg_list_value,
    }
}
