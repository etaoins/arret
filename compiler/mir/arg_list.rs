use std::rc::Rc;

use syntax::span::Span;

use runtime::abitype;

use crate::mir::builder::{Builder, BuiltReg};
use crate::mir::eval_hir::EvalHirCtx;
use crate::mir::ops;
use crate::mir::polymorph::PolymorphABI;
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
pub fn build_load_arg_list_value(b: &mut Builder, polymorph_abi: &PolymorphABI) -> LoadedArgList {
    let closure_reg = if polymorph_abi.has_closure {
        Some(b.alloc_local())
    } else {
        None
    };

    let fixed_reg_values = polymorph_abi
        .arret_fixed_params()
        .map(|abi_type| Rc::new(value::RegValue::new(b.alloc_local(), abi_type.clone())))
        .collect::<Vec<Rc<value::RegValue>>>();

    let rest_reg_value = polymorph_abi
        .arret_rest_param()
        .map(|abi_type| Rc::new(value::RegValue::new(b.alloc_local(), abi_type.clone())));

    let param_regs = closure_reg
        .into_iter()
        .map(|built_reg| built_reg.into())
        .chain(
            fixed_reg_values
                .iter()
                .map(|reg_value| reg_value.reg.into()),
        )
        .chain(rest_reg_value.iter().map(|reg_value| reg_value.reg.into()))
        .collect();

    let arg_list_value = Value::List(
        fixed_reg_values.into_iter().map(Value::Reg).collect(),
        rest_reg_value.map(|reg_value| Box::new(Value::Reg(reg_value))),
    );

    LoadedArgList {
        closure_reg,
        param_regs,
        arg_list_value,
    }
}

pub fn build_save_arg_list_to_regs<'a>(
    ehx: &mut EvalHirCtx,
    b: &mut Builder,
    span: Span,
    arg_list_value: Value,
    fixed_abi_types: impl DoubleEndedIterator<Item = &'a abitype::ABIType>,
    rest_abi_type: Option<&'a abitype::ABIType>,
) -> Vec<ops::RegId> {
    use crate::mir::value::build_reg::value_to_reg;

    let mut list_iter = arg_list_value.into_list_iter();

    let mut arg_regs = vec![];
    for abi_type in fixed_abi_types {
        let fixed_value = list_iter.next_unchecked(b, span);
        let reg_id = value_to_reg(ehx, b, span, &fixed_value, &abi_type);
        arg_regs.push(reg_id.into());
    }

    if let Some(rest_abi_type) = rest_abi_type {
        let reg_id = value_to_reg(ehx, b, span, &list_iter.into_rest(), &rest_abi_type);
        arg_regs.push(reg_id.into());
    };

    arg_regs
}
