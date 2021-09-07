use arret_syntax::span::Span;

use arret_runtime::abitype;

use crate::mir::builder::{Builder, BuiltReg};
use crate::mir::eval_hir::EvalHirCtx;
use crate::mir::ops;
use crate::mir::polymorph::PolymorphAbi;
use crate::mir::value::Value;
use crate::ty;

pub struct LoadedArgList {
    /// Reg holding the captures parameter
    pub captures_reg: Option<BuiltReg>,

    /// All regs the function takes including the captures
    pub param_regs: Box<[ops::RegId]>,

    /// Built list value of the arguments
    pub arg_list_value: Value,
}

/// Builds the regs and ops for loading the argument list of a function
///
/// This results in an argument list value which contains all arguments passed to the function.
pub fn build_load_arg_list_value(
    ehx: &mut EvalHirCtx,
    b: &mut Builder,
    polymorph_abi: &PolymorphAbi,
    param_list_poly: &ty::List<ty::Poly>,
) -> LoadedArgList {
    use crate::mir::value::from_reg::reg_to_value;
    use crate::ty::list_iter::ListIterator;

    let captures_reg: Option<BuiltReg> = if polymorph_abi.has_captures {
        Some(b.alloc_local())
    } else {
        None
    };

    let mut param_list_poly_iter = ListIterator::new(param_list_poly);

    let fixed_reg_values: Vec<(ops::RegId, Value)> = polymorph_abi
        .fixed_params
        .iter()
        .map(|abi_type| {
            let reg = b.alloc_local();
            let param_type = param_list_poly_iter.next().unwrap();

            (reg.into(), reg_to_value(ehx, reg, abi_type, param_type))
        })
        .collect();

    let rest_reg_value: Option<(ops::RegId, Value)> =
        polymorph_abi.rest_param.as_ref().map(|abi_type| {
            let reg = b.alloc_local();
            let tail_type = param_list_poly_iter.tail_type();

            (
                reg.into(),
                reg_to_value(ehx, reg, abi_type, &tail_type.into()),
            )
        });

    let param_regs = captures_reg
        .into_iter()
        .map(Into::into)
        .chain(fixed_reg_values.iter().map(|(reg, _)| *reg))
        .chain(rest_reg_value.iter().map(|(reg, _)| *reg))
        .collect();

    let arg_list_value = Value::List(
        fixed_reg_values
            .into_iter()
            .map(|(_, value)| value)
            .collect(),
        rest_reg_value.map(|(_, value)| Box::new(value)),
    );

    LoadedArgList {
        captures_reg,
        param_regs,
        arg_list_value,
    }
}

pub fn build_save_arg_list_to_regs<'a>(
    ehx: &mut EvalHirCtx,
    b: &mut Builder,
    span: Span,
    arg_list_value: Value,
    fixed_abi_types: impl ExactSizeIterator<Item = &'a abitype::AbiType>,
    rest_abi_type: Option<&'a abitype::AbiType>,
) -> Vec<ops::RegId> {
    use crate::mir::value::build_reg::value_to_reg;

    let mut list_iter = arg_list_value.into_unsized_list_iter();

    let mut arg_regs = vec![];
    for abi_type in fixed_abi_types {
        let fixed_value = list_iter.next_unchecked(b, span);
        let reg_id = value_to_reg(ehx, b, span, &fixed_value, abi_type);
        arg_regs.push(reg_id.into());
    }

    if let Some(rest_abi_type) = rest_abi_type {
        let reg_id = value_to_reg(ehx, b, span, &list_iter.into_rest(), rest_abi_type);
        arg_regs.push(reg_id.into());
    };

    arg_regs
}
