use std::collections::{BTreeMap, BTreeSet, HashMap};
use std::rc::Rc;

use arret_runtime::intern;

use super::AnalysedFun;
use crate::mir::ops;

fn add_op_global_interned_names(names: &mut BTreeSet<Rc<str>>, op: &ops::Op) {
    use ops::OpKind;

    match op.kind() {
        OpKind::ConstInternedSym(_, name) | OpKind::ConstBoxedSym(_, name) => {
            if intern::InternedSym::try_from_inline_name(name).is_none() {
                names.insert(name.clone());
            }
        }
        OpKind::Cond(ops::CondOp {
            true_ops,
            false_ops,
            ..
        }) => {
            for op in true_ops.iter().rev().chain(false_ops.iter().rev()) {
                add_op_global_interned_names(names, op);
            }
        }
        _ => {}
    }
}

fn add_fun_global_interned_names(fun: &ops::Fun, names: &mut BTreeSet<Rc<str>>) {
    for op in fun.ops.iter() {
        add_op_global_interned_names(names, op);
    }
}

/// Finds all global interned names in the program and returns them in sorted order
pub fn calc_program_global_interned_names(
    private_funs: &HashMap<ops::PrivateFunId, AnalysedFun<'_>>,
    entry_fun: &ops::Fun,
) -> BTreeMap<Rc<str>, intern::InternedSym> {
    let mut names: BTreeSet<Rc<str>> = BTreeSet::new();

    for fun in private_funs
        .values()
        .map(|af| af.ops_fun)
        .chain(std::iter::once(entry_fun))
    {
        add_fun_global_interned_names(fun, &mut names);
    }

    names
        .into_iter()
        .enumerate()
        .map(|(idx, name)| (name, intern::InternedSym::from_global_index(idx as u32)))
        .collect()
}

#[cfg(test)]
mod test {
    use super::*;
    use arret_runtime::abitype::RetABIType;
    use arret_runtime::boxed;
    use arret_syntax::span::EMPTY_SPAN;

    #[test]
    fn simple_global_interned_names() {
        let param_reg = ops::RegId::alloc();

        let inline_reg = ops::RegId::alloc();
        let alpha_reg = ops::RegId::alloc();
        let beta_reg = ops::RegId::alloc();
        let gamma_reg = ops::RegId::alloc();

        let test_fun = ops::Fun {
            span: EMPTY_SPAN,
            source_name: None,

            abi: ops::OpsABI {
                call_conv: ops::CallConv::FastCC,
                params: Box::new([boxed::TypeTag::Int.into()]),
                ret: RetABIType::Void,
            },
            param_regs: Box::new([]),
            ops: Box::new([
                ops::OpKind::ConstBoxedSym(inline_reg, "inline".into()).into(),
                ops::OpKind::Cond(ops::CondOp {
                    reg_phi: None,
                    test_reg: param_reg,
                    true_ops: Box::new([ops::OpKind::ConstBoxedSym(
                        beta_reg,
                        "beta NOT INLINE".into(),
                    )
                    .into()]),
                    false_ops: Box::new([ops::OpKind::ConstInternedSym(
                        gamma_reg,
                        "gamma NOT INLINE".into(),
                    )
                    .into()]),
                })
                .into(),
                ops::OpKind::ConstBoxedSym(alpha_reg, "alpha NOT INLINE".into()).into(),
            ]),
        };

        let global_interned_names = calc_program_global_interned_names(&HashMap::new(), &test_fun);

        assert_eq!(
            [
                ("alpha NOT INLINE", 0u32),
                ("beta NOT INLINE", 1u32),
                ("gamma NOT INLINE", 2u32)
            ]
            .iter()
            .map(|(name, idx)| ((*name).into(), intern::InternedSym::from_global_index(*idx)))
            .collect::<BTreeMap<Rc<str>, _>>(),
            global_interned_names
        );
    }
}
