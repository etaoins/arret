use std::collections::BTreeSet;

use arret_syntax::span::Span;

use arret_runtime::boxed;

use crate::mir::builder::Builder;
use crate::mir::error;
use crate::mir::error::{Error, Result};
use crate::mir::eval_hir::EvalHirCtx;
use crate::mir::ops;
use crate::mir::polymorph::PolymorphABI;
use crate::mir::value;
use crate::mir::value::Value;

fn ideal_polymorph_abi_for_arret_fun(arret_fun: &value::ArretFun) -> PolymorphABI {
    use crate::hir::destruc::poly_for_list_destruc;
    use crate::mir::polymorph::polymorph_abi_for_list_ty;

    let has_env = !arret_fun.env_values().free_values.is_empty();
    let fun_expr = arret_fun.fun_expr();
    let param_list_type = poly_for_list_destruc(&fun_expr.params);

    polymorph_abi_for_list_ty(has_env, &param_list_type, &fun_expr.ret_ty)
}

fn add_ops_categories<'a>(
    categories: &mut BTreeSet<ops::OpCategory>,
    ops: impl IntoIterator<Item = &'a ops::Op>,
) {
    for op in ops {
        categories.insert(op.kind().category());

        if let ops::OpKind::Cond(cond_op) = op.kind() {
            add_ops_categories(categories, cond_op.true_ops.iter());
            add_ops_categories(categories, cond_op.false_ops.iter());
        }
    }
}

fn op_category_to_string(category: ops::OpCategory) -> &'static str {
    use crate::mir::ops::OpCategory;

    match category {
        OpCategory::AllocBoxed => ":alloc-boxed",
        OpCategory::Call => ":call",
        OpCategory::ConstBox => ":const-box",
        OpCategory::ConstCastBoxed => ":const-cast-box",
        OpCategory::ConstReg => ":const-reg",
        OpCategory::Cond => ":cond",
        OpCategory::MakeCallback => ":make-callback",
        OpCategory::MemLoad => ":mem-load",
        OpCategory::CastBoxed => ":cast-boxed",
        OpCategory::RegOp => ":reg-op",
        OpCategory::Ret => ":ret",
        OpCategory::Unreachable => ":unreachable",
    }
}

pub fn fn_op_categories(
    ehx: &mut EvalHirCtx,
    b: &mut Option<Builder>,
    span: Span,
    arg_list_value: &Value,
) -> Result<Option<Value>> {
    let mut iter = arg_list_value.unsized_list_iter();
    let single_arg = iter.next_unchecked(b, span);

    let arret_fun = if let Value::ArretFun(arret_fun) = single_arg {
        arret_fun
    } else {
        return Err(Error::Panic(error::Panic::new(
            span,
            "argument must be an Arret function".to_owned(),
        )));
    };

    let ideal_polymorph_abi = ideal_polymorph_abi_for_arret_fun(&arret_fun);
    let ops_fun = ehx.ops_for_arret_fun(&arret_fun, ideal_polymorph_abi);

    let mut categories = BTreeSet::<ops::OpCategory>::new();
    add_ops_categories(&mut categories, ops_fun.ops.iter());

    let category_list = boxed::List::from_values(
        ehx,
        categories.into_iter().map(op_category_to_string),
        boxed::Sym::new,
    );

    Ok(Some(category_list.into()))
}
