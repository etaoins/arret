use std::rc::Rc;

use arret_syntax::datum::DataStr;

use crate::hir;
use crate::mir::env_values::EnvValues;
use crate::ty;
use crate::ty::ty_args::TyArgs;

new_global_id_type!(ArretFunId);

#[derive(Clone, Debug)]
struct ArretFunConsts {
    id: ArretFunId,
    source_name: Option<DataStr>,
    env_ty_args: TyArgs<ty::Mono>,
    fun_expr: hir::Fun<hir::Inferred>,
}

#[derive(Clone, Debug)]
pub struct ArretFun {
    consts: Rc<ArretFunConsts>,
    env_values: EnvValues,
}

impl ArretFun {
    pub fn new(
        source_name: Option<DataStr>,
        env_ty_args: TyArgs<ty::Mono>,
        env_values: EnvValues,
        fun_expr: hir::Fun<hir::Inferred>,
    ) -> Self {
        Self {
            consts: Rc::new(ArretFunConsts {
                id: ArretFunId::alloc(),
                source_name,
                env_ty_args,
                fun_expr,
            }),
            env_values,
        }
    }

    pub fn id(&self) -> ArretFunId {
        self.consts.id
    }

    pub fn source_name(&self) -> &Option<DataStr> {
        &self.consts.source_name
    }

    pub fn env_ty_args(&self) -> &TyArgs<ty::Mono> {
        &self.consts.env_ty_args
    }

    pub fn env_values(&self) -> &EnvValues {
        &self.env_values
    }

    pub fn env_values_mut(&mut self) -> &mut EnvValues {
        &mut self.env_values
    }

    pub fn fun_expr(&self) -> &hir::Fun<hir::Inferred> {
        &self.consts.fun_expr
    }

    pub fn with_env_values(&self, env_values: EnvValues) -> ArretFun {
        ArretFun {
            consts: self.consts.clone(),
            env_values,
        }
    }

    /// Indicates if this `ArretFun` is used in multiple places
    ///
    /// This is a heuristic; if a `Fun` is bound to a variable this will return true regardless
    /// of the number of usages.
    pub fn has_multiple_usages(&self) -> bool {
        // This is a hack but has the benefit of not requiring a separate analysis pass
        Rc::strong_count(&self.consts) > 1
    }
}
