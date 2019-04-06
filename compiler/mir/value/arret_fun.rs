use std::rc::Rc;

use syntax::datum::DataStr;
use syntax::span::Span;

use crate::hir;
use crate::mir::closure::Closure;
use crate::ty;
use crate::ty::ty_args::TyArgs;

new_global_id_type!(ArretFunId);

#[derive(Clone, Debug)]
pub struct ArretFun {
    id: ArretFunId,
    span: Span,
    source_name: Option<DataStr>,
    env_ty_args: TyArgs<ty::Mono>,
    closure: Closure,
    fun_expr: Rc<hir::Fun<hir::Inferred>>,
}

impl ArretFun {
    pub fn new(
        span: Span,
        source_name: Option<DataStr>,
        env_ty_args: TyArgs<ty::Mono>,
        closure: Closure,
        fun_expr: hir::Fun<hir::Inferred>,
    ) -> Self {
        Self {
            id: ArretFunId::alloc(),
            span,
            source_name,
            env_ty_args,
            closure,
            fun_expr: Rc::new(fun_expr),
        }
    }

    pub fn id(&self) -> ArretFunId {
        self.id
    }

    pub fn span(&self) -> Span {
        self.span
    }

    pub fn source_name(&self) -> &Option<DataStr> {
        &self.source_name
    }

    pub fn env_ty_args(&self) -> &TyArgs<ty::Mono> {
        &self.env_ty_args
    }

    pub fn closure(&self) -> &Closure {
        &self.closure
    }

    pub fn closure_mut(&mut self) -> &mut Closure {
        &mut self.closure
    }

    pub fn fun_expr(&self) -> &hir::Fun<hir::Inferred> {
        &self.fun_expr
    }

    /// Indicates if this `ArretFun` is used in multiple places
    ///
    /// This is a heuristic; if a `Fun` is bound to a variable this will return true regardless
    /// of the number of usages.
    pub fn has_multiple_usages(&self) -> bool {
        // This is a hack but has the benefit of not requiring a separate analysis pass
        Rc::strong_count(&self.fun_expr) > 1
    }
}
