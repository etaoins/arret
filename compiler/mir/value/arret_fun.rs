use std::rc::Rc;

use syntax::datum::DataStr;
use syntax::span::Span;

use crate::hir;
use crate::mir::closure::Closure;
use crate::ty;
use crate::ty::ty_args::TyArgs;

new_global_id_type!(ArretFunId);

#[derive(Clone, Debug)]
struct ArretFunConsts {
    id: ArretFunId,
    span: Span,
    source_name: Option<DataStr>,
    env_ty_args: TyArgs<ty::Mono>,
    fun_expr: hir::Fun<hir::Inferred>,
}

#[derive(Clone, Debug)]
pub struct ArretFun {
    consts: Rc<ArretFunConsts>,
    closure: Closure,
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
            consts: Rc::new(ArretFunConsts {
                id: ArretFunId::alloc(),
                span,
                source_name,
                env_ty_args,
                fun_expr,
            }),
            closure,
        }
    }

    pub fn id(&self) -> ArretFunId {
        self.consts.id
    }

    pub fn span(&self) -> Span {
        self.consts.span
    }

    pub fn source_name(&self) -> &Option<DataStr> {
        &self.consts.source_name
    }

    pub fn env_ty_args(&self) -> &TyArgs<ty::Mono> {
        &self.consts.env_ty_args
    }

    pub fn closure(&self) -> &Closure {
        &self.closure
    }

    pub fn closure_mut(&mut self) -> &mut Closure {
        &mut self.closure
    }

    pub fn fun_expr(&self) -> &hir::Fun<hir::Inferred> {
        &self.consts.fun_expr
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
