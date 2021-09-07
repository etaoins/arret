use std::collections::HashMap;
use std::result;
use std::sync::Arc;

use crate::context::ModuleId;
use crate::hir;
use crate::hir::destruc;
use crate::rfi;
use crate::ty;
use crate::ty::list_iter::ListIterator;
use crate::ty::purity;
use crate::ty::purity::Purity;
use crate::ty::record;
use crate::ty::ty_args::TyArgs;
use crate::ty::Ty;
use crate::typeck;
use crate::typeck::dce::expr_can_side_effect;
use crate::typeck::error::{Error, ErrorKind, IsNotRetTy, WantedArity};

use arret_syntax::datum::Datum;
use arret_syntax::span::Span;

type Result<T> = result::Result<T, Error>;

/// Result of inferring the type for a HIR expression
pub struct InferredNode {
    /// Expression with all free types replaced with poly types
    expr: hir::Expr<hir::Inferred>,
    /// Type conditions depending the poly type of this node
    type_conds: Vec<VarTypeCond>,
}

impl InferredNode {
    fn is_divergent(&self) -> bool {
        self.expr.result_ty.is_never()
    }

    pub fn into_expr(self) -> hir::Expr<hir::Inferred> {
        self.expr
    }

    pub fn result_ty(&self) -> &ty::Ref<ty::Poly> {
        &self.expr.result_ty
    }
}

#[derive(Clone, Copy, PartialEq)]
enum NodeBool {
    True,
    False,
}

struct VarTypeCond {
    when: NodeBool,
    override_local_id: hir::LocalId,
    override_type: ty::Ref<ty::Poly>,
}

impl VarTypeCond {
    fn with_when(self, when: NodeBool) -> VarTypeCond {
        VarTypeCond { when, ..self }
    }

    fn into_inverted(self) -> VarTypeCond {
        VarTypeCond {
            when: if self.when == NodeBool::True {
                NodeBool::False
            } else {
                NodeBool::True
            },
            ..self
        }
    }
}

new_indexing_id_type!(FreeTyId, u32);
new_indexing_id_type!(InputDefId, u32);

/// Partially inferred function application
///
/// The function has been inferred while the arguments have not
struct FunApp {
    fun_expr: hir::Expr<hir::Inferred>,
    fixed_arg_exprs: Vec<hir::Expr<hir::Lowered>>,
    rest_arg_expr: Option<hir::Expr<hir::Lowered>>,
}

enum VarType {
    // Introduced a definition that has yet to be processed
    Pending(InputDefId),
    // (def) currently having its type inferred
    Recursive,

    /// Type depends on a value that failed to type check
    Error,

    /// Scalar value being inferred
    ParamScalar(FreeTyId),

    /// Rest list being inferred
    ///
    /// The referenced free type is the member type of the uniform rest list.
    ParamRest(FreeTyId),

    // Declared or previously inferred type
    Known(ty::Ref<ty::Poly>),
}

#[derive(Clone)]
enum PurityVar {
    Free(purity::Ref),
    Known(purity::Ref),
}

#[derive(Clone)]
struct RetExprResultUse<'a> {
    /// Span of the declared return type if any
    ret_type_span: Option<Span>,
    ret_type: &'a ty::Ref<ty::Poly>,
    known_self_type: Option<&'a ty::Fun>,
}

/// Describes the use of an expression's result value
#[derive(Clone)]
enum ResultUse<'a> {
    /// Non-return expression with a used value
    InnerExpr(&'a ty::Ref<ty::Poly>),

    /// Expression used as the return value of a function
    RetExpr(RetExprResultUse<'a>),

    /// Expression with an unused value
    ///
    /// The most common example is a non-terminal expression in a `(do)`.
    Unused(&'a ty::Ref<ty::Poly>),
}

impl<'a> ResultUse<'a> {
    /// Returns the required type for an expression's result value
    fn required_type(&self) -> &'a ty::Ref<ty::Poly> {
        match self {
            ResultUse::InnerExpr(required_type) => required_type,
            ResultUse::RetExpr(RetExprResultUse { ret_type, .. }) => ret_type,
            ResultUse::Unused(required_type) => required_type,
        }
    }

    /// Returns `true` if the expression's result value is used
    fn is_used(&self) -> bool {
        !matches!(self, ResultUse::Unused(_))
    }
}

impl PurityVar {
    fn into_poly(self) -> purity::Ref {
        match self {
            PurityVar::Free(poly) => poly,
            PurityVar::Known(poly) => poly,
        }
    }
}

enum InputDef {
    Pending(hir::Def<hir::Lowered>),
    Complete,
}

pub type InferredLocals = HashMap<hir::LocalId, ty::Ref<ty::Poly>>;
pub type InferredModuleVars = HashMap<ModuleId, Arc<InferredLocals>>;

pub struct InferredModule {
    pub inferred_locals: InferredLocals,
    pub defs: Vec<hir::Def<hir::Inferred>>,
}

struct RecursiveDefsCtx<'types> {
    input_defs: Vec<InputDef>,
    complete_defs: Vec<hir::Def<hir::Inferred>>,

    // The inferred types for free types in the order they're encountered
    //
    // Each (def), (let) and (fn) push entries to `free_ty_polys` before they evaluate their body
    // and then pop them off afterwards.
    free_ty_polys: Vec<ty::Ref<ty::Poly>>,

    self_locals: HashMap<hir::LocalId, VarType>,
    imported_vars: &'types InferredModuleVars,
}

/// Tries to convert a polymorphic type to a literal boolean value
fn try_to_bool(poly: &ty::Ref<ty::Poly>) -> Option<bool> {
    match poly {
        ty::Ref::Fixed(Ty::LitBool(v)) => Some(*v),
        _ => None,
    }
}

fn unify_app_purity(pv: &mut PurityVar, app_purity: &purity::Ref) {
    if let PurityVar::Free(ref mut free_purity) = pv {
        *free_purity = ty::unify::unify_purity_refs(free_purity, app_purity)
    };
}

/// Inspects the mismatched sub and parent types to attempt to produce an understandable type error
fn error_kind_for_type_error(
    value_poly: &ty::Ref<ty::Poly>,
    value_use: &ResultUse<'_>,
) -> ErrorKind {
    if let ty::Ref::Fixed(Ty::TopFun(top_fun)) = value_use.required_type() {
        let topmost_fun = ty::TopFun::new(Purity::Impure.into(), Ty::Any.into()).into();
        let impure_top_fun = ty::TopFun::new(Purity::Impure.into(), top_fun.ret().clone()).into();

        if !ty::is_a::ty_ref_is_a(value_poly, &topmost_fun) {
            // We aren't a function at all
            return ErrorKind::IsNotFun(value_poly.clone());
        } else if ty::is_a::ty_ref_is_a(value_poly, &impure_top_fun) {
            // We have the right return type but the wrong purity
            return ErrorKind::IsNotPurity(value_poly.clone(), top_fun.purity().clone());
        }
    }

    match value_use {
        ResultUse::Unused(required_type) | ResultUse::InnerExpr(required_type) => {
            ErrorKind::IsNotTy(value_poly.clone(), (*required_type).clone())
        }
        ResultUse::RetExpr(RetExprResultUse {
            ret_type_span,
            ret_type,
            ..
        }) => ErrorKind::IsNotRetTy(IsNotRetTy::new(
            value_poly.clone(),
            (*ret_type).clone(),
            *ret_type_span,
        )),
    }
}

/// Ensures `value_poly` is appropriate for `value_use`
fn ensure_is_a(
    span: Span,
    value_poly: &ty::Ref<ty::Poly>,
    value_use: &ResultUse<'_>,
) -> Result<()> {
    if !value_use.is_used() {
        // We don't throw type errors for unused values
        return Ok(());
    }

    if ty::is_a::ty_ref_is_a(value_poly, value_use.required_type()) {
        return Ok(());
    }

    let error_kind = error_kind_for_type_error(value_poly, value_use);
    Err(Error::new(span, error_kind))
}

fn member_type_for_poly_list(
    span: Span,
    poly_type: &ty::Ref<ty::Poly>,
) -> Result<ty::Ref<ty::Poly>> {
    if poly_type == &Ty::Any.into() {
        return Ok(Ty::Any.into());
    }

    let list = poly_type
        .find_member(|t| {
            if let Ty::List(list) = t {
                Some(list)
            } else {
                None
            }
        })
        .ok_or_else(|| {
            Error::new(
                span,
                ErrorKind::IsNotTy(
                    poly_type.clone(),
                    ty::List::new_uniform(Ty::Any.into()).into(),
                ),
            )
        })?;

    Ok(ListIterator::new(list).collect_rest())
}

/// Preserves expressions for their side effects
///
/// `side_effect_exprs` are discarded if they can't cause side effects. `value_expr` will be used as
/// the value of the returned expression.
fn keep_exprs_for_side_effects(
    side_effect_exprs: impl IntoIterator<Item = hir::Expr<hir::Inferred>>,
    value_expr: hir::Expr<hir::Inferred>,
) -> hir::Expr<hir::Inferred> {
    let mut needed_exprs: Vec<_> = side_effect_exprs
        .into_iter()
        .filter(expr_can_side_effect)
        .collect();

    if needed_exprs.is_empty() {
        // We don't need any of the `side_effect_exprs`
        return value_expr;
    }

    let result_ty = value_expr.result_ty.clone();
    needed_exprs.push(value_expr);

    hir::Expr {
        result_ty,
        kind: hir::ExprKind::Do(needed_exprs),
    }
}

impl<'types> RecursiveDefsCtx<'types> {
    fn new(
        imported_vars: &'types InferredModuleVars,
        defs: Vec<hir::Def<hir::Lowered>>,
    ) -> RecursiveDefsCtx<'types> {
        let mut self_locals = HashMap::new();

        // We do this in reverse order because we infer our defs in reverse order. This doesn't
        // matter for correctness. However, presumably most definitions have more dependencies
        // before them than after them. Visiting them in forward order should cause less
        // recursive resolution.
        let input_defs = defs
            .into_iter()
            .rev()
            .enumerate()
            .map(|(idx, hir_def)| {
                let def_id = InputDefId::new(idx);

                typeck::destruc::visit_locals(&hir_def.destruc, &mut |local_id, decl_type| {
                    let var_type = match decl_type {
                        hir::DeclTy::Known(poly_type) => VarType::Known(poly_type.clone()),
                        hir::DeclTy::Free => {
                            // Record the definition ID so we can deal with forward type references
                            VarType::Pending(def_id)
                        }
                    };

                    self_locals.insert(local_id, var_type);
                });

                InputDef::Pending(hir_def)
            })
            .collect::<Vec<InputDef>>();

        RecursiveDefsCtx {
            complete_defs: Vec::with_capacity(input_defs.len()),
            input_defs,
            free_ty_polys: vec![],

            self_locals,
            imported_vars,
        }
    }

    fn new_local_ref_node(
        &self,
        span: Span,
        local_id: hir::LocalId,
        poly_type: ty::Ref<ty::Poly>,
    ) -> InferredNode {
        // We can't override type conditions across modules
        let type_conds = if poly_type == Ty::Bool.into() {
            // This seems useless but it allows occurrence typing to work if this type
            // flows through another node such as `(do)` or `(let)`
            vec![
                VarTypeCond {
                    when: NodeBool::True,
                    override_local_id: local_id,
                    override_type: Ty::LitBool(true).into(),
                },
                VarTypeCond {
                    when: NodeBool::False,
                    override_local_id: local_id,
                    override_type: Ty::LitBool(false).into(),
                },
            ]
        } else {
            vec![]
        };

        InferredNode {
            expr: hir::Expr {
                result_ty: poly_type,
                kind: hir::ExprKind::LocalRef(span, local_id),
            },
            type_conds,
        }
    }

    fn insert_free_ty(&mut self, initial_type: ty::Ref<ty::Poly>) -> FreeTyId {
        FreeTyId::new_entry_id(&mut self.free_ty_polys, initial_type)
    }

    fn visit_lit(&mut self, result_use: &ResultUse<'_>, datum: Datum) -> Result<InferredNode> {
        let lit_type = ty::datum::ty_ref_for_datum(&datum);
        ensure_is_a(datum.span(), &lit_type, result_use)?;

        Ok(InferredNode {
            expr: hir::Expr {
                result_ty: lit_type,
                kind: hir::ExprKind::Lit(datum),
            },
            type_conds: vec![],
        })
    }

    /// Calls the passed function with var types overridden by the specified type conds
    ///
    /// The var types will be restored after the function returns.
    #[allow(clippy::needless_collect)]
    fn with_type_conds_applied<F, R>(
        &mut self,
        type_conds: &[VarTypeCond],
        node_bool: NodeBool,
        inner: F,
    ) -> R
    where
        F: FnOnce(&mut Self) -> R,
    {
        let restore_var_types = type_conds
            .iter()
            .filter(|tc| tc.when == node_bool)
            .map(|type_cond| {
                let VarTypeCond {
                    override_local_id,
                    ref override_type,
                    ..
                } = *type_cond;

                (
                    override_local_id,
                    self.self_locals
                        .insert(override_local_id, VarType::Known(override_type.clone()))
                        .unwrap(),
                )
            })
            .collect::<Vec<(hir::LocalId, VarType)>>();

        let result = inner(self);

        // Restore the original types
        // We need to use `rev()` to make sure we restore the original type if multiple conds
        // applied to a single var.
        for (local_id, original_var_type) in restore_var_types.into_iter().rev() {
            self.self_locals.insert(local_id, original_var_type);
        }

        result
    }

    fn visit_cond(
        &mut self,
        pv: &mut PurityVar,
        result_use: &ResultUse<'_>,
        cond: hir::Cond<hir::Lowered>,
    ) -> Result<InferredNode> {
        use std::iter;

        let hir::Cond {
            span,
            test_expr,
            true_expr,
            false_expr,
            ..
        } = cond;

        let test_node = self.visit_expr(pv, &ResultUse::InnerExpr(&Ty::Bool.into()), test_expr)?;
        let test_known_bool = try_to_bool(test_node.result_ty());

        // If a branch isn't taken it doesn't need to match the type of the cond expression
        let unused_use = &ResultUse::Unused(result_use.required_type());
        let (true_result_use, false_result_use) = if test_node.is_divergent() {
            (unused_use, unused_use)
        } else {
            match test_known_bool {
                Some(true) => (result_use, unused_use),
                Some(false) => (unused_use, result_use),
                None => (result_use, result_use),
            }
        };

        let true_node =
            self.with_type_conds_applied(&test_node.type_conds, NodeBool::True, |s| {
                s.visit_expr(pv, true_result_use, true_expr)
            })?;

        let false_node =
            self.with_type_conds_applied(&test_node.type_conds, NodeBool::False, |s| {
                s.visit_expr(pv, false_result_use, false_expr)
            })?;

        if test_node.is_divergent() {
            // Test diverged; we don't need the branches
            return Ok(test_node);
        }

        // If the test is static then we can significantly optimise
        match test_known_bool {
            Some(true) => Ok(InferredNode {
                // Preserve the test expr in case it has side effects but remove the cond
                expr: keep_exprs_for_side_effects(iter::once(test_node.expr), true_node.expr),
                ..true_node
            }),
            Some(false) => Ok(InferredNode {
                expr: keep_exprs_for_side_effects(iter::once(test_node.expr), false_node.expr),
                ..false_node
            }),
            None => {
                let result_ty =
                    ty::unify::unify_to_ty_ref(true_node.result_ty(), false_node.result_ty());

                let false_node_bool = try_to_bool(false_node.result_ty());
                let true_node_bool = try_to_bool(true_node.result_ty());

                let mut type_conds: Vec<VarTypeCond> = test_node
                    .type_conds
                    .into_iter()
                    .filter_map(|type_cond| match type_cond.when {
                        NodeBool::True => {
                            if false_node_bool == Some(false) {
                                // If the false node is always false then our result type being
                                // true implies the test was true
                                Some(type_cond.with_when(NodeBool::True))
                            } else if false_node_bool == Some(true) {
                                // If the false node is always true then our result type being
                                // false implies the test was true
                                Some(type_cond.with_when(NodeBool::False))
                            } else {
                                None
                            }
                        }
                        NodeBool::False => {
                            if true_node_bool == Some(true) {
                                Some(type_cond.with_when(NodeBool::False))
                            } else if true_node_bool == Some(false) {
                                Some(type_cond.with_when(NodeBool::True))
                            } else {
                                None
                            }
                        }
                    })
                    .collect();

                // If the false branch is always false we can move the occurrence typing from the
                // true branch upwards. The same reasoning applies for the true branch. Note that
                // this may override conds that we brought in from our test node. These should
                // already have the outer occurrence typing applied so they will be more specific.
                if false_node_bool == Some(false) {
                    type_conds.extend(true_node.type_conds);
                }
                if true_node_bool == Some(true) {
                    type_conds.extend(false_node.type_conds);
                }

                Ok(InferredNode {
                    expr: hir::Expr {
                        result_ty,
                        kind: hir::ExprKind::Cond(Box::new(hir::Cond {
                            span,
                            test_expr: test_node.expr,
                            true_expr: true_node.expr,
                            false_expr: false_node.expr,
                        })),
                    },
                    type_conds,
                })
            }
        }
    }

    fn visit_ty_pred(
        &self,
        result_use: &ResultUse<'_>,
        span: Span,
        test_ty: ty::pred::TestTy,
    ) -> Result<InferredNode> {
        let pred_type = Ty::TyPred(test_ty.clone()).into();
        ensure_is_a(span, &pred_type, result_use)?;

        Ok(InferredNode {
            expr: hir::Expr {
                result_ty: pred_type,
                kind: hir::ExprKind::TyPred(span, test_ty),
            },
            type_conds: vec![],
        })
    }

    fn visit_eq_pred(&self, result_use: &ResultUse<'_>, span: Span) -> Result<InferredNode> {
        let pred_type = Ty::EqPred.into();
        ensure_is_a(span, &pred_type, result_use)?;

        Ok(InferredNode {
            expr: hir::Expr {
                result_ty: pred_type,
                kind: hir::ExprKind::EqPred(span),
            },
            type_conds: vec![],
        })
    }

    fn visit_record_cons(
        &self,
        result_use: &ResultUse<'_>,
        span: Span,
        record_cons: record::ConsId,
    ) -> Result<InferredNode> {
        let value_cons_fun_type = record::Cons::value_cons_fun_type(&record_cons).into();
        ensure_is_a(span, &value_cons_fun_type, result_use)?;

        Ok(InferredNode {
            expr: hir::Expr {
                result_ty: value_cons_fun_type,
                kind: hir::ExprKind::RecordCons(span, record_cons),
            },
            type_conds: vec![],
        })
    }

    fn visit_field_accessor(
        &self,
        result_use: &ResultUse<'_>,
        field_accessor: Box<hir::FieldAccessor>,
    ) -> Result<InferredNode> {
        let record_cons = &field_accessor.record_cons;
        let record_field = &record_cons.fields()[field_accessor.field_index];

        let field_accessor_fun_type = record_field.accessor_fun_type(record_cons).into();
        ensure_is_a(field_accessor.span, &field_accessor_fun_type, result_use)?;

        Ok(InferredNode {
            expr: hir::Expr {
                result_ty: field_accessor_fun_type,
                kind: hir::ExprKind::FieldAccessor(field_accessor),
            },
            type_conds: vec![],
        })
    }

    fn type_for_free_ref(
        &self,
        required_type: &ty::Ref<ty::Poly>,
        span: Span,
        current_type: &ty::Ref<ty::Poly>,
    ) -> Result<ty::Ref<ty::Poly>> {
        // Unlike references to known variables the `current_type` and `required_type` have equal
        // footing. We intersect here to find the commonality between the two types. This will
        // become the new type of the variable.
        ty::intersect::intersect_ty_refs(required_type, current_type).map_err(|_| {
            Error::new(
                span,
                ErrorKind::VarHasEmptyType(current_type.clone(), required_type.clone()),
            )
        })
    }

    fn visit_export_ref(
        &mut self,
        result_use: &ResultUse<'_>,
        span: Span,
        export_id: hir::ExportId,
    ) -> Result<InferredNode> {
        // This comes from an imported module
        let module_id = export_id.module_id();
        let local_id = export_id.local_id();

        let known_type = &self.imported_vars[&module_id][&local_id];
        ensure_is_a(span, known_type, result_use)?;

        Ok(InferredNode {
            expr: hir::Expr {
                result_ty: known_type.clone(),
                kind: hir::ExprKind::ExportRef(span, export_id),
            },
            type_conds: vec![],
        })
    }

    fn visit_local_ref(
        &mut self,
        result_use: &ResultUse<'_>,
        span: Span,
        local_id: hir::LocalId,
    ) -> Result<InferredNode> {
        let pending_def_id = match self.self_locals[&local_id] {
            VarType::Pending(def_id) => def_id,
            VarType::Recursive => return Err(Error::new(span, ErrorKind::RecursiveType)),
            VarType::Error => return Err(Error::new(span, ErrorKind::DependsOnError)),
            VarType::Known(ref known_type) => {
                ensure_is_a(span, known_type, result_use)?;
                return Ok(self.new_local_ref_node(span, local_id, known_type.clone()));
            }
            VarType::ParamScalar(free_ty_id) => {
                let current_type = &self.free_ty_polys[free_ty_id.to_usize()];
                let new_free_type =
                    self.type_for_free_ref(result_use.required_type(), span, current_type)?;

                self.free_ty_polys[free_ty_id.to_usize()] = new_free_type.clone();
                return Ok(self.new_local_ref_node(span, local_id, new_free_type));
            }
            VarType::ParamRest(free_ty_id) => {
                let current_member_type = &self.free_ty_polys[free_ty_id.to_usize()];
                let required_member_type =
                    member_type_for_poly_list(span, result_use.required_type())?;

                let new_free_type =
                    self.type_for_free_ref(&required_member_type, span, current_member_type)?;

                self.free_ty_polys[free_ty_id.to_usize()] = new_free_type.clone();
                let rest_list_type = ty::List::new_uniform(new_free_type).into();

                // Make sure we didn't require a specific list type e.g. `(List Int Int Int)`
                ensure_is_a(span, &rest_list_type, result_use)?;

                return Ok(self.new_local_ref_node(span, local_id, rest_list_type));
            }
        };

        self.recurse_into_def_id(pending_def_id)?;
        // This assumes `recurse_into_def_id` has populated our variables now
        self.visit_local_ref(result_use, span, local_id)
    }

    fn visit_do(
        &mut self,
        pv: &mut PurityVar,
        result_use: &ResultUse<'_>,
        mut exprs: Vec<hir::Expr<hir::Lowered>>,
    ) -> Result<InferredNode> {
        let terminal_expr = if let Some(terminal_expr) = exprs.pop() {
            terminal_expr
        } else {
            return Ok(InferredNode {
                expr: hir::Expr {
                    result_ty: Ty::unit().into(),
                    kind: hir::ExprKind::Do(vec![]),
                },
                type_conds: vec![],
            });
        };

        let mut is_divergent = false;
        let mut inferred_exprs = Vec::with_capacity(exprs.len() + 1);
        for non_terminal_expr in exprs {
            let was_divergent = is_divergent;
            // The type of this expression doesn't matter; its value is discarded
            let node =
                self.visit_expr(pv, &ResultUse::Unused(&Ty::Any.into()), non_terminal_expr)?;

            is_divergent = was_divergent || node.is_divergent();
            if !was_divergent && expr_can_side_effect(&node.expr) {
                inferred_exprs.push(node.expr);
            }
        }

        if is_divergent {
            self.visit_expr(pv, &ResultUse::Unused(&Ty::Any.into()), terminal_expr)?;

            Ok(InferredNode {
                expr: hir::Expr {
                    result_ty: Ty::never().into(),
                    kind: hir::ExprKind::Do(inferred_exprs),
                },
                type_conds: vec![],
            })
        } else {
            let terminal_node = self.visit_expr(pv, result_use, terminal_expr)?;
            let result_ty = terminal_node.result_ty().clone();

            if result_use.is_used() || expr_can_side_effect(&terminal_node.expr) {
                inferred_exprs.push(terminal_node.expr);
            }

            Ok(InferredNode {
                expr: hir::Expr {
                    result_ty,
                    kind: hir::ExprKind::Do(inferred_exprs),
                },
                type_conds: terminal_node.type_conds,
            })
        }
    }

    /// Visits a function expression
    ///
    /// This does a limited amount of backwards type propagation; it will attempt to fill in any
    /// free param or ret types from `required_type`. All declared types in the function will be
    /// taken as-is.
    fn visit_fun(
        &mut self,
        result_use: &ResultUse<'_>,
        decl_fun: hir::Fun<hir::Lowered>,
        self_local_id: Option<hir::LocalId>,
    ) -> Result<InferredNode> {
        let span = decl_fun.span;

        // This is set to false if we encounter any free types in our params or ret
        let mut decl_tys_are_known = true;

        let required_fun_type = result_use.required_type().find_member(|t| {
            if let Ty::Fun(fun) = t {
                Some(fun.as_ref())
            } else {
                None
            }
        });

        let required_top_fun_type = required_fun_type.map(ty::Fun::top_fun).or_else(|| {
            result_use.required_type().find_member(|t| {
                if let Ty::TopFun(top_fun) = t {
                    Some(top_fun.as_ref())
                } else {
                    None
                }
            })
        });

        let initial_param_type: ty::List<ty::Poly> = typeck::destruc::type_for_decl_list_destruc(
            &decl_fun.params,
            // Use the required type as a guide for any free types in the parameter list
            required_fun_type.map(|fun| ListIterator::new(fun.params())),
        );

        // Bind all of our parameter variables
        let free_ty_offset = self.destruc_list_value(
            &decl_fun.params,
            ListIterator::new(&initial_param_type),
            // If a parameter has a free decl type then we can refine the type
            true,
        );

        if free_ty_offset != self.free_ty_polys.len() {
            // We have free parameter types
            decl_tys_are_known = false;
        }

        // Use the declared return type if possible
        let wanted_ret_type = match decl_fun.ret_ty {
            hir::DeclTy::Known(poly) => poly,
            hir::DeclTy::Free => {
                decl_tys_are_known = false;

                if let Some(required_top_fun_type) = required_top_fun_type {
                    // Fall back to the backwards type
                    required_top_fun_type.ret().clone()
                } else {
                    // Use Any as a last resort
                    Ty::Any.into()
                }
            }
        };

        let mut known_self_type: Option<ty::Fun> = None;

        let mut fun_pv = match decl_fun.purity {
            hir::DeclPurity::Known(poly_purity) => {
                if decl_tys_are_known {
                    let self_type = ty::Fun::new(
                        decl_fun.pvars.clone(),
                        decl_fun.tvars.clone(),
                        ty::TopFun::new(poly_purity.clone(), wanted_ret_type.clone()),
                        initial_param_type,
                    );

                    // We have a fully known type; allow recursive calls
                    if let Some(self_local_id) = self_local_id {
                        self.self_locals
                            .insert(self_local_id, VarType::Known(self_type.clone().into()));
                    }
                    known_self_type = Some(self_type);
                }

                PurityVar::Known(poly_purity)
            }
            hir::DeclPurity::Free => {
                // Functions start pure until proven otherwise
                PurityVar::Free(Purity::Pure.into())
            }
        };

        let body_result_use = ResultUse::RetExpr(RetExprResultUse {
            ret_type_span: decl_fun.ret_ty_span,
            ret_type: &wanted_ret_type,
            known_self_type: known_self_type.as_ref(),
        });

        let body_node = self.visit_expr(&mut fun_pv, &body_result_use, decl_fun.body_expr)?;
        let revealed_ret_type = body_node.result_ty();
        let revealed_purity = fun_pv.into_poly();

        let revealed_param_destruc = {
            let mut inferred_free_types = self.free_ty_polys.drain(free_ty_offset..);

            destruc::subst_list_destruc(&mut inferred_free_types, decl_fun.params)
        };
        let revealed_param_type = hir::destruc::poly_for_list_destruc(&revealed_param_destruc);

        let revealed_type = ty::Fun::new(
            decl_fun.pvars.clone(),
            decl_fun.tvars.clone(),
            ty::TopFun::new(revealed_purity.clone(), revealed_ret_type.clone()),
            revealed_param_type,
        )
        .into();

        let revealed_fun = hir::Fun::<hir::Inferred> {
            span,
            pvars: decl_fun.pvars,
            tvars: decl_fun.tvars,
            purity: revealed_purity,
            params: revealed_param_destruc,
            ret_ty: revealed_ret_type.clone(),
            ret_ty_span: decl_fun.ret_ty_span,
            body_expr: body_node.expr,
        };

        ensure_is_a(span, &revealed_type, result_use)?;

        Ok(InferredNode {
            expr: hir::Expr {
                result_ty: revealed_type,
                kind: hir::ExprKind::Fun(Box::new(revealed_fun)),
            },
            type_conds: vec![],
        })
    }

    /// Visit a function application
    ///
    /// This has a fairly convoluted algorithm for resolving type variables. The essential
    /// problem is we don't know the type of our parameters until we visit their expressions.
    /// However, we also need to provide the parameters with backwards type information which may
    /// come from other parameters. We need to decide in which order to reveal the types which
    /// maximise the amount of useful information.
    ///
    /// Firstly, we visit every non-function fixed parameter and the rest parameter with the
    /// evidence from the return type. We collect our evidence in to a staged selection context to
    /// ensure if a type variable appears in multiple parameters they unify instead of supersede
    /// each other.
    ///
    /// In the next phase we visit every function fixed parameter. This is done in a second phase
    /// as these functions frequently relate to both the type of the parameters and the return
    /// type (e.g. `map`).
    ///
    /// The final phase selects the return type. This uses all the evidence collected above.
    fn visit_fun_app(
        &mut self,
        pv: &mut PurityVar,
        result_use: &ResultUse<'_>,
        span: Span,
        fun_type: &ty::Fun,
        fun_app: FunApp,
    ) -> Result<InferredNode> {
        let FunApp {
            fun_expr,
            fixed_arg_exprs,
            rest_arg_expr,
        } = fun_app;

        // The context used to select the types for our non-function parameters
        let mut non_fun_param_stx = ty::select::SelectCtx::new(fun_type.pvars(), fun_type.tvars());

        if let PurityVar::Known(purity_type) = pv {
            if purity_type != &Purity::Impure.into() {
                // Add our purity information
                non_fun_param_stx.add_evidence_purity(fun_type.purity(), purity_type);
            }
        }

        // The context used to select the types for our function parameters. This includes the
        // evidence gathered when visiting non-function parameters.
        let mut fun_param_stx = non_fun_param_stx.clone();
        let non_fun_param_pta = non_fun_param_stx.into_poly_ty_args();

        // Iterate over our parameter type to feed type information in to the arguments
        let mut param_iter = ListIterator::new(fun_type.params());

        let supplied_arg_count = fixed_arg_exprs.len();
        let wanted_arity = WantedArity::new(param_iter.fixed_len(), param_iter.has_rest());

        let mut is_divergent = false;

        struct PendingFixedArg<'ty> {
            index: usize,
            param_type: &'ty ty::Ref<ty::Poly>,
            expr: hir::Expr<hir::Lowered>,
        }

        let mut fun_fixed_args: Vec<PendingFixedArg<'_>> = vec![];
        let mut non_fun_fixed_args: Vec<PendingFixedArg<'_>> = vec![];
        let mut inferred_fixed_arg_exprs: Vec<(usize, hir::Expr<hir::Inferred>)> =
            Vec::with_capacity(fixed_arg_exprs.len());

        // Pre-visit our fixed args and categorise them as fun and non-fun
        for (index, fixed_arg_expr) in fixed_arg_exprs.into_iter().enumerate() {
            let param_type = param_iter.next().ok_or_else(|| {
                Error::new(
                    span,
                    ErrorKind::WrongArity(supplied_arg_count, wanted_arity),
                )
            })?;

            let pending_fixed_arg = PendingFixedArg {
                index,
                param_type,
                expr: fixed_arg_expr,
            };

            if let ty::Ref::Fixed(Ty::Fun(_)) = param_type {
                fun_fixed_args.push(pending_fixed_arg);
            } else {
                non_fun_fixed_args.push(pending_fixed_arg);
            }
        }

        for PendingFixedArg {
            index,
            param_type,
            expr,
        } in non_fun_fixed_args
        {
            let wanted_arg_type = ty::subst::subst_poly(&non_fun_param_pta, param_type);
            let fixed_arg_node =
                self.visit_expr(pv, &ResultUse::InnerExpr(&wanted_arg_type), expr)?;

            is_divergent = is_divergent || fixed_arg_node.is_divergent();

            fun_param_stx.add_evidence(param_type, fixed_arg_node.result_ty());
            inferred_fixed_arg_exprs.push((index, fixed_arg_node.expr));
        }

        // Visit our rest arg next so it's grouped in the first phase
        let inferred_rest_arg_expr = if let Some(rest_arg_expr) = rest_arg_expr {
            let tail_type = param_iter.tail_type().into();
            let wanted_tail_type = ty::subst::subst_poly(&non_fun_param_pta, &tail_type);
            let rest_arg_node =
                self.visit_expr(pv, &ResultUse::InnerExpr(&wanted_tail_type), rest_arg_expr)?;

            is_divergent = is_divergent || rest_arg_node.is_divergent();

            fun_param_stx.add_evidence(&tail_type, rest_arg_node.result_ty());
            Some(rest_arg_node.expr)
        } else if param_iter.fixed_len() > 0 {
            // We wanted more args!
            return Err(Error::new(
                span,
                ErrorKind::WrongArity(supplied_arg_count, wanted_arity),
            ));
        } else {
            // We can use the lack of a rest arg as type evidence
            fun_param_stx.add_evidence(&param_iter.collect_rest(), &Ty::never().into());
            None
        };

        // The context used to select our return type. This includes the evidence gathered when
        // visiting all parameters.
        let mut ret_stx = fun_param_stx.clone();
        let fun_param_pta = fun_param_stx.into_poly_ty_args();

        for PendingFixedArg {
            index,
            param_type,
            expr,
        } in fun_fixed_args
        {
            let wanted_arg_type = ty::subst::subst_poly(&fun_param_pta, param_type);
            let fixed_arg_node =
                self.visit_expr(pv, &ResultUse::InnerExpr(&wanted_arg_type), expr)?;

            is_divergent = is_divergent || fixed_arg_node.is_divergent();

            ret_stx.add_evidence(param_type, fixed_arg_node.result_ty());
            inferred_fixed_arg_exprs.push((index, fixed_arg_node.expr));
        }

        inferred_fixed_arg_exprs.sort_unstable_by_key(|k| k.0);
        let inferred_fixed_arg_exprs = inferred_fixed_arg_exprs.into_iter().map(|e| e.1).collect();

        let ret_pta = ret_stx
            .into_complete_poly_ty_args()
            .map_err(|error| match error {
                ty::select::Error::UnselectedPVar(pvar) => {
                    Error::new(span, ErrorKind::UnselectedPVar(pvar.clone()))
                }
                ty::select::Error::UnselectedTVar(tvar) => {
                    Error::new(span, ErrorKind::UnselectedTVar(tvar.clone()))
                }
            })?;

        let ret_type = if is_divergent {
            Ty::never().into()
        } else {
            ty::subst::subst_poly(&ret_pta, fun_type.ret())
        };

        // Keep track of the purity from the application
        let app_purity = ty::subst::subst_purity(&ret_pta, fun_type.purity());
        unify_app_purity(pv, &app_purity);

        ensure_is_a(span, &ret_type, result_use)?;

        Ok(InferredNode {
            expr: hir::Expr {
                result_ty: ret_type,
                kind: hir::ExprKind::App(Box::new(hir::App {
                    span,
                    fun_expr,
                    ty_args: ret_pta,
                    fixed_arg_exprs: inferred_fixed_arg_exprs,
                    rest_arg_expr: inferred_rest_arg_expr,
                })),
            },
            type_conds: vec![],
        })
    }

    /// Visit a `(recur)`
    ///
    /// This is similar to `visit_fun_app`. However, we require that the `(recur)`'s arguments match
    /// the generic function type. This allows us to tail recurse when monomorphising polymorphic
    /// functions because we know we can re-enter the same polymorph the `(recur)` occurs in.
    ///
    /// This sounds more complicated than normal function application but it's actual significantly
    /// easier due to not having to perform type variable selection.
    fn visit_recur(
        &mut self,
        pv: &mut PurityVar,
        result_use: &ResultUse<'_>,
        recur: hir::Recur<hir::Lowered>,
    ) -> Result<InferredNode> {
        let hir::Recur {
            span,
            fixed_arg_exprs,
            rest_arg_expr,
            ..
        } = recur;

        let ret_expr_use = if let ResultUse::RetExpr(ret_expr_use) = result_use {
            ret_expr_use
        } else {
            return Err(Error::new(span, ErrorKind::NonTailRecur));
        };

        let self_type = if let Some(self_type) = ret_expr_use.known_self_type {
            self_type
        } else {
            return Err(Error::new(span, ErrorKind::RecurWithoutFunTypeDecl));
        };

        // Iterate over our parameter type to feed type information in to the arguments
        let mut param_iter = ListIterator::new(self_type.params());

        let supplied_arg_count = fixed_arg_exprs.len();
        let wanted_arity = WantedArity::new(param_iter.fixed_len(), param_iter.has_rest());

        let mut is_divergent = false;

        let inferred_fixed_arg_exprs = fixed_arg_exprs
            .into_iter()
            .map(|fixed_arg_expr| {
                let param_type = param_iter.next().ok_or_else(|| {
                    Error::new(
                        span,
                        ErrorKind::WrongArity(supplied_arg_count, wanted_arity),
                    )
                })?;

                let fixed_arg_node =
                    self.visit_expr(pv, &ResultUse::InnerExpr(param_type), fixed_arg_expr)?;

                is_divergent = is_divergent || fixed_arg_node.is_divergent();
                Ok(fixed_arg_node.expr)
            })
            .collect::<Result<Vec<_>>>()?;

        let inferred_rest_arg_expr = if let Some(rest_arg_expr) = rest_arg_expr {
            let tail_type = param_iter.tail_type().into();
            let rest_arg_node =
                self.visit_expr(pv, &ResultUse::InnerExpr(&tail_type), rest_arg_expr)?;

            is_divergent = is_divergent || rest_arg_node.is_divergent();
            Some(rest_arg_node.expr)
        } else if param_iter.fixed_len() > 0 {
            // We wanted more args!
            return Err(Error::new(
                span,
                ErrorKind::WrongArity(supplied_arg_count, wanted_arity),
            ));
        } else {
            None
        };

        let ret_type: ty::Ref<ty::Poly> = if is_divergent {
            Ty::never().into()
        } else {
            self_type.ret().clone()
        };

        ensure_is_a(span, &ret_type, result_use)?;

        Ok(InferredNode {
            expr: hir::Expr {
                result_ty: ret_type,
                kind: hir::ExprKind::Recur(Box::new(hir::Recur {
                    span,
                    fixed_arg_exprs: inferred_fixed_arg_exprs,
                    rest_arg_expr: inferred_rest_arg_expr,
                })),
            },
            type_conds: vec![],
        })
    }

    /// Visit type predicate application with a single fixed arg
    ///
    /// This supports full occurrence typing
    fn visit_fixed_ty_pred_app(
        &mut self,
        pv: &mut PurityVar,
        span: Span,
        fun_expr: hir::Expr<hir::Inferred>,
        test_ty: &ty::pred::TestTy,
        subject_expr: hir::Expr<hir::Lowered>,
    ) -> Result<InferredNode> {
        use std::iter;

        let subject_local_id = if let hir::ExprKind::LocalRef(_, local_id) = &subject_expr.kind {
            Some(*local_id)
        } else {
            None
        };

        let subject_node =
            self.visit_expr(pv, &ResultUse::InnerExpr(&Ty::Any.into()), subject_expr)?;

        let subject_poly = subject_node.result_ty();
        match test_ty.match_subject_ref(subject_poly) {
            Some(known_result) => {
                let result_ty = if subject_node.is_divergent() {
                    Ty::never().into()
                } else {
                    Ty::LitBool(known_result).into()
                };

                // Get rid of the predicate application entirely
                // Keep the subject expr around for its side effect
                Ok(InferredNode {
                    expr: keep_exprs_for_side_effects(
                        iter::once(subject_node.expr),
                        hir::Expr {
                            result_ty,
                            kind: hir::ExprKind::Lit(Datum::Bool(span, known_result)),
                        },
                    ),
                    type_conds: vec![],
                })
            }
            None => {
                let poly_type = if subject_node.is_divergent() {
                    Ty::never().into()
                } else {
                    Ty::Bool.into()
                };

                let type_conds = if let Some(override_local_id) = subject_local_id {
                    let test_poly = test_ty.to_ty().into();
                    let type_if_true = ty::intersect::intersect_ty_refs(subject_poly, &test_poly)
                        .unwrap_or_else(|_| subject_poly.clone());

                    let type_if_false = ty::subtract::subtract_ty_refs(subject_poly, &test_poly);

                    vec![
                        VarTypeCond {
                            when: NodeBool::True,
                            override_local_id,
                            override_type: type_if_true,
                        },
                        VarTypeCond {
                            when: NodeBool::False,
                            override_local_id,
                            override_type: type_if_false,
                        },
                    ]
                } else {
                    vec![]
                };

                Ok(InferredNode {
                    expr: hir::Expr {
                        result_ty: poly_type,
                        kind: hir::ExprKind::App(Box::new(hir::App {
                            span,
                            fun_expr,
                            ty_args: TyArgs::empty(),
                            fixed_arg_exprs: vec![subject_node.expr],
                            rest_arg_expr: None,
                        })),
                    },
                    type_conds,
                })
            }
        }
    }

    /// Visit type predicate application with a rest argument
    ///
    /// This can do static evaluation but it does not support occurrence typing. This is only
    /// included to be orthogonal with other function applications; it's not terribly useful
    /// otherwise.
    fn visit_rest_ty_pred_app(
        &mut self,
        pv: &mut PurityVar,
        span: Span,
        fun_expr: hir::Expr<hir::Inferred>,
        test_ty: &ty::pred::TestTy,
        subject_list_expr: hir::Expr<hir::Lowered>,
    ) -> Result<InferredNode> {
        use std::iter;

        let wanted_subject_list_type = ty::List::new_tuple(Box::new([Ty::Any.into()])).into();

        let subject_list_node = self.visit_expr(
            pv,
            &ResultUse::InnerExpr(&wanted_subject_list_type),
            subject_list_expr,
        )?;

        let subject_type = ListIterator::try_new_from_ty_ref(subject_list_node.result_ty())
            .and_then(|mut iter| iter.next())
            .expect("Unable to extract type argument from type predicate rest list");

        match test_ty.match_subject_ref(subject_type) {
            Some(known_bool) => {
                let result_ty = if subject_list_node.is_divergent() {
                    Ty::never().into()
                } else {
                    Ty::LitBool(known_bool).into()
                };

                Ok(InferredNode {
                    expr: keep_exprs_for_side_effects(
                        iter::once(subject_list_node.expr),
                        hir::Expr {
                            result_ty,
                            kind: hir::ExprKind::Lit(Datum::Bool(span, known_bool)),
                        },
                    ),
                    type_conds: vec![],
                })
            }
            None => {
                let poly_type = if subject_list_node.is_divergent() {
                    // The subject diverged so we diverged
                    Ty::never().into()
                } else {
                    Ty::Bool.into()
                };

                Ok(InferredNode {
                    expr: hir::Expr {
                        result_ty: poly_type,
                        kind: hir::ExprKind::App(Box::new(hir::App {
                            span,
                            fun_expr,
                            ty_args: TyArgs::empty(),
                            fixed_arg_exprs: vec![],
                            rest_arg_expr: Some(subject_list_node.expr),
                        })),
                    },
                    type_conds: vec![],
                })
            }
        }
    }

    /// Visit equality predicate application with two fixed args
    ///
    /// This supports full occurrence typing
    fn visit_fixed_eq_pred_app(
        &mut self,
        pv: &mut PurityVar,
        span: Span,
        fun_expr: hir::Expr<hir::Inferred>,
        left_expr: hir::Expr<hir::Lowered>,
        right_expr: hir::Expr<hir::Lowered>,
    ) -> Result<InferredNode> {
        use crate::ty::props::is_literal;
        use std::iter;

        let left_local_id = if let hir::ExprKind::LocalRef(_, local_id) = &left_expr.kind {
            Some(*local_id)
        } else {
            None
        };

        let right_local_id = if let hir::ExprKind::LocalRef(_, local_id) = &right_expr.kind {
            Some(*local_id)
        } else {
            None
        };

        let left_node = self.visit_expr(pv, &ResultUse::InnerExpr(&Ty::Any.into()), left_expr)?;
        let left_ty = left_node.result_ty();

        let right_node = self.visit_expr(pv, &ResultUse::InnerExpr(&Ty::Any.into()), right_expr)?;
        let right_ty = right_node.result_ty();

        // Optimise away comparisons between booleans and literal true
        // This allows their type conditions to flow through
        if try_to_bool(left_ty) == Some(true)
            && ty::is_a::ty_ref_is_a(right_ty, &ty::Ty::Bool.into())
        {
            return Ok(right_node);
        }

        if try_to_bool(right_ty) == Some(true)
            && ty::is_a::ty_ref_is_a(left_ty, &ty::Ty::Bool.into())
        {
            return Ok(left_node);
        }

        let left_is_literal = is_literal(left_ty);
        let right_is_literal = is_literal(right_ty);
        let is_divergent = left_node.is_divergent() || right_node.is_divergent();

        if left_is_literal && right_is_literal && left_ty == right_ty {
            // We were comparing literal types; this is a static true
            let result_ty = if is_divergent {
                Ty::never().into()
            } else {
                Ty::LitBool(true).into()
            };

            return Ok(InferredNode {
                expr: keep_exprs_for_side_effects(
                    iter::once(left_node.expr).chain(iter::once(right_node.expr)),
                    hir::Expr {
                        result_ty,
                        kind: hir::ExprKind::Lit(Datum::Bool(span, true)),
                    },
                ),
                type_conds: vec![],
            });
        };

        let intersected_type = match ty::intersect::intersect_ty_refs(left_ty, right_ty) {
            Ok(intersected_type) => intersected_type,
            Err(ty::intersect::Error::Disjoint) => {
                let result_ty = if is_divergent {
                    Ty::never().into()
                } else {
                    Ty::LitBool(false).into()
                };

                return Ok(InferredNode {
                    expr: keep_exprs_for_side_effects(
                        iter::once(left_node.expr).chain(iter::once(right_node.expr)),
                        hir::Expr {
                            result_ty,
                            kind: hir::ExprKind::Lit(Datum::Bool(span, false)),
                        },
                    ),
                    type_conds: vec![],
                });
            }
        };

        let mut type_conds = vec![];

        if let Some(override_local_id) = left_local_id {
            type_conds.push(VarTypeCond {
                when: NodeBool::True,
                override_local_id,
                override_type: intersected_type.clone(),
            });

            if right_is_literal {
                let subtracted_type = ty::subtract::subtract_ty_refs(left_ty, right_ty);

                type_conds.push(VarTypeCond {
                    when: NodeBool::False,
                    override_local_id,
                    override_type: subtracted_type,
                });
            }
        }

        if let Some(override_local_id) = right_local_id {
            type_conds.push(VarTypeCond {
                when: NodeBool::True,
                override_local_id,
                override_type: intersected_type,
            });

            if left_is_literal {
                let subtracted_type = ty::subtract::subtract_ty_refs(right_ty, left_ty);

                type_conds.push(VarTypeCond {
                    when: NodeBool::False,
                    override_local_id,
                    override_type: subtracted_type,
                });
            }
        }

        // Invert type conditions for comparisons between a boolean and non-true value
        if ty::is_a::ty_ref_is_a(right_ty, &ty::Ty::Bool.into())
            && ty::intersect::intersect_ty_refs(left_ty, &ty::Ty::LitBool(true).into()).is_err()
        {
            type_conds.extend(
                right_node
                    .type_conds
                    .into_iter()
                    .map(VarTypeCond::into_inverted),
            );
        } else if ty::is_a::ty_ref_is_a(left_ty, &ty::Ty::Bool.into())
            && ty::intersect::intersect_ty_refs(right_ty, &ty::Ty::LitBool(true).into()).is_err()
        {
            type_conds.extend(
                left_node
                    .type_conds
                    .into_iter()
                    .map(VarTypeCond::into_inverted),
            );
        }

        let result_ty = if is_divergent {
            Ty::never().into()
        } else {
            Ty::Bool.into()
        };

        Ok(InferredNode {
            expr: hir::Expr {
                result_ty,
                kind: hir::ExprKind::App(Box::new(hir::App {
                    span,
                    fun_expr,
                    ty_args: TyArgs::empty(),
                    fixed_arg_exprs: vec![left_node.expr, right_node.expr],
                    rest_arg_expr: None,
                })),
            },
            type_conds,
        })
    }

    fn visit_app(
        &mut self,
        pv: &mut PurityVar,
        result_use: &ResultUse<'_>,
        app: hir::App<hir::Lowered>,
    ) -> Result<InferredNode> {
        let hir::App {
            span,
            fun_expr,
            mut fixed_arg_exprs,
            rest_arg_expr,
            ..
        } = app;

        // The only type information we can feed back is that we want a function of a certain
        // purity returning a certain value
        let wanted_purity = match pv {
            PurityVar::Free(_) => {
                // We're inferring the purity; this application can have any purity
                Purity::Impure.into()
            }
            PurityVar::Known(purity_type) => {
                // We have a specific declared purity
                purity_type.clone()
            }
        };

        let wanted_fun_type =
            ty::TopFun::new(wanted_purity, result_use.required_type().clone()).into();

        let fun_node = self.visit_expr(pv, &ResultUse::InnerExpr(&wanted_fun_type), fun_expr)?;
        let revealed_fun_type = fun_node.result_ty().clone();

        match revealed_fun_type.resolve_to_ty() {
            Ty::TopFun(_) => Err(Error::new(span, ErrorKind::TopFunApply(revealed_fun_type))),
            Ty::TyPred(test_ty) => {
                let wanted_arity = WantedArity::new(1, false);

                match (fixed_arg_exprs.len(), rest_arg_expr) {
                    (1, None) => {
                        let subject_expr = fixed_arg_exprs.pop().unwrap();
                        self.visit_fixed_ty_pred_app(pv, span, fun_node.expr, test_ty, subject_expr)
                    }
                    (0, Some(subject_list_expr)) => self.visit_rest_ty_pred_app(
                        pv,
                        span,
                        fun_node.expr,
                        test_ty,
                        subject_list_expr,
                    ),
                    (supplied_arg_count, _) => Err(Error::new(
                        span,
                        ErrorKind::WrongArity(supplied_arg_count, wanted_arity),
                    )),
                }
            }
            Ty::EqPred => {
                if fixed_arg_exprs.len() == 2 && rest_arg_expr.is_none() {
                    let right_expr = fixed_arg_exprs.pop().unwrap();
                    let left_expr = fixed_arg_exprs.pop().unwrap();

                    self.visit_fixed_eq_pred_app(pv, span, fun_node.expr, left_expr, right_expr)
                } else {
                    let fun_app = FunApp {
                        fun_expr: fun_node.expr,
                        fixed_arg_exprs,
                        rest_arg_expr,
                    };

                    self.visit_fun_app(pv, result_use, span, &ty::Fun::new_for_eq_pred(), fun_app)
                }
            }
            Ty::Fun(fun_type) => {
                let fun_app = FunApp {
                    fun_expr: fun_node.expr,
                    fixed_arg_exprs,
                    rest_arg_expr,
                };

                self.visit_fun_app(pv, result_use, span, fun_type, fun_app)
            }
            _ => panic!("Unexpected type"),
        }
    }

    fn visit_let(
        &mut self,
        pv: &mut PurityVar,
        result_use: &ResultUse<'_>,
        hir_let: hir::Let<hir::Lowered>,
    ) -> Result<InferredNode> {
        let hir::Let {
            span,
            destruc,
            value_expr,
            body_expr,
        } = hir_let;

        let required_destruc_type = typeck::destruc::type_for_decl_destruc(&destruc, None);

        // Pre-bind our variables to deal with recursive definitions
        let self_local_id = typeck::destruc::visit_locals(&destruc, &mut |local_id, decl_type| {
            let var_type = match decl_type {
                hir::DeclTy::Known(poly_type) => VarType::Known(poly_type.clone()),
                hir::DeclTy::Free => VarType::Recursive,
            };

            self.self_locals.insert(local_id, var_type);
        });

        let value_node = self.visit_expr_with_self_local_id(
            pv,
            &ResultUse::InnerExpr(&required_destruc_type),
            value_expr,
            self_local_id,
        )?;

        let free_ty_offset = self.destruc_value(&destruc, value_node.result_ty(), false);

        let body_node = self.visit_expr(pv, result_use, body_expr)?;
        let mut inferred_free_types = self.free_ty_polys.drain(free_ty_offset..);

        let result_ty = if value_node.is_divergent() {
            // Value was divergent
            Ty::never().into()
        } else {
            body_node.result_ty().clone()
        };

        Ok(InferredNode {
            expr: hir::Expr {
                result_ty,
                kind: hir::ExprKind::Let(Box::new(hir::Let {
                    span,
                    destruc: destruc::subst_destruc(&mut inferred_free_types, destruc),
                    value_expr: value_node.expr,
                    body_expr: body_node.expr,
                })),
            },
            type_conds: body_node.type_conds,
        })
    }

    fn visit_rust_fun(
        &self,
        result_use: &ResultUse<'_>,
        rust_fun: Arc<rfi::Fun>,
    ) -> Result<InferredNode> {
        let span = rust_fun.span();

        // Rust functions have their types validated by the RFI system when they're loaded
        // We just need to make sure we satisfy `result_use` and convert to an `InferredNode`
        let poly_type = Ty::Fun(Box::new(rust_fun.arret_fun_type().clone())).into();
        ensure_is_a(span, &poly_type, result_use)?;

        Ok(InferredNode {
            expr: hir::Expr {
                result_ty: poly_type,
                kind: hir::ExprKind::RustFun(rust_fun),
            },
            type_conds: vec![],
        })
    }

    fn visit_expr_with_self_local_id(
        &mut self,
        pv: &mut PurityVar,
        result_use: &ResultUse<'_>,
        expr: hir::Expr<hir::Lowered>,
        self_local_id: Option<hir::LocalId>,
    ) -> Result<InferredNode> {
        use crate::hir::ExprKind;
        match expr.kind {
            ExprKind::Lit(datum) => self.visit_lit(result_use, datum),
            ExprKind::Cond(cond) => self.visit_cond(pv, result_use, *cond),
            ExprKind::Do(exprs) => self.visit_do(pv, result_use, exprs),
            ExprKind::Fun(fun) => self.visit_fun(result_use, *fun, self_local_id),
            ExprKind::RustFun(rust_fun) => self.visit_rust_fun(result_use, rust_fun),
            ExprKind::TyPred(span, test_type) => self.visit_ty_pred(result_use, span, test_type),
            ExprKind::EqPred(span) => self.visit_eq_pred(result_use, span),
            ExprKind::RecordCons(span, record_cons) => {
                self.visit_record_cons(result_use, span, record_cons)
            }
            ExprKind::FieldAccessor(field_accessor) => {
                self.visit_field_accessor(result_use, field_accessor)
            }
            ExprKind::Let(hir_let) => self.visit_let(pv, result_use, *hir_let),
            ExprKind::LocalRef(span, local_id) => self.visit_local_ref(result_use, span, local_id),
            ExprKind::ExportRef(span, export_id) => {
                self.visit_export_ref(result_use, span, export_id)
            }
            ExprKind::App(app) => self.visit_app(pv, result_use, *app),
            ExprKind::Recur(recur) => self.visit_recur(pv, result_use, *recur),
            ExprKind::MacroExpand(span, inner_expr) => self
                .visit_expr_with_self_local_id(pv, result_use, *inner_expr, self_local_id)
                .map(|inferred| InferredNode {
                    expr: hir::Expr {
                        result_ty: inferred.expr.result_ty.clone(),
                        kind: ExprKind::MacroExpand(span, Box::new(inferred.expr)),
                    },
                    ..inferred
                })
                .map_err(|err| err.with_macro_invocation_span(span)),
        }
    }

    fn visit_expr(
        &mut self,
        pv: &mut PurityVar,
        result_use: &ResultUse<'_>,
        expr: hir::Expr<hir::Lowered>,
    ) -> Result<InferredNode> {
        self.visit_expr_with_self_local_id(pv, result_use, expr, None)
    }

    fn destruc_scalar_value(
        &mut self,
        scalar: &destruc::Scalar<hir::Lowered>,
        value_type: &ty::Ref<ty::Poly>,
        is_param: bool,
    ) -> usize {
        let start_offset = self.free_ty_polys.len();

        let free_ty_id = if *scalar.ty() == hir::DeclTy::Free {
            Some(self.insert_free_ty(value_type.clone()))
        } else {
            None
        };

        if let Some(local_id) = *scalar.local_id() {
            let var_type = if let (Some(free_ty_id), true) = (free_ty_id, is_param) {
                VarType::ParamScalar(free_ty_id)
            } else {
                VarType::Known(value_type.clone())
            };

            self.self_locals.insert(local_id, var_type);
        }

        start_offset
    }

    fn destruc_rest_value(
        &mut self,
        rest: &destruc::Scalar<hir::Lowered>,
        value_type_iter: ListIterator<'_, ty::Poly>,
        is_param: bool,
    ) {
        let param_free_ty_id = if *rest.ty() == hir::DeclTy::Free {
            // Start with member type as a guide
            let member_type = value_type_iter.clone().collect_rest();

            let free_ty_id = self.insert_free_ty(member_type);
            Some(free_ty_id).filter(|_| is_param)
        } else {
            None
        };

        if let Some(local_id) = *rest.local_id() {
            let var_type = if let Some(param_free_ty_id) = param_free_ty_id {
                VarType::ParamRest(param_free_ty_id)
            } else {
                // If we're not a rest parameter we know our exact tail type. We can't subst
                // the tail type in to the destruc because it only takes a member type.
                // However, we can use the exact tail type whenever we reference the var.
                VarType::Known(value_type_iter.tail_type().into())
            };

            self.self_locals.insert(local_id, var_type);
        }
    }

    fn destruc_list_value(
        &mut self,
        list: &destruc::List<hir::Lowered>,
        mut value_type_iter: ListIterator<'_, ty::Poly>,
        is_param: bool,
    ) -> usize {
        let start_offset = self.free_ty_polys.len();

        for fixed_destruc in list.fixed() {
            let member_type = value_type_iter
                .next()
                .expect("Destructured value with unexpected type");

            self.destruc_value(fixed_destruc, member_type, is_param);
        }

        if let Some(rest) = list.rest() {
            self.destruc_rest_value(rest, value_type_iter, is_param);
        }

        start_offset
    }

    fn destruc_value(
        &mut self,
        destruc: &destruc::Destruc<hir::Lowered>,
        value_type: &ty::Ref<ty::Poly>,
        is_param: bool,
    ) -> usize {
        match destruc {
            destruc::Destruc::Scalar(_, scalar) => {
                self.destruc_scalar_value(scalar, value_type, is_param)
            }
            destruc::Destruc::List(_, list) => {
                let value_type_iter = ListIterator::try_new_from_ty_ref(value_type)
                    .expect("Tried to destruc non-list");
                self.destruc_list_value(list, value_type_iter, is_param)
            }
        }
    }

    fn visit_def(&mut self, hir_def: hir::Def<hir::Lowered>) -> Result<hir::Def<hir::Inferred>> {
        let hir::Def {
            span,
            macro_invocation_span,
            destruc,
            value_expr,
        } = hir_def;

        // Module definitions must be pure
        let mut pv = PurityVar::Known(Purity::Pure.into());

        // Mark all of our free typed variable as recursive
        let self_local_id = typeck::destruc::visit_locals(&destruc, &mut |local_id, decl_type| {
            if *decl_type == hir::DeclTy::Free {
                self.self_locals.insert(local_id, VarType::Recursive);
            }
        });

        let required_type = typeck::destruc::type_for_decl_destruc(&destruc, None);
        let value_node = match self.visit_expr_with_self_local_id(
            &mut pv,
            &ResultUse::InnerExpr(&required_type),
            value_expr,
            self_local_id,
        ) {
            Ok(value_node) => value_node,
            Err(error) => {
                // Mark this def as an error so we can suppress cascade errors
                typeck::destruc::visit_locals(&destruc, &mut |local_id, _| {
                    self.self_locals.insert(local_id, VarType::Error);
                });
                return Err(error);
            }
        };

        let free_ty_offset = self.destruc_value(&destruc, value_node.result_ty(), false);
        let mut inferred_free_types = self.free_ty_polys.drain(free_ty_offset..);

        Ok(hir::Def {
            span,
            macro_invocation_span,
            destruc: destruc::subst_destruc(&mut inferred_free_types, destruc),
            value_expr: value_node.expr,
        })
    }

    fn recurse_into_def_id(&mut self, def_id: InputDefId) -> Result<()> {
        let def_index = def_id.to_usize();

        let previous_state = std::mem::replace(&mut self.input_defs[def_index], InputDef::Complete);

        if let InputDef::Pending(def) = previous_state {
            let inferred_def = self.visit_def(def)?;
            self.complete_defs.push(inferred_def);
        } else {
            panic!("Tried to infer already complete def. An error previously occurred?")
        }

        Ok(())
    }

    fn into_inferred_module(mut self) -> result::Result<InferredModule, Vec<Error>> {
        let mut errs = vec![];
        while let Some(def_state) = self.input_defs.pop() {
            match def_state {
                InputDef::Pending(def) => match self.visit_def(def) {
                    Ok(inferred_def) => {
                        self.complete_defs.push(inferred_def);
                    }
                    Err(err) => {
                        // If this is due to a previous error it's just noise to report it
                        if err.kind() != &ErrorKind::DependsOnError {
                            errs.push(err);
                        }
                    }
                },
                InputDef::Complete => {}
            }
        }

        if !errs.is_empty() {
            return Err(errs);
        }

        let inferred_locals: InferredLocals = self
            .self_locals
            .into_iter()
            .flat_map(|(local_id, var_type)| match var_type {
                VarType::Known(poly) => Some((local_id, poly)),
                _ => None,
            })
            .collect();

        Ok(InferredModule {
            inferred_locals,
            defs: self.complete_defs,
        })
    }
}

pub fn ensure_main_type(
    fallback_span: Span,
    complete_defs: &[hir::Def<hir::Inferred>],
    main_local_id: hir::LocalId,
    inferred_main_type: &ty::Ref<ty::Poly>,
) -> Result<()> {
    let expected_main_type = ty::Fun::new_for_main().into();

    if !ty::is_a::ty_ref_is_a(inferred_main_type, &expected_main_type) {
        use crate::reporting::LocTrace;

        // Try to find where `(main!)` was defined
        let main_loc_trace = complete_defs
            .iter()
            .find_map(|def| {
                if let destruc::Destruc::Scalar(_, ref scalar) = def.destruc {
                    if scalar.local_id() == &Some(main_local_id) {
                        return Some(LocTrace::new(def.span, def.macro_invocation_span));
                    }
                }

                None
            })
            // Fall back to the `Span` we were given
            .unwrap_or_else(|| fallback_span.into());

        return Err(Error::new_with_loc_trace(
            main_loc_trace,
            ErrorKind::IsNotTy(inferred_main_type.clone(), expected_main_type),
        ));
    };

    Ok(())
}

pub fn infer_module(
    imported_inferred_vars: &InferredModuleVars,
    defs: Vec<hir::Def<hir::Lowered>>,
) -> result::Result<InferredModule, Vec<Error>> {
    RecursiveDefsCtx::new(imported_inferred_vars, defs).into_inferred_module()
}

pub fn infer_repl_expr(
    all_inferred_vars: &InferredModuleVars,
    expr: hir::Expr<hir::Lowered>,
) -> Result<InferredNode> {
    let mut rdcx = RecursiveDefsCtx::new(all_inferred_vars, vec![]);
    let mut pv = PurityVar::Known(Purity::Impure.into());

    rdcx.visit_expr(&mut pv, &ResultUse::InnerExpr(&Ty::Any.into()), expr)
}

#[cfg(test)]
mod test {
    use super::*;
    use crate::hir::lowering::expr_for_str;
    use arret_syntax::span::t2s;

    fn type_for_expr(
        required_type: &ty::Ref<ty::Poly>,
        expr: hir::Expr<hir::Lowered>,
    ) -> Result<ty::Ref<ty::Poly>> {
        let imported_vars = HashMap::new();
        let mut rdcx = RecursiveDefsCtx::new(&imported_vars, vec![]);

        let mut pv = PurityVar::Known(Purity::Pure.into());

        rdcx.visit_expr(&mut pv, &ResultUse::InnerExpr(required_type), expr)
            .map(|node| node.expr.result_ty)
    }

    fn assert_type_for_expr(ty_str: &str, expr_str: &str) {
        let expr = expr_for_str(expr_str);
        let poly = hir::poly_for_str(ty_str);

        assert_eq!(poly, type_for_expr(&Ty::Any.into(), expr).unwrap());
    }

    fn assert_constrained_type_for_expr(expected_ty_str: &str, expr_str: &str, guide_ty_str: &str) {
        let expr = expr_for_str(expr_str);
        let expected_poly = hir::poly_for_str(expected_ty_str);
        let guide_poly = hir::poly_for_str(guide_ty_str);

        assert_eq!(expected_poly, type_for_expr(&guide_poly, expr).unwrap());
    }

    fn assert_type_error(err: &Error, expr_str: &str) {
        let expr = expr_for_str(expr_str);
        assert_eq!(err, &type_for_expr(&Ty::Any.into(), expr).unwrap_err())
    }

    #[test]
    fn literal_expr() {
        assert_type_for_expr("Int", "1");
    }

    #[test]
    fn do_expr() {
        assert_type_for_expr("'()", "(do)");
        assert_type_for_expr("Int", "(do 'one 'two 3)");

        // We have no diverging primitives so we can't test this case easily. This is covered in
        // run-pass.
    }

    #[test]
    fn cond_expr() {
        assert_type_for_expr("'true-branch", "(if true 'true-branch 'false-branch)");
        assert_type_for_expr("'false-branch", "(if false 'true-branch 'false-branch)");
        assert_type_for_expr("(Bool -> Bool)", "(fn (x) (if x true false))");

        // This is a reduced version of `(and)`
        // We shouldn't complain about the type in the false branch because it's unreachable
        assert_constrained_type_for_expr("true", "(if true true false)", "true");
    }

    #[test]
    fn fun_expr() {
        assert_type_for_expr("(-> ())", "(fn ())");
        assert_type_for_expr("(Any -> true)", "(fn (_) true)");
        assert_type_for_expr("(Str -> Str)", "(fn ([x Str]) x)");

        // We should feed our wanted type in to the function type
        assert_constrained_type_for_expr("(Sym -> true)", "(fn (_) true)", "(Sym -> true)");
        assert_constrained_type_for_expr("(Sym -> Sym)", "(fn (x) x)", "(Sym -> Any))");

        // Function with free types being bound to an incompatible type
        let j = "(let [[f (Sym -> true)] (fn ([_ Str]) true)])";
        let t = "                        ^^^^^^^^^^^^^^^^^^^  ";
        let err = Error::new(
            t2s(t),
            ErrorKind::IsNotTy(
                hir::poly_for_str("(Str -> true)"),
                hir::poly_for_str("(Sym -> true)"),
            ),
        );
        assert_type_error(&err, j);

        // Function with a known type being bound to an incompatible type
        let j = "(let [[f (Sym -> true)] (fn ([_ Str]) -> true true)])";
        let t = "                        ^^^^^^^^^^^^^^^^^^^^^^^^^^^  ";
        let err = Error::new(
            t2s(t),
            ErrorKind::IsNotTy(
                hir::poly_for_str("(Str -> true)"),
                hir::poly_for_str("(Sym -> true)"),
            ),
        );
        assert_type_error(&err, j);

        let j = "(fn ([x Str]) -> Sym x)";
        let t = "                 ^^^   ";
        let u = "                     ^ ";

        let err = Error::new(
            t2s(u),
            ErrorKind::IsNotRetTy(IsNotRetTy::new(
                hir::poly_for_str("Str"),
                hir::poly_for_str("Sym"),
                Some(t2s(t)),
            )),
        );
        assert_type_error(&err, j);
    }

    #[test]
    fn app_types() {
        assert_type_for_expr("'foo", "((fn () 'foo))");

        assert_type_for_expr("true", "(sym? 'foo)");
        assert_type_for_expr("false", "(sym? false)");

        assert_type_for_expr("Int", "((fn #{A} ([value A]) -> A value) 1)");
        assert_type_for_expr("'foo", "((fn #{A} ([value A]) -> A value) & '(foo))");

        assert_type_for_expr(
            "(List & Bool)",
            "((fn #{A} (& [rest A]) -> (List & A) rest) true false)",
        );

        assert_type_for_expr(
            "Int",
            // This is essentially `(map)` without the use of lists
            "((fn #{I O} ([mapper (I -> O)] [i I]) -> O (mapper i)) (fn (x) x) 1))",
        );

        assert_type_for_expr(
            "Int",
            // With the argument positions swapped
            "((fn #{I O} ([i I] [mapper (I -> O)]) -> O (mapper i)) 1 (fn (x) x)))",
        );

        assert_type_for_expr(
            "Int",
            // With explicit type annotations
            "((fn #{I O} ([i I] [mapper (I -> O)]) -> O (mapper i)) 1 (fn ([x Int]) -> Int x)))",
        );
    }

    #[test]
    fn recur_expr() {
        assert_type_for_expr("'foo", "((fn ([x Int]) -> 'foo (recur x)) 1)");

        let j = "((fn () -> () (recur) ()))";
        let t = "              ^^^^^^^     ";
        let err = Error::new(t2s(t), ErrorKind::NonTailRecur);
        assert_type_error(&err, j);

        let j = "((fn () (recur)))";
        let t = "        ^^^^^^^  ";
        let err = Error::new(t2s(t), ErrorKind::RecurWithoutFunTypeDecl);
        assert_type_error(&err, j);

        let j = "((fn (x) -> 'foo (recur x)) 1)";
        let t = "                 ^^^^^^^^^    ";
        let err = Error::new(t2s(t), ErrorKind::RecurWithoutFunTypeDecl);
        assert_type_error(&err, j);

        let j = "((fn ([x Int]) (recur x)) 1)";
        let t = "               ^^^^^^^^^    ";
        let err = Error::new(t2s(t), ErrorKind::RecurWithoutFunTypeDecl);
        assert_type_error(&err, j);
    }

    #[test]
    fn app_purity() {
        // An empty function is pure
        assert_type_for_expr("(-> false)", "(fn () false)");

        // Calling a pure function in an inferred purity should leave us pure
        assert_type_for_expr("(-> false)", "(fn () ((fn () -> false false)))");

        // Calling the impure function in an inferred purity should make us impure
        assert_type_for_expr("(->! false)", "(fn () ((fn () ->! false false)))");
    }

    #[test]
    fn impure_app_within_pure() {
        // Calling an impure function inside a function declared as pure should fail
        let j = "(fn () -> Bool ((fn () ->! false false)))";
        let t = "                ^^^^^^^^^^^^^^^^^^^^^^^  ";

        let err = Error::new(
            t2s(t),
            ErrorKind::IsNotPurity(hir::poly_for_str("(->! false)"), Purity::Pure.into()),
        );
        assert_type_error(&err, j);
    }

    #[test]
    fn too_many_args() {
        let j = "((fn ()) 1)";
        let t = "^^^^^^^^^^^";

        let wanted_arity = WantedArity::new(0, false);
        let err = Error::new(t2s(t), ErrorKind::WrongArity(1, wanted_arity));
        assert_type_error(&err, j);
    }

    #[test]
    fn not_enough_args() {
        let j = "((fn (_ _)) 1)";
        let t = "^^^^^^^^^^^^^^";

        let wanted_arity = WantedArity::new(2, false);
        let err = Error::new(t2s(t), ErrorKind::WrongArity(1, wanted_arity));
        assert_type_error(&err, j);
    }

    #[test]
    fn list_destruc() {
        assert_type_for_expr("Int", "(let [(x) '(1)] x)");
        assert_type_for_expr(
            "(List true false)",
            "(let [(_ & rest) '(1 true false)] rest)",
        );
    }

    #[test]
    fn var_ref() {
        assert_type_for_expr("Int", "(let [x 1] x)")
    }

    #[test]
    fn ty_pred() {
        assert_type_for_expr("true", "(sym? & '(foo))");
        assert_type_for_expr("true", "(sym? 'foo)");
        assert_type_for_expr("false", "(int? & '(foo))");
        assert_type_for_expr("false", "(int? 'bar)");
    }

    #[test]
    fn eq_pred() {
        assert_type_for_expr("true", "(= 'foo 'foo)");
        assert_type_for_expr("false", "(= 'bar 'foo)");

        // This looks stupid but we can only evaluate based on types. Both 1 and 2 are `Int`.
        assert_type_for_expr("Bool", "(= 1 2)");
    }
}
