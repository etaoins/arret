use std;
use std::collections::HashMap;
use std::result;

use crate::hir;
use crate::hir::destruc;
use crate::hir::rfi;
use crate::ty;
use crate::ty::list_iter::ListIterator;
use crate::ty::purity;
use crate::ty::purity::Purity;
use crate::ty::ty_args::PolyTyArgs;
use crate::typeck;
use crate::typeck::error::{Error, ErrorKind, WantedArity};

use syntax::datum::Datum;
use syntax::span::{Span, EMPTY_SPAN};

type Result<T> = result::Result<T, Error>;

/// Result of inferring the type for a HIR expression
pub struct InferredNode {
    /// Expression with all free types replaced with poly types
    expr: hir::Expr<hir::Inferred>,
    /// Type conditions depending the poly type of this node
    type_conds: Vec<VarTypeCond>,
}

impl InferredNode {
    fn new_ref_node(span: Span, var_id: hir::VarId, poly_type: ty::Ref<ty::Poly>) -> InferredNode {
        let type_conds = if poly_type == ty::Ty::Bool.into() {
            // This seems useless but it allows occurrence typing to work if this type
            // flows through another node such as `(do)` or `(let)`
            vec![VarTypeCond {
                var_id,
                type_if_true: ty::Ty::LitBool(true).into(),
                type_if_false: ty::Ty::LitBool(false).into(),
            }]
        } else {
            vec![]
        };

        InferredNode {
            expr: hir::Expr {
                span,
                result_ty: poly_type,
                kind: hir::ExprKind::Ref(var_id),
            },
            type_conds,
        }
    }

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

struct VarTypeCond {
    var_id: hir::VarId,
    type_if_true: ty::Ref<ty::Poly>,
    type_if_false: ty::Ref<ty::Poly>,
}

impl VarTypeCond {
    fn reversed(self) -> VarTypeCond {
        VarTypeCond {
            var_id: self.var_id,
            type_if_true: self.type_if_false,
            type_if_false: self.type_if_true,
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
    Free(purity::Poly),
    Known(purity::Poly),
}

impl PurityVar {
    fn into_poly(self) -> purity::Poly {
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

struct RecursiveDefsCtx<'types> {
    input_defs: Vec<InputDef>,
    complete_defs: Vec<hir::Def<hir::Inferred>>,

    // The inferred types for free types in the order they're encountered
    //
    // Each (def), (let) and (fn) push entries to `free_ty_polys` before they evaluate their body
    // and then pop them off afterwards.
    free_ty_polys: Vec<ty::Ref<ty::Poly>>,

    var_to_type: &'types mut HashMap<hir::VarId, VarType>,
}

#[derive(Clone)]
struct FunCtx {
    pvars: purity::PVars,
    tvars: ty::TVars,
}

/// Tries to convert a polymorphic type to a literal boolean value
fn try_to_bool(poly: &ty::Ref<ty::Poly>) -> Option<bool> {
    match poly {
        ty::Ref::Fixed(ty::Ty::LitBool(v)) => Some(*v),
        _ => None,
    }
}

fn str_for_poly(fcx: &FunCtx, poly: &ty::Ref<ty::Poly>) -> Box<str> {
    hir::str_for_poly(&fcx.pvars, &fcx.tvars, poly).into_boxed_str()
}

fn unify_app_purity(pv: &mut PurityVar, app_purity: &purity::Poly) {
    if let PurityVar::Free(ref mut free_purity) = pv {
        *free_purity = ty::unify::unify_purity_refs(free_purity, &app_purity)
    };
}

/// Inspects the mismatched sub and parent types to attempt to produce an understandable type error
fn error_kind_for_type_error(
    fcx: &FunCtx,
    sub_poly: &ty::Ref<ty::Poly>,
    parent_poly: &ty::Ref<ty::Poly>,
) -> ErrorKind {
    let incorrect_ty_str = str_for_poly(fcx, &sub_poly);

    if let ty::Ref::Fixed(ty::Ty::TopFun(top_fun)) = parent_poly {
        let topmost_fun = ty::TopFun::new(Purity::Impure.into(), ty::Ty::Any.into()).into();

        let impure_top_fun = ty::TopFun::new(Purity::Impure.into(), top_fun.ret().clone()).into();

        if !ty::is_a::ty_ref_is_a(&fcx.tvars, sub_poly, &topmost_fun).to_bool() {
            // We aren't a function at all
            return ErrorKind::IsNotFun(incorrect_ty_str);
        } else if ty::is_a::ty_ref_is_a(&fcx.tvars, sub_poly, &impure_top_fun).to_bool() {
            let purity_str = if top_fun.purity() == &Purity::Pure.into() {
                // `->` might be confusing here
                "pure".into()
            } else {
                format!("`{}`", hir::str_for_purity(&fcx.pvars, top_fun.purity())).into()
            };

            // We have the right return type but the wrong purity
            return ErrorKind::IsNotPurity(incorrect_ty_str, purity_str);
        }
    }

    ErrorKind::IsNotTy(incorrect_ty_str, str_for_poly(fcx, parent_poly))
}

/// Ensures `sub_poly` is a subtype of `parent_poly`
fn ensure_is_a(
    fcx: &FunCtx,
    span: Span,
    sub_poly: &ty::Ref<ty::Poly>,
    parent_poly: &ty::Ref<ty::Poly>,
) -> Result<()> {
    if ty::is_a::ty_ref_is_a(&fcx.tvars, sub_poly, parent_poly).to_bool() {
        return Ok(());
    }

    let error_kind = error_kind_for_type_error(fcx, sub_poly, parent_poly);
    Err(Error::new(span, error_kind))
}

fn member_type_for_poly_list(
    fcx: &FunCtx,
    span: Span,
    poly_type: &ty::Ref<ty::Poly>,
) -> Result<ty::Ref<ty::Poly>> {
    if poly_type == &ty::Ty::Any.into() {
        return Ok(ty::Ty::Any.into());
    }

    let list = poly_type
        .find_member(|t| {
            if let ty::Ty::List(list) = t {
                Some(list)
            } else {
                None
            }
        })
        .ok_or_else(|| {
            Error::new(
                span,
                ErrorKind::IsNotTy(str_for_poly(fcx, &poly_type), "(Listof Any)".into()),
            )
        })?;

    Ok(ListIterator::new(list)
        .collect_rest(&fcx.tvars)
        .unwrap_or_else(|| ty::Ty::Any.into()))
}

/// Preserves expressions for their side effects
///
/// `side_effect_exprs` are evaluated but have their value discarded. `value_expr` will be used as
/// the value of the returned expression.
fn keep_exprs_for_side_effects(
    side_effect_exprs: impl IntoIterator<Item = hir::Expr<hir::Inferred>>,
    value_expr: hir::Expr<hir::Inferred>,
) -> hir::Expr<hir::Inferred> {
    use std::iter;

    hir::Expr {
        span: EMPTY_SPAN,
        result_ty: value_expr.result_ty.clone(),
        kind: hir::ExprKind::Do(
            side_effect_exprs
                .into_iter()
                .chain(iter::once(value_expr))
                .collect(),
        ),
    }
}

impl<'types> RecursiveDefsCtx<'types> {
    fn new(
        defs: Vec<hir::Def<hir::Lowered>>,
        var_to_type: &'types mut HashMap<hir::VarId, VarType>,
    ) -> RecursiveDefsCtx<'types> {
        // We do this in reverse order because we infer our defs in reverse order. This doesn't
        // matter for correctness. However, presumably most definitions have more depedencies
        // before them than after them. Visiting them in forward order should cause less
        // recursive resolution.
        let input_defs = defs
            .into_iter()
            .rev()
            .enumerate()
            .map(|(idx, hir_def)| {
                let def_id = InputDefId::new(idx);

                typeck::destruc::visit_vars(&hir_def.destruc, |var_id, decl_type| {
                    let var_type = match decl_type {
                        hir::DeclTy::Known(poly_type) => VarType::Known(poly_type.clone()),
                        hir::DeclTy::Free => {
                            // Record the definition ID so we can deal with forward type references
                            VarType::Pending(def_id)
                        }
                    };

                    var_to_type.insert(var_id, var_type);
                });

                InputDef::Pending(hir_def)
            })
            .collect::<Vec<InputDef>>();

        let complete_defs = Vec::with_capacity(input_defs.len());
        RecursiveDefsCtx {
            input_defs,
            complete_defs,
            free_ty_polys: vec![],
            var_to_type,
        }
    }

    fn insert_free_ty(&mut self, initial_type: ty::Ref<ty::Poly>) -> FreeTyId {
        FreeTyId::new_entry_id(&mut self.free_ty_polys, initial_type)
    }

    fn visit_lit(
        &mut self,
        fcx: &FunCtx,
        required_type: &ty::Ref<ty::Poly>,
        datum: Datum,
    ) -> Result<InferredNode> {
        let lit_type = ty::datum::ty_ref_for_datum(&datum);
        ensure_is_a(fcx, datum.span(), &lit_type, required_type)?;

        Ok(InferredNode {
            expr: hir::Expr {
                span: datum.span(),
                result_ty: lit_type,
                kind: hir::ExprKind::Lit(datum),
            },
            type_conds: vec![],
        })
    }

    fn visit_cond(
        &mut self,
        fcx: &FunCtx,
        pv: &mut PurityVar,
        required_type: &ty::Ref<ty::Poly>,
        span: Span,
        cond: hir::Cond<hir::Lowered>,
    ) -> Result<InferredNode> {
        use std::iter;

        let hir::Cond {
            test_expr,
            true_expr,
            false_expr,
            ..
        } = cond;

        let test_required_type = &ty::Ty::Bool.into();
        let test_node = self.visit_expr(fcx, pv, test_required_type, test_expr)?;

        if test_node.is_divergent() {
            // Test diverged; we don't need the branches
            return Ok(test_node);
        }
        let test_known_bool = try_to_bool(test_node.result_ty());

        // If a branch isn't taken it doesn't need to match the type of the cond expression
        let any_type = &ty::Ty::Any.into();
        let (true_required_type, false_required_type) = match test_known_bool {
            Some(true) => (required_type, any_type),
            Some(false) => (any_type, required_type),
            None => (required_type, required_type),
        };

        // Patch our occurrence types in to the `var_to_type` and restore it after. We avoid
        // `?`ing our results until the end to make sure the original types are properly restored.
        let restore_var_types = test_node
            .type_conds
            .iter()
            .map(|type_cond| {
                let VarTypeCond {
                    var_id,
                    ref type_if_true,
                    ..
                } = *type_cond;

                (
                    var_id,
                    self.var_to_type
                        .insert(var_id, VarType::Known(type_if_true.clone()))
                        .unwrap(),
                )
            })
            .collect::<Vec<(hir::VarId, VarType)>>();

        let true_result = self.visit_expr(fcx, pv, true_required_type, true_expr);

        // Now set the false branch types
        for type_cond in test_node.type_conds.iter() {
            let VarTypeCond {
                var_id,
                ref type_if_false,
                ..
            } = *type_cond;

            self.var_to_type
                .insert(var_id, VarType::Known(type_if_false.clone()));
        }

        let false_result = self.visit_expr(fcx, pv, false_required_type, false_expr);

        // Restore the original types
        for (var_id, original_var_type) in restore_var_types {
            self.var_to_type.insert(var_id, original_var_type);
        }

        let true_node = true_result?;
        let false_node = false_result?;

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
                let result_ty = ty::unify::unify_to_ty_ref(
                    &fcx.tvars,
                    true_node.result_ty(),
                    false_node.result_ty(),
                );

                let false_node_bool = try_to_bool(false_node.result_ty());
                let true_node_bool = try_to_bool(true_node.result_ty());

                // This is an analog of (not). We can flip the test's type condition
                let mut type_conds =
                    if true_node_bool == Some(false) && false_node_bool == Some(true) {
                        test_node
                            .type_conds
                            .into_iter()
                            .map(|type_cond| type_cond.reversed())
                            .collect()
                    } else {
                        vec![]
                    };

                // If the false branch is always false we can move an occurrence typing from the
                // true branch upwards. The same reasoning applies for the true branch.
                if false_node_bool == Some(false) {
                    type_conds.extend(true_node.type_conds);
                }
                if true_node_bool == Some(true) {
                    type_conds.extend(false_node.type_conds);
                }

                Ok(InferredNode {
                    expr: hir::Expr {
                        span,
                        result_ty,
                        kind: hir::ExprKind::Cond(Box::new(hir::Cond {
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
        fcx: &FunCtx,
        required_type: &ty::Ref<ty::Poly>,
        span: Span,
        test_ty: ty::pred::TestTy,
    ) -> Result<InferredNode> {
        let pred_type = ty::Ty::TyPred(test_ty).into();
        ensure_is_a(fcx, span, &pred_type, required_type)?;

        Ok(InferredNode {
            expr: hir::Expr {
                span,
                result_ty: pred_type,
                kind: hir::ExprKind::TyPred(test_ty),
            },
            type_conds: vec![],
        })
    }

    fn visit_eq_pred(
        &self,
        fcx: &FunCtx,
        required_type: &ty::Ref<ty::Poly>,
        span: Span,
    ) -> Result<InferredNode> {
        let pred_type = ty::Ty::EqPred.into();
        ensure_is_a(fcx, span, &pred_type, required_type)?;

        Ok(InferredNode {
            expr: hir::Expr {
                span,
                result_ty: pred_type,
                kind: hir::ExprKind::EqPred,
            },
            type_conds: vec![],
        })
    }

    fn type_for_free_ref(
        &self,
        fcx: &FunCtx,
        required_type: &ty::Ref<ty::Poly>,
        span: Span,
        current_type: &ty::Ref<ty::Poly>,
    ) -> Result<ty::Ref<ty::Poly>> {
        // Unlike references to known variables the `current_type` and `required_type` have equal
        // footing. We intersect here to find the commonality between the two types. This will
        // become the new type of the variable.
        ty::intersect::intersect_ty_refs(&fcx.tvars, required_type, current_type).map_err(|_| {
            Error::new(
                span,
                ErrorKind::VarHasEmptyType(
                    str_for_poly(fcx, required_type),
                    str_for_poly(fcx, current_type),
                ),
            )
        })
    }

    fn visit_ref(
        &mut self,
        fcx: &FunCtx,
        required_type: &ty::Ref<ty::Poly>,
        span: Span,
        var_id: hir::VarId,
    ) -> Result<InferredNode> {
        let pending_def_id = match self.var_to_type[&var_id] {
            VarType::Pending(def_id) => def_id,
            VarType::Recursive => return Err(Error::new(span, ErrorKind::RecursiveType)),
            VarType::Error => return Err(Error::new(span, ErrorKind::DependsOnError)),
            VarType::Known(ref known_type) => {
                ensure_is_a(fcx, span, known_type, required_type)?;
                return Ok(InferredNode::new_ref_node(span, var_id, known_type.clone()));
            }
            VarType::ParamScalar(free_ty_id) => {
                let current_type = &self.free_ty_polys[free_ty_id.to_usize()];
                let new_free_type =
                    self.type_for_free_ref(fcx, required_type, span, current_type)?;

                self.free_ty_polys[free_ty_id.to_usize()] = new_free_type.clone();
                return Ok(InferredNode::new_ref_node(span, var_id, new_free_type));
            }
            VarType::ParamRest(free_ty_id) => {
                let current_member_type = &self.free_ty_polys[free_ty_id.to_usize()];
                let required_member_type = member_type_for_poly_list(fcx, span, required_type)?;

                let new_free_type =
                    self.type_for_free_ref(fcx, &required_member_type, span, current_member_type)?;

                self.free_ty_polys[free_ty_id.to_usize()] = new_free_type.clone();
                let rest_list_type =
                    ty::Ty::List(ty::List::new(Box::new([]), Some(new_free_type))).into();

                // Make sure we didn't require a specific list type e.g. `(List Int Int Int)`
                ensure_is_a(fcx, span, &rest_list_type, required_type)?;

                return Ok(InferredNode::new_ref_node(span, var_id, rest_list_type));
            }
        };

        self.recurse_into_def_id(pending_def_id)?;
        // This assumes `recurse_into_def_id` has populated our variables now
        self.visit_ref(fcx, required_type, span, var_id)
    }

    fn visit_do(
        &mut self,
        fcx: &FunCtx,
        pv: &mut PurityVar,
        required_type: &ty::Ref<ty::Poly>,
        span: Span,
        mut exprs: Vec<hir::Expr<hir::Lowered>>,
    ) -> Result<InferredNode> {
        let terminal_expr = if let Some(terminal_expr) = exprs.pop() {
            terminal_expr
        } else {
            return Ok(InferredNode {
                expr: hir::Expr {
                    span,
                    result_ty: ty::Ty::unit().into(),
                    kind: hir::ExprKind::Do(vec![]),
                },
                type_conds: vec![],
            });
        };

        let mut is_divergent = false;
        let mut inferred_exprs = exprs
            .into_iter()
            .map(|non_terminal_expr| {
                // The type of this expression doesn't matter; its value is discarded
                let node = self.visit_expr(fcx, pv, &ty::Ty::Any.into(), non_terminal_expr)?;
                is_divergent = is_divergent || node.is_divergent();

                Ok(node.expr)
            })
            .collect::<Result<Vec<hir::Expr<hir::Inferred>>>>()?;

        if is_divergent {
            self.visit_expr(fcx, pv, &ty::Ty::Any.into(), terminal_expr)?;

            Ok(InferredNode {
                expr: hir::Expr {
                    span,
                    result_ty: ty::Ty::never().into(),
                    kind: hir::ExprKind::Do(inferred_exprs),
                },
                type_conds: vec![],
            })
        } else {
            let terminal_node = self.visit_expr(fcx, pv, required_type, terminal_expr)?;
            let result_ty = terminal_node.result_ty().clone();
            inferred_exprs.push(terminal_node.expr);

            Ok(InferredNode {
                expr: hir::Expr {
                    span,
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
        outer_fcx: &FunCtx,
        required_type: &ty::Ref<ty::Poly>,
        span: Span,
        decl_fun: hir::Fun<hir::Lowered>,
        self_var_id: Option<hir::VarId>,
    ) -> Result<InferredNode> {
        // This is set to false if we encounter any free types in our params or ret
        let mut decl_tys_are_known = true;

        let required_fun_type = required_type.find_member(|t| {
            if let ty::Ty::Fun(fun) = t {
                Some(fun.as_ref())
            } else {
                None
            }
        });

        let required_top_fun_type = required_fun_type
            .map(|fun_type| fun_type.top_fun())
            .or_else(|| {
                required_type.find_member(|t| {
                    if let ty::Ty::TopFun(top_fun) = t {
                        Some(top_fun.as_ref())
                    } else {
                        None
                    }
                })
            });

        let fun_pvars = purity::merge_pvars(&outer_fcx.pvars, &decl_fun.pvars);
        let fun_tvars = ty::merge_tvars(&outer_fcx.tvars, &decl_fun.tvars);

        let initial_param_type: ty::List<ty::Poly> = typeck::destruc::type_for_decl_list_destruc(
            &fun_tvars,
            &decl_fun.params,
            // Use the required type as a guide for any free types in the parameter list
            required_fun_type.map(|fun| ListIterator::new(fun.params())),
        );

        // Bind all of our parameter variables
        let free_ty_offset = self.destruc_list_value(
            &fun_tvars,
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
            hir::DeclTy::Known(poly) => poly.clone(),
            hir::DeclTy::Free => {
                decl_tys_are_known = false;

                if let Some(ref required_top_fun_type) = required_top_fun_type {
                    // Fall back to the backwards type
                    required_top_fun_type.ret().clone()
                } else {
                    // Use Any as a last resort
                    ty::Ty::Any.into()
                }
            }
        };

        let mut fun_pv = match decl_fun.purity {
            hir::DeclPurity::Known(poly_purity) => {
                if let (Some(self_var_id), true) = (self_var_id, decl_tys_are_known) {
                    let self_type = ty::Fun::new(
                        decl_fun.pvars.clone(),
                        decl_fun.tvars.clone(),
                        ty::TopFun::new(poly_purity.clone(), wanted_ret_type.clone()),
                        initial_param_type,
                    )
                    .into();

                    // We have a fully known type; allow recursive calls
                    self.var_to_type
                        .insert(self_var_id, VarType::Known(self_type));
                }

                PurityVar::Known(poly_purity)
            }
            hir::DeclPurity::Free => {
                // Functions start pure until proven otherwise
                PurityVar::Free(Purity::Pure.into())
            }
        };

        let fun_fcx = FunCtx {
            tvars: fun_tvars,
            pvars: fun_pvars,
        };

        let body_node =
            self.visit_expr(&fun_fcx, &mut fun_pv, &wanted_ret_type, decl_fun.body_expr)?;
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
            pvars: decl_fun.pvars,
            tvars: decl_fun.tvars,
            purity: revealed_purity,
            params: revealed_param_destruc,
            ret_ty: revealed_ret_type.clone(),
            body_expr: body_node.expr,
        };

        ensure_is_a(outer_fcx, span, &revealed_type, required_type)?;

        Ok(InferredNode {
            expr: hir::Expr {
                span,
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
    /// Firstly, we use the required type of the application itself as any evidence against a
    /// polymorphic return type. For example, if our required type is `Int` and our return type is
    /// `R` then `R` is an `Int`.
    ///
    /// We then visit every non-function fixed parameter and the rest parameter with the evidence
    /// from the return type. We collect our evidence in to a staged selection context to ensure
    /// if a type variable appears in multiple parameters they unify instead of supersede each
    /// other.
    ///
    /// In the next phase we visit every function fixed parameter. This is done in a second phase
    /// as these functions frequently relate to both the type of the parameters and the return
    /// type (e.g. `map`).
    ///
    /// The final phase selects the return type. This uses all the evidence collected above except
    /// for the original evidence from the wanted return type. If the original evidence is included
    /// we we typically "over-unify" whatever the wanted type was (e.g. `Any`).
    fn visit_fun_app(
        &mut self,
        fcx: &FunCtx,
        pv: &mut PurityVar,
        required_type: &ty::Ref<ty::Poly>,
        span: Span,
        fun_type: &ty::Fun,
        fun_app: FunApp,
    ) -> Result<InferredNode> {
        let FunApp {
            fun_expr,
            fixed_arg_exprs,
            rest_arg_expr,
        } = fun_app;

        // The context used to select the type for our return value. It collects evidence from the
        // parameters as they're evaluated.
        let mut ret_stx =
            ty::select::SelectCtx::new(&fcx.tvars, fun_type.pvars(), fun_type.tvars());

        // The context used to select the types for our non-function parameters. Its initial
        // evidence is the wanted return type which is used for backwards type propagation.
        let mut non_fun_param_stx = ret_stx.clone();

        // Add our return type information
        non_fun_param_stx.add_evidence(fun_type.ret(), required_type);

        if let PurityVar::Known(purity_type) = pv {
            // Add our purity information
            non_fun_param_stx.add_evidence_purity(fun_type.purity(), purity_type);
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
        };

        let mut fun_fixed_args: Vec<PendingFixedArg<'_>> = vec![];
        let mut non_fun_fixed_args: Vec<PendingFixedArg<'_>> = vec![];

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

            if let ty::Ref::Fixed(ty::Ty::Fun(_)) = param_type {
                fun_fixed_args.push(pending_fixed_arg);
            } else {
                non_fun_fixed_args.push(pending_fixed_arg);
            }
        }

        let mut inferred_fixed_arg_exprs: Vec<(usize, hir::Expr<hir::Inferred>)> = vec![];

        for PendingFixedArg {
            index,
            param_type,
            expr,
        } in non_fun_fixed_args
        {
            let wanted_arg_type = ty::subst::subst_poly(&non_fun_param_pta, param_type);
            let fixed_arg_node = self.visit_expr(fcx, pv, &wanted_arg_type, expr)?;

            is_divergent = is_divergent || fixed_arg_node.is_divergent();

            ret_stx.add_evidence(param_type, fixed_arg_node.result_ty());
            fun_param_stx.add_evidence(param_type, fixed_arg_node.result_ty());

            inferred_fixed_arg_exprs.push((index, fixed_arg_node.expr));
        }

        // Visit our rest arg next so it's grouped in the first phase
        let inferred_rest_arg_expr = if let Some(rest_arg_expr) = rest_arg_expr {
            let tail_type = ty::Ty::List(param_iter.tail_type()).into();
            let wanted_tail_type = ty::subst::subst_poly(&non_fun_param_pta, &tail_type);
            let rest_arg_node = self.visit_expr(fcx, pv, &wanted_tail_type, rest_arg_expr)?;

            is_divergent = is_divergent || rest_arg_node.is_divergent();

            ret_stx.add_evidence(&tail_type, rest_arg_node.result_ty());
            fun_param_stx.add_evidence(&tail_type, rest_arg_node.result_ty());

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

        let fun_param_pta = fun_param_stx.into_poly_ty_args();
        for PendingFixedArg {
            index,
            param_type,
            expr,
        } in fun_fixed_args
        {
            let wanted_arg_type = ty::subst::subst_poly(&fun_param_pta, param_type);
            let fixed_arg_node = self.visit_expr(fcx, pv, &wanted_arg_type, expr)?;

            is_divergent = is_divergent || fixed_arg_node.is_divergent();

            ret_stx.add_evidence(param_type, fixed_arg_node.result_ty());
            inferred_fixed_arg_exprs.push((index, fixed_arg_node.expr));
        }

        inferred_fixed_arg_exprs.sort_unstable_by_key(|k| k.0);
        let inferred_fixed_arg_exprs = inferred_fixed_arg_exprs.into_iter().map(|e| e.1).collect();

        let ret_pta = ret_stx.into_poly_ty_args();
        let ret_type = if is_divergent {
            ty::Ty::never().into()
        } else {
            ty::subst::subst_poly(&ret_pta, fun_type.ret())
        };

        // Keep track of the purity from the application
        let app_purity = ty::subst::subst_purity(&ret_pta, fun_type.purity());
        unify_app_purity(pv, &app_purity);

        ensure_is_a(fcx, span, &ret_type, required_type)?;

        Ok(InferredNode {
            expr: hir::Expr {
                span,
                result_ty: ret_type,
                kind: hir::ExprKind::App(Box::new(hir::App {
                    fun_expr,
                    ty_args: ret_pta,
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
        fcx: &FunCtx,
        pv: &mut PurityVar,
        span: Span,
        fun_expr: hir::Expr<hir::Inferred>,
        test_ty: ty::pred::TestTy,
        subject_expr: hir::Expr<hir::Lowered>,
    ) -> Result<InferredNode> {
        use std::iter;
        use syntax::datum::Datum;

        let subject_var_id = if let hir::ExprKind::Ref(var_id) = &subject_expr.kind {
            Some(*var_id)
        } else {
            None
        };

        let subject_node = self.visit_expr(fcx, pv, &ty::Ty::Any.into(), subject_expr)?;

        let test_poly = test_ty.to_ty().into();
        let subject_poly = subject_node.result_ty();

        let pred_result = ty::is_a::ty_ref_is_a(&fcx.tvars, subject_poly, &test_poly);

        match pred_result {
            ty::is_a::Result::Yes | ty::is_a::Result::No => {
                let bool_result = pred_result.to_bool();

                let result_ty = if subject_node.is_divergent() {
                    ty::Ty::never().into()
                } else {
                    ty::Ty::LitBool(bool_result).into()
                };

                // Get rid of the predicate application entirely
                // Keep the subject expr around for its side effect
                Ok(InferredNode {
                    expr: keep_exprs_for_side_effects(
                        iter::once(subject_node.expr),
                        hir::Expr {
                            span,
                            result_ty,
                            kind: hir::ExprKind::Lit(Datum::Bool(span, bool_result)),
                        },
                    ),
                    type_conds: vec![],
                })
            }
            ty::is_a::Result::May => {
                let poly_type = if subject_node.is_divergent() {
                    ty::Ty::never().into()
                } else {
                    ty::Ty::Bool.into()
                };

                let type_conds = if let Some(var_id) = subject_var_id {
                    let type_if_true =
                        ty::intersect::intersect_ty_refs(&fcx.tvars, subject_poly, &test_poly)
                            .unwrap_or_else(|_| subject_poly.clone());

                    let type_if_false =
                        ty::subtract::subtract_ty_refs(&fcx.tvars, subject_poly, &test_poly);

                    vec![VarTypeCond {
                        var_id,
                        type_if_true,
                        type_if_false,
                    }]
                } else {
                    vec![]
                };

                Ok(InferredNode {
                    expr: hir::Expr {
                        span,
                        result_ty: poly_type,
                        kind: hir::ExprKind::App(Box::new(hir::App {
                            fun_expr,
                            ty_args: PolyTyArgs::empty(),
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
        fcx: &FunCtx,
        pv: &mut PurityVar,
        span: Span,
        fun_expr: hir::Expr<hir::Inferred>,
        test_ty: ty::pred::TestTy,
        subject_list_expr: hir::Expr<hir::Lowered>,
    ) -> Result<InferredNode> {
        use std::iter;

        let wanted_subject_list_type =
            ty::Ty::List(ty::List::new(Box::new([ty::Ty::Any.into()]), None)).into();

        let subject_list_node =
            self.visit_expr(fcx, pv, &wanted_subject_list_type, subject_list_expr)?;

        let subject_type = ListIterator::try_new_from_ty_ref(subject_list_node.result_ty())
            .and_then(|mut iter| iter.next())
            .expect("Unable to extract type argument from type predicate rest list");

        let pred_result = ty::is_a::ty_ref_is_a(&fcx.tvars, subject_type, &test_ty.to_ty().into());

        match pred_result {
            ty::is_a::Result::Yes | ty::is_a::Result::No => {
                let bool_result = pred_result.to_bool();

                let result_ty = if subject_list_node.is_divergent() {
                    ty::Ty::never().into()
                } else {
                    ty::Ty::LitBool(bool_result).into()
                };

                Ok(InferredNode {
                    expr: keep_exprs_for_side_effects(
                        iter::once(subject_list_node.expr),
                        hir::Expr {
                            span,
                            result_ty,
                            kind: hir::ExprKind::Lit(Datum::Bool(span, bool_result)),
                        },
                    ),
                    type_conds: vec![],
                })
            }
            ty::is_a::Result::May => {
                let poly_type = if subject_list_node.is_divergent() {
                    // The subject diverged so we diverged
                    ty::Ty::never().into()
                } else {
                    ty::Ty::Bool.into()
                };

                Ok(InferredNode {
                    expr: hir::Expr {
                        span,
                        result_ty: poly_type,
                        kind: hir::ExprKind::App(Box::new(hir::App {
                            fun_expr,
                            ty_args: PolyTyArgs::empty(),
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
        fcx: &FunCtx,
        pv: &mut PurityVar,
        span: Span,
        fun_expr: hir::Expr<hir::Inferred>,
        left_expr: hir::Expr<hir::Lowered>,
        right_expr: hir::Expr<hir::Lowered>,
    ) -> Result<InferredNode> {
        use crate::ty::props::is_literal;
        use std::iter;
        use syntax::datum::Datum;

        let left_var_id = if let hir::ExprKind::Ref(var_id) = &left_expr.kind {
            Some(*var_id)
        } else {
            None
        };

        let right_var_id = if let hir::ExprKind::Ref(var_id) = &right_expr.kind {
            Some(*var_id)
        } else {
            None
        };

        let left_node = self.visit_expr(fcx, pv, &ty::Ty::Any.into(), left_expr)?;
        let left_ty = left_node.result_ty();
        let left_is_literal = is_literal(left_ty);

        let right_node = self.visit_expr(fcx, pv, &ty::Ty::Any.into(), right_expr)?;
        let right_ty = right_node.result_ty();
        let right_is_literal = is_literal(right_ty);

        let is_divergent = left_node.is_divergent() || right_node.is_divergent();

        if left_is_literal && right_is_literal && left_ty == right_ty {
            // We were comparing literal types; this is a static true
            let result_ty = if is_divergent {
                ty::Ty::never().into()
            } else {
                ty::Ty::LitBool(true).into()
            };

            return Ok(InferredNode {
                expr: keep_exprs_for_side_effects(
                    iter::once(left_node.expr).chain(iter::once(right_node.expr)),
                    hir::Expr {
                        span,
                        result_ty,
                        kind: hir::ExprKind::Lit(Datum::Bool(span, true)),
                    },
                ),
                type_conds: vec![],
            });
        };

        let intersected_type = match ty::intersect::intersect_ty_refs(&fcx.tvars, left_ty, right_ty)
        {
            Ok(intersected_type) => intersected_type,
            Err(ty::intersect::Error::Disjoint) => {
                let result_ty = if is_divergent {
                    ty::Ty::never().into()
                } else {
                    ty::Ty::LitBool(false).into()
                };

                return Ok(InferredNode {
                    expr: keep_exprs_for_side_effects(
                        iter::once(left_node.expr).chain(iter::once(right_node.expr)),
                        hir::Expr {
                            span,
                            result_ty,
                            kind: hir::ExprKind::Lit(Datum::Bool(span, false)),
                        },
                    ),
                    type_conds: vec![],
                });
            }
        };

        let mut type_conds = vec![];

        if let Some(left_var_id) = left_var_id {
            let type_if_false = if right_is_literal {
                ty::subtract::subtract_ty_refs(&fcx.tvars, left_ty, right_ty)
            } else {
                left_ty.clone()
            };

            type_conds.push(VarTypeCond {
                var_id: left_var_id,
                type_if_true: intersected_type.clone(),
                type_if_false,
            });
        }

        if let Some(right_var_id) = right_var_id {
            let type_if_false = if left_is_literal {
                ty::subtract::subtract_ty_refs(&fcx.tvars, right_ty, left_ty)
            } else {
                right_ty.clone()
            };

            type_conds.push(VarTypeCond {
                var_id: right_var_id,
                type_if_true: intersected_type,
                type_if_false,
            });
        };

        let result_ty = if is_divergent {
            ty::Ty::never().into()
        } else {
            ty::Ty::Bool.into()
        };

        Ok(InferredNode {
            expr: hir::Expr {
                span,
                result_ty,
                kind: hir::ExprKind::App(Box::new(hir::App {
                    fun_expr,
                    ty_args: PolyTyArgs::empty(),
                    fixed_arg_exprs: vec![left_node.expr, right_node.expr],
                    rest_arg_expr: None,
                })),
            },
            type_conds,
        })
    }

    fn visit_app(
        &mut self,
        fcx: &FunCtx,
        pv: &mut PurityVar,
        required_type: &ty::Ref<ty::Poly>,
        span: Span,
        app: hir::App<hir::Lowered>,
    ) -> Result<InferredNode> {
        let hir::App {
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

        let wanted_fun_type = ty::TopFun::new(wanted_purity, required_type.clone()).into();

        let fun_node = self.visit_expr(fcx, pv, &wanted_fun_type, fun_expr)?;
        let revealed_fun_type = fun_node.result_ty().clone();

        match revealed_fun_type.resolve_to_ty(&fcx.tvars) {
            ty::Ty::TopFun(_) => Err(Error::new(
                span,
                ErrorKind::TopFunApply(str_for_poly(fcx, &revealed_fun_type)),
            )),
            ty::Ty::TyPred(test_ty) => {
                let wanted_arity = WantedArity::new(1, false);

                match (fixed_arg_exprs.len(), rest_arg_expr) {
                    (1, None) => {
                        let subject_expr = fixed_arg_exprs.pop().unwrap();
                        self.visit_fixed_ty_pred_app(
                            fcx,
                            pv,
                            span,
                            fun_node.expr,
                            *test_ty,
                            subject_expr,
                        )
                    }
                    (0, Some(subject_list_expr)) => self.visit_rest_ty_pred_app(
                        fcx,
                        pv,
                        span,
                        fun_node.expr,
                        *test_ty,
                        subject_list_expr,
                    ),
                    (supplied_arg_count, _) => Err(Error::new(
                        span,
                        ErrorKind::WrongArity(supplied_arg_count, wanted_arity),
                    )),
                }
            }
            ty::Ty::EqPred => {
                if fixed_arg_exprs.len() == 2 && rest_arg_expr.is_none() {
                    let right_expr = fixed_arg_exprs.pop().unwrap();
                    let left_expr = fixed_arg_exprs.pop().unwrap();

                    self.visit_fixed_eq_pred_app(
                        fcx,
                        pv,
                        span,
                        fun_node.expr,
                        left_expr,
                        right_expr,
                    )
                } else {
                    let fun_app = FunApp {
                        fun_expr: fun_node.expr,
                        fixed_arg_exprs,
                        rest_arg_expr,
                    };

                    self.visit_fun_app(
                        fcx,
                        pv,
                        required_type,
                        span,
                        &ty::Fun::new_for_eq_pred(),
                        fun_app,
                    )
                }
            }
            ty::Ty::Fun(fun_type) => {
                let fun_app = FunApp {
                    fun_expr: fun_node.expr,
                    fixed_arg_exprs,
                    rest_arg_expr,
                };

                self.visit_fun_app(fcx, pv, required_type, span, &fun_type.clone(), fun_app)
            }
            _ => panic!("Unexpected type"),
        }
    }

    fn visit_let(
        &mut self,
        fcx: &FunCtx,
        pv: &mut PurityVar,
        required_type: &ty::Ref<ty::Poly>,
        span: Span,
        hir_let: hir::Let<hir::Lowered>,
    ) -> Result<InferredNode> {
        let hir::Let {
            destruc,
            value_expr,
            body_expr,
        } = hir_let;

        let required_destruc_type =
            typeck::destruc::type_for_decl_destruc(&fcx.tvars, &destruc, None);

        // Pre-bind our variables to deal with recursive definitions
        let self_var_id = typeck::destruc::visit_vars(&destruc, |var_id, decl_type| {
            let var_type = match decl_type {
                hir::DeclTy::Known(poly_type) => VarType::Known(poly_type.clone()),
                hir::DeclTy::Free => VarType::Recursive,
            };

            self.var_to_type.insert(var_id, var_type);
        });

        let value_node = self.visit_expr_with_self_var_id(
            fcx,
            pv,
            &required_destruc_type,
            value_expr,
            self_var_id,
        )?;

        let free_ty_offset =
            self.destruc_value(&fcx.tvars, &destruc, value_node.result_ty(), false);

        let body_node = self.visit_expr(fcx, pv, required_type, body_expr)?;
        let mut inferred_free_types = self.free_ty_polys.drain(free_ty_offset..);

        let result_ty = if value_node.is_divergent() {
            // Value was divergent
            ty::Ty::never().into()
        } else {
            body_node.result_ty().clone()
        };

        Ok(InferredNode {
            expr: hir::Expr {
                span,
                result_ty,
                kind: hir::ExprKind::Let(Box::new(hir::Let {
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
        fcx: &FunCtx,
        required_type: &ty::Ref<ty::Poly>,
        span: Span,
        rust_fun: Box<rfi::Fun>,
    ) -> Result<InferredNode> {
        // Rust functions have their types validated by the RFI system when they're loaded
        // We just need to make sure we satisfy `required_type` and convert to an `InferredNode`
        let poly_type = ty::Ty::Fun(Box::new(rust_fun.arret_fun_type().clone())).into();
        ensure_is_a(fcx, span, &poly_type, required_type)?;

        Ok(InferredNode {
            expr: hir::Expr {
                span,
                result_ty: poly_type,
                kind: hir::ExprKind::RustFun(rust_fun),
            },
            type_conds: vec![],
        })
    }

    fn visit_expr_with_self_var_id(
        &mut self,
        fcx: &FunCtx,
        pv: &mut PurityVar,
        required_type: &ty::Ref<ty::Poly>,
        expr: hir::Expr<hir::Lowered>,
        self_var_id: Option<hir::VarId>,
    ) -> Result<InferredNode> {
        let hir::Expr { span, kind, .. } = expr;

        use crate::hir::ExprKind;
        match kind {
            ExprKind::Lit(datum) => self.visit_lit(fcx, required_type, datum),
            ExprKind::Cond(cond) => self.visit_cond(fcx, pv, required_type, span, *cond),
            ExprKind::Do(exprs) => self.visit_do(fcx, pv, required_type, span, exprs),
            ExprKind::Fun(fun) => self.visit_fun(fcx, required_type, span, *fun, self_var_id),
            ExprKind::RustFun(rust_fun) => self.visit_rust_fun(fcx, required_type, span, rust_fun),
            ExprKind::TyPred(test_type) => self.visit_ty_pred(fcx, required_type, span, test_type),
            ExprKind::EqPred => self.visit_eq_pred(fcx, required_type, span),
            ExprKind::Let(hir_let) => self.visit_let(fcx, pv, required_type, span, *hir_let),
            ExprKind::Ref(var_id) => self.visit_ref(fcx, required_type, span, var_id),
            ExprKind::App(app) => self.visit_app(fcx, pv, required_type, span, *app),
            ExprKind::MacroExpand(inner_expr) => self
                .visit_expr_with_self_var_id(fcx, pv, required_type, *inner_expr, self_var_id)
                .map(|inferred| InferredNode {
                    expr: hir::Expr {
                        span,
                        result_ty: inferred.expr.result_ty.clone(),
                        kind: ExprKind::MacroExpand(Box::new(inferred.expr)),
                    },
                    ..inferred
                })
                .map_err(|err| err.with_macro_invocation_span(span)),
        }
    }

    fn visit_expr(
        &mut self,
        fcx: &FunCtx,
        pv: &mut PurityVar,
        required_type: &ty::Ref<ty::Poly>,
        expr: hir::Expr<hir::Lowered>,
    ) -> Result<InferredNode> {
        self.visit_expr_with_self_var_id(fcx, pv, required_type, expr, None)
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

        if let Some(var_id) = *scalar.var_id() {
            let var_type = if let (Some(free_ty_id), true) = (free_ty_id, is_param) {
                VarType::ParamScalar(free_ty_id)
            } else {
                VarType::Known(value_type.clone())
            };

            self.var_to_type.insert(var_id, var_type);
        }

        start_offset
    }

    fn destruc_rest_value(
        &mut self,
        tvars: &ty::TVars,
        rest: &destruc::Scalar<hir::Lowered>,
        value_type_iter: ListIterator<'_, ty::Poly>,
        is_param: bool,
    ) {
        let param_free_ty_id = if *rest.ty() == hir::DeclTy::Free {
            // Start with member type as a guide
            let member_type = value_type_iter
                .clone()
                .collect_rest(tvars)
                .unwrap_or_else(|| ty::Ty::Any.into());

            let free_ty_id = self.insert_free_ty(member_type);
            Some(free_ty_id).filter(|_| is_param)
        } else {
            None
        };

        if let Some(var_id) = *rest.var_id() {
            let var_type = if let Some(param_free_ty_id) = param_free_ty_id {
                VarType::ParamRest(param_free_ty_id)
            } else {
                // If we're not a rest parameter we know our exact tail type. We can't subst
                // the tail type in to the destruc because it only takes a member type.
                // However, we can use the exact tail type whenever we reference the var.
                VarType::Known(ty::Ty::List(value_type_iter.tail_type()).into())
            };

            self.var_to_type.insert(var_id, var_type);
        }
    }

    fn destruc_list_value(
        &mut self,
        tvars: &ty::TVars,
        list: &destruc::List<hir::Lowered>,
        mut value_type_iter: ListIterator<'_, ty::Poly>,
        is_param: bool,
    ) -> usize {
        let start_offset = self.free_ty_polys.len();

        for fixed_destruc in list.fixed() {
            let member_type = value_type_iter
                .next()
                .expect("Destructured value with unexpected type");

            self.destruc_value(tvars, fixed_destruc, member_type, is_param);
        }

        if let Some(rest) = list.rest() {
            self.destruc_rest_value(tvars, rest, value_type_iter, is_param);
        }

        start_offset
    }

    fn destruc_value(
        &mut self,
        tvars: &ty::TVars,
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
                self.destruc_list_value(tvars, list, value_type_iter, is_param)
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

        let fcx = FunCtx {
            pvars: purity::PVars::new(),
            tvars: ty::TVars::new(),
        };

        // Module definitions must be pure
        let mut pv = PurityVar::Known(Purity::Pure.into());

        // Mark all of our free typed variable as recursive
        let self_var_id = typeck::destruc::visit_vars(&destruc, |var_id, decl_type| {
            if *decl_type == hir::DeclTy::Free {
                self.var_to_type.insert(var_id, VarType::Recursive);
            }
        });

        let required_type = typeck::destruc::type_for_decl_destruc(&fcx.tvars, &destruc, None);
        let value_node = match self.visit_expr_with_self_var_id(
            &fcx,
            &mut pv,
            &required_type,
            value_expr,
            self_var_id,
        ) {
            Ok(value_node) => value_node,
            Err(error) => {
                // Mark this def as an error so we can suppress cascade errors
                typeck::destruc::visit_vars(&destruc, |var_id, _| {
                    self.var_to_type.insert(var_id, VarType::Error);
                });
                return Err(error);
            }
        };

        let free_ty_offset =
            self.destruc_value(&fcx.tvars, &destruc, value_node.result_ty(), false);
        let mut inferred_free_types = self.free_ty_polys.drain(free_ty_offset..);

        Ok(hir::Def {
            span,
            macro_invocation_span,
            destruc: destruc::subst_destruc(&mut inferred_free_types, destruc),
            value_expr: value_node.expr,
        })
    }

    fn recurse_into_def_id(&mut self, def_id: InputDefId) -> Result<()> {
        let def_idx = def_id.to_usize();

        let previous_state = std::mem::replace(&mut self.input_defs[def_idx], InputDef::Complete);

        if let InputDef::Pending(def) = previous_state {
            let inferred_def = self.visit_def(def)?;
            self.complete_defs.push(inferred_def);
        } else {
            panic!("Tried to infer already complete def. An error previously occurred?[")
        }

        Ok(())
    }

    fn infer_input_defs(&mut self) -> result::Result<(), Vec<Error>> {
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

        if errs.is_empty() {
            Ok(())
        } else {
            Err(errs)
        }
    }
}

pub(crate) struct InferCtx {
    var_to_type: HashMap<hir::VarId, VarType>,
}

impl InferCtx {
    pub fn new() -> InferCtx {
        InferCtx {
            var_to_type: HashMap::new(),
        }
    }

    pub fn infer_defs(
        &mut self,
        defs: Vec<hir::Def<hir::Lowered>>,
    ) -> result::Result<Vec<hir::Def<hir::Inferred>>, Vec<Error>> {
        let mut rdcx = RecursiveDefsCtx::new(defs, &mut self.var_to_type);
        rdcx.infer_input_defs()?;
        Ok(rdcx.complete_defs)
    }

    pub fn infer_expr(&mut self, expr: hir::Expr<hir::Lowered>) -> Result<InferredNode> {
        let mut rdcx = RecursiveDefsCtx::new(vec![], &mut self.var_to_type);
        let fcx = FunCtx {
            pvars: purity::PVars::new(),
            tvars: ty::TVars::new(),
        };
        let mut pv = PurityVar::Known(Purity::Impure.into());

        rdcx.visit_expr(&fcx, &mut pv, &ty::Ty::Any.into(), expr)
    }
}

fn ensure_main_type(
    complete_defs: &[hir::Def<hir::Inferred>],
    main_var_id: hir::VarId,
    inferred_main_type: &ty::Ref<ty::Poly>,
) -> Result<()> {
    let expected_main_type = ty::Fun::new_for_main().into();
    let empty_pvars = purity::PVars::new();
    let empty_tvars = ty::TVars::new();

    if !ty::is_a::ty_ref_is_a(&empty_tvars, inferred_main_type, &expected_main_type).to_bool() {
        use crate::reporting::LocTrace;
        use syntax::span::EMPTY_SPAN;

        // Try to find where `(main!)` was defined
        let main_loc_trace = complete_defs
            .iter()
            .find_map(|def| {
                if let destruc::Destruc::Scalar(_, ref scalar) = def.destruc {
                    if scalar.var_id() == &Some(main_var_id) {
                        return Some(LocTrace::new(def.span, def.macro_invocation_span));
                    }
                }

                None
            })
            .unwrap_or_else(|| EMPTY_SPAN.into());

        let expected_str =
            hir::str_for_poly(&empty_pvars, &empty_tvars, &expected_main_type).into_boxed_str();
        let inferred_str =
            hir::str_for_poly(&empty_pvars, &empty_tvars, inferred_main_type).into_boxed_str();

        return Err(Error::new_with_loc_trace(
            main_loc_trace,
            ErrorKind::IsNotTy(inferred_str, expected_str),
        ));
    };

    Ok(())
}

pub fn infer_program(
    defs: Vec<Vec<hir::Def<hir::Lowered>>>,
    main_var_id: hir::VarId,
) -> result::Result<Vec<hir::Def<hir::Inferred>>, Vec<Error>> {
    let mut var_to_type = HashMap::with_capacity(defs.len());
    let mut complete_defs = vec![];

    for recursive_defs in defs {
        let mut rdcx = RecursiveDefsCtx::new(recursive_defs, &mut var_to_type);

        rdcx.infer_input_defs()?;
        complete_defs.append(&mut rdcx.complete_defs);
    }

    let inferred_main_type = if let VarType::Known(ref poly_type) = var_to_type[&main_var_id] {
        poly_type
    } else {
        panic!("Unable to find (main!) var type");
    };

    ensure_main_type(&complete_defs, main_var_id, &inferred_main_type).map_err(|err| vec![err])?;

    Ok(complete_defs)
}

#[cfg(test)]
mod test {
    use super::*;
    use crate::hir::lowering::expr_for_str;
    use syntax::span::t2s;

    fn type_for_expr(
        required_type: &ty::Ref<ty::Poly>,
        expr: hir::Expr<hir::Lowered>,
    ) -> Result<ty::Ref<ty::Poly>> {
        let mut var_to_type = HashMap::new();
        let mut rdcx = RecursiveDefsCtx::new(vec![], &mut var_to_type);

        let fcx = FunCtx {
            pvars: purity::PVars::new(),
            tvars: ty::TVars::new(),
        };
        let mut pv = PurityVar::Known(Purity::Pure.into());

        rdcx.visit_expr(&fcx, &mut pv, required_type, expr)
            .map(|node| node.expr.result_ty)
    }

    fn assert_type_for_expr(ty_str: &str, expr_str: &str) {
        let expr = expr_for_str(expr_str);
        let poly = hir::poly_for_str(ty_str);

        assert_eq!(poly, type_for_expr(&ty::Ty::Any.into(), expr).unwrap());
    }

    fn assert_constrained_type_for_expr(expected_ty_str: &str, expr_str: &str, guide_ty_str: &str) {
        let expr = expr_for_str(expr_str);
        let expected_poly = hir::poly_for_str(expected_ty_str);
        let guide_poly = hir::poly_for_str(guide_ty_str);

        assert_eq!(expected_poly, type_for_expr(&guide_poly, expr).unwrap());
    }

    fn assert_type_error(err: &Error, expr_str: &str) {
        let expr = expr_for_str(expr_str);

        assert_eq!(err, &type_for_expr(&ty::Ty::Any.into(), expr).unwrap_err())
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
            ErrorKind::IsNotTy("(Str -> true)".into(), "(Sym -> true)".into()),
        );
        assert_type_error(&err, j);

        // Function with a known type being bound to an incompatible type
        let j = "(let [[f (Sym -> true)] (fn ([_ Str]) -> true true)])";
        let t = "                        ^^^^^^^^^^^^^^^^^^^^^^^^^^^  ";
        let err = Error::new(
            t2s(t),
            ErrorKind::IsNotTy("(Str -> true)".into(), "(Sym -> true)".into()),
        );
        assert_type_error(&err, j);

        let j = "(fn ([x Str]) -> Sym x)";
        let t = "                     ^ ";
        let err = Error::new(t2s(t), ErrorKind::IsNotTy("Str".into(), "Sym".into()));
        assert_type_error(&err, j);

        // Instantiating a polymorphic function
        // We can't name polymorphic types so we need the (let) hack
        assert_type_for_expr("()", "(let [[_ (Sym -> Sym)] (fn #{T} ([x T]) -> T x)])");

        let j = "(let [[_ (Sym -> Str)] (fn #{T} ([x T]) -> T x)])";
        let t = "                       ^^^^^^^^^^^^^^^^^^^^^^^^  ";
        let err = Error::new(
            t2s(t),
            ErrorKind::IsNotTy("(All #{T} T -> T)".into(), "(Sym -> Str)".into()),
        );
        assert_type_error(&err, j);
    }

    #[test]
    fn app_types() {
        assert_type_for_expr("'foo", "((fn () 'foo))");

        assert_type_for_expr("true", "(sym? 'foo)");
        assert_type_for_expr("false", "(sym? false)");

        assert_type_for_expr("Int", "((fn #{A} ([value A]) -> A value) 1)");
        assert_type_for_expr("'foo", "((fn #{A} ([value A]) -> A value) '(foo) ...)");

        assert_type_for_expr(
            "(Listof Bool)",
            "((fn #{A} ([rest A] ...) -> (Listof A) rest) true false)",
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
    fn recursive_app() {
        assert_type_for_expr(
            "'foo",
            "(let [[recurse (-> 'foo)] (fn () (recurse))] (recurse))",
        );

        assert_type_for_expr(
            "'foo",
            "(let [recurse (fn ([x Int]) -> 'foo (recurse x))] (recurse 1))",
        );

        let j = "(let [recurse (fn () (recurse))] (recurse))";
        let t = "                      ^^^^^^^              ";
        let err = Error::new(t2s(t), ErrorKind::RecursiveType);
        assert_type_error(&err, j);

        let j = "(let [recurse (fn (x) -> 'foo (recurse x))] (recurse 1))";
        let t = "                               ^^^^^^^                  ";
        let err = Error::new(t2s(t), ErrorKind::RecursiveType);
        assert_type_error(&err, j);

        let j = "(let [recurse (fn ([x Int]) (recurse x))] (recurse 1))";
        let t = "                             ^^^^^^^                  ";
        let err = Error::new(t2s(t), ErrorKind::RecursiveType);
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
            ErrorKind::IsNotPurity("(->! false)".into(), "pure".into()),
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
            "(let [(_ rest ...) '(1 true false)] rest)",
        );
    }

    #[test]
    fn var_ref() {
        assert_type_for_expr("Int", "(let [x 1] x)")
    }

    #[test]
    fn ty_pred() {
        assert_type_for_expr("true", "(sym? '(foo) ...)");
        assert_type_for_expr("true", "(sym? 'foo)");
        assert_type_for_expr("false", "(int? '(foo) ...)");
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
