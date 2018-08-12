use std;
use std::collections::HashMap;
use std::result;

use crate::hir;
use crate::hir::destruc;
use crate::hir::rfi;
use crate::ty;
use crate::ty::list_iter::ListIterator;
use crate::ty::purity::Purity;
use crate::ty::TyRef;
use crate::typeck;
use crate::typeck::error::{Error, ErrorKind, WantedArity};
use syntax::datum::Datum;
use syntax::span::Span;

type Result<T> = result::Result<T, Error>;

/// Result of inferring the type for a HIR expression
pub struct InferredNode {
    /// Expression with all free types replaced with poly types
    expr: hir::Expr<ty::Poly>,
    /// Inferred type for the expression's value
    poly_type: ty::Poly,
    /// Type condition depending the poly type of this node
    type_cond: Option<Box<VarTypeCond>>,
}

impl InferredNode {
    /// Create a node with a var type condition depending on its type
    fn new_cond_type_node(
        expr: hir::Expr<ty::Poly>,
        var_id: hir::VarId,
        type_if_true: ty::Poly,
        type_if_false: ty::Poly,
    ) -> InferredNode {
        InferredNode {
            expr,
            poly_type: ty::Ty::Bool.into_poly(),
            type_cond: Some(Box::new(VarTypeCond {
                var_id,
                type_if_true,
                type_if_false,
            })),
        }
    }

    fn new_ref_node(span: Span, var_id: hir::VarId, poly_type: ty::Poly) -> InferredNode {
        let type_cond = if poly_type == ty::Ty::Bool.into_poly() {
            // This seems useless but it allows occurrence typing to work if this type
            // flows through another node such as `(do)` or `(let)`
            Some(Box::new(VarTypeCond {
                var_id,
                type_if_true: ty::Ty::LitBool(true).into_poly(),
                type_if_false: ty::Ty::LitBool(false).into_poly(),
            }))
        } else {
            None
        };

        InferredNode {
            expr: hir::Expr::Ref(span, var_id),
            poly_type,
            type_cond,
        }
    }

    pub fn poly_type(&self) -> &ty::Poly {
        &self.poly_type
    }
}

struct VarTypeCond {
    var_id: hir::VarId,
    type_if_true: ty::Poly,
    type_if_false: ty::Poly,
}

new_indexing_id_type!(FreeTyId, u32);
new_indexing_id_type!(InputDefId, u32);

/// Partially inferred function application
///
/// The function has been inferred while the arguments have not
struct FunApp {
    fun_expr: hir::Expr<ty::Poly>,
    fixed_arg_exprs: Vec<hir::Expr<ty::Decl>>,
    rest_arg_expr: Option<hir::Expr<ty::Decl>>,
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
    Known(ty::Poly),
}

#[derive(Clone)]
enum PurityVarType {
    Free(ty::purity::Poly),
    Known(ty::purity::Poly),
}

impl PurityVarType {
    fn into_poly(self) -> ty::purity::Poly {
        match self {
            PurityVarType::Free(poly) => poly,
            PurityVarType::Known(poly) => poly,
        }
    }
}

enum InputDef {
    Pending(hir::Def<ty::Decl>),
    Complete,
}

struct RecursiveDefsCtx<'vars, 'types> {
    input_defs: Vec<InputDef>,
    complete_defs: Vec<hir::Def<ty::Poly>>,

    pvars: &'vars [ty::purity::PVar],
    tvars: &'vars [ty::TVar],

    // The inferred types for free types in the order they're encountered
    //
    // Each (def), (let) and (fn) push entries to `free_ty_polys` before they evaluate their body
    // and then pop them off afterwards.
    free_ty_polys: Vec<ty::Poly>,

    var_to_type: &'types mut HashMap<hir::VarId, VarType>,
}

#[derive(Clone)]
struct FunCtx {
    purity: PurityVarType,
}

impl<'vars, 'types> RecursiveDefsCtx<'vars, 'types> {
    fn new(
        pvars: &'vars [ty::purity::PVar],
        tvars: &'vars [ty::TVar],
        defs: Vec<hir::Def<ty::Decl>>,
        var_to_type: &'types mut HashMap<hir::VarId, VarType>,
    ) -> RecursiveDefsCtx<'vars, 'types> {
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
                        ty::Decl::Known(poly_type) => VarType::Known(poly_type.clone()),
                        ty::Decl::Free => {
                            // Record the definition ID so we can deal with forward type references
                            VarType::Pending(def_id)
                        }
                    };

                    var_to_type.insert(var_id, var_type);
                });

                InputDef::Pending(hir_def)
            }).collect::<Vec<InputDef>>();

        let complete_defs = Vec::with_capacity(input_defs.len());
        RecursiveDefsCtx {
            input_defs,
            complete_defs,
            pvars,
            tvars,
            free_ty_polys: vec![],
            var_to_type,
        }
    }

    fn insert_free_ty(&mut self, initial_type: ty::Poly) -> FreeTyId {
        FreeTyId::new_entry_id(&mut self.free_ty_polys, initial_type)
    }

    fn str_for_poly(&self, poly: &ty::Poly) -> Box<str> {
        hir::str_for_poly(self.pvars, self.tvars, poly).into_boxed_str()
    }

    /// Ensures `sub_poly` is a subtype of `parent_poly`
    fn ensure_is_a(&self, span: Span, sub_poly: &ty::Poly, parent_poly: &ty::Poly) -> Result<()> {
        if ty::is_a::poly_is_a(self.tvars, sub_poly, parent_poly).to_bool() {
            return Ok(());
        }

        // Try to see if this type check would have passed if the parent was impure. This is purely
        // a hack to catch applying impure functions in a pure context.
        if let ty::Poly::Fixed(ty::Ty::TopFun(top_fun)) = parent_poly {
            let impure_top_fun = ty::TopFun::new(Purity::Impure.into_poly(), top_fun.ret().clone());

            if ty::is_a::poly_is_a(
                self.tvars,
                sub_poly,
                &ty::Ty::TopFun(Box::new(impure_top_fun)).into_poly(),
            ).to_bool()
            {
                let purity_str = if top_fun.purity() == &Purity::Pure.into_poly() {
                    // `->` might be confusing here
                    "pure".into()
                } else {
                    format!("`{}`", hir::str_for_purity(self.pvars, top_fun.purity())).into()
                };

                return Err(Error::new(
                    span,
                    ErrorKind::IsNotPurity(self.str_for_poly(&sub_poly), purity_str),
                ));
            }
        }

        Err(Error::new(
            span,
            ErrorKind::IsNotTy(self.str_for_poly(&sub_poly), self.str_for_poly(parent_poly)),
        ))
    }

    fn visit_lit(&mut self, required_type: &ty::Poly, datum: Datum) -> Result<InferredNode> {
        let lit_type = ty::datum::poly_for_datum(&datum);
        self.ensure_is_a(datum.span(), &lit_type, required_type)?;

        Ok(InferredNode {
            expr: hir::Expr::Lit(datum),
            poly_type: lit_type,
            type_cond: None,
        })
    }

    fn visit_cond(
        &mut self,
        fcx: &mut FunCtx,
        required_type: &ty::Poly,
        span: Span,
        cond: hir::Cond<ty::Decl>,
    ) -> Result<InferredNode> {
        let hir::Cond {
            test_expr,
            true_expr,
            false_expr,
        } = cond;

        let test_required_type = &ty::Ty::Bool.into_poly();
        let test_node = self.visit_expr(fcx, test_required_type, test_expr)?;

        let (true_node, false_node) = if let Some(type_cond) = test_node.type_cond {
            let VarTypeCond {
                var_id,
                type_if_true,
                type_if_false,
            } = *type_cond;

            // Patch our occurrence types in to the `var_to_type` and restore it after. We
            // avoid `?`ing our results until the end to make sure the original types are
            // properly restored.
            let original_var_type = self
                .var_to_type
                .insert(var_id, VarType::Known(type_if_true.clone()))
                .unwrap();

            let true_result = self.visit_expr(fcx, required_type, true_expr);

            self.var_to_type
                .insert(var_id, VarType::Known(type_if_false.clone()));
            let false_result = self.visit_expr(fcx, required_type, false_expr);

            self.var_to_type.insert(var_id, original_var_type);

            let true_node = true_result?;
            let false_node = false_result?;

            // This is an analog of (not). We can flip the test's type condition
            if true_node.poly_type.try_to_bool() == Some(false)
                && false_node.poly_type.try_to_bool() == Some(true)
            {
                return Ok(InferredNode {
                    expr: hir::Expr::Cond(
                        span,
                        Box::new(hir::Cond {
                            test_expr: test_node.expr,
                            true_expr: true_node.expr,
                            false_expr: false_node.expr,
                        }),
                    ),
                    poly_type: test_node.poly_type,
                    type_cond: Some(Box::new(VarTypeCond {
                        var_id,
                        type_if_true: type_if_false,
                        type_if_false: type_if_true,
                    })),
                });
            }

            (true_node, false_node)
        } else {
            match test_node.poly_type.try_to_bool() {
                Some(true) => {
                    let true_node = self.visit_expr(fcx, required_type, true_expr)?;
                    self.visit_expr(fcx, &ty::Ty::Any.into_poly(), false_expr)?;
                    return Ok(true_node);
                }
                Some(false) => {
                    self.visit_expr(fcx, &ty::Ty::Any.into_poly(), true_expr)?;
                    let false_node = self.visit_expr(fcx, required_type, false_expr)?;
                    return Ok(false_node);
                }
                None => {
                    let true_node = self.visit_expr(fcx, required_type, true_expr)?;
                    let false_node = self.visit_expr(fcx, required_type, false_expr)?;
                    (true_node, false_node)
                }
            }
        };

        let cond_expr = hir::Expr::Cond(
            span,
            Box::new(hir::Cond {
                test_expr: test_node.expr,
                true_expr: true_node.expr,
                false_expr: false_node.expr,
            }),
        );

        // If the false branch is always false we can move an occurrence typing from the true
        // branch upwards. The same reasoning applies for the true branch.
        if false_node.poly_type.try_to_bool() == Some(false) {
            if let Some(true_type_cond) = true_node.type_cond {
                return Ok(InferredNode {
                    expr: cond_expr,
                    poly_type: true_node.poly_type,
                    type_cond: Some(true_type_cond),
                });
            }
        } else if true_node.poly_type.try_to_bool() == Some(true) {
            if let Some(false_type_cond) = false_node.type_cond {
                return Ok(InferredNode {
                    expr: cond_expr,
                    poly_type: false_node.poly_type,
                    type_cond: Some(false_type_cond),
                });
            }
        }

        let poly_type = if test_node.poly_type == ty::Ty::never().into_poly() {
            // Test diverged
            ty::Ty::never().into_poly()
        } else {
            ty::unify::poly_unify_to_poly(self.tvars, &true_node.poly_type, &false_node.poly_type)
        };

        Ok(InferredNode {
            expr: cond_expr,
            poly_type,
            type_cond: None,
        })
    }

    fn visit_ty_pred(
        &self,
        required_type: &ty::Poly,
        span: Span,
        test_type: ty::Poly,
    ) -> Result<InferredNode> {
        let pred_type = ty::Ty::TyPred(Box::new(test_type.clone())).into_poly();

        self.ensure_is_a(span, &pred_type, required_type)?;

        Ok(InferredNode {
            expr: hir::Expr::TyPred(span, test_type),
            poly_type: pred_type,
            type_cond: None,
        })
    }

    fn unify_app_purity(&self, fcx: &mut FunCtx, app_purity: &ty::purity::Poly) {
        if let PurityVarType::Free(ref mut free_purity) = fcx.purity {
            *free_purity = ty::unify::poly_unify_purity(free_purity, &app_purity)
        };
    }

    fn type_for_free_ref(
        &self,
        required_type: &ty::Poly,
        span: Span,
        current_type: &ty::Poly,
    ) -> Result<ty::Poly> {
        // Unlike references to known variables the `current_type` and `required_type` have equal
        // footing. We intersect here to find the commonality between the two types. This will
        // become the new type of the variable.
        ty::intersect::poly_intersect(self.tvars, required_type, current_type).map_err(|_| {
            Error::new(
                span,
                ErrorKind::VarHasEmptyType(
                    self.str_for_poly(required_type),
                    self.str_for_poly(current_type),
                ),
            )
        })
    }

    fn member_type_for_poly_list(&self, span: Span, poly_type: &ty::Poly) -> Result<ty::Poly> {
        if poly_type == &ty::Ty::Any.into_poly() {
            return Ok(ty::Ty::Any.into_poly());
        }

        let list = poly_type
            .find_member(|t| {
                if let ty::Ty::List(list) = t {
                    Some(list)
                } else {
                    None
                }
            }).ok_or_else(|| {
                Error::new(
                    span,
                    ErrorKind::IsNotTy(self.str_for_poly(&poly_type), "(Listof Any)".into()),
                )
            })?;

        Ok(ListIterator::new(list)
            .collect_rest(self.tvars)
            .unwrap_or_else(|| ty::Ty::Any.into_poly()))
    }

    fn visit_ref(
        &mut self,
        fcx: &FunCtx,
        required_type: &ty::Poly,
        span: Span,
        var_id: hir::VarId,
    ) -> Result<InferredNode> {
        let pending_def_id = match self.var_to_type[&var_id] {
            VarType::Pending(def_id) => def_id,
            VarType::Recursive => return Err(Error::new(span, ErrorKind::RecursiveType)),
            VarType::Error => return Err(Error::new(span, ErrorKind::DependsOnError)),
            VarType::Known(ref known_type) => {
                self.ensure_is_a(span, known_type, required_type)?;
                return Ok(InferredNode::new_ref_node(span, var_id, known_type.clone()));
            }
            VarType::ParamScalar(free_ty_id) => {
                let current_type = &self.free_ty_polys[free_ty_id.to_usize()];
                let new_free_type = self.type_for_free_ref(required_type, span, current_type)?;

                self.free_ty_polys[free_ty_id.to_usize()] = new_free_type.clone();
                return Ok(InferredNode::new_ref_node(span, var_id, new_free_type));
            }
            VarType::ParamRest(free_ty_id) => {
                let current_member_type = &self.free_ty_polys[free_ty_id.to_usize()];
                let required_member_type = self.member_type_for_poly_list(span, required_type)?;

                let new_free_type =
                    self.type_for_free_ref(&required_member_type, span, current_member_type)?;

                self.free_ty_polys[free_ty_id.to_usize()] = new_free_type.clone();
                let rest_list_type =
                    ty::Ty::List(ty::List::new(Box::new([]), Some(new_free_type))).into_poly();

                // Make sure we didn't require a specific list type e.g. `(List Int Int Int)`
                self.ensure_is_a(span, &rest_list_type, required_type)?;

                return Ok(InferredNode::new_ref_node(span, var_id, rest_list_type));
            }
        };

        self.recurse_into_def_id(pending_def_id)?;
        // This assumes `recurse_into_def_id` has populated our variables now
        self.visit_ref(fcx, required_type, span, var_id)
    }

    fn visit_do(
        &mut self,
        fcx: &mut FunCtx,
        required_type: &ty::Poly,
        mut exprs: Vec<hir::Expr<ty::Decl>>,
    ) -> Result<InferredNode> {
        let terminal_expr = if let Some(terminal_expr) = exprs.pop() {
            terminal_expr
        } else {
            return Ok(InferredNode {
                expr: hir::Expr::Do(vec![]),
                poly_type: ty::Ty::unit().into_poly(),
                type_cond: None,
            });
        };

        let mut is_divergent = false;
        let mut inferred_exprs = exprs
            .into_iter()
            .map(|non_terminal_expr| {
                // The type of this expression doesn't matter; its value is discarded
                let node = self.visit_expr(fcx, &ty::Ty::Any.into_poly(), non_terminal_expr)?;

                if node.poly_type == ty::Ty::never().into_poly() {
                    is_divergent = true;
                }

                Ok(node.expr)
            }).collect::<Result<Vec<hir::Expr<ty::Poly>>>>()?;

        if is_divergent {
            self.visit_expr(fcx, &ty::Ty::Any.into_poly(), terminal_expr)?;

            Ok(InferredNode {
                expr: hir::Expr::Do(inferred_exprs),
                poly_type: ty::Ty::never().into_poly(),
                type_cond: None,
            })
        } else {
            let terminal_node = self.visit_expr(fcx, required_type, terminal_expr)?;
            inferred_exprs.push(terminal_node.expr);

            Ok(InferredNode {
                expr: hir::Expr::Do(inferred_exprs),
                poly_type: terminal_node.poly_type,
                type_cond: terminal_node.type_cond,
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
        required_type: &ty::Poly,
        span: Span,
        decl_fun: hir::Fun<ty::Decl>,
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

        let initial_param_type: ty::List<ty::Poly> = typeck::destruc::type_for_decl_list_destruc(
            self.tvars,
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
            ty::Decl::Known(poly) => poly.clone(),
            ty::Decl::Free => {
                decl_tys_are_known = false;

                if let Some(ref required_top_fun_type) = required_top_fun_type {
                    // Fall back to the backwards type
                    required_top_fun_type.ret().clone()
                } else {
                    // Use Any as a last resort
                    ty::Ty::Any.into_poly()
                }
            }
        };

        let purity_var = match decl_fun.purity {
            ty::purity::Decl::Known(poly_purity) => {
                if let (Some(self_var_id), true) = (self_var_id, decl_tys_are_known) {
                    let self_type = ty::Fun::new(
                        decl_fun.pvar_ids.clone(),
                        decl_fun.tvar_ids.clone(),
                        ty::TopFun::new(poly_purity.clone(), wanted_ret_type.clone()),
                        initial_param_type,
                    ).into_ty_ref();

                    // We have a fully known type; allow recursive calls
                    self.var_to_type
                        .insert(self_var_id, VarType::Known(self_type));
                }

                PurityVarType::Known(poly_purity)
            }
            ty::purity::Decl::Free => {
                // Functions start pure until proven otherwise
                PurityVarType::Free(Purity::Pure.into_poly())
            }
        };

        let mut fun_fcx = FunCtx { purity: purity_var };

        let body_node = self.visit_expr(&mut fun_fcx, &wanted_ret_type, decl_fun.body_expr)?;
        let revealed_ret_type = body_node.poly_type;
        let revealed_purity = fun_fcx.purity.into_poly();

        let revealed_param_destruc = {
            let mut inferred_free_types = self.free_ty_polys.drain(free_ty_offset..);
            destruc::subst_list_destruc(&mut inferred_free_types, decl_fun.params)
        };
        let revealed_param_type = hir::destruc::poly_for_list_destruc(&revealed_param_destruc);

        let revealed_type = ty::Fun::new(
            decl_fun.pvar_ids.clone(),
            decl_fun.tvar_ids.clone(),
            ty::TopFun::new(revealed_purity.clone(), revealed_ret_type.clone()),
            revealed_param_type,
        ).into_ty_ref();

        let revealed_fun = hir::Fun::<ty::Poly> {
            pvar_ids: decl_fun.pvar_ids.clone(),
            tvar_ids: decl_fun.tvar_ids.clone(),
            purity: revealed_purity,
            params: revealed_param_destruc,
            ret_ty: revealed_ret_type,
            body_expr: body_node.expr,
        };

        self.ensure_is_a(span, &revealed_type, required_type)?;

        Ok(InferredNode {
            expr: hir::Expr::Fun(span, Box::new(revealed_fun)),
            poly_type: revealed_type,
            type_cond: None,
        })
    }

    fn visit_fun_app(
        &mut self,
        fcx: &mut FunCtx,
        required_type: &ty::Poly,
        span: Span,
        fun_type: &ty::Fun<ty::Poly>,
        fun_app: FunApp,
    ) -> Result<InferredNode> {
        let FunApp {
            fun_expr,
            fixed_arg_exprs,
            rest_arg_expr,
        } = fun_app;

        // The context used to select the types for our parameters. It's only evidence is the
        // wanted return type which is used for backwards type propagation.
        let mut param_select_ctx = ty::select::SelectCtx::new(
            fun_type.pvar_ids().clone(),
            fun_type.tvar_ids().clone(),
            self.tvars,
        );

        // The context used to select the type for our return value. It collects evidence from the
        // arguments as they're evaluated.
        let mut ret_select_ctx = param_select_ctx.clone();

        // Add our return type information
        param_select_ctx.add_evidence(fun_type.ret(), required_type);

        // Iterate over our parameter type to feed type information in to the arguments
        let mut param_iter = ListIterator::new(fun_type.params());

        let supplied_arg_count = fixed_arg_exprs.len();
        let wanted_arity = WantedArity::new(param_iter.fixed_len(), param_iter.has_rest());

        let mut is_divergent = false;
        let mut inferred_fixed_arg_exprs = vec![];
        for fixed_arg_expr in fixed_arg_exprs {
            let param_type = param_iter.next().ok_or_else(|| {
                Error::new(
                    span,
                    ErrorKind::TooManyArgs(supplied_arg_count, wanted_arity),
                )
            })?;

            let wanted_arg_type = ty::subst::inst_ty_selection(&param_select_ctx, param_type);
            let fixed_arg_node = self.visit_expr(fcx, &wanted_arg_type, fixed_arg_expr)?;

            if fixed_arg_node.poly_type == ty::Ty::never().into_poly() {
                is_divergent = true;
            }

            ret_select_ctx.add_evidence(param_type, &fixed_arg_node.poly_type);
            inferred_fixed_arg_exprs.push(fixed_arg_node.expr);
        }

        let inferred_rest_arg_expr = if let Some(rest_arg_expr) = rest_arg_expr {
            let tail_type = ty::Ty::List(param_iter.tail_type()).into_poly();
            let wanted_tail_type = ty::subst::inst_ty_selection(&param_select_ctx, &tail_type);
            let rest_arg_node = self.visit_expr(fcx, &wanted_tail_type, rest_arg_expr)?;

            if rest_arg_node.poly_type == ty::Ty::never().into_poly() {
                is_divergent = true;
            }

            ret_select_ctx.add_evidence(&tail_type, &rest_arg_node.poly_type);
            Some(rest_arg_node.expr)
        } else if param_iter.fixed_len() > 0 {
            // We wanted more args!
            return Err(Error::new(
                span,
                ErrorKind::InsufficientArgs(supplied_arg_count, wanted_arity),
            ));
        } else {
            None
        };

        let ret_type = if is_divergent {
            ty::Ty::never().into_poly()
        } else {
            ty::subst::inst_ty_selection(&ret_select_ctx, fun_type.ret())
        };

        // Keep track of the purity from the application
        let app_purity = ty::subst::inst_purity_selection(&ret_select_ctx, fun_type.purity());
        self.unify_app_purity(fcx, &app_purity);

        self.ensure_is_a(span, &ret_type, required_type)?;

        Ok(InferredNode {
            expr: hir::Expr::App(
                span,
                Box::new(hir::App {
                    fun_expr,
                    fixed_arg_exprs: inferred_fixed_arg_exprs,
                    rest_arg_expr: inferred_rest_arg_expr,
                }),
            ),
            poly_type: ret_type,
            type_cond: None,
        })
    }

    fn visit_app(
        &mut self,
        fcx: &mut FunCtx,
        required_type: &ty::Poly,
        span: Span,
        app: hir::App<ty::Decl>,
    ) -> Result<InferredNode> {
        let hir::App {
            fun_expr,
            mut fixed_arg_exprs,
            rest_arg_expr,
        } = app;

        // The only type information we can feed back is that we want a function of a certain
        // purity returning a certain value
        let wanted_purity = match &fcx.purity {
            PurityVarType::Free(_) => {
                // We're inferring the purity; this application can have any purity
                Purity::Impure.into_poly()
            }
            PurityVarType::Known(purity_type) => {
                // We have a specific declared purity
                purity_type.clone()
            }
        };

        let wanted_fun_type = ty::TopFun::new(wanted_purity, required_type.clone()).into_ty_ref();

        let fun_node = self.visit_expr(fcx, &wanted_fun_type, fun_expr)?;
        let revealed_fun_type = fun_node.poly_type;

        match ty::resolve::resolve_poly_ty(self.tvars, &revealed_fun_type).as_ty() {
            ty::Ty::TopFun(_) => Err(Error::new(
                span,
                ErrorKind::TopFunApply(self.str_for_poly(&revealed_fun_type)),
            )),
            ty::Ty::TyPred(test_poly) => {
                let supplied_arg_count = fixed_arg_exprs.len();
                let wanted_arity = WantedArity::new(1, false);

                if supplied_arg_count > 1 {
                    return Err(Error::new(
                        span,
                        ErrorKind::TooManyArgs(supplied_arg_count, wanted_arity),
                    ));
                }

                let subject_expr = fixed_arg_exprs.pop().ok_or_else(|| {
                    Error::new(span, ErrorKind::InsufficientArgs(0, wanted_arity))
                })?;

                let subject_var_id = if let hir::Expr::Ref(_, var_id) = subject_expr {
                    Some(var_id)
                } else {
                    None
                };

                let subject_node = self.visit_expr(fcx, &ty::Ty::Any.into_poly(), subject_expr)?;
                let app_expr = hir::Expr::App(
                    span,
                    Box::new(hir::App {
                        fun_expr: fun_node.expr,
                        fixed_arg_exprs: vec![subject_node.expr],
                        rest_arg_expr: None,
                    }),
                );

                let pred_result =
                    ty::pred::interpret_poly_pred(self.tvars, &subject_node.poly_type, &test_poly);

                use crate::ty::pred::InterpretedPred;
                let pred_result_type = match pred_result {
                    InterpretedPred::Static(result) => ty::Ty::LitBool(result),
                    InterpretedPred::Dynamic(type_if_true, type_if_false) => {
                        if let Some(subject_var_id) = subject_var_id {
                            return Ok(InferredNode::new_cond_type_node(
                                app_expr,
                                subject_var_id,
                                type_if_true,
                                type_if_false,
                            ));
                        }

                        ty::Ty::Bool
                    }
                }.into_poly();

                let poly_type = if subject_node.poly_type == ty::Ty::never().into_poly() {
                    // The subject diverged so we diverged
                    ty::Ty::never().into_poly()
                } else {
                    pred_result_type
                };

                Ok(InferredNode {
                    expr: app_expr,
                    poly_type,
                    type_cond: None,
                })
            }
            ty::Ty::Fun(fun_type) => {
                let fun_app = FunApp {
                    fun_expr: fun_node.expr,
                    fixed_arg_exprs,
                    rest_arg_expr,
                };

                self.visit_fun_app(fcx, required_type, span, fun_type, fun_app)
            }
            _ => panic!("Unexpected type"),
        }
    }

    fn visit_let(
        &mut self,
        fcx: &mut FunCtx,
        required_type: &ty::Poly,
        span: Span,
        hir_let: hir::Let<ty::Decl>,
    ) -> Result<InferredNode> {
        let hir::Let {
            destruc,
            value_expr,
            body_expr,
        } = hir_let;

        let required_destruc_type =
            typeck::destruc::type_for_decl_destruc(self.tvars, &destruc, None);

        // Pre-bind our variables to deal with recursive definitions
        let self_var_id = typeck::destruc::visit_vars(&destruc, |var_id, decl_type| {
            let var_type = match decl_type {
                ty::Decl::Known(poly_type) => VarType::Known(poly_type.clone()),
                ty::Decl::Free => VarType::Recursive,
            };

            self.var_to_type.insert(var_id, var_type);
        });

        let value_node =
            self.visit_expr_with_self_var_id(fcx, &required_destruc_type, value_expr, self_var_id)?;

        let free_ty_offset = self.destruc_value(&destruc, &value_node.poly_type, false);

        let body_node = self.visit_expr(fcx, required_type, body_expr)?;
        let mut inferred_free_types = self.free_ty_polys.drain(free_ty_offset..);

        let poly_type = if value_node.poly_type == ty::Ty::never().into_poly() {
            // Value was divergent
            ty::Ty::never().into_poly()
        } else {
            body_node.poly_type
        };

        Ok(InferredNode {
            expr: hir::Expr::Let(
                span,
                Box::new(hir::Let {
                    destruc: destruc::subst_destruc(&mut inferred_free_types, destruc),
                    value_expr: value_node.expr,
                    body_expr: body_node.expr,
                }),
            ),
            poly_type,
            type_cond: body_node.type_cond,
        })
    }

    fn visit_rust_fun(
        &self,
        required_type: &ty::Poly,
        span: Span,
        rust_fun: Box<rfi::Fun>,
    ) -> Result<InferredNode> {
        // Rust functions have their types validated by the RFI system when they're loaded
        // We just need to make sure we satisfy `required_type` and convert to an `InferredNode`
        let poly_type = ty::Ty::Fun(Box::new(rust_fun.arret_fun_type().clone())).into_poly();
        self.ensure_is_a(span, &poly_type, required_type)?;

        Ok(InferredNode {
            expr: hir::Expr::RustFun(span, rust_fun),
            poly_type,
            type_cond: None,
        })
    }

    fn visit_expr_with_self_var_id(
        &mut self,
        fcx: &mut FunCtx,
        required_type: &ty::Poly,
        expr: hir::Expr<ty::Decl>,
        self_var_id: Option<hir::VarId>,
    ) -> Result<InferredNode> {
        match expr {
            hir::Expr::Lit(datum) => self.visit_lit(required_type, datum),
            hir::Expr::Cond(span, cond) => self.visit_cond(fcx, required_type, span, *cond),
            hir::Expr::Do(exprs) => self.visit_do(fcx, required_type, exprs),
            hir::Expr::Fun(span, fun) => self.visit_fun(required_type, span, *fun, self_var_id),
            hir::Expr::RustFun(span, rust_fun) => {
                self.visit_rust_fun(required_type, span, rust_fun)
            }
            hir::Expr::TyPred(span, test_type) => {
                self.visit_ty_pred(required_type, span, test_type)
            }
            hir::Expr::Let(span, hir_let) => self.visit_let(fcx, required_type, span, *hir_let),
            hir::Expr::Ref(span, var_id) => self.visit_ref(fcx, required_type, span, var_id),
            hir::Expr::App(span, app) => self.visit_app(fcx, required_type, span, *app),
        }
    }

    fn visit_expr(
        &mut self,
        fcx: &mut FunCtx,
        required_type: &ty::Poly,
        expr: hir::Expr<ty::Decl>,
    ) -> Result<InferredNode> {
        self.visit_expr_with_self_var_id(fcx, required_type, expr, None)
    }

    fn destruc_scalar_value(
        &mut self,
        scalar: &destruc::Scalar<ty::Decl>,
        value_type: &ty::Poly,
        is_param: bool,
    ) -> usize {
        let start_offset = self.free_ty_polys.len();

        let free_ty_id = if *scalar.ty() == ty::Decl::Free {
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
        rest: &destruc::Scalar<ty::Decl>,
        value_type_iter: ListIterator<'_, ty::Poly>,
        is_param: bool,
    ) {
        let param_free_ty_id = if *rest.ty() == ty::Decl::Free {
            // Start with member type as a guide
            let member_type = value_type_iter
                .clone()
                .collect_rest(self.tvars)
                .unwrap_or_else(|| ty::Ty::Any.into_poly());

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
                VarType::Known(ty::Ty::List(value_type_iter.tail_type()).into_poly())
            };

            self.var_to_type.insert(var_id, var_type);
        }
    }

    fn destruc_list_value(
        &mut self,
        list: &destruc::List<ty::Decl>,
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
        destruc: &destruc::Destruc<ty::Decl>,
        value_type: &ty::Poly,
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

    fn visit_def(&mut self, hir_def: hir::Def<ty::Decl>) -> Result<hir::Def<ty::Poly>> {
        let hir::Def {
            span,
            destruc,
            value_expr,
        } = hir_def;

        let mut fcx = FunCtx {
            // Module definitions must be pure
            purity: PurityVarType::Known(Purity::Pure.into_poly()),
        };

        // Mark all of our free typed variable as recursive
        let self_var_id = typeck::destruc::visit_vars(&destruc, |var_id, decl_type| {
            if *decl_type == ty::Decl::Free {
                self.var_to_type.insert(var_id, VarType::Recursive);
            }
        });

        let required_type = typeck::destruc::type_for_decl_destruc(self.tvars, &destruc, None);
        let value_node = match self.visit_expr_with_self_var_id(
            &mut fcx,
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

        let free_ty_offset = self.destruc_value(&destruc, &value_node.poly_type, false);
        let mut inferred_free_types = self.free_ty_polys.drain(free_ty_offset..);

        Ok(hir::Def {
            span,
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
        pvars: &[ty::purity::PVar],
        tvars: &[ty::TVar],
        defs: Vec<hir::Def<ty::Decl>>,
    ) -> result::Result<Vec<hir::Def<ty::Poly>>, Vec<Error>> {
        let mut rdcx = RecursiveDefsCtx::new(pvars, tvars, defs, &mut self.var_to_type);
        rdcx.infer_input_defs()?;
        Ok(rdcx.complete_defs)
    }

    pub fn infer_expr(
        &mut self,
        pvars: &[ty::purity::PVar],
        tvars: &[ty::TVar],
        expr: hir::Expr<ty::Decl>,
    ) -> Result<InferredNode> {
        let mut rdcx = RecursiveDefsCtx::new(pvars, tvars, vec![], &mut self.var_to_type);
        let mut fcx = FunCtx {
            purity: PurityVarType::Known(Purity::Impure.into_poly()),
        };

        rdcx.visit_expr(&mut fcx, &ty::Ty::Any.into_poly(), expr)
    }
}

pub fn infer_program(
    pvars: &[ty::purity::PVar],
    tvars: &[ty::TVar],
    defs: Vec<Vec<hir::Def<ty::Decl>>>,
) -> result::Result<Vec<hir::Def<ty::Poly>>, Vec<Error>> {
    let mut var_to_type = HashMap::with_capacity(defs.len());
    let mut complete_defs = vec![];

    for recursive_defs in defs {
        let mut rdcx = RecursiveDefsCtx::new(pvars, tvars, recursive_defs, &mut var_to_type);

        rdcx.infer_input_defs()?;
        complete_defs.append(&mut rdcx.complete_defs);
    }

    Ok(complete_defs)
}

#[cfg(test)]
mod test {
    use super::*;
    use crate::hir::lowering::lowered_expr_for_str;
    use syntax::span::t2s;

    fn type_for_lowered_expr(
        required_type: &ty::Poly,
        lowered_expr: hir::lowering::LoweredTestExpr,
    ) -> Result<ty::Poly> {
        let mut var_to_type = HashMap::new();
        let mut rdcx = RecursiveDefsCtx::new(
            &lowered_expr.pvars,
            &lowered_expr.tvars,
            vec![],
            &mut var_to_type,
        );
        let mut fcx = FunCtx {
            purity: PurityVarType::Known(Purity::Pure.into_poly()),
        };

        rdcx.visit_expr(&mut fcx, required_type, lowered_expr.expr)
            .map(|node| node.poly_type)
    }

    fn assert_type_for_expr(ty_str: &str, expr_str: &str) {
        let lowered_expr = lowered_expr_for_str(expr_str);
        let poly = hir::poly_for_str(ty_str);

        assert_eq!(
            poly,
            type_for_lowered_expr(&ty::Ty::Any.into_poly(), lowered_expr).unwrap()
        );
    }

    fn assert_constrained_type_for_expr(expected_ty_str: &str, expr_str: &str, guide_ty_str: &str) {
        let lowered_expr = lowered_expr_for_str(expr_str);
        let expected_poly = hir::poly_for_str(expected_ty_str);
        let guide_poly = hir::poly_for_str(guide_ty_str);

        assert_eq!(
            expected_poly,
            type_for_lowered_expr(&guide_poly, lowered_expr).unwrap()
        );
    }

    fn assert_type_error(err: &Error, expr_str: &str) {
        let lowered_expr = lowered_expr_for_str(expr_str);

        assert_eq!(
            err,
            &type_for_lowered_expr(&ty::Ty::Any.into_poly(), lowered_expr).unwrap_err()
        )
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
        assert_type_for_expr("(Str -> Str)", "(fn ([x : Str]) x)");

        // We should feed our wanted type in to the function type
        assert_constrained_type_for_expr("(Sym -> true)", "(fn (_) true)", "(Sym -> true)");
        assert_constrained_type_for_expr("(Sym -> Sym)", "(fn (x) x)", "(Sym -> Any))");

        // Function with free types being bound to an incompatible type
        let j = "(let [[f : (Sym -> true)] (fn ([_ : Str]) true)])";
        let t = "                          ^^^^^^^^^^^^^^^^^^^^^  ";
        let err = Error::new(
            t2s(t),
            ErrorKind::IsNotTy("(Str -> true)".into(), "(Sym -> true)".into()),
        );
        assert_type_error(&err, j);

        // Function with a known type being bound to an incompatible type
        let j = "(let [[f : (Sym -> true)] (fn ([_ : Str]) -> true true)])";
        let t = "                          ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^  ";
        let err = Error::new(
            t2s(t),
            ErrorKind::IsNotTy("(Str -> true)".into(), "(Sym -> true)".into()),
        );
        assert_type_error(&err, j);

        let j = "(fn ([x : Str]) -> Sym x)";
        let t = "                       ^ ";
        let err = Error::new(t2s(t), ErrorKind::IsNotTy("Str".into(), "Sym".into()));
        assert_type_error(&err, j);

        // Instantiating a polymorphic function
        // We can't name polymorphic types so we need the (let) hack
        assert_type_for_expr(
            "()",
            "(let [[_ : (Sym -> Sym)] (fn #{T} ([x : T]) -> T x)])",
        );

        let j = "(let [[_ : (Sym -> Str)] (fn #{T} ([x : T]) -> T x)])";
        let t = "                         ^^^^^^^^^^^^^^^^^^^^^^^^^^  ";
        let err = Error::new(
            t2s(t),
            ErrorKind::IsNotTy("#{T} (T -> T)".into(), "(Sym -> Str)".into()),
        );
        assert_type_error(&err, j);
    }

    #[test]
    fn app_types() {
        assert_type_for_expr("'foo", "((fn () 'foo))");

        assert_type_for_expr("true", "((type-predicate 'foo) 'foo)");
        assert_type_for_expr("false", "((type-predicate 'foo) 'false)");

        assert_type_for_expr("Int", "((fn #{A} ([value : A]) -> A value) 1)");
        assert_type_for_expr("'foo", "((fn #{A} ([value : A]) -> A value) '(foo) ...)");

        assert_type_for_expr(
            "(Listof Bool)",
            "((fn #{A} ([rest : A] ...) -> (Listof A) rest) true false)",
        );
    }

    #[test]
    fn recursive_app() {
        assert_type_for_expr(
            "'foo",
            "(let [[recurse : (-> 'foo)] (fn () (recurse))] (recurse))",
        );

        assert_type_for_expr(
            "'foo",
            "(let [recurse (fn ([x : Int]) -> 'foo (recurse x))] (recurse 1))",
        );

        let j = "(let [recurse (fn () (recurse))] (recurse))";
        let t = "                     ^^^^^^^^^             ";
        let err = Error::new(t2s(t), ErrorKind::RecursiveType);
        assert_type_error(&err, j);

        let j = "(let [recurse (fn (x) -> 'foo (recurse x))] (recurse 1))";
        let t = "                              ^^^^^^^^^^^               ";
        let err = Error::new(t2s(t), ErrorKind::RecursiveType);
        assert_type_error(&err, j);

        let j = "(let [recurse (fn ([x : Int]) (recurse x))] (recurse 1))";
        let t = "                              ^^^^^^^^^^^               ";
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
        let err = Error::new(t2s(t), ErrorKind::TooManyArgs(1, wanted_arity));
        assert_type_error(&err, j);
    }

    #[test]
    fn not_enough_args() {
        let j = "((fn (_ _)) 1)";
        let t = "^^^^^^^^^^^^^^";

        let wanted_arity = WantedArity::new(2, false);
        let err = Error::new(t2s(t), ErrorKind::InsufficientArgs(1, wanted_arity));
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
        assert_type_for_expr("(Type? Sym)", "(type-predicate Sym)")
    }
}
