use std;
use std::collections::HashMap;
use std::result;

use hir;
use hir::destruc;
use syntax::datum::Datum;
use syntax::span::Span;
use ty;
use ty::list_iter::ListIterator;
use ty::purity::Purity;
use typeck;
use typeck::error::{Error, ErrorKind};

type Result<T> = result::Result<T, Error>;

struct InferredNode {
    pub expr: hir::Expr<ty::Poly>,
    pub poly_type: ty::Poly,
}

enum OccurrenceTypedNode {
    VarTyCond(hir::Expr<ty::Poly>, hir::VarId, ty::Poly, ty::Poly),
    Other(InferredNode),
}

impl OccurrenceTypedNode {
    fn into_node(self) -> InferredNode {
        match self {
            OccurrenceTypedNode::VarTyCond(expr, _, _, _) => InferredNode {
                expr,
                poly_type: ty::Ty::Bool.into_poly(),
            },
            OccurrenceTypedNode::Other(other) => other,
        }
    }
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
    // non-(def) free type being inferred
    Free(FreeTyId),
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

struct InferCtx<'a> {
    input_defs: Vec<InputDef>,
    complete_defs: Vec<hir::Def<ty::Poly>>,

    pvars: &'a [ty::purity::PVar],
    tvars: &'a [ty::TVar],

    // The inferred types for free types in the order they're encountered
    //
    // Each (def), (let) and (fn) push entries to `free_ty_polys` before they evaluate their body
    // and then pop them off afterwards.
    free_ty_polys: Vec<ty::Poly>,

    var_to_type: HashMap<hir::VarId, VarType>,
}

#[derive(Clone)]
struct FunCtx {
    purity: PurityVarType,
}

fn unit_type() -> ty::Poly {
    ty::Ty::List(ty::List::new(Box::new([]), None)).into_poly()
}

impl<'a> InferCtx<'a> {
    fn new(pvars: &'a [ty::purity::PVar], tvars: &'a [ty::TVar]) -> InferCtx<'a> {
        InferCtx {
            input_defs: vec![],
            complete_defs: vec![],
            pvars,
            tvars,
            free_ty_polys: vec![],
            var_to_type: HashMap::new(),
        }
    }

    fn insert_free_ty(&mut self, initial_type: ty::Poly) -> FreeTyId {
        FreeTyId::new_entry_id(&mut self.free_ty_polys, initial_type)
    }

    fn str_for_poly(&self, poly: &ty::Poly) -> Box<str> {
        hir::str_for_poly(self.pvars, self.tvars, poly).clone()
    }

    /// Ensures `sub_poly` is a subtype of `parent_poly`
    fn ensure_is_a(&self, span: Span, sub_poly: &ty::Poly, parent_poly: &ty::Poly) -> Result<()> {
        if !ty::is_a::poly_is_a(self.tvars, sub_poly, parent_poly).to_bool() {
            Err(Error::new(
                span,
                ErrorKind::IsNotA(self.str_for_poly(&sub_poly), self.str_for_poly(parent_poly)),
            ))
        } else {
            Ok(())
        }
    }

    fn visit_lit(&mut self, required_type: &ty::Poly, datum: Datum) -> Result<InferredNode> {
        let lit_type = ty::datum::poly_for_datum(&datum);
        self.ensure_is_a(datum.span(), &lit_type, required_type)?;

        Ok(InferredNode {
            expr: hir::Expr::Lit(datum),
            poly_type: lit_type,
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
        let occurrence_test_node = match test_expr {
            hir::Expr::Ref(span, var_id) => {
                let ref_node = self.visit_ref(fcx, test_required_type, span, var_id)?;

                // If we're testing a variable directly we know its type in the branches
                OccurrenceTypedNode::VarTyCond(
                    ref_node.expr,
                    var_id,
                    ty::Ty::LitBool(true).into_poly(),
                    ty::Ty::LitBool(false).into_poly(),
                )
            }
            hir::Expr::App(span, app) => {
                // Visit the application directly so we don't lose the OccurrenceTypedNode
                self.visit_app(fcx, test_required_type, span, *app)?
            }
            other_expr => self
                .visit_expr(fcx, test_required_type, other_expr)
                .map(OccurrenceTypedNode::Other)?,
        };

        let (test_node, true_node, false_node) = match occurrence_test_node {
            OccurrenceTypedNode::VarTyCond(expr, var_id, true_type, false_type) => {
                // Patch our occurrence types in to the `var_to_type` and restore it after. We
                // avoid `?`ing our results until the end to make sure the original types are
                // properly restored.
                let original_var_type = std::mem::replace(
                    self.var_to_type.get_mut(&var_id).unwrap(),
                    VarType::Known(true_type),
                );
                let true_result = self.visit_expr(fcx, required_type, true_expr);

                self.var_to_type.insert(var_id, VarType::Known(false_type));
                let false_result = self.visit_expr(fcx, required_type, false_expr);

                self.var_to_type.insert(var_id, original_var_type);
                (
                    InferredNode {
                        expr,
                        poly_type: ty::Ty::Bool.into_poly(),
                    },
                    true_result?,
                    false_result?,
                )
            }
            OccurrenceTypedNode::Other(other) => match other.poly_type {
                ty::Poly::Fixed(ty::Ty::LitBool(true)) => {
                    let true_node = self.visit_expr(fcx, required_type, true_expr)?;
                    self.visit_expr(fcx, &ty::Ty::Any.into_poly(), false_expr)?;
                    return Ok(true_node);
                }
                ty::Poly::Fixed(ty::Ty::LitBool(false)) => {
                    self.visit_expr(fcx, &ty::Ty::Any.into_poly(), true_expr)?;
                    let false_node = self.visit_expr(fcx, required_type, false_expr)?;
                    return Ok(false_node);
                }
                _ => {
                    let true_node = self.visit_expr(fcx, required_type, true_expr)?;
                    let false_node = self.visit_expr(fcx, required_type, false_expr)?;
                    (other, true_node, false_node)
                }
            },
        };

        let unified_type =
            ty::unify::poly_unify_to_poly(self.tvars, &true_node.poly_type, &false_node.poly_type);

        Ok(InferredNode {
            expr: hir::Expr::Cond(
                span,
                Box::new(hir::Cond {
                    test_expr: test_node.expr,
                    true_expr: true_node.expr,
                    false_expr: false_node.expr,
                }),
            ),
            poly_type: unified_type,
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
        free_ty_id: FreeTyId,
    ) -> Result<ty::Poly> {
        let current_type = &self.free_ty_polys[free_ty_id.to_usize()];

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

    fn visit_ref(
        &mut self,
        fcx: &FunCtx,
        required_type: &ty::Poly,
        span: Span,
        var_id: hir::VarId,
    ) -> Result<InferredNode> {
        let ref_expr = hir::Expr::Ref(span, var_id);

        let pending_def_id = match self.var_to_type[&var_id] {
            VarType::Pending(def_id) => def_id,
            VarType::Recursive => return Err(Error::new(span, ErrorKind::RecursiveType)),
            VarType::Known(ref known_type) => {
                self.ensure_is_a(span, known_type, required_type)?;

                return Ok(InferredNode {
                    expr: ref_expr,
                    poly_type: known_type.clone(),
                });
            }
            VarType::Free(free_ty_id) => {
                let new_free_type = self.type_for_free_ref(required_type, span, free_ty_id)?;

                // Update the free var's type
                self.free_ty_polys[free_ty_id.to_usize()] = new_free_type.clone();

                return Ok(InferredNode {
                    expr: ref_expr,
                    poly_type: new_free_type,
                });
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
                poly_type: unit_type(),
            });
        };

        let mut inferred_exprs = exprs
            .into_iter()
            .map(|non_terminal_expr| {
                // The type of this expression doesn't matter; its value is discarded
                self.visit_expr(fcx, &ty::Ty::Any.into_poly(), non_terminal_expr)
                    .map(|node| node.expr)
            })
            .collect::<Result<Vec<hir::Expr<ty::Poly>>>>()?;

        let terminal_node = self.visit_expr(fcx, required_type, terminal_expr)?;
        inferred_exprs.push(terminal_node.expr);

        Ok(InferredNode {
            expr: hir::Expr::Do(inferred_exprs),
            poly_type: terminal_node.poly_type,
        })
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

        // TODO: Union types
        let required_fun_type = match required_type {
            ty::Poly::Fixed(ty::Ty::Fun(fun)) => Some(fun),
            _ => None,
        };

        let required_top_fun_type = required_fun_type
            .map(|fun_type| fun_type.top_fun())
            .or_else(|| match required_type {
                ty::Poly::Fixed(ty::Ty::TopFun(top_fun)) => Some(top_fun),
                _ => None,
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
            span,
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

        let mut inferred_free_types = self.free_ty_polys.split_off(free_ty_offset);

        let revealed_param_destruc =
            destruc::subst_list_destruc(&mut inferred_free_types, decl_fun.params);
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
        let mut param_select_ctx = ty::select::SelectContext::new(
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

        // TODO: Better arity messages
        let supplied_arg_count = fixed_arg_exprs.len();

        let mut inferred_fixed_arg_exprs = vec![];
        for fixed_arg_expr in fixed_arg_exprs {
            let param_type = param_iter.next().ok_or_else(|| {
                Error::new(
                    span,
                    ErrorKind::TooManyArgs(supplied_arg_count, fun_type.params().fixed().len()),
                )
            })?;

            let wanted_arg_type = ty::subst::inst_ty_selection(&param_select_ctx, param_type);
            let fixed_arg_node = self.visit_expr(fcx, &wanted_arg_type, fixed_arg_expr)?;

            ret_select_ctx.add_evidence(param_type, &fixed_arg_node.poly_type);
            inferred_fixed_arg_exprs.push(fixed_arg_node.expr);
        }

        let inferred_rest_arg_expr = if let Some(rest_arg_expr) = rest_arg_expr {
            let tail_type = ty::Ty::List(param_iter.tail_type()).into_poly();
            let wanted_tail_type = ty::subst::inst_ty_selection(&param_select_ctx, &tail_type);
            let rest_arg_node = self.visit_expr(fcx, &wanted_tail_type, rest_arg_expr)?;

            ret_select_ctx.add_evidence(&tail_type, &rest_arg_node.poly_type);
            Some(rest_arg_node.expr)
        } else if param_iter.next().is_some() && fun_type.params().rest().is_none() {
            // We wanted more args!
            return Err(Error::new(
                span,
                ErrorKind::InsufficientArgs(supplied_arg_count, fun_type.params().fixed().len()),
            ));
        } else {
            None
        };

        let ret_type = ty::subst::inst_ty_selection(&ret_select_ctx, fun_type.ret());

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
        })
    }

    fn visit_app(
        &mut self,
        fcx: &mut FunCtx,
        required_type: &ty::Poly,
        span: Span,
        app: hir::App<ty::Decl>,
    ) -> Result<OccurrenceTypedNode> {
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
                // TODO: Arity
                let subject_expr = fixed_arg_exprs.pop().unwrap();
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

                use ty::pred::InterpretedPred;
                let pred_result_type = match pred_result {
                    InterpretedPred::Static(result) => ty::Ty::LitBool(result),
                    InterpretedPred::Dynamic(true_type, false_type) => {
                        if let Some(subject_var_id) = subject_var_id {
                            return Ok(OccurrenceTypedNode::VarTyCond(
                                app_expr,
                                subject_var_id,
                                true_type,
                                false_type,
                            ));
                        }

                        ty::Ty::Bool
                    }
                }.into_poly();

                Ok(OccurrenceTypedNode::Other(InferredNode {
                    expr: app_expr,
                    poly_type: pred_result_type,
                }))
            }
            ty::Ty::Fun(fun_type) => {
                let fun_app = FunApp {
                    fun_expr: fun_node.expr,
                    fixed_arg_exprs,
                    rest_arg_expr,
                };

                self.visit_fun_app(fcx, required_type, span, fun_type, fun_app)
                    .map(OccurrenceTypedNode::Other)
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

        let value_span = value_expr.span().unwrap_or(span);
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

        let free_ty_offset = self.destruc_value(
            &destruc,
            value_span,
            &value_node.poly_type,
            // We know the exact type of these variables; do not infer further even if their type
            // wasn't declared
            false,
        );

        let body_node = self.visit_expr(fcx, required_type, body_expr)?;
        let mut inferred_free_types = self.free_ty_polys.split_off(free_ty_offset);

        Ok(InferredNode {
            expr: hir::Expr::Let(
                span,
                Box::new(hir::Let {
                    destruc: destruc::subst_destruc(&mut inferred_free_types, destruc),
                    value_expr: value_node.expr,
                    body_expr: body_node.expr,
                }),
            ),
            poly_type: body_node.poly_type,
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
            hir::Expr::TyPred(span, test_type) => {
                self.visit_ty_pred(required_type, span, test_type)
            }
            hir::Expr::Let(span, hir_let) => self.visit_let(fcx, required_type, span, *hir_let),
            hir::Expr::Ref(span, var_id) => self.visit_ref(fcx, required_type, span, var_id),
            hir::Expr::App(span, app) => self
                .visit_app(fcx, required_type, span, *app)
                .map(|app_node| app_node.into_node()),
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
        can_refine: bool,
    ) -> usize {
        let start_offset = self.free_ty_polys.len();

        let free_ty_id = if *scalar.ty() == ty::Decl::Free {
            Some(self.insert_free_ty(value_type.clone()))
        } else {
            None
        };

        if let Some(var_id) = *scalar.var_id() {
            let var_type = if let (Some(free_ty_id), true) = (free_ty_id, can_refine) {
                VarType::Free(free_ty_id)
            } else {
                VarType::Known(value_type.clone())
            };

            self.var_to_type.insert(var_id, var_type);
        }

        start_offset
    }

    fn destruc_list_value(
        &mut self,
        list: &destruc::List<ty::Decl>,
        value_span: Span,
        mut value_type_iter: ListIterator<ty::Poly>,
        can_refine: bool,
    ) -> usize {
        let start_offset = self.free_ty_polys.len();

        for fixed_destruc in list.fixed() {
            let member_type = value_type_iter
                .next()
                .expect("Destructured value with unexpected type");

            self.destruc_value(fixed_destruc, value_span, member_type, can_refine);
        }

        if let Some(scalar) = list.rest() {
            let tail_type = ty::Ty::List(value_type_iter.tail_type()).into_poly();
            self.destruc_scalar_value(scalar, &tail_type, can_refine);
        }

        start_offset
    }

    fn destruc_value(
        &mut self,
        destruc: &destruc::Destruc<ty::Decl>,
        value_span: Span,
        value_type: &ty::Poly,
        can_refine: bool,
    ) -> usize {
        match destruc {
            destruc::Destruc::Scalar(_, scalar) => {
                self.destruc_scalar_value(scalar, value_type, can_refine)
            }
            destruc::Destruc::List(_, list) => {
                let value_type_iter = ListIterator::try_new_from_ty_ref(value_type)
                    .expect("Tried to destruc non-list");
                self.destruc_list_value(list, value_span, value_type_iter, can_refine)
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
        let value_span = value_expr.span().unwrap_or(span);
        let value_node =
            self.visit_expr_with_self_var_id(&mut fcx, &required_type, value_expr, self_var_id)?;

        let free_ty_offset = self.destruc_value(
            &destruc,
            value_span,
            &value_node.poly_type,
            // We know the exact type of these variables; do not infer further even if their type
            // wasn't declared
            false,
        );

        let mut inferred_free_types = self.free_ty_polys.split_off(free_ty_offset);
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

    fn infer_program(
        mut self,
        defs: Vec<hir::Def<ty::Decl>>,
    ) -> result::Result<Vec<hir::Def<ty::Poly>>, Vec<Error>> {
        // First, visit all definitions to bind their variables

        // We do this in reverse order because we infer our defs in reverse order. This doesn't
        // stricly matter but it should require less recursive resolution in the forward direction.
        for hir_def in defs.into_iter().rev() {
            let def_id = InputDefId::new(self.input_defs.len());

            typeck::destruc::visit_vars(&hir_def.destruc, |var_id, decl_type| {
                let var_type = match decl_type {
                    ty::Decl::Known(poly_type) => VarType::Known(poly_type.clone()),
                    ty::Decl::Free => {
                        // Record the definition ID so we can deal with forward type references
                        VarType::Pending(def_id)
                    }
                };

                self.var_to_type.insert(var_id, var_type);
            });

            self.input_defs.push(InputDef::Pending(hir_def));
        }

        let mut errs = vec![];
        while let Some(def_state) = self.input_defs.pop() {
            match def_state {
                InputDef::Pending(def) => match self.visit_def(def) {
                    Ok(inferred_def) => {
                        self.complete_defs.push(inferred_def);
                    }
                    Err(err) => {
                        errs.push(err);
                    }
                },
                InputDef::Complete => {}
            }
        }

        if errs.is_empty() {
            Ok(self.complete_defs)
        } else {
            Err(errs)
        }
    }
}

pub fn infer_program(
    pvars: &[ty::purity::PVar],
    tvars: &[ty::TVar],
    defs: Vec<hir::Def<ty::Decl>>,
) -> result::Result<Vec<hir::Def<ty::Poly>>, Vec<Error>> {
    let icx = InferCtx::new(pvars, tvars);
    icx.infer_program(defs)
}

#[cfg(test)]
mod test {
    use super::*;
    use hir::lowering::lowered_expr_for_str;
    use syntax::span::t2s;

    fn type_for_lowered_expr(
        required_type: &ty::Poly,
        lowered_expr: hir::lowering::LoweredTestExpr,
    ) -> Result<ty::Poly> {
        let mut icx = InferCtx::new(&lowered_expr.pvars, &lowered_expr.tvars);
        let mut fcx = FunCtx {
            purity: PurityVarType::Known(Purity::Pure.into_poly()),
        };

        icx.visit_expr(&mut fcx, required_type, lowered_expr.expr)
            .map(|node| node.poly_type)
    }

    fn assert_type_for_expr(ty_str: &str, expr_str: &str) {
        let lowered_expr = lowered_expr_for_str(expr_str).unwrap();
        let poly = hir::poly_for_str(ty_str).unwrap();

        assert_eq!(
            poly,
            type_for_lowered_expr(&ty::Ty::Any.into_poly(), lowered_expr).unwrap()
        );
    }

    fn assert_constrained_type_for_expr(expected_ty_str: &str, expr_str: &str, guide_ty_str: &str) {
        let lowered_expr = lowered_expr_for_str(expr_str).unwrap();
        let expected_poly = hir::poly_for_str(expected_ty_str).unwrap();
        let guide_poly = hir::poly_for_str(guide_ty_str).unwrap();

        assert_eq!(
            expected_poly,
            type_for_lowered_expr(&guide_poly, lowered_expr).unwrap()
        );
    }

    fn assert_type_error(err: &Error, expr_str: &str) {
        let lowered_expr = lowered_expr_for_str(expr_str).unwrap();

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
        assert_type_for_expr("(String -> String)", "(fn ([x : String]) x)");

        // We should feed our wanted type in to the function type
        assert_constrained_type_for_expr("(Symbol -> true)", "(fn (_) true)", "(Symbol -> true)");
        assert_constrained_type_for_expr("(Symbol -> Symbol)", "(fn (x) x)", "(Symbol -> Any))");

        // Function with free types being bound to an incompatible type
        let j = "(let [[f : (Symbol -> true)] (fn ([_ : String]) true)])";
        let t = "                             ^^^^^^^^^^^^^^^^^^^^^^^^  ";
        let err = Error::new(
            t2s(t),
            ErrorKind::IsNotA("(String -> true)".into(), "(Symbol -> true)".into()),
        );
        assert_type_error(&err, j);

        // Function with a known type being bound to an incompatible type
        let j = "(let [[f : (Symbol -> true)] (fn ([_ : String]) -> true true)])";
        let t = "                             ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^  ";
        let err = Error::new(
            t2s(t),
            ErrorKind::IsNotA("(String -> true)".into(), "(Symbol -> true)".into()),
        );
        assert_type_error(&err, j);

        let j = "(fn ([x : String]) -> Symbol x)";
        let t = "                             ^ ";
        let err = Error::new(t2s(t), ErrorKind::IsNotA("String".into(), "Symbol".into()));
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
            // TODO: This error is technically correct but a bit inscrutable
            ErrorKind::IsNotA("(->! false)".into(), "(... -> Bool)".into()),
        );
        assert_type_error(&err, j);
    }

    #[test]
    fn too_many_args() {
        let j = "((fn ()) 1)";
        let t = "^^^^^^^^^^^";

        let err = Error::new(t2s(t), ErrorKind::TooManyArgs(1, 0));
        assert_type_error(&err, j);
    }

    #[test]
    fn not_enough_args() {
        let j = "((fn (_ _)) 1)";
        let t = "^^^^^^^^^^^^^^";

        let err = Error::new(t2s(t), ErrorKind::InsufficientArgs(1, 2));
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
        assert_type_for_expr("(Type? Symbol)", "(type-predicate Symbol)")
    }

    #[test]
    fn occurrence_typing() {
        // The type of x should be known in the branches
        let j = "(fn ([x : Bool])
                  (if x
                    (let [[_ : true] x])
                    (let [[_ : false] x])))";

        assert_type_for_expr("(Bool -> ())", j);

        let j = "(fn ([x : (RawU Symbol String)]) -> ()
                  (if ((type-predicate Symbol) x)
                    (let [[_ : Symbol] x])
                    (let [[_ : String] x])))";

        assert_type_for_expr("((RawU Symbol String) -> ())", j);
    }
}
