use std;
use std::collections::HashMap;
use std::result;

use hir;
use hir::destruc;
use syntax::datum::Datum;
use syntax::span::Span;
use ty;
use ty::purity::Purity;
use typeck;
use typeck::error::{Error, ErrorKind};
use typeck::list_type;
use typeck::list_type::ListTypeIterator;

type Result<T> = result::Result<T, Error>;

struct InferredNode {
    pub expr: hir::Expr<ty::Poly>,
    pub poly_type: ty::Poly,
}

new_indexing_id_type!(FreeTyId, u32);
new_indexing_id_type!(DefId, u32);

/// Partially inferred function application
///
/// The function has been inferred while the arguments have not
struct FunApp {
    fun_expr: hir::Expr<ty::Poly>,
    fixed_arg_exprs: Vec<hir::Expr<ty::Decl>>,
    rest_arg_expr: Option<Box<hir::Expr<ty::Decl>>>,
}

enum VarType {
    // Introduced a definition that has yet to be processed
    Pending(DefId),
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

#[allow(large_enum_variant)]
enum DefState {
    Pending(hir::Def<ty::Decl>),
    Complete,
}

struct InferCtx<'a> {
    def_states: Vec<DefState>,
    pvars: &'a [ty::purity::PVar],
    tvars: &'a [ty::TVar],

    // The inferred types for free types in the order they're encountered
    //
    // Each (def), (let) and (fn) push entries to `free_ty_polys` before they evaluate their body
    // and then pop them off afterwards.
    free_ty_polys: Vec<ty::Poly>,

    var_to_type: HashMap<hir::VarId, VarType>,
}

struct SubtreeCtx {
    fun_purity: PurityVarType,
}

fn unit_type() -> ty::Poly {
    ty::Ty::Nil.into_poly()
}

impl<'a> InferCtx<'a> {
    fn new(pvars: &'a [ty::purity::PVar], tvars: &'a [ty::TVar]) -> InferCtx<'a> {
        InferCtx {
            def_states: vec![],
            pvars,
            tvars,
            free_ty_polys: vec![],
            var_to_type: HashMap::new(),
        }
    }

    fn insert_free_ty(&mut self, initial_type: ty::Poly) -> FreeTyId {
        FreeTyId::new_entry_id(&mut self.free_ty_polys, initial_type)
    }

    fn str_for_poly(&self, poly: &ty::Poly) -> String {
        hir::str_for_poly(self.pvars, self.tvars, poly)
    }

    fn new_union_conflict_error(&self, span: Span, unify_err: &ty::unify::PolyError) -> Error {
        match unify_err {
            ty::unify::PolyError::TyConflict(left, right) => Error::new(
                span,
                ErrorKind::PolyUnionConflict(self.str_for_poly(&left), self.str_for_poly(&right)),
            ),
            ty::unify::PolyError::PurityConflict(purity1, purity2) => Error::new(
                span,
                ErrorKind::PolyUnionConflict(
                    hir::str_for_purity(self.pvars, &purity1),
                    hir::str_for_purity(self.pvars, &purity2),
                ),
            ),
        }
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
        scx: &mut SubtreeCtx,
        required_type: &ty::Poly,
        span: Span,
        cond: hir::Cond<ty::Decl>,
    ) -> Result<InferredNode> {
        let hir::Cond {
            test_expr,
            true_expr,
            false_expr,
        } = cond;

        let test_node = self.visit_expr(scx, &ty::Ty::Bool.into_poly(), *test_expr)?;
        let true_node = self.visit_expr(scx, required_type, *true_expr)?;
        let false_node = self.visit_expr(scx, required_type, *false_expr)?;

        match test_node.poly_type {
            ty::Poly::Fixed(ty::Ty::LitBool(true)) => Ok(true_node),
            ty::Poly::Fixed(ty::Ty::LitBool(false)) => Ok(false_node),
            _ => ty::unify::poly_unify_to_poly(
                self.tvars,
                &true_node.poly_type,
                &false_node.poly_type,
            ).map(|unified_type| InferredNode {
                expr: hir::Expr::Cond(
                    span,
                    hir::Cond {
                        test_expr: Box::new(test_node.expr),
                        true_expr: Box::new(true_node.expr),
                        false_expr: Box::new(false_node.expr),
                    },
                ),
                poly_type: unified_type,
            })
                .map_err(|unify_err| self.new_union_conflict_error(span, &unify_err)),
        }
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

    fn unify_app_purity(
        &self,
        scx: &mut SubtreeCtx,
        span: Span,
        app_purity: &ty::purity::Poly,
    ) -> Result<()> {
        if let PurityVarType::Free(ref mut free_purity) = scx.fun_purity {
            match ty::unify::poly_unify_purity(free_purity, &app_purity) {
                Ok(unified_purity) => {
                    *free_purity = unified_purity;
                }
                Err(unify_err) => {
                    return Err(self.new_union_conflict_error(span, &unify_err));
                }
            }
        };

        Ok(())
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
        let common_type = ty::intersect::poly_intersect(self.tvars, required_type, current_type)
            .map_err(|_| {
                Error::new(
                    span,
                    ErrorKind::VarHasEmptyType(
                        self.str_for_poly(required_type),
                        self.str_for_poly(current_type),
                    ),
                )
            })?;

        Ok(common_type)
    }

    fn visit_ref(
        &mut self,
        required_type: &ty::Poly,
        span: Span,
        var_id: hir::VarId,
    ) -> Result<InferredNode> {
        let pending_def_id = match self.var_to_type[&var_id] {
            VarType::Pending(def_id) => def_id,
            VarType::Recursive => return Err(Error::new(span, ErrorKind::RecursiveType)),
            VarType::Known(ref known_type) => {
                self.ensure_is_a(span, known_type, required_type)?;

                return Ok(InferredNode {
                    expr: hir::Expr::Ref(span, var_id),
                    poly_type: known_type.clone(),
                });
            }
            VarType::Free(free_ty_id) => {
                let new_free_type = self.type_for_free_ref(required_type, span, free_ty_id)?;

                // Update the free var's type
                self.free_ty_polys[free_ty_id.to_usize()] = new_free_type.clone();

                return Ok(InferredNode {
                    expr: hir::Expr::Ref(span, var_id),
                    poly_type: new_free_type,
                });
            }
        };

        self.visit_def_id(pending_def_id)?;
        // This assumes `visit_def_id` has populated our variables now
        self.visit_ref(required_type, span, var_id)
    }

    fn visit_do(
        &mut self,
        scx: &mut SubtreeCtx,
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
                self.visit_expr(scx, &ty::Ty::Any.into_poly(), non_terminal_expr)
                    .map(|node| node.expr)
            })
            .collect::<Result<Vec<hir::Expr<ty::Poly>>>>()?;

        let terminal_node = self.visit_expr(scx, required_type, terminal_expr)?;
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
    ) -> Result<InferredNode> {
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

        let initial_param_type: ty::Params<ty::Poly> = typeck::destruc::type_for_decl_list_destruc(
            self.tvars,
            &decl_fun.params,
            // Use the required type as a guide for any free types in the parameter list
            required_fun_type.map(|fun| list_type::ParamsIterator::new(fun.params())),
        );

        // Bind all of our parameter variables
        let free_ty_offset = self.destruc_list_value(
            &decl_fun.params,
            span,
            list_type::ParamsIterator::new(&initial_param_type),
            // If a parameter has a free decl type then we can refine the type
            true,
        )?;

        // Use the declared return type if possible
        let wanted_ret_type = decl_fun.ret_ty.try_to_poly().unwrap_or_else(|| {
            if let Some(ref required_top_fun_type) = required_top_fun_type {
                // Fall back to the backwards type
                required_top_fun_type.ret().clone()
            } else {
                // Use Any as a last resort
                ty::Ty::Any.into_poly()
            }
        });

        let fun_purity_var = decl_fun
            .purity
            .try_to_poly()
            .map(|poly_purity| {
                // This function has a declared purity
                PurityVarType::Known(poly_purity)
            })
            // Functions start pure until proven otherwise
            .unwrap_or_else(|| PurityVarType::Free(Purity::Pure.into_poly()));

        let mut fun_scx = SubtreeCtx {
            fun_purity: fun_purity_var,
        };

        let body_node = self.visit_expr(&mut fun_scx, &wanted_ret_type, *decl_fun.body_expr)?;
        let actual_ret_type = body_node.poly_type;
        let actual_purity = fun_scx.fun_purity.into_poly();

        let mut inferred_free_types = self.free_ty_polys.split_off(free_ty_offset);

        let actual_param_destruc =
            destruc::subst_list_destruc(&mut inferred_free_types, decl_fun.params);
        let actual_param_type = typeck::destruc::type_for_poly_list_destruc(&actual_param_destruc);

        let found_type = ty::Fun::new(
            decl_fun.pvar_ids.clone(),
            decl_fun.tvar_ids.clone(),
            ty::TopFun::new(actual_purity.clone(), actual_ret_type.clone()),
            actual_param_type,
        ).into_ref();

        let found_fun = hir::Fun::<ty::Poly> {
            pvar_ids: decl_fun.pvar_ids.clone(),
            tvar_ids: decl_fun.tvar_ids.clone(),
            purity: actual_purity,
            params: actual_param_destruc,
            ret_ty: actual_ret_type,
            body_expr: Box::new(body_node.expr),
        };

        self.ensure_is_a(span, &found_type, required_type)?;

        Ok(InferredNode {
            expr: hir::Expr::Fun(span, found_fun),
            poly_type: found_type,
        })
    }

    fn visit_fun_app(
        &mut self,
        scx: &mut SubtreeCtx,
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
        let mut param_iter = list_type::ParamsIterator::new(fun_type.params());

        // TODO: Better arity messages
        let supplied_arg_count = fixed_arg_exprs.len();

        let mut inferred_fixed_arg_exprs = vec![];
        for fixed_arg_expr in fixed_arg_exprs {
            let param_type = if let Some(param_type) = param_iter.next().expect("TODO") {
                param_type
            } else {
                return Err(Error::new(
                    span,
                    ErrorKind::TooManyArgs(supplied_arg_count, fun_type.params().fixed().len()),
                ));
            };

            let wanted_arg_type = ty::subst::inst_ty_selection(&param_select_ctx, param_type);
            let fixed_arg_node = self.visit_expr(scx, &wanted_arg_type, fixed_arg_expr)?;

            ret_select_ctx.add_evidence(param_type, &fixed_arg_node.poly_type);
            inferred_fixed_arg_exprs.push(fixed_arg_node.expr);
        }

        let inferred_rest_arg_expr = if let Some(rest_arg_expr) = rest_arg_expr {
            let tail_type = param_iter.tail_type();
            let wanted_tail_type = ty::subst::inst_ty_selection(&param_select_ctx, &tail_type);
            let rest_arg_node = self.visit_expr(scx, &wanted_tail_type, *rest_arg_expr)?;

            ret_select_ctx.add_evidence(&tail_type, &rest_arg_node.poly_type);
            Some(Box::new(rest_arg_node.expr))
        } else if param_iter.next().expect("TODO") != None && fun_type.params().rest().is_none() {
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
        self.unify_app_purity(scx, span, &app_purity)?;

        self.ensure_is_a(span, &ret_type, required_type)?;

        Ok(InferredNode {
            expr: hir::Expr::App(
                span,
                hir::App {
                    fun_expr: Box::new(fun_expr),
                    fixed_arg_exprs: inferred_fixed_arg_exprs,
                    rest_arg_expr: inferred_rest_arg_expr,
                },
            ),
            poly_type: ret_type,
        })
    }

    fn visit_app(
        &mut self,
        scx: &mut SubtreeCtx,
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
        let wanted_purity = match scx.fun_purity.clone() {
            PurityVarType::Free(_) => {
                // We're inferring the purity; this application can have any purity
                Purity::Impure.into_poly()
            }
            PurityVarType::Known(purity_type) => {
                // We have a specific declared purity
                purity_type.clone()
            }
        };

        let wanted_fun_type = ty::TopFun::new(wanted_purity, required_type.clone()).into_ref();

        let fun_node = self.visit_expr(scx, &wanted_fun_type, *fun_expr)?;
        let actual_fun_type = fun_node.poly_type;

        match ty::resolve::resolve_poly_ty(self.tvars, &actual_fun_type).as_ty() {
            ty::Ty::TopFun(_) => Err(Error::new(
                span,
                ErrorKind::TopFunApply(self.str_for_poly(&actual_fun_type)),
            )),
            ty::Ty::TyPred(subject_poly) => {
                // TODO: Arity
                let test_node = self.visit_expr(
                    scx,
                    &ty::Ty::Any.into_poly(),
                    fixed_arg_exprs.pop().unwrap(),
                )?;

                let pred_result =
                    ty::pred::interpret_poly_pred(self.tvars, &subject_poly, &test_node.poly_type)
                        .map_err(|ty::pred::Error::TypeErased(subject, testing)| {
                            Error::new(
                                span,
                                ErrorKind::PredTypeErased(
                                    self.str_for_poly(&subject),
                                    self.str_for_poly(&testing),
                                ),
                            )
                        })?;

                use ty::pred::InterpretedPred;
                let pred_result_type = match pred_result {
                    InterpretedPred::StaticTrue => ty::Ty::LitBool(true),
                    InterpretedPred::StaticFalse => ty::Ty::LitBool(false),
                    InterpretedPred::Dynamic(_, _) => {
                        // TODO: Occurence typing
                        ty::Ty::Bool
                    }
                }.into_poly();

                Ok(InferredNode {
                    expr: hir::Expr::App(
                        span,
                        hir::App {
                            fun_expr: Box::new(fun_node.expr),
                            fixed_arg_exprs: vec![test_node.expr],
                            rest_arg_expr: None,
                        },
                    ),
                    poly_type: pred_result_type,
                })
            }
            ty::Ty::Fun(fun_type) => {
                let fun_app = FunApp {
                    fun_expr: fun_node.expr,
                    fixed_arg_exprs,
                    rest_arg_expr,
                };

                self.visit_fun_app(scx, required_type, span, fun_type, fun_app)
            }
            _ => panic!("Unexpected type"),
        }
    }

    fn visit_let(
        &mut self,
        scx: &mut SubtreeCtx,
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
        let value_node = self.visit_expr(scx, &required_destruc_type, *value_expr)?;

        let free_ty_offset = self.destruc_value(
            &destruc,
            value_span,
            &value_node.poly_type,
            // We know the exact type of these variables; do not infer further even if their type
            // wasn't declared
            false,
        )?;

        let body_node = self.visit_expr(scx, required_type, *body_expr)?;
        let mut inferred_free_types = self.free_ty_polys.split_off(free_ty_offset);

        Ok(InferredNode {
            expr: hir::Expr::Let(
                span,
                hir::Let {
                    destruc: destruc::subst_destruc(&mut inferred_free_types, destruc),
                    value_expr: Box::new(value_node.expr),
                    body_expr: Box::new(body_node.expr),
                },
            ),
            poly_type: body_node.poly_type,
        })
    }

    fn visit_expr(
        &mut self,
        scx: &mut SubtreeCtx,
        required_type: &ty::Poly,
        expr: hir::Expr<ty::Decl>,
    ) -> Result<InferredNode> {
        match expr {
            hir::Expr::Lit(datum) => self.visit_lit(required_type, datum),
            hir::Expr::Cond(span, cond) => self.visit_cond(scx, required_type, span, cond),
            hir::Expr::Do(exprs) => self.visit_do(scx, required_type, exprs),
            hir::Expr::Fun(span, fun) => self.visit_fun(required_type, span, fun),
            hir::Expr::TyPred(span, test_type) => {
                self.visit_ty_pred(required_type, span, test_type)
            }
            hir::Expr::Let(span, hir_let) => self.visit_let(scx, required_type, span, hir_let),
            hir::Expr::Ref(span, var_id) => self.visit_ref(required_type, span, var_id),
            hir::Expr::App(span, app) => self.visit_app(scx, required_type, span, app),
        }
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

    fn destruc_list_value<I>(
        &mut self,
        list: &destruc::List<ty::Decl>,
        value_span: Span,
        mut value_type_iter: I,
        can_refine: bool,
    ) -> Result<usize>
    where
        I: ListTypeIterator,
    {
        let start_offset = self.free_ty_polys.len();

        for fixed_destruc in list.fixed() {
            let member_type = match value_type_iter.next() {
                Ok(Some(member_type)) => member_type,
                Err(list_type::Error::UnifyError(unify_err)) => {
                    return Err(self.new_union_conflict_error(value_span, &unify_err))
                }
                _ => panic!("Destructured value with unexpected type"),
            };

            self.destruc_value(fixed_destruc, value_span, member_type, can_refine)?;
        }

        if let Some(scalar) = list.rest() {
            self.destruc_scalar_value(scalar, &value_type_iter.tail_type(), can_refine);
        }

        Ok(start_offset)
    }

    fn destruc_value(
        &mut self,
        destruc: &destruc::Destruc<ty::Decl>,
        value_span: Span,
        value_type: &ty::Poly,
        can_refine: bool,
    ) -> Result<usize> {
        match destruc {
            destruc::Destruc::Scalar(_, scalar) => {
                Ok(self.destruc_scalar_value(scalar, value_type, can_refine))
            }
            destruc::Destruc::List(_, list) => {
                let value_type_iter = list_type::PolyIterator::new(value_type);
                self.destruc_list_value(list, value_span, value_type_iter, can_refine)
            }
        }
    }

    fn visit_def(&mut self, hir_def: hir::Def<ty::Decl>) -> Result<()> {
        let hir::Def {
            span,
            destruc,
            value_expr,
        } = hir_def;

        let mut scx = SubtreeCtx {
            // Module definitions must be pure
            fun_purity: PurityVarType::Known(Purity::Pure.into_poly()),
        };

        // Mark all of our free typed variable as recursive
        typeck::destruc::visit_vars(&destruc, |var_id, decl_type| {
            if *decl_type == ty::Decl::Free {
                self.var_to_type.insert(var_id, VarType::Recursive);
            }
        });

        let required_type = typeck::destruc::type_for_decl_destruc(self.tvars, &destruc, None);
        let value_span = value_expr.span().unwrap_or(span);
        let value_node = self.visit_expr(&mut scx, &required_type, value_expr)?;

        self.destruc_value(
            &destruc,
            value_span,
            &value_node.poly_type,
            // We know the exact type of these variables; do not infer further even if their type
            // wasn't declared
            false,
        )?;

        Ok(())
    }

    fn visit_def_id(&mut self, def_id: DefId) -> Result<()> {
        let mut taken_state = DefState::Complete;
        std::mem::swap(&mut taken_state, &mut self.def_states[def_id.to_usize()]);

        if let DefState::Pending(def) = taken_state {
            self.visit_def(def)?;
        }

        Ok(())
    }

    fn infer_program(mut self, defs: Vec<hir::Def<ty::Decl>>) -> result::Result<(), Vec<Error>> {
        // First, visit all definitions to bind their variables
        for hir_def in defs {
            let def_id = DefId::new(self.def_states.len());

            typeck::destruc::visit_vars(&hir_def.destruc, |var_id, decl_type| {
                match decl_type.try_to_poly() {
                    Some(poly_type) => {
                        self.var_to_type.insert(var_id, VarType::Known(poly_type));
                    }
                    None => {
                        // Record the definition ID so we can deal with forward type references
                        self.var_to_type.insert(var_id, VarType::Pending(def_id));
                    }
                }
            });

            self.def_states.push(DefState::Pending(hir_def));
        }

        let errs = (0..self.def_states.len())
            .flat_map(|def_id| self.visit_def_id(DefId::new(def_id)).err())
            .collect::<Vec<Error>>();

        if errs.is_empty() {
            Ok(())
        } else {
            Err(errs)
        }
    }
}

pub fn infer_program(
    pvars: &[ty::purity::PVar],
    tvars: &[ty::TVar],
    defs: Vec<hir::Def<ty::Decl>>,
) -> result::Result<(), Vec<Error>> {
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
        let mut scx = SubtreeCtx {
            fun_purity: PurityVarType::Known(Purity::Pure.into_poly()),
        };

        icx.visit_expr(&mut scx, required_type, lowered_expr.expr)
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

    fn assert_guided_type_for_expr(expected_ty_str: &str, expr_str: &str, guide_ty_str: &str) {
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
    }

    #[test]
    fn fun_expr() {
        assert_type_for_expr("(-> ())", "(fn ())");
        assert_type_for_expr("(Any -> true)", "(fn (_) true)");
        assert_type_for_expr("(String -> String)", "(fn ([x : String]) x)");

        // We should feed our wanted type in to the function type
        assert_guided_type_for_expr("(Symbol -> true)", "(fn (_) true)", "(Symbol -> true)");
        assert_guided_type_for_expr("(Symbol -> Symbol)", "(fn (x) x)", "(Symbol -> Any))");

        let j = "(let [[f : (Symbol -> true)] (fn ([_ : String]) true)])";
        let t = "                             ^^^^^^^^^^^^^^^^^^^^^^^^  ";
        let err = Error::new(
            t2s(t),
            ErrorKind::IsNotA("(String -> true)".to_owned(), "(Symbol -> true)".to_owned()),
        );
        assert_type_error(&err, j);

        let j = "(fn ([x : String]) -> Symbol x)";
        let t = "                             ^ ";
        let err = Error::new(
            t2s(t),
            ErrorKind::IsNotA("String".to_owned(), "Symbol".to_owned()),
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
            ErrorKind::IsNotA("(->! false)".to_owned(), "(... -> Bool)".to_owned()),
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
    fn ty_pref() {
        assert_type_for_expr("(Type? Symbol)", "(type-predicate Symbol)")
    }
}
