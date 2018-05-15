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

enum VarType {
    Free(ty::Poly),
    Known(ty::Poly),
}

struct InferCtx<'a> {
    pvars: &'a [ty::purity::PVar],
    tvars: &'a [ty::TVar],

    free_ty_to_poly: HashMap<ty::FreeTyId, ty::Poly>,
    var_to_type: HashMap<hir::VarId, VarType>,
}

impl<'a> InferCtx<'a> {
    fn new(pvars: &'a [ty::purity::PVar], tvars: &'a [ty::TVar]) -> InferCtx<'a> {
        InferCtx {
            pvars,
            tvars,
            free_ty_to_poly: HashMap::new(),
            var_to_type: HashMap::new(),
        }
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
    fn ensure_is_a(
        &self,
        span: Span,
        sub_poly: ty::Poly,
        parent_poly: &ty::Poly,
    ) -> Result<ty::Poly> {
        if !ty::is_a::poly_is_a(self.tvars, &sub_poly, parent_poly).to_bool() {
            Err(Error::new(
                span,
                ErrorKind::IsNotA(self.str_for_poly(&sub_poly), self.str_for_poly(parent_poly)),
            ))
        } else {
            Ok(sub_poly)
        }
    }

    fn visit_lit(&mut self, required_type: &ty::Poly, datum: &Datum) -> Result<ty::Poly> {
        let lit_type = ty::datum::poly_for_datum(datum);
        self.ensure_is_a(datum.span(), lit_type, required_type)
    }

    fn visit_cond(
        &mut self,
        required_type: &ty::Poly,
        span: Span,
        cond: &hir::Cond,
    ) -> Result<ty::Poly> {
        self.visit_expr(&ty::Ty::Bool.into_poly(), cond.test_expr())?;

        let true_type = self.visit_expr(required_type, cond.true_expr())?;
        let false_type = self.visit_expr(required_type, cond.false_expr())?;

        ty::unify::poly_unify_to_poly(self.tvars, &true_type, &false_type)
            .map_err(|unify_err| self.new_union_conflict_error(span, &unify_err))
    }

    fn visit_ty_pred(
        &self,
        required_type: &ty::Poly,
        span: Span,
        test_type: &ty::Poly,
    ) -> Result<ty::Poly> {
        self.ensure_is_a(
            span,
            ty::Ty::TyPred(Box::new(test_type.clone())).into_poly(),
            required_type,
        )
    }

    fn type_for_known_ref(
        &self,
        required_type: &ty::Poly,
        span: Span,
        known_type: &ty::Poly,
    ) -> Result<ty::Poly> {
        self.ensure_is_a(span, known_type.clone(), required_type)
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
    ) -> Result<ty::Poly> {
        let new_free_type = match self.var_to_type[&var_id] {
            VarType::Known(ref known_type) => {
                return self.type_for_known_ref(required_type, span, known_type);
            }
            VarType::Free(ref current_type) => {
                self.type_for_free_ref(required_type, span, current_type)?
            }
        };

        // Update the free var's type
        self.var_to_type
            .insert(var_id, VarType::Free(new_free_type.clone()));

        Ok(new_free_type)
    }

    fn visit_do(&mut self, required_type: &ty::Poly, exprs: &[hir::Expr]) -> Result<ty::Poly> {
        if exprs.is_empty() {
            return Ok(ty::Ty::Union(vec![]).into_poly());
        }

        for non_terminal_expr in &exprs[0..exprs.len() - 1] {
            // The type of this expression doesn't matter; its value is discarded
            self.visit_expr(&ty::Ty::Any.into_poly(), non_terminal_expr)?;
        }

        self.visit_expr(required_type, exprs.last().unwrap())
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
        decl_fun: &hir::Fun,
    ) -> Result<ty::Poly> {
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

        let actual_param_type: ty::Params<ty::Poly> = typeck::destruc::type_for_list_destruc(
            self.tvars,
            decl_fun.params(),
            // Use the required type as a guide for any free types in the parameter list
            required_fun_type.map(|fun| list_type::ParamsIterator::new(fun.params())),
        );

        // Bind all of our parameter variables
        self.destruc_list_value(
            decl_fun.params(),
            span,
            list_type::ParamsIterator::new(&actual_param_type),
            // If a parameter has a free decl type then perform type inference on it
            VarType::Free,
        )?;

        let wanted_ret_type = decl_fun.ret_ty().try_to_poly().unwrap_or_else(|| {
            if let Some(ref required_top_fun_type) = required_top_fun_type {
                required_top_fun_type.ret().clone()
            } else {
                ty::Ty::Any.into_poly()
            }
        });

        let actual_ret_type = self.visit_expr(&wanted_ret_type, decl_fun.body_expr())?;

        if let ty::Decl::Free(free_ty_id) = decl_fun.ret_ty() {
            self.free_ty_to_poly
                .insert(*free_ty_id, actual_ret_type.clone());
        }

        // TODO: Purity
        let found_type = ty::Fun::new(
            decl_fun.pvar_ids().clone(),
            decl_fun.tvar_ids().clone(),
            ty::TopFun::new(Purity::Pure.into_poly(), actual_ret_type),
            actual_param_type,
        ).into_ref();

        self.ensure_is_a(span, found_type, required_type)
    }

    fn visit_fun_app(
        &mut self,
        required_type: &ty::Poly,
        span: Span,
        app: &hir::App,
        fun: &ty::Fun<ty::Poly>,
    ) -> Result<ty::Poly> {
        // The context used to select the types for our parameters. It's only evidence is the
        // wanted return type which is used for backwards type propagation.
        let mut param_select_ctx = ty::select::SelectContext::new(
            fun.pvar_ids().clone(),
            fun.tvar_ids().clone(),
            self.tvars,
        );

        // The context used to select the type for our return value. It collects evidence from the
        // arguments as they're evaluated.
        let mut ret_select_ctx = param_select_ctx.clone();

        // Add our return type information
        param_select_ctx.add_evidence(fun.ret(), required_type);

        // Iterate over our parameter type to feed type information in to the arguments
        let mut param_iter = list_type::ParamsIterator::new(fun.params());

        for fixed_arg_expr in app.fixed_arg_exprs() {
            let param_type = if let Some(param_type) = param_iter.next().expect("TODO") {
                param_type
            } else {
                return Err(Error::new(
                    span,
                    ErrorKind::TooManyArgs(app.fixed_arg_exprs().len(), fun.params().fixed().len()),
                ));
            };

            let wanted_arg_type = ty::subst::inst_poly_selection(&param_select_ctx, param_type);
            let actual_arg_type = self.visit_expr(&wanted_arg_type, fixed_arg_expr)?;

            ret_select_ctx.add_evidence(param_type, &actual_arg_type);
        }

        if let Some(rest_arg_expr) = app.rest_arg_expr() {
            let tail_type = param_iter.tail_type();
            let wanted_tail_type = ty::subst::inst_poly_selection(&param_select_ctx, &tail_type);
            let actual_tail_type = self.visit_expr(&wanted_tail_type, rest_arg_expr)?;

            ret_select_ctx.add_evidence(&tail_type, &actual_tail_type);
        } else if param_iter.next().expect("TODO") != None && fun.params().rest().is_none() {
            // We wanted more args!
            return Err(Error::new(
                span,
                ErrorKind::InsufficientArgs(
                    app.fixed_arg_exprs().len(),
                    fun.params().fixed().len(),
                ),
            ));
        }

        let ret_type = ty::subst::inst_poly_selection(&ret_select_ctx, fun.ret());
        self.ensure_is_a(span, ret_type, required_type)
    }

    fn visit_app(
        &mut self,
        required_type: &ty::Poly,
        span: Span,
        app: &hir::App,
    ) -> Result<ty::Poly> {
        // The only type information we can feed back is that we want a function returning a
        // certain value
        let wanted_fun_type = ty::TopFun::new(
            // TODO: Purity
            Purity::Impure.into_poly(),
            required_type.clone(),
        ).into_ref();

        let actual_fun_type = self.visit_expr(&wanted_fun_type, app.fun_expr())?;

        match ty::resolve::resolve_poly_ty(self.tvars, &actual_fun_type).as_ty() {
            ty::Ty::TopFun(_) => Err(Error::new(
                span,
                ErrorKind::TopFunApply(self.str_for_poly(&actual_fun_type)),
            )),
            ty::Ty::TyPred(subject_poly) => {
                // TODO: Arity
                let test_poly =
                    self.visit_expr(&ty::Ty::Any.into_poly(), &app.fixed_arg_exprs()[0])?;

                let result = ty::pred::interpret_poly_pred(self.tvars, &subject_poly, &test_poly)
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
                Ok(match result {
                    InterpretedPred::StaticTrue => ty::Ty::LitBool(true),
                    InterpretedPred::StaticFalse => ty::Ty::LitBool(false),
                    InterpretedPred::Dynamic(_, _) => {
                        // TODO: Occurence typing
                        ty::Ty::Bool
                    }
                }.into_poly())
            }
            ty::Ty::Fun(fun) => self.visit_fun_app(required_type, span, app, fun),
            _ => panic!("Unexpected type"),
        }
    }

    fn visit_expr(&mut self, required_type: &ty::Poly, expr: &hir::Expr) -> Result<ty::Poly> {
        match expr {
            hir::Expr::Lit(datum) => self.visit_lit(required_type, datum),
            hir::Expr::Cond(span, cond) => self.visit_cond(required_type, *span, cond),
            hir::Expr::Do(exprs) => self.visit_do(required_type, exprs),
            hir::Expr::Fun(span, fun) => self.visit_fun(required_type, *span, fun),
            hir::Expr::TyPred(span, test_type) => {
                self.visit_ty_pred(required_type, *span, test_type)
            }
            hir::Expr::Def(span, destruc, expr) => {
                self.visit_def(*span, destruc, expr)?;
                Ok(ty::Ty::Union(vec![]).into_poly())
            }
            hir::Expr::Ref(span, var_id) => self.visit_ref(required_type, *span, *var_id),
            hir::Expr::App(span, app) => self.visit_app(required_type, *span, app),
        }
    }

    fn destruc_scalar_value<F>(
        &mut self,
        scalar: &destruc::Scalar,
        value_type: &ty::Poly,
        free_vartype_cons: F,
    ) where
        F: Copy + Fn(ty::Poly) -> VarType,
    {
        let is_free = if let ty::Decl::Free(free_ty_id) = *scalar.ty() {
            self.free_ty_to_poly.insert(free_ty_id, value_type.clone());
            true
        } else {
            false
        };

        if let Some(var_id) = *scalar.var_id() {
            let var_type = if is_free {
                free_vartype_cons(value_type.clone())
            } else {
                VarType::Known(value_type.clone())
            };

            self.var_to_type.insert(var_id, var_type);
        }
    }

    fn destruc_list_value<F, I>(
        &mut self,
        list: &destruc::List,
        value_span: Span,
        mut value_type_iter: I,
        free_vartype_cons: F,
    ) -> Result<()>
    where
        F: Copy + Fn(ty::Poly) -> VarType,
        I: ListTypeIterator,
    {
        for fixed_destruc in list.fixed() {
            let member_type = match value_type_iter.next() {
                Ok(Some(member_type)) => member_type,
                Err(list_type::Error::UnifyError(unify_err)) => {
                    return Err(self.new_union_conflict_error(value_span, &unify_err))
                }
                _ => panic!("Destructured value with unexpected type"),
            };

            self.destruc_value(fixed_destruc, value_span, member_type, free_vartype_cons)?;
        }

        if let Some(scalar) = list.rest() {
            self.destruc_scalar_value(scalar, &value_type_iter.tail_type(), free_vartype_cons);
        }

        Ok(())
    }

    fn destruc_value<F>(
        &mut self,
        destruc: &destruc::Destruc,
        value_span: Span,
        value_type: &ty::Poly,
        free_vartype_cons: F,
    ) -> Result<()>
    where
        F: Copy + Fn(ty::Poly) -> VarType,
    {
        match destruc {
            destruc::Destruc::Scalar(_, scalar) => {
                self.destruc_scalar_value(scalar, value_type, free_vartype_cons);
                Ok(())
            }
            destruc::Destruc::List(_, list) => {
                let value_type_iter = list_type::PolyIterator::new(value_type);
                self.destruc_list_value(list, value_span, value_type_iter, free_vartype_cons)
            }
        }
    }

    fn visit_def(
        &mut self,
        span: Span,
        destruc: &destruc::Destruc,
        value_expr: &hir::Expr,
    ) -> Result<()> {
        let required_type = typeck::destruc::type_for_destruc(self.tvars, destruc, None);
        let actual_type = self.visit_expr(&required_type, value_expr)?;

        self.destruc_value(
            destruc,
            value_expr.span().unwrap_or(span),
            &actual_type,
            // We know the exact type of these variables; do not infer further even if their type
            // wasn't declared
            VarType::Known,
        )
    }

    fn visit_module_def(&mut self, module_def: &hir::module::ModuleDef) -> Result<()> {
        self.visit_def(*module_def.span(), module_def.destruc(), module_def.value())
    }
}

pub fn infer_module(
    pvars: &[ty::purity::PVar],
    tvars: &[ty::TVar],
    module: &hir::module::Module,
) -> result::Result<(), Vec<Error>> {
    let errs = module
        .defs()
        .iter()
        .flat_map(|module_def| {
            let mut icx = InferCtx::new(pvars, tvars);
            icx.visit_module_def(&module_def).err()
        })
        .collect::<Vec<Error>>();

    if errs.is_empty() {
        Ok(())
    } else {
        Err(errs)
    }
}

#[cfg(test)]
mod test {
    use super::*;
    use hir::lowering::body_for_str;
    use syntax::span::t2s;

    fn type_for_body(
        required_type: &ty::Poly,
        body: &hir::lowering::LoweredTestBody,
    ) -> Result<ty::Poly> {
        let mut icx = InferCtx::new(&body.pvars, &body.tvars);
        icx.visit_expr(required_type, &body.expr)
    }

    fn assert_type_for_expr(ty_str: &str, expr_str: &str) {
        let body = body_for_str(expr_str).unwrap();
        let poly = hir::poly_for_str(ty_str).unwrap();

        assert_eq!(
            poly,
            type_for_body(&ty::Ty::Any.into_poly(), &body).unwrap()
        );
    }

    fn assert_guided_type_for_expr(expected_ty_str: &str, expr_str: &str, guide_ty_str: &str) {
        let body = body_for_str(expr_str).unwrap();
        let expected_poly = hir::poly_for_str(expected_ty_str).unwrap();
        let guide_poly = hir::poly_for_str(guide_ty_str).unwrap();

        assert_eq!(expected_poly, type_for_body(&guide_poly, &body).unwrap());
    }

    fn assert_type_error(err: &Error, expr_str: &str) {
        let body = body_for_str(expr_str).unwrap();

        assert_eq!(
            err,
            &type_for_body(&ty::Ty::Any.into_poly(), &body).unwrap_err()
        )
    }

    #[test]
    fn literal_expr() {
        assert_type_for_expr("Int", "1");
    }

    #[test]
    fn cond_expr() {
        assert_type_for_expr("Bool", "(if true true false)");
    }

    #[test]
    fn fun_expr() {
        assert_type_for_expr("(Any -> true)", "(fn (_) true)");
        assert_type_for_expr("(String -> String)", "(fn ([x : String]) x)");

        // We should feed our wanted type in to the function type
        assert_guided_type_for_expr("(Symbol -> true)", "(fn (_) true)", "(Symbol -> true)");
        assert_guided_type_for_expr("(Symbol -> Symbol)", "(fn (x) x)", "(Symbol -> Any))");

        let j = "(def [f : (Symbol -> true)] (fn ([_ : String]) true)) f";
        let t = "                            ^^^^^^^^^^^^^^^^^^^^^^^^   ";
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
        assert_type_for_expr("Int", "(def (x) '(1)) x");
        assert_type_for_expr(
            "(List true false)",
            "(def (_ rest ...) '(1 true false)) rest",
        );
    }

    #[test]
    fn var_ref() {
        assert_type_for_expr("Int", "(def x 1) x")
    }

    #[test]
    fn ty_pref() {
        assert_type_for_expr("(Type? Symbol)", "(type-predicate Symbol)")
    }
}
