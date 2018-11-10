use std::collections::HashMap;
use std::ffi;
use std::os::raw::c_void;
use std::panic;
use std::rc::Rc;

use runtime;
use runtime::boxed;
use runtime::boxed::prelude::*;
use runtime::boxed::refs::Gc;
use runtime::callback::EntryPointABIType as CallbackEntryPointABIType;

use runtime_syntax::reader;
use syntax::datum::Datum;
use syntax::span::{Span, EMPTY_SPAN};

use crate::codegen;
use crate::hir;
use crate::mir::builder::Builder;
use crate::mir::error::{Error, ErrorKind, Result};
use crate::mir::ops;
use crate::mir::value;
use crate::mir::{Expr, Value};
use crate::ty;
use crate::ty::purity;

pub struct EvalHirCtx {
    runtime_task: runtime::task::Task,
    global_values: HashMap<hir::VarId, Value>,

    private_funs: Vec<ops::Fun>,

    rust_fun_thunks: HashMap<*const c_void, boxed::ThunkEntry>,
    thunk_fun_values: HashMap<Gc<boxed::FunThunk>, Value>,
    thunk_jit: codegen::jit::JITCtx,
}

pub struct DefCtx {
    pvar_purities: HashMap<purity::PVarId, purity::Purity>,
    tvar_types: HashMap<ty::TVarId, ty::Ty<ty::Mono>>,
    local_values: HashMap<hir::VarId, Value>,
}

impl DefCtx {
    pub fn monomorphise(&self, poly: &ty::Poly) -> ty::Mono {
        ty::subst::monomorphise(&self.pvar_purities, &self.tvar_types, poly)
    }
}

struct BuiltCondBranch {
    b: Builder,
    value: Value,
}

pub struct BuiltProgram {
    pub main: ops::Fun,
    pub private_funs: Vec<ops::Fun>,
}

impl BuiltProgram {
    pub fn is_empty(&self) -> bool {
        self.private_funs.is_empty() && self.main.ops.len() == 1
    }
}

impl DefCtx {
    pub fn new() -> DefCtx {
        DefCtx {
            pvar_purities: HashMap::new(),
            tvar_types: HashMap::new(),
            local_values: HashMap::new(),
        }
    }
}

impl Default for DefCtx {
    fn default() -> DefCtx {
        DefCtx::new()
    }
}

impl EvalHirCtx {
    pub fn new(optimising: bool) -> EvalHirCtx {
        let thunk_jit = codegen::jit::JITCtx::new(optimising);

        EvalHirCtx {
            runtime_task: runtime::task::Task::new(),
            global_values: HashMap::new(),

            private_funs: vec![],

            rust_fun_thunks: HashMap::new(),
            thunk_fun_values: HashMap::new(),
            thunk_jit,
        }
    }

    fn destruc_scalar(
        var_values: &mut HashMap<hir::VarId, Value>,
        scalar: &hir::destruc::Scalar<hir::Inferred>,
        value: Value,
    ) {
        if let Some(var_id) = scalar.var_id() {
            var_values.insert(*var_id, value);
        }
    }

    fn destruc_list(
        b: &mut Option<Builder>,
        span: Span,
        var_values: &mut HashMap<hir::VarId, Value>,
        list: &hir::destruc::List<hir::Inferred>,
        value: Value,
    ) {
        let mut iter = value.into_list_iter();

        for fixed_destruc in list.fixed() {
            let guide_type = iter.next_unchecked(b, span).clone();
            Self::destruc_value(b, var_values, fixed_destruc, guide_type);
        }

        if let Some(rest_destruc) = list.rest() {
            Self::destruc_scalar(var_values, rest_destruc, iter.into_rest())
        }
    }

    fn destruc_value(
        b: &mut Option<Builder>,
        var_values: &mut HashMap<hir::VarId, Value>,
        destruc: &hir::destruc::Destruc<hir::Inferred>,
        value: Value,
    ) {
        use crate::hir::destruc::Destruc;

        match destruc {
            Destruc::Scalar(_, scalar) => Self::destruc_scalar(var_values, scalar, value),
            Destruc::List(span, list) => Self::destruc_list(b, *span, var_values, list, value),
        }
    }

    fn destruc_source_name(destruc: &hir::destruc::Destruc<hir::Inferred>) -> Option<&str> {
        use crate::hir::destruc::Destruc;

        match destruc {
            Destruc::Scalar(_, scalar) => Some(scalar.source_name()),
            Destruc::List(_, _) => None,
        }
    }

    fn eval_ref(&self, dcx: &DefCtx, var_id: hir::VarId) -> Value {
        // Try local values first
        if let Some(value) = dcx.local_values.get(&var_id) {
            return value.clone();
        }

        self.global_values[&var_id].clone()
    }

    fn eval_do(
        &mut self,
        dcx: &mut DefCtx,
        b: &mut Option<Builder>,
        exprs: &[Expr],
    ) -> Result<Value> {
        let initial_value = Value::List(Box::new([]), None);

        // TODO: This needs to handle Never values once we can create them
        exprs
            .iter()
            .try_fold(initial_value, |_, expr| self.eval_expr(dcx, b, expr))
    }

    fn eval_let(
        &mut self,
        dcx: &mut DefCtx,
        b: &mut Option<Builder>,
        hir_let: &hir::Let<hir::Inferred>,
    ) -> Result<Value> {
        let source_name = Self::destruc_source_name(&hir_let.destruc);
        let value = self.eval_expr_with_source_name(dcx, b, &hir_let.value_expr, source_name)?;

        Self::destruc_value(b, &mut dcx.local_values, &hir_let.destruc, value);

        self.eval_expr(dcx, b, &hir_let.body_expr)
    }

    fn eval_lit(&mut self, literal: &Datum) -> Value {
        match literal {
            Datum::List(_, elems) => {
                let elem_values: Vec<Value> =
                    elems.iter().map(|elem| self.eval_lit(elem)).collect();

                Value::List(elem_values.into_boxed_slice(), None)
            }
            other => {
                let boxed = reader::box_syntax_datum(self, other);
                Value::Const(boxed)
            }
        }
    }

    fn eval_arret_fun_app(
        &mut self,
        dcx: &mut DefCtx,
        b: &mut Option<Builder>,
        span: Span,
        arret_fun: &value::ArretFun,
        arg_list_value: Value,
    ) -> Result<Value> {
        let fun_expr = &arret_fun.fun_expr;

        Self::destruc_list(
            b,
            span,
            &mut dcx.local_values,
            &fun_expr.params,
            arg_list_value,
        );

        self.eval_expr(dcx, b, &fun_expr.body_expr)
    }

    fn eval_ty_pred_app(
        &mut self,
        b: &mut Option<Builder>,
        span: Span,
        arg_list_value: &Value,
        test_ty: ty::pred::TestTy,
    ) -> Value {
        use crate::mir::typred::eval_ty_pred;

        let subject_value = arg_list_value.list_iter().next_unchecked(b, span);
        eval_ty_pred(self, b, span, &subject_value, test_ty)
    }

    fn eval_eq_pred_app(
        &mut self,
        b: &mut Option<Builder>,
        span: Span,
        arg_list_value: &Value,
    ) -> Value {
        use crate::mir::equality::eval_equality;

        let mut iter = arg_list_value.list_iter();

        let left_value = iter.next_unchecked(b, span);
        let right_value = iter.next_unchecked(b, span);

        eval_equality(self, b, span, &left_value, &right_value)
    }

    pub fn rust_fun_to_jit_boxed(&mut self, rust_fun: Rc<hir::rfi::Fun>) -> Gc<boxed::FunThunk> {
        let closure = boxed::NIL_INSTANCE.as_any_ref();
        let entry = self.jit_thunk_for_rust_fun(&rust_fun);
        let new_boxed = boxed::FunThunk::new(self, (closure, entry));

        let rust_fun_value = Value::RustFun(rust_fun);
        self.thunk_fun_values.insert(new_boxed, rust_fun_value);

        new_boxed
    }

    fn jit_thunk_for_rust_fun(&mut self, rust_fun: &hir::rfi::Fun) -> boxed::ThunkEntry {
        // Create a dynamic thunk to this Rust function if it doesn't exist
        if let Some(thunk) = self.rust_fun_thunks.get(&rust_fun.entry_point()) {
            return *thunk;
        }

        let thunk = unsafe {
            use crate::mir::rust_fun::ops_for_rust_fun_thunk;
            use std::mem;

            let ops_fun = ops_for_rust_fun_thunk(self, EMPTY_SPAN, rust_fun);
            let address = self
                .thunk_jit
                .compile_fun(self.private_funs.as_slice(), &ops_fun);

            mem::transmute(address as usize)
        };

        self.rust_fun_thunks.insert(rust_fun.entry_point(), thunk);
        thunk
    }

    /// Ensures the passed `RustFun` is known by the JIT
    ///
    /// This can be called multiple times with the same Rust fun. Calling it with a fun that's
    /// never used by the JIT is harmless
    pub fn register_rust_fun_with_jit(&mut self, rust_fun: &hir::rfi::Fun) {
        let symbol_cstring = ffi::CString::new(rust_fun.symbol()).unwrap();

        // Add the inner symbol
        self.thunk_jit.add_symbol(
            symbol_cstring.as_bytes_with_nul(),
            rust_fun.entry_point() as u64,
        );
    }

    pub fn rust_fun_to_thunk_reg(
        &mut self,
        b: &mut Builder,
        span: Span,
        rust_fun: &hir::rfi::Fun,
    ) -> ops::RegId {
        use crate::mir::ops::*;
        use crate::mir::rust_fun::ops_for_rust_fun_thunk;
        use runtime::abitype;

        let ops_fun = ops_for_rust_fun_thunk(self, span, rust_fun);

        let nil_reg = b.push_reg(span, OpKind::ConstBoxedNil, ());
        let closure_reg = b.cast_boxed(span, nil_reg, abitype::BoxedABIType::Any);

        let private_fun_id = ops::PrivateFunId::new_entry_id(&mut self.private_funs, ops_fun);
        b.push_reg(
            span,
            OpKind::ConstBoxedFunThunk,
            BoxFunThunkOp {
                closure_reg,
                callee: ops::Callee::PrivateFun(private_fun_id),
            },
        )
    }

    fn call_native_fun<F>(span: Span, block: F) -> Result<Value>
    where
        F: FnOnce() -> Gc<boxed::Any>,
    {
        // By convention convert string panics in to our `ErrorKind::Panic`
        panic::catch_unwind(panic::AssertUnwindSafe(block))
            .map(Value::Const)
            .map_err(|err| {
                let message = if let Some(message) = err.downcast_ref::<String>() {
                    message.clone()
                } else {
                    "Unexpected panic type".to_owned()
                };

                Error::new(span, ErrorKind::Panic(message))
            })
    }

    fn eval_rust_fun_app(
        &mut self,
        b: &mut Option<Builder>,
        span: Span,
        ret_ty: &ty::Mono,
        rust_fun: &hir::rfi::Fun,
        arg_list_value: Value,
    ) -> Result<Value> {
        use crate::mir::intrinsic;
        use crate::mir::rust_fun::build_rust_fun_app;
        use crate::mir::value::to_const::value_to_const;

        // TODO: Fix for polymorphism once it's supported
        let can_const_eval =
            b.is_none() || rust_fun.arret_fun_type().purity() == &purity::Purity::Pure.into_poly();

        if let Some(intrinsic_name) = rust_fun.intrinsic_name() {
            // Attempt specialised evaluation
            if let Some(value) =
                intrinsic::try_eval(self, b, span, intrinsic_name, &arg_list_value)?
            {
                return Ok(value);
            }
        }

        if can_const_eval {
            let boxed_arg_list = value_to_const(self, &arg_list_value);

            if let Some(boxed_arg_list) = boxed_arg_list {
                let thunk = self.jit_thunk_for_rust_fun(rust_fun);

                // By convention convert string panics in to our `ErrorKind::Panic`
                let runtime_task = &mut self.runtime_task;
                return Self::call_native_fun(span, || {
                    let closure = boxed::NIL_INSTANCE.as_any_ref();
                    thunk(runtime_task, closure, boxed_arg_list)
                });
            }
        }

        if let Some(b) = b {
            Ok(build_rust_fun_app(
                self,
                b,
                span,
                ret_ty,
                rust_fun,
                arg_list_value,
            ))
        } else {
            panic!("Need builder for non-const function application");
        }
    }

    fn eval_const_fun_thunk_app(
        &mut self,
        dcx: &mut DefCtx,
        b: &mut Option<Builder>,
        span: Span,
        ret_ty: &ty::Mono,
        fun_thunk: Gc<boxed::FunThunk>,
        arg_list_value: Value,
    ) -> Result<Value> {
        use crate::mir::value::to_const::value_to_const;

        if let Some(actual_value) = self.thunk_fun_values.get(&fun_thunk) {
            return self.eval_value_app(dcx, b, span, ret_ty, &actual_value.clone(), arg_list_value);
        }

        if b.is_some() {
            unimplemented!("attempt to apply unknown fun thunk during compile phase");
        }

        let const_arg_list =
            value_to_const(self, &arg_list_value).expect("non-constant value during apply");

        Self::call_native_fun(span, || {
            fun_thunk.apply(&mut self.runtime_task, const_arg_list)
        })
    }

    fn build_reg_fun_thunk_app(
        &mut self,
        b: &mut Builder,
        span: Span,
        fun_reg_value: &value::RegValue,
        arg_list_value: &Value,
    ) -> Value {
        use crate::mir::ops::*;
        use crate::mir::value::build_reg::value_to_reg;
        use runtime::abitype;

        let fun_boxed_abi_type =
            if let abitype::ABIType::Boxed(ref fun_boxed_abi_type) = fun_reg_value.abi_type {
                fun_boxed_abi_type
            } else {
                panic!(
                    "Attempted to apply reg value with unboxed ABI type of {:?}",
                    fun_reg_value.abi_type
                )
            };

        let fun_thunk_reg = b.cast_boxed_cond(
            span,
            fun_boxed_abi_type,
            fun_reg_value.reg,
            boxed::TypeTag::FunThunk.into(),
        );

        let closure_reg = b.push_reg(span, OpKind::LoadBoxedFunThunkClosure, fun_thunk_reg);
        let arg_list_reg = value_to_reg(
            self,
            b,
            span,
            &arg_list_value,
            &abitype::TOP_LIST_BOXED_ABI_TYPE.into(),
        );

        let ret_reg = b.push_reg(
            span,
            OpKind::Call,
            CallOp {
                callee: Callee::BoxedFunThunk(fun_thunk_reg),
                impure: true,
                args: vec![closure_reg, arg_list_reg.into()].into_boxed_slice(),
            },
        );

        Value::Reg(Rc::new(value::RegValue {
            reg: ret_reg,
            abi_type: abitype::BoxedABIType::Any.into(),
        }))
    }

    fn eval_value_app(
        &mut self,
        dcx: &mut DefCtx,
        b: &mut Option<Builder>,
        span: Span,
        ret_ty: &ty::Mono,
        fun_value: &Value,
        arg_list_value: Value,
    ) -> Result<Value> {
        match fun_value {
            Value::ArretFun(arret_fun) => {
                use crate::mir::closure;

                closure::load_from_current_fun(&mut dcx.local_values, &arret_fun.closure);
                self.eval_arret_fun_app(dcx, b, span, &arret_fun, arg_list_value)
            }
            Value::RustFun(rust_fun) => {
                self.eval_rust_fun_app(b, span, ret_ty, &rust_fun, arg_list_value)
            }
            Value::TyPred(test_ty) => Ok(self.eval_ty_pred_app(b, span, &arg_list_value, *test_ty)),
            Value::EqPred => Ok(self.eval_eq_pred_app(b, span, &arg_list_value)),
            Value::Const(boxed_fun) => match boxed_fun.as_subtype() {
                boxed::AnySubtype::FunThunk(fun_thunk) => self.eval_const_fun_thunk_app(
                    dcx,
                    b,
                    span,
                    ret_ty,
                    unsafe { Gc::new(fun_thunk) },
                    arg_list_value,
                ),
                other => unimplemented!("applying boxed function value type: {:?}", other),
            },
            Value::Reg(reg_value) => {
                if let Some(b) = b {
                    Ok(self.build_reg_fun_thunk_app(b, span, reg_value, &arg_list_value))
                } else {
                    panic!("Need builder for reg function application");
                }
            }
            other => {
                unimplemented!("applying function value type: {:?}", other);
            }
        }
    }

    fn eval_app(
        &mut self,
        dcx: &mut DefCtx,
        b: &mut Option<Builder>,
        span: Span,
        app: &hir::App<hir::Inferred>,
    ) -> Result<Value> {
        let fun_value = self.eval_expr(dcx, b, &app.fun_expr)?;

        let fixed_values = app
            .fixed_arg_exprs
            .iter()
            .map(|arg| self.eval_expr(dcx, b, arg))
            .collect::<Result<Vec<Value>>>()?;

        let rest_value = match &app.rest_arg_expr {
            Some(rest_arg) => Some(Box::new(self.eval_expr(dcx, b, rest_arg)?)),
            None => None,
        };

        let ret_ty = dcx.monomorphise(&app.ret_ty);
        let arg_list_value = Value::List(fixed_values.into_boxed_slice(), rest_value);
        self.eval_value_app(dcx, b, span, &ret_ty, &fun_value, arg_list_value)
    }

    fn eval_cond(
        &mut self,
        dcx: &mut DefCtx,
        b: &mut Option<Builder>,
        span: Span,
        cond: &hir::Cond<hir::Inferred>,
    ) -> Result<Value> {
        let test_value = self.eval_expr(dcx, b, &cond.test_expr)?;

        match test_value {
            Value::Const(any_ref) => {
                let bool_ref = any_ref.downcast_ref::<boxed::Bool>().unwrap();

                if bool_ref.as_bool() {
                    self.eval_expr(dcx, b, &cond.true_expr)
                } else {
                    self.eval_expr(dcx, b, &cond.false_expr)
                }
            }
            dynamic_value => {
                if let Some(b) = b {
                    self.build_cond(dcx, b, span, &dynamic_value, cond)
                } else {
                    panic!("need builder for dynamic cond");
                }
            }
        }
    }

    fn build_cond_branch(
        &mut self,
        dcx: &mut DefCtx,
        branch_expr: &hir::Expr<hir::Inferred>,
    ) -> Result<BuiltCondBranch> {
        let b = Builder::new();

        let mut some_b = Some(b);
        let value = self.eval_expr(dcx, &mut some_b, branch_expr)?;
        let b = some_b.unwrap();

        Ok(BuiltCondBranch { b, value })
    }

    fn build_cond(
        &mut self,
        dcx: &mut DefCtx,
        b: &mut Builder,
        span: Span,
        test_value: &Value,
        cond: &hir::Cond<hir::Inferred>,
    ) -> Result<Value> {
        use crate::mir::value::build_reg::value_to_reg;
        use crate::mir::value::from_reg::reg_to_value;
        use crate::mir::value::plan_phi::plan_phi_abi_type;
        use runtime::abitype;

        let test_reg = value_to_reg(self, b, span, test_value, &abitype::ABIType::Bool);

        let mut built_true = self.build_cond_branch(dcx, &cond.true_expr)?;
        let mut built_false = self.build_cond_branch(dcx, &cond.false_expr)?;

        let true_is_divergent = built_true.value.is_divergent();
        let false_is_divergent = built_false.value.is_divergent();

        let output_value;
        let reg_phi;

        match (true_is_divergent, false_is_divergent) {
            (true, true) => {
                output_value = Value::Divergent;
                reg_phi = None;
            }
            (false, true) => {
                output_value = built_true.value;
                reg_phi = None;
            }
            (true, false) => {
                output_value = built_false.value;
                reg_phi = None;
            }
            (false, false) => {
                let phi_ty = dcx.monomorphise(&cond.phi_ty);
                let phi_abi_type =
                    plan_phi_abi_type(&built_true.value, &built_false.value, &phi_ty);

                let true_result_reg = value_to_reg(
                    self,
                    &mut built_true.b,
                    span,
                    &built_true.value,
                    &phi_abi_type,
                );

                let false_result_reg = value_to_reg(
                    self,
                    &mut built_false.b,
                    span,
                    &built_false.value,
                    &phi_abi_type,
                );

                let output_reg = b.alloc_reg();

                output_value = reg_to_value(self, output_reg, &phi_abi_type, &phi_ty);
                reg_phi = Some(ops::RegPhi {
                    output_reg,
                    true_result_reg: true_result_reg.into(),
                    false_result_reg: false_result_reg.into(),
                });
            }
        };

        b.push(
            span,
            ops::OpKind::Cond(ops::CondOp {
                reg_phi,
                test_reg: test_reg.into(),
                true_ops: built_true.b.into_ops(),
                false_ops: built_false.b.into_ops(),
            }),
        );

        Ok(output_value)
    }

    fn eval_arret_fun(
        &mut self,
        dcx: &mut DefCtx,
        span: Span,
        fun_expr: Rc<hir::Fun<hir::Inferred>>,
        source_name: Option<&str>,
    ) -> Value {
        use crate::mir::closure;

        let closure = closure::calculate_closure(&dcx.local_values, &fun_expr.body_expr);

        Value::ArretFun(value::ArretFun {
            span,
            source_name: source_name.map(|source_name| source_name.to_owned()),
            fun_expr,
            closure,
        })
    }

    pub fn arret_fun_to_jit_boxed(&mut self, arret_fun: &value::ArretFun) -> Gc<boxed::FunThunk> {
        use std::mem;

        let wanted_abi = ops::OpsABI::thunk_abi();
        let ops_fun = self
            .ops_for_arret_fun(&arret_fun, wanted_abi, true)
            .expect("error during arret fun boxing");

        let address = self
            .thunk_jit
            .compile_fun(self.private_funs.as_slice(), &ops_fun);
        let entry = unsafe { mem::transmute(address as usize) };

        let closure = boxed::NIL_INSTANCE.as_any_ref();
        let new_boxed = boxed::FunThunk::new(self, (closure, entry));

        let arret_fun_value = Value::ArretFun(arret_fun.clone());
        self.thunk_fun_values.insert(new_boxed, arret_fun_value);
        new_boxed
    }

    pub fn arret_fun_to_thunk_reg(
        &mut self,
        b: &mut Builder,
        span: Span,
        arret_fun: &value::ArretFun,
    ) -> ops::RegId {
        use crate::mir::closure;
        use crate::mir::ops::*;
        use runtime::abitype;

        let wanted_abi = OpsABI::thunk_abi();

        let ops_fun = self
            .ops_for_arret_fun(&arret_fun, wanted_abi, true)
            .expect("error during arret fun boxing");

        let outer_closure_reg = closure::save_to_closure_reg(self, b, span, &arret_fun.closure);
        let private_fun_id = ops::PrivateFunId::new_entry_id(&mut self.private_funs, ops_fun);

        if let Some(outer_closure_reg) = outer_closure_reg {
            b.push_reg(
                span,
                OpKind::AllocBoxedFunThunk,
                BoxFunThunkOp {
                    closure_reg: outer_closure_reg,
                    callee: ops::Callee::PrivateFun(private_fun_id),
                },
            )
        } else {
            let nil_reg = b.push_reg(span, OpKind::ConstBoxedNil, ());
            let outer_closure_reg = b.cast_boxed(span, nil_reg, abitype::BoxedABIType::Any);

            b.push_reg(
                span,
                OpKind::ConstBoxedFunThunk,
                BoxFunThunkOp {
                    closure_reg: outer_closure_reg,
                    callee: ops::Callee::PrivateFun(private_fun_id),
                },
            )
        }
    }

    fn ops_for_arret_fun(
        &mut self,
        arret_fun: &value::ArretFun,
        wanted_abi: ops::OpsABI,
        has_rest: bool,
    ) -> Result<ops::Fun> {
        use crate::mir::arg_list::{build_load_arg_list_value, LoadedArgList};
        use crate::mir::closure;
        use crate::mir::optimise::optimise_fun;
        use crate::mir::ret_value::build_ret_value;

        let mut b = Builder::new();
        let span = arret_fun.span;

        let LoadedArgList {
            closure_reg,
            param_regs,
            arg_list_value,
        } = build_load_arg_list_value(
            &mut b,
            &wanted_abi,
            arret_fun.closure.needs_closure_param(),
            has_rest,
        );

        // TODO: Make an actual DefCtx
        let mut dcx = DefCtx::new();
        closure::load_from_closure_param(&mut dcx.local_values, &arret_fun.closure, closure_reg);

        let mut some_b = Some(b);
        let result_value =
            self.eval_arret_fun_app(&mut dcx, &mut some_b, span, arret_fun, arg_list_value)?;
        let mut b = some_b.unwrap();

        build_ret_value(self, &mut b, span, &result_value, &wanted_abi.ret);

        Ok(optimise_fun(ops::Fun {
            span: arret_fun.span,
            source_name: arret_fun.source_name.clone(),

            abi: wanted_abi,
            params: param_regs,
            ops: b.into_ops(),
        }))
    }

    /// Builds a function with a callback ABI that calls a thunk passed as its closure
    fn ops_for_callback_to_thunk_adapter(
        &mut self,
        entry_point_abi: CallbackEntryPointABIType,
    ) -> ops::Fun {
        use crate::mir::arg_list::{build_load_arg_list_value, LoadedArgList};
        use crate::mir::optimise::optimise_fun;
        use crate::mir::ret_value::build_ret_value;
        use runtime::abitype;

        let ops_abi: ops::OpsABI = entry_point_abi.into();

        let mut b = Builder::new();

        let LoadedArgList {
            closure_reg,
            param_regs,
            arg_list_value,
        } = build_load_arg_list_value(&mut b, &ops_abi, true, false);

        let fun_reg_value = value::RegValue {
            reg: closure_reg.unwrap(),
            abi_type: abitype::BoxedABIType::Any.into(),
        };

        let result_value =
            self.build_reg_fun_thunk_app(&mut b, EMPTY_SPAN, &fun_reg_value, &arg_list_value);

        build_ret_value(self, &mut b, EMPTY_SPAN, &result_value, &ops_abi.ret);

        optimise_fun(ops::Fun {
            span: EMPTY_SPAN,
            source_name: Some("callback_to_thunk_adapter".to_owned()),

            abi: ops_abi,
            params: param_regs,
            ops: b.into_ops(),
        })
    }

    pub fn thunk_reg_to_callback_reg(
        &mut self,
        b: &mut Builder,
        span: Span,
        thunk_reg_abi_type: &::runtime::abitype::BoxedABIType,
        thunk_reg: ops::RegId,
        entry_point_abi: &CallbackEntryPointABIType,
    ) -> ops::RegId {
        use crate::mir::ops::*;
        use runtime::abitype;

        // Closures are of type `Any`
        let closure_reg = b.cast_boxed_cond(
            EMPTY_SPAN,
            thunk_reg_abi_type,
            thunk_reg,
            abitype::BoxedABIType::Any,
        );

        let ops_fun = self.ops_for_callback_to_thunk_adapter(entry_point_abi.clone());
        let private_fun_id = ops::PrivateFunId::new_entry_id(&mut self.private_funs, ops_fun);

        b.push_reg(
            span,
            OpKind::MakeCallback,
            MakeCallbackOp {
                closure_reg,
                callee: ops::Callee::PrivateFun(private_fun_id),
            },
        )
    }

    pub fn consume_def(&mut self, def: hir::Def<hir::Inferred>) -> Result<()> {
        let hir::Def {
            destruc,
            value_expr,
            ..
        } = def;

        let mut dcx = DefCtx::new();

        // Don't pass a builder; we should never generate ops based on a def
        let source_name = Self::destruc_source_name(&destruc);
        let value =
            self.consume_expr_with_source_name(&mut dcx, &mut None, value_expr, source_name)?;

        Self::destruc_value(&mut None, &mut self.global_values, &destruc, value);
        Ok(())
    }

    /// Collect any boxed values that are no longer reachable
    pub fn collect_garbage(&mut self) {
        use runtime::boxed::collect;
        use std::mem;

        let old_heap = mem::replace(self.runtime_task.heap_mut(), boxed::Heap::with_capacity(0));
        let mut strong_pass = collect::StrongPass::new(old_heap);

        // Move all of our global values to the new heap
        for value_ref in self.global_values.values_mut() {
            value::visit_value_root(&mut strong_pass, value_ref);
        }

        for value_ref in self.thunk_fun_values.values_mut() {
            // TODO: This can cause a circular reference with the weak pass below
            value::visit_value_root(&mut strong_pass, value_ref);
        }

        // Any function values that are still live need to be updated
        let weak_pass = strong_pass.into_weak_pass();

        let old_thunk_fun_values = mem::replace(&mut self.thunk_fun_values, HashMap::new());
        self.thunk_fun_values = old_thunk_fun_values
            .into_iter()
            .filter_map(|(fun_thunk, value)| {
                weak_pass
                    .new_heap_ref_for(fun_thunk)
                    .map(|new_fun_thunk| (new_fun_thunk, value))
            })
            .collect();

        *self.runtime_task.heap_mut() = weak_pass.into_new_heap();
    }

    fn eval_expr_with_source_name(
        &mut self,
        dcx: &mut DefCtx,
        b: &mut Option<Builder>,
        expr: &Expr,
        source_name: Option<&str>,
    ) -> Result<Value> {
        match expr {
            hir::Expr::Lit(literal) => Ok(self.eval_lit(literal)),
            hir::Expr::Do(exprs) => self.eval_do(dcx, b, &exprs),
            hir::Expr::Fun(span, fun_expr) => Ok(self.eval_arret_fun(
                dcx,
                *span,
                Rc::new(fun_expr.as_ref().clone()),
                source_name,
            )),
            hir::Expr::RustFun(_, rust_fun) => {
                Ok(Value::RustFun(Rc::new(rust_fun.as_ref().clone())))
            }
            hir::Expr::TyPred(_, test_ty) => Ok(Value::TyPred(*test_ty)),
            hir::Expr::EqPred(_) => Ok(Value::EqPred),
            hir::Expr::Ref(_, var_id) => Ok(self.eval_ref(dcx, *var_id)),
            hir::Expr::Let(_, hir_let) => self.eval_let(dcx, b, hir_let),
            hir::Expr::App(span, app) => self.eval_app(dcx, b, *span, app),
            hir::Expr::MacroExpand(span, expr) => self
                .eval_expr(dcx, b, expr)
                .map_err(|err| err.with_macro_invocation_span(*span)),
            hir::Expr::Cond(span, cond) => self.eval_cond(dcx, b, *span, cond),
        }
    }

    pub fn eval_expr(
        &mut self,
        dcx: &mut DefCtx,
        b: &mut Option<Builder>,
        expr: &Expr,
    ) -> Result<Value> {
        self.eval_expr_with_source_name(dcx, b, expr, None)
    }

    fn consume_expr_with_source_name(
        &mut self,
        dcx: &mut DefCtx,
        b: &mut Option<Builder>,
        expr: Expr,
        source_name: Option<&str>,
    ) -> Result<Value> {
        match expr {
            hir::Expr::Fun(span, fun_expr) => {
                Ok(self.eval_arret_fun(dcx, span, fun_expr.into(), source_name))
            }
            hir::Expr::RustFun(_, rust_fun) => Ok(Value::RustFun(rust_fun.into())),
            other => self.eval_expr_with_source_name(dcx, b, &other, source_name),
        }
    }

    pub fn consume_expr(
        &mut self,
        dcx: &mut DefCtx,
        b: &mut Option<Builder>,
        expr: Expr,
    ) -> Result<Value> {
        self.consume_expr_with_source_name(dcx, b, expr, None)
    }

    /// Evaluates the main function of a program
    ///
    /// This is intended for use by run-pass tests to avoid creating a temporary binary
    pub fn eval_main_fun(&mut self, main_var_id: hir::VarId) -> Result<()> {
        use syntax::span::EMPTY_SPAN;

        let mut dcx = DefCtx::new();
        let main_value = self.eval_ref(&dcx, main_var_id);
        let empty_list_value = Value::List(Box::new([]), None);

        self.eval_value_app(
            &mut dcx,
            &mut None,
            EMPTY_SPAN,
            &ty::Ty::unit().into_mono(),
            &main_value,
            empty_list_value,
        )?;

        Ok(())
    }

    /// Builds the main function of the program
    pub fn into_built_program(mut self, main_var_id: hir::VarId) -> Result<BuiltProgram> {
        use runtime::abitype;

        let dcx = DefCtx::new();
        let main_value = self.eval_ref(&dcx, main_var_id);

        let main_arret_fun = if let Value::ArretFun(main_arret_fun) = main_value {
            main_arret_fun
        } else {
            unimplemented!("Non-Arret main!");
        };

        let main_abi = ops::OpsABI {
            // Main is a top-level function; it can't capture
            external_call_conv: false,
            params: Box::new([]),
            ret: abitype::RetABIType::Void,
        };

        let main = self.ops_for_arret_fun(&main_arret_fun, main_abi, false)?;

        Ok(BuiltProgram {
            main,
            private_funs: self.private_funs,
        })
    }

    pub fn value_to_const(&mut self, value: &Value) -> Option<Gc<boxed::Any>> {
        use crate::mir::value::to_const::value_to_const;
        value_to_const(self, value)
    }
}

impl AsHeap for EvalHirCtx {
    fn as_heap(&self) -> &boxed::Heap {
        self.runtime_task.heap()
    }

    fn as_heap_mut(&mut self) -> &mut boxed::Heap {
        self.runtime_task.heap_mut()
    }
}
