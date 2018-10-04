use std::collections::HashMap;
use std::ffi;
use std::os::raw::c_void;
use std::panic;
use std::rc::Rc;

use runtime;
use runtime::boxed;
use runtime::boxed::prelude::*;
use runtime::boxed::refs::Gc;

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

    built_funs: Vec<ops::Fun>,

    rust_fun_thunks: HashMap<*const c_void, boxed::ThunkEntry>,
    thunk_fun_values: HashMap<Gc<boxed::FunThunk>, Value>,
    // This is important for drop order!
    thunk_jit: codegen::jit::JITCtx,
    thunk_gen: codegen::CodegenCtx,
}

pub struct DefCtx {
    pvar_purities: HashMap<purity::PVarId, purity::Purity>,
    tvar_types: HashMap<ty::TVarId, ty::Ty<ty::Mono>>,

    tvars: ty::TVars,
    local_values: HashMap<hir::VarId, Value>,
}

pub struct BuiltProgram {
    pub main: ops::Fun,
    pub funs: Vec<ops::Fun>,
}

impl BuiltProgram {
    pub fn is_empty(&self) -> bool {
        self.funs.is_empty() && self.main.ops.len() == 1
    }
}

impl DefCtx {
    pub fn new() -> DefCtx {
        DefCtx {
            pvar_purities: HashMap::new(),
            tvar_types: HashMap::new(),

            tvars: ty::TVars::new(),
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
    pub fn new() -> EvalHirCtx {
        EvalHirCtx {
            runtime_task: runtime::task::Task::new(),
            global_values: HashMap::new(),

            built_funs: vec![],

            rust_fun_thunks: HashMap::new(),
            thunk_fun_values: HashMap::new(),
            thunk_jit: codegen::jit::JITCtx::new(),
            thunk_gen: codegen::CodegenCtx::new(),
        }
    }

    fn destruc_scalar(
        var_values: &mut HashMap<hir::VarId, Value>,
        scalar: &hir::destruc::Scalar<ty::Poly>,
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
        list: &hir::destruc::List<ty::Poly>,
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
        destruc: &hir::destruc::Destruc<ty::Poly>,
        value: Value,
    ) {
        use crate::hir::destruc::Destruc;

        match destruc {
            Destruc::Scalar(_, scalar) => Self::destruc_scalar(var_values, scalar, value),
            Destruc::List(span, list) => Self::destruc_list(b, *span, var_values, list, value),
        }
    }

    fn destruc_source_name(destruc: &hir::destruc::Destruc<ty::Poly>) -> Option<&str> {
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
        hir_let: &hir::Let<ty::Poly>,
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

        // Include any captured values from the closure in `local_values`
        dcx.local_values.extend(
            arret_fun
                .captures
                .iter()
                .map(|(var_id, value)| (*var_id, value.clone())),
        );

        self.eval_expr(dcx, b, &fun_expr.body_expr)
    }

    fn eval_ty_pred_app(
        &mut self,
        dcx: &mut DefCtx,
        b: &mut Option<Builder>,
        span: Span,
        test_mono: &ty::Mono,
        arg_list_value: Value,
    ) -> Result<Value> {
        use crate::mir::value::mono_for_value;
        use crate::ty::pred::InterpretedPred;

        let mut arg_list_iter = arg_list_value.into_list_iter();
        let subject_value = arg_list_iter.next_unchecked(b, span);

        let subject_mono = mono_for_value(self.runtime_task.heap().interner(), &subject_value);

        match ty::pred::interpret_ty_refs(&dcx.tvars, &subject_mono, test_mono) {
            InterpretedPred::Static(value) => {
                Ok(Value::Const(boxed::Bool::singleton_ref(value).as_any_ref()))
            }
            InterpretedPred::Dynamic(_, _) => unimplemented!("Runtime type check"),
        }
    }

    pub fn rust_fun_to_jit_boxed(&mut self, rust_fun: Rc<hir::rfi::Fun>) -> Gc<boxed::FunThunk> {
        use std::ptr;

        let closure = ptr::null();
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

            // Create some names
            let inner_symbol = ffi::CString::new(rust_fun.symbol()).unwrap();

            // Add the inner symbol
            self.thunk_jit.add_symbol(
                inner_symbol.as_bytes_with_nul(),
                rust_fun.entry_point() as u64,
            );

            let ops_fun = ops_for_rust_fun_thunk(self, EMPTY_SPAN, rust_fun);
            let address = self.thunk_jit.compile_fun(&mut self.thunk_gen, &ops_fun);

            mem::transmute(address)
        };

        self.rust_fun_thunks.insert(rust_fun.entry_point(), thunk);
        thunk
    }

    pub fn rust_fun_to_thunk_reg(
        &mut self,
        b: &mut Builder,
        span: Span,
        rust_fun: &hir::rfi::Fun,
    ) -> ops::RegId {
        use crate::mir::ops::*;
        use crate::mir::rust_fun::ops_for_rust_fun_thunk;

        let ops_fun = ops_for_rust_fun_thunk(self, span, rust_fun);

        let built_fun_id = ops::BuiltFunId::new_entry_id(&mut self.built_funs, ops_fun);
        b.push_reg(
            span,
            OpKind::ConstBoxedFunThunk,
            ops::Callee::BuiltFun(built_fun_id),
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
        rust_fun: &hir::rfi::Fun,
        arg_list_value: Value,
    ) -> Result<Value> {
        use crate::mir::intrinsic;
        use crate::mir::rust_fun::build_rust_fun_app;
        use crate::mir::value::to_const::value_to_const;
        use std::ptr;

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
                    thunk(runtime_task, ptr::null(), boxed_arg_list)
                });
            }
        }

        if let Some(b) = b {
            Ok(build_rust_fun_app(self, b, span, rust_fun, arg_list_value))
        } else {
            panic!("Need builder for non-const function application");
        }
    }

    fn eval_const_fun_thunk_app(
        &mut self,
        dcx: &mut DefCtx,
        b: &mut Option<Builder>,
        span: Span,
        fun_thunk: Gc<boxed::FunThunk>,
        arg_list_value: Value,
    ) -> Result<Value> {
        use crate::mir::value::to_const::value_to_const;

        if let Some(actual_value) = self.thunk_fun_values.get(&fun_thunk) {
            return self.eval_value_app(dcx, b, span, &actual_value.clone(), arg_list_value);
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

    fn eval_reg_fun_thunk_app(
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

        let task_reg = b.push_reg(span, OpKind::CurrentTask, ());
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
                args: vec![task_reg, closure_reg, arg_list_reg.into()].into_boxed_slice(),
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
        fun_value: &Value,
        arg_list_value: Value,
    ) -> Result<Value> {
        match fun_value {
            Value::ArretFun(arret_fun) => {
                self.eval_arret_fun_app(dcx, b, span, &arret_fun, arg_list_value)
            }
            Value::RustFun(rust_fun) => self.eval_rust_fun_app(b, span, &rust_fun, arg_list_value),
            Value::TyPred(test_poly) => {
                self.eval_ty_pred_app(dcx, b, span, test_poly.as_ref(), arg_list_value)
            }
            Value::Const(boxed_fun) => match boxed_fun.as_subtype() {
                boxed::AnySubtype::FunThunk(fun_thunk) => self.eval_const_fun_thunk_app(
                    dcx,
                    b,
                    span,
                    unsafe { Gc::new(fun_thunk) },
                    arg_list_value,
                ),
                other => unimplemented!("applying boxed function value type: {:?}", other),
            },
            Value::Reg(reg_value) => {
                if let Some(b) = b {
                    Ok(self.eval_reg_fun_thunk_app(b, span, reg_value, &arg_list_value))
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
        app: &hir::App<ty::Poly>,
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

        let arg_list_value = Value::List(fixed_values.into_boxed_slice(), rest_value);
        self.eval_value_app(dcx, b, span, &fun_value, arg_list_value)
    }

    fn eval_cond(
        &mut self,
        dcx: &mut DefCtx,
        b: &mut Option<Builder>,
        cond: &hir::Cond<ty::Poly>,
    ) -> Result<Value> {
        let test_value = self.eval_expr(dcx, b, &cond.test_expr)?;

        let test_bool = match test_value {
            Value::Const(any_ref) => {
                let bool_ref = any_ref.downcast_ref::<boxed::Bool>().unwrap();
                bool_ref.as_bool()
            }
            _ => {
                unimplemented!("Non-constant condition");
            }
        };

        if test_bool {
            self.eval_expr(dcx, b, &cond.true_expr)
        } else {
            self.eval_expr(dcx, b, &cond.false_expr)
        }
    }

    fn eval_arret_fun(
        &mut self,
        dcx: &mut DefCtx,
        span: Span,
        fun_expr: Rc<hir::Fun<ty::Poly>>,
        source_name: Option<&str>,
    ) -> Value {
        let mut captures = HashMap::new();

        // Only process captures if there are local values. This is to avoid visiting the function
        // body when capturing isn't possible.
        if !dcx.local_values.is_empty() {
            // Look for references to variables inside the function
            hir::visitor::visit_exprs(&fun_expr.body_expr, &mut |expr| {
                if let hir::Expr::Ref(_, var_id) = expr {
                    // Avoiding cloning the value if we've already captured
                    if !captures.contains_key(var_id) {
                        if let Some(value) = dcx.local_values.get(var_id) {
                            // Local value is referenced; capture
                            captures.insert(*var_id, value.clone());
                        }
                    }
                }
            });
        }

        Value::ArretFun(value::ArretFun {
            span,
            source_name: source_name.map(|source_name| source_name.to_owned()),
            fun_expr,
            captures,
        })
    }

    pub fn arret_fun_to_jit_boxed(&mut self, arret_fun: &value::ArretFun) -> Gc<boxed::FunThunk> {
        use std::{mem, ptr};

        let wanted_abi = ops::FunABI::thunk_abi();
        let ops_fun = self
            .ops_for_arret_fun(&arret_fun, wanted_abi, true)
            .expect("error during arret fun boxing");

        let address = self.thunk_jit.compile_fun(&mut self.thunk_gen, &ops_fun);
        let entry = unsafe { mem::transmute(address) };

        let new_boxed = boxed::FunThunk::new(self, (ptr::null(), entry));

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
        use crate::mir::ops::*;
        use crate::mir::optimise::optimise_fun;

        let wanted_abi = FunABI::thunk_abi();

        let unopt_fun = self
            .ops_for_arret_fun(&arret_fun, wanted_abi, true)
            .expect("error during arret fun boxing");

        let opt_fun = optimise_fun(unopt_fun);

        let built_fun_id = ops::BuiltFunId::new_entry_id(&mut self.built_funs, opt_fun);
        b.push_reg(
            span,
            OpKind::ConstBoxedFunThunk,
            ops::Callee::BuiltFun(built_fun_id),
        )
    }

    fn ops_for_arret_fun(
        &mut self,
        arret_fun: &value::ArretFun,
        wanted_abi: ops::FunABI,
        has_rest: bool,
    ) -> Result<ops::Fun> {
        use crate::mir::value::build_reg::value_to_reg;
        use runtime::abitype;

        // TODO: Make an actual DefCtx
        let mut dcx = DefCtx::new();
        let mut b = Builder::new();
        let span = arret_fun.span;

        let mut abi_params_iter = wanted_abi.params.iter();
        let rest_reg_value = if has_rest {
            let abi_type = abi_params_iter.next_back().unwrap();

            Some(Rc::new(value::RegValue {
                reg: b.alloc_reg(),
                abi_type: abi_type.clone(),
            }))
        } else {
            None
        };

        let fixed_reg_values = abi_params_iter
            .map(|abi_type| {
                Rc::new(value::RegValue {
                    reg: b.alloc_reg(),
                    abi_type: abi_type.clone(),
                })
            })
            .collect::<Vec<Rc<value::RegValue>>>();

        let param_regs = fixed_reg_values
            .iter()
            .chain(rest_reg_value.iter())
            .map(|reg_value| reg_value.reg)
            .collect::<Vec<ops::RegId>>()
            .into_boxed_slice();

        let arg_list_value = Value::List(
            fixed_reg_values
                .into_iter()
                .map(Value::Reg)
                .collect::<Vec<Value>>()
                .into_boxed_slice(),
            rest_reg_value.map(|reg_value| Box::new(Value::Reg(reg_value))),
        );

        let mut some_b = Some(b);
        let result_value =
            self.eval_arret_fun_app(&mut dcx, &mut some_b, span, arret_fun, arg_list_value)?;

        let mut b = some_b.unwrap();
        match &wanted_abi.ret {
            abitype::RetABIType::Inhabited(abi_type) => {
                let ret_reg = value_to_reg(self, &mut b, span, &result_value, abi_type);
                b.push(span, ops::OpKind::Ret(ret_reg.into()));
            }
            abitype::RetABIType::Never => {
                b.push(span, ops::OpKind::Unreachable);
            }
            abitype::RetABIType::Void => {
                b.push(span, ops::OpKind::RetVoid);
            }
        }

        Ok(ops::Fun {
            source_name: arret_fun.source_name.clone(),
            abi: wanted_abi,
            params: param_regs,
            ops: b.into_ops(),
        })
    }

    pub fn consume_def(&mut self, def: hir::Def<ty::Poly>) -> Result<()> {
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
            hir::Expr::TyPred(_, test_poly) => {
                let test_mono =
                    ty::subst::monomorphise(&dcx.pvar_purities, &dcx.tvar_types, test_poly);
                Ok(Value::TyPred(Rc::new(test_mono)))
            }
            hir::Expr::Ref(_, var_id) => Ok(self.eval_ref(dcx, *var_id)),
            hir::Expr::Let(_, hir_let) => self.eval_let(dcx, b, hir_let),
            hir::Expr::App(span, app) => self.eval_app(dcx, b, *span, app),
            hir::Expr::MacroExpand(span, expr) => self
                .eval_expr(dcx, b, expr)
                .map_err(|err| err.with_macro_invocation_span(*span)),
            hir::Expr::Cond(_, cond) => self.eval_cond(dcx, b, cond),
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

        let main_abi = ops::FunABI {
            takes_task: true,
            takes_closure: false,
            params: Box::new([]),
            ret: abitype::RetABIType::Void,
        };

        Ok(BuiltProgram {
            main: self.ops_for_arret_fun(&main_arret_fun, main_abi, false)?,
            funs: self.built_funs,
        })
    }

    pub fn value_to_const(&mut self, value: &Value) -> Option<Gc<boxed::Any>> {
        use crate::mir::value::to_const::value_to_const;
        value_to_const(self, value)
    }
}

impl Default for EvalHirCtx {
    fn default() -> EvalHirCtx {
        EvalHirCtx::new()
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
