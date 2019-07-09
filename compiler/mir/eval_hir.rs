use std::collections::HashMap;
use std::rc::Rc;
use std::{alloc, ffi, panic};

use arret_runtime::boxed;
use arret_runtime::boxed::prelude::*;
use arret_runtime::boxed::refs::Gc;
use arret_runtime::callback::EntryPointABIType as CallbackEntryPointABIType;
use arret_runtime::intern::{AsInterner, Interner};

use arret_runtime_syntax::reader;
use arret_syntax::datum::{DataStr, Datum};
use arret_syntax::span::{Span, EMPTY_SPAN};

use crate::codegen;
use crate::hir;
use crate::mir::builder::{Builder, BuiltReg, TryToBuilder};
use crate::mir::error::{Error, Result};
use crate::mir::inliner;
use crate::mir::ops;
use crate::mir::polymorph::PolymorphABI;
use crate::mir::value;
use crate::mir::value::synthetic_fun::SyntheticFuns;
use crate::mir::{Expr, Value};
use crate::rfi;
use crate::ty;
use crate::ty::purity::Purity;
use crate::ty::record;
use crate::ty::ty_args::TyArgs;
use crate::ty::Ty;

#[derive(PartialEq, Eq, Hash)]
struct RustFunKey {
    symbol: &'static str,
    polymorph_abi: PolymorphABI,
}

#[derive(PartialEq, Eq, Hash)]
struct ArretFunKey {
    arret_fun_id: value::ArretFunId,
    polymorph_abi: PolymorphABI,
}

#[derive(PartialEq, Eq)]
pub struct EvaledRecordClass {
    pub jit_record_class_id: boxed::RecordClassId,
    pub jit_data_layout: alloc::Layout,
    pub record_struct: ops::RecordStructId,
}

pub struct EvalHirCtx {
    runtime_task: arret_runtime::task::Task,
    global_values: HashMap<hir::VarId, Value>,

    private_fun_id_counter: ops::PrivateFunIdCounter,
    private_funs: HashMap<ops::PrivateFunId, ops::Fun>,
    rust_funs: HashMap<RustFunKey, ops::PrivateFunId>,
    arret_funs: HashMap<ArretFunKey, ops::PrivateFunId>,
    synthetic_funs: SyntheticFuns,

    rust_fun_thunks: HashMap<usize, boxed::ThunkEntry>,
    arret_fun_thunks: HashMap<value::ArretFunId, boxed::ThunkEntry>,

    // This uses pointers because `FunThunk` is always inequal to itself
    thunk_fun_values: HashMap<*const boxed::FunThunk, Value>,
    thunk_jit: codegen::jit::JITCtx,

    record_class_for_cons: HashMap<record::ConsId, EvaledRecordClass>,
    cons_for_jit_record_class_id: HashMap<boxed::RecordClassId, record::ConsId>,
}

pub struct FunCtx {
    mono_ty_args: TyArgs<ty::Mono>,
    local_values: HashMap<hir::VarId, Value>,

    pub(super) inliner_stack: inliner::ApplyStack,
}

impl FunCtx {
    pub fn new() -> FunCtx {
        FunCtx::with_mono_ty_args(TyArgs::empty())
    }

    pub fn with_mono_ty_args(mono_ty_args: TyArgs<ty::Mono>) -> FunCtx {
        FunCtx {
            mono_ty_args,
            local_values: HashMap::new(),

            inliner_stack: inliner::ApplyStack::new(),
        }
    }

    pub fn monomorphise(&self, poly: &ty::Ref<ty::Poly>) -> ty::Ref<ty::Mono> {
        ty::subst::monomorphise(&self.mono_ty_args, poly)
    }
}

struct BuiltCondBranch {
    b: Builder,
    result: Result<Value>,
}

pub struct BuiltProgram {
    pub main: ops::Fun,
    pub private_funs: HashMap<ops::PrivateFunId, ops::Fun>,
}

impl BuiltProgram {
    /// Returns true if the program always executes successfully with no output or side effects
    pub fn is_empty(&self) -> bool {
        match self.main.ops.as_ref() {
            [ops::Op {
                kind: ops::OpKind::RetVoid,
                ..
            }] => true,
            _ => false,
        }
    }
}

#[derive(Clone)]
pub(super) struct ApplyArgs<'tyargs> {
    ty_args: &'tyargs TyArgs<ty::Poly>,
    pub(super) list_value: Value,
}

/// Merge poly type args in to existing mono type args
///
/// This is used when applying a polymorphic function. The `subst_with` are used to monomorphise
/// the `apply_ty_args` which are then added to the existing `scope` and returned.
fn merge_apply_ty_args_into_scope(
    scope: &TyArgs<ty::Mono>,
    apply_ty_args: &TyArgs<ty::Poly>,
    subst_with: &TyArgs<ty::Mono>,
) -> TyArgs<ty::Mono> {
    use crate::ty::subst;

    let pvar_purities = scope
        .pvar_purities()
        .iter()
        .chain(apply_ty_args.pvar_purities().iter())
        .map(|(pvar, v)| (pvar.clone(), v.clone()))
        .collect();

    let tvar_types = scope
        .tvar_types()
        .iter()
        .map(|(tvar, mono)| (tvar.clone(), mono.clone()))
        .chain(apply_ty_args.tvar_types().iter().map(|(tvar, poly_type)| {
            let mono_ty = subst::monomorphise(subst_with, poly_type);
            (tvar.clone(), mono_ty)
        }))
        .collect();

    TyArgs::new(pvar_purities, tvar_types)
}

impl Default for FunCtx {
    fn default() -> FunCtx {
        FunCtx::new()
    }
}

impl EvalHirCtx {
    pub fn new(optimising: bool) -> EvalHirCtx {
        let thunk_jit = codegen::jit::JITCtx::new(optimising);

        EvalHirCtx {
            runtime_task: arret_runtime::task::Task::new(),
            global_values: HashMap::new(),

            private_fun_id_counter: ops::PrivateFunIdCounter::new(),
            private_funs: HashMap::new(),
            rust_funs: HashMap::new(),
            arret_funs: HashMap::new(),
            synthetic_funs: SyntheticFuns::new(),

            rust_fun_thunks: HashMap::new(),
            arret_fun_thunks: HashMap::new(),

            thunk_fun_values: HashMap::new(),
            thunk_jit,

            record_class_for_cons: HashMap::new(),
            cons_for_jit_record_class_id: HashMap::new(),
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
        let mut iter = value.into_unsized_list_iter();

        for fixed_destruc in list.fixed() {
            let value = iter.next_unchecked(b, span);
            Self::destruc_value(b, var_values, fixed_destruc, value);
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

    fn destruc_source_name(destruc: &hir::destruc::Destruc<hir::Inferred>) -> Option<&DataStr> {
        use crate::hir::destruc::Destruc;

        match destruc {
            Destruc::Scalar(_, scalar) => Some(scalar.source_name()),
            Destruc::List(_, _) => None,
        }
    }

    fn eval_ref(&self, fcx: &FunCtx, var_id: hir::VarId) -> Value {
        fcx.local_values
            .get(&var_id)
            .unwrap_or_else(|| &self.global_values[&var_id])
            .clone()
    }

    fn eval_do(
        &mut self,
        fcx: &mut FunCtx,
        b: &mut Option<Builder>,
        exprs: &[Expr],
    ) -> Result<Value> {
        let initial_value = Value::List(Box::new([]), None);

        exprs
            .iter()
            .try_fold(initial_value, |_, expr| self.eval_expr(fcx, b, expr))
    }

    fn eval_let(
        &mut self,
        fcx: &mut FunCtx,
        b: &mut Option<Builder>,
        hir_let: &hir::Let<hir::Inferred>,
    ) -> Result<Value> {
        let source_name = Self::destruc_source_name(&hir_let.destruc);
        let value = self.eval_expr_with_source_name(fcx, b, &hir_let.value_expr, source_name)?;

        Self::destruc_value(b, &mut fcx.local_values, &hir_let.destruc, value);

        self.eval_expr(fcx, b, &hir_let.body_expr)
    }

    fn eval_lit(&mut self, literal: &Datum) -> Value {
        reader::box_syntax_datum(self, literal).into()
    }

    pub(super) fn synthetic_funs(&mut self) -> &mut SyntheticFuns {
        &mut self.synthetic_funs
    }

    pub(super) fn build_arret_fun_app(
        &mut self,
        b: &mut Builder,
        span: Span,
        ret_ty: &ty::Ref<ty::Mono>,
        arret_fun: &value::ArretFun,
        apply_args: &ApplyArgs<'_>,
    ) -> Result<Value> {
        use crate::mir::app_purity::fun_app_purity;
        use crate::mir::arg_list::build_save_arg_list_to_regs;
        use crate::mir::closure;
        use crate::mir::ops::*;
        use crate::mir::polymorph::polymorph_abi_for_arg_list_value;
        use crate::mir::ret_value::ret_reg_to_value;

        let ApplyArgs {
            list_value: arg_list_value,
            ty_args: apply_ty_args,
        } = apply_args;

        let closure_reg = closure::save_to_closure_reg(self, b, span, &arret_fun.closure());

        let wanted_abi =
            polymorph_abi_for_arg_list_value(closure_reg.is_some(), arg_list_value, ret_ty);
        let ret_abi = wanted_abi.ops_abi.ret.clone();

        let mut arg_regs: Vec<RegId> = vec![];
        if let Some(closure_reg) = closure_reg {
            arg_regs.push(closure_reg.into());
        }

        arg_regs.extend(build_save_arg_list_to_regs(
            self,
            b,
            span,
            arg_list_value.clone(),
            wanted_abi.arret_fixed_params(),
            wanted_abi.arret_rest_param(),
        ));

        let private_fun_id = self.id_for_arret_fun(arret_fun, wanted_abi);

        let outer_purities = arret_fun.env_ty_args().pvar_purities();
        let apply_purities = apply_ty_args.pvar_purities();
        let fun_expr = arret_fun.fun_expr();

        let app_purity = fun_app_purity(
            outer_purities,
            apply_purities,
            &fun_expr.purity,
            &fun_expr.ret_ty,
        );

        let ret_reg = b.push_reg(
            span,
            OpKind::Call,
            CallOp {
                callee: Callee::PrivateFun(private_fun_id),
                impure: app_purity == Purity::Impure,
                args: arg_regs.into_boxed_slice(),
            },
        );

        ret_reg_to_value(ret_reg, ret_abi)
    }

    pub(super) fn inline_arret_fun_app(
        &mut self,
        outer_fcx: &FunCtx,
        b: &mut Option<Builder>,
        span: Span,
        arret_fun: &value::ArretFun,
        apply_args: ApplyArgs<'_>,
        inliner_stack: inliner::ApplyStack,
    ) -> Result<Value> {
        let fun_expr = arret_fun.fun_expr();

        let mut inner_fcx = FunCtx {
            mono_ty_args: merge_apply_ty_args_into_scope(
                arret_fun.env_ty_args(),
                apply_args.ty_args,
                &outer_fcx.mono_ty_args,
            ),
            local_values: outer_fcx.local_values.clone(),

            inliner_stack,
        };

        Self::destruc_list(
            b,
            span,
            &mut inner_fcx.local_values,
            &fun_expr.params,
            apply_args.list_value,
        );

        self.eval_expr(&mut inner_fcx, b, &fun_expr.body_expr)
    }

    fn eval_arret_fun_app(
        &mut self,
        fcx: &mut FunCtx,
        b: &mut Option<Builder>,
        span: Span,
        ret_ty: &ty::Ref<ty::Mono>,
        arret_fun: &value::ArretFun,
        apply_args: ApplyArgs<'_>,
    ) -> Result<Value> {
        if arret_fun.has_multiple_usages() {
            if let Some(outer_b) = b {
                return inliner::cond_inline(
                    self, fcx, outer_b, span, ret_ty, arret_fun, apply_args,
                );
            }
        }

        // Always inline
        self.inline_arret_fun_app(
            fcx,
            b,
            span,
            arret_fun,
            apply_args,
            inliner::ApplyStack::new(),
        )
    }

    fn eval_ty_pred_app(
        &mut self,
        b: &mut Option<Builder>,
        span: Span,
        arg_list_value: &Value,
        test_ty: &ty::pred::TestTy,
    ) -> Value {
        use crate::mir::typred::eval_ty_pred;

        let subject_value = arg_list_value.unsized_list_iter().next_unchecked(b, span);
        eval_ty_pred(self, b, span, &subject_value, test_ty)
    }

    fn eval_eq_pred_app(
        &mut self,
        b: &mut impl TryToBuilder,
        span: Span,
        arg_list_value: &Value,
    ) -> Value {
        use crate::mir::equality::eval_equality;

        let mut iter = arg_list_value.unsized_list_iter();

        let left_value = iter.next_unchecked(b, span);
        let right_value = iter.next_unchecked(b, span);

        eval_equality(self, b, span, &left_value, &right_value).into()
    }

    fn eval_record_cons_app(
        &mut self,
        b: &mut Option<Builder>,
        span: Span,
        record_cons: &record::ConsId,
        arg_list_value: &Value,
    ) -> Value {
        let mut iter = arg_list_value.unsized_list_iter();

        let field_values = record_cons
            .fields()
            .iter()
            .map(|_| iter.next_unchecked(b, span))
            .collect();

        Value::Record(record_cons.clone(), field_values)
    }

    fn eval_field_accessor_app(
        &mut self,
        b: &mut Option<Builder>,
        span: Span,
        record_cons: &record::ConsId,
        field_index: usize,
        arg_list_value: &Value,
    ) -> Value {
        use crate::mir::record_field::load_record_field;

        let mut iter = arg_list_value.unsized_list_iter();
        let record_value = iter.next_unchecked(b, span);

        load_record_field(self, b, span, record_cons, &record_value, field_index)
    }

    pub fn rust_fun_to_jit_boxed(&mut self, rust_fun: Rc<rfi::Fun>) -> Gc<boxed::FunThunk> {
        let closure = boxed::NIL_INSTANCE.as_any_ref();
        let entry = self.jit_thunk_for_rust_fun(&rust_fun);
        let new_boxed = boxed::FunThunk::new(self, closure, entry);

        let rust_fun_value = Value::RustFun(rust_fun);
        self.thunk_fun_values
            .insert(new_boxed.as_ptr(), rust_fun_value);

        new_boxed
    }

    fn jit_thunk_for_rust_fun(&mut self, rust_fun: &rfi::Fun) -> boxed::ThunkEntry {
        // Create a dynamic thunk to this Rust function if it doesn't exist
        if let Some(thunk) = self.rust_fun_thunks.get(&rust_fun.entry_point()) {
            return *thunk;
        }

        let thunk = unsafe {
            use crate::mir::rust_fun::ops_for_rust_fun;
            use std::mem;

            let wanted_abi = PolymorphABI::thunk_abi();
            let ops_fun = ops_for_rust_fun(self, rust_fun, wanted_abi);
            let address = self.thunk_jit.compile_fun(
                &self.private_funs,
                self.runtime_task.heap_mut().type_info_mut().interner_mut(),
                &ops_fun,
            );

            mem::transmute(address as usize)
        };

        self.rust_fun_thunks.insert(rust_fun.entry_point(), thunk);
        thunk
    }

    /// Ensures the passed `RustFun` is known by the JIT
    ///
    /// This can be called multiple times with the same Rust fun. Calling it with a fun that's
    /// never used by the JIT is harmless
    pub fn register_rust_fun_with_jit(&mut self, rust_fun: &rfi::Fun) {
        let symbol_cstring = ffi::CString::new(rust_fun.symbol()).unwrap();

        // Add the inner symbol
        self.thunk_jit.add_symbol(
            symbol_cstring.as_bytes_with_nul(),
            rust_fun.entry_point() as u64,
        );
    }

    /// Returns a private fun ID for the wanted Rust fun and ABI
    ///
    /// This will return a cached ID if available
    fn id_for_rust_fun(
        &mut self,
        rust_fun: &rfi::Fun,
        wanted_abi: PolymorphABI,
    ) -> ops::PrivateFunId {
        use crate::mir::rust_fun::ops_for_rust_fun;

        let rust_fun_key = RustFunKey {
            symbol: rust_fun.symbol(),
            polymorph_abi: wanted_abi.clone(),
        };

        if let Some(private_fun_id) = self.rust_funs.get(&rust_fun_key) {
            return *private_fun_id;
        }

        let private_fun_id = self.private_fun_id_counter.alloc();
        self.rust_funs.insert(rust_fun_key, private_fun_id);

        let ops_fun = ops_for_rust_fun(self, rust_fun, wanted_abi);
        self.private_funs.insert(private_fun_id, ops_fun);

        private_fun_id
    }

    pub fn rust_fun_to_thunk_reg(
        &mut self,
        b: &mut Builder,
        span: Span,
        rust_fun: &rfi::Fun,
    ) -> BuiltReg {
        use crate::mir::ops::*;
        use arret_runtime::abitype;

        let wanted_abi = PolymorphABI::thunk_abi();
        let private_fun_id = self.id_for_rust_fun(rust_fun, wanted_abi);

        let nil_reg = b.push_reg(span, OpKind::ConstBoxedNil, ());
        let closure_reg = b.cast_boxed(span, nil_reg, abitype::BoxedABIType::Any);

        b.push_reg(
            span,
            OpKind::ConstBoxedFunThunk,
            BoxFunThunkOp {
                closure_reg: closure_reg.into(),
                callee: ops::Callee::PrivateFun(private_fun_id),
            },
        )
    }

    pub fn rust_fun_to_callback_reg(
        &mut self,
        b: &mut Builder,
        span: Span,
        rust_fun: &rfi::Fun,
        entry_point_abi: &CallbackEntryPointABIType,
    ) -> BuiltReg {
        use crate::mir::ops::*;
        use arret_runtime::abitype;

        let wanted_abi = entry_point_abi.clone().into();
        let private_fun_id = self.id_for_rust_fun(rust_fun, wanted_abi);

        let nil_reg = b.push_reg(span, OpKind::ConstBoxedNil, ());
        let closure_reg = b.cast_boxed(span, nil_reg, abitype::BoxedABIType::Any);

        b.push_reg(
            span,
            OpKind::MakeCallback,
            MakeCallbackOp {
                closure_reg: closure_reg.into(),
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
                use crate::mir::error;

                let message = if let Some(message) = err.downcast_ref::<String>() {
                    message.clone()
                } else {
                    "Unexpected panic type".to_owned()
                };

                Error::Panic(error::Panic::new(span, message))
            })
    }

    fn eval_rust_fun_app(
        &mut self,
        fcx: &mut FunCtx,
        b: &mut Option<Builder>,
        span: Span,
        ret_ty: &ty::Ref<ty::Mono>,
        rust_fun: &rfi::Fun,
        apply_args: ApplyArgs<'_>,
    ) -> Result<Value> {
        use crate::mir::app_purity::fun_app_purity;
        use crate::mir::intrinsic;
        use crate::mir::rust_fun::build_rust_fun_app;
        use crate::mir::value::to_const::value_to_const;

        let ApplyArgs {
            list_value: arg_list_value,
            ty_args: apply_ty_args,
        } = apply_args;

        let outer_purities = fcx.mono_ty_args.pvar_purities();
        let apply_purities = apply_ty_args.pvar_purities();
        let arret_fun_type = rust_fun.arret_fun_type();

        let call_purity = fun_app_purity(
            outer_purities,
            apply_purities,
            arret_fun_type.purity(),
            arret_fun_type.ret(),
        );

        let can_const_eval = b.is_none() || (call_purity == Purity::Pure);

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

                let native_result = Self::call_native_fun(span, || {
                    let closure = boxed::NIL_INSTANCE.as_any_ref();
                    thunk(runtime_task, closure, boxed_arg_list)
                });

                // If we receive a panic while building we want to still build the function call.
                // This `panic` could be in conditional code and we want to build all the
                // expressions before this panic for their side effects.
                if native_result.is_ok() || !b.is_some() {
                    return native_result;
                }
            }
        }

        if let Some(b) = b {
            if let Some(intrinsic_name) = rust_fun.intrinsic_name() {
                if let Some(value) =
                    intrinsic::try_build(self, b, span, intrinsic_name, &arg_list_value)?
                {
                    return Ok(value);
                }
            }

            build_rust_fun_app(self, b, span, ret_ty, rust_fun, call_purity, arg_list_value)
        } else {
            panic!("Need builder for non-const function application");
        }
    }

    fn eval_const_fun_thunk_app(
        &mut self,
        fcx: &mut FunCtx,
        b: &mut Option<Builder>,
        span: Span,
        ret_ty: &ty::Ref<ty::Mono>,
        fun_thunk: Gc<boxed::FunThunk>,
        apply_args: ApplyArgs<'_>,
    ) -> Result<Value> {
        use crate::mir::value::to_const::value_to_const;

        if let Some(actual_value) = self.thunk_fun_values.get(&fun_thunk.as_ptr()) {
            let actual_value = actual_value.clone();
            return self.eval_value_app(fcx, b, span, ret_ty, &actual_value, apply_args);
        }

        if b.is_some() {
            panic!("attempt to apply unknown fun thunk during compile phase");
        }

        let const_arg_list =
            value_to_const(self, &apply_args.list_value).expect("non-constant value during apply");

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
        use arret_runtime::abitype;

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

        let closure_reg = b.push_reg(span, OpKind::LoadBoxedFunThunkClosure, fun_thunk_reg.into());
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
                callee: Callee::BoxedFunThunk(fun_thunk_reg.into()),
                impure: true,
                args: Box::new([closure_reg.into(), arg_list_reg.into()]),
            },
        );

        value::RegValue::new(ret_reg, abitype::BoxedABIType::Any.into()).into()
    }

    fn eval_value_app(
        &mut self,
        fcx: &mut FunCtx,
        b: &mut Option<Builder>,
        span: Span,
        ret_ty: &ty::Ref<ty::Mono>,
        fun_value: &Value,
        apply_args: ApplyArgs<'_>,
    ) -> Result<Value> {
        match fun_value {
            Value::ArretFun(arret_fun) => {
                use crate::mir::closure;

                closure::load_from_current_fun(&mut fcx.local_values, arret_fun.closure());
                self.eval_arret_fun_app(fcx, b, span, ret_ty, &arret_fun, apply_args)
            }
            Value::RustFun(rust_fun) => {
                self.eval_rust_fun_app(fcx, b, span, ret_ty, &rust_fun, apply_args)
            }
            Value::TyPred(test_ty) => {
                Ok(self.eval_ty_pred_app(b, span, &apply_args.list_value, test_ty))
            }
            Value::EqPred => Ok(self.eval_eq_pred_app(b, span, &apply_args.list_value)),
            Value::RecordCons(record_cons) => {
                Ok(self.eval_record_cons_app(b, span, record_cons, &apply_args.list_value))
            }
            Value::FieldAccessor(record_cons, field_index) => Ok(self.eval_field_accessor_app(
                b,
                span,
                record_cons,
                *field_index,
                &apply_args.list_value,
            )),
            Value::Const(boxed_fun) => match boxed_fun.as_subtype() {
                boxed::AnySubtype::FunThunk(fun_thunk) => self.eval_const_fun_thunk_app(
                    fcx,
                    b,
                    span,
                    ret_ty,
                    unsafe { Gc::new(fun_thunk) },
                    apply_args,
                ),
                other => unimplemented!("applying boxed function value type: {:?}", other),
            },
            Value::Reg(reg_value) => {
                if let Some(b) = b {
                    Ok(self.build_reg_fun_thunk_app(b, span, reg_value, &apply_args.list_value))
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
        fcx: &mut FunCtx,
        b: &mut Option<Builder>,
        result_ty: &ty::Ref<ty::Poly>,
        app: &hir::App<hir::Inferred>,
    ) -> Result<Value> {
        let fun_value = self.eval_expr(fcx, b, &app.fun_expr)?;

        let fixed_values = app
            .fixed_arg_exprs
            .iter()
            .map(|arg| self.eval_expr(fcx, b, arg))
            .collect::<Result<Box<[Value]>>>()?;

        let rest_value = match &app.rest_arg_expr {
            Some(rest_arg) => Some(Box::new(self.eval_expr(fcx, b, rest_arg)?)),
            None => None,
        };

        let ret_ty = fcx.monomorphise(result_ty);
        let arg_list_value = Value::List(fixed_values, rest_value);
        self.eval_value_app(
            fcx,
            b,
            app.span,
            &ret_ty,
            &fun_value,
            ApplyArgs {
                ty_args: &app.ty_args,
                list_value: arg_list_value,
            },
        )
    }

    fn eval_cond(
        &mut self,
        fcx: &mut FunCtx,
        b: &mut Option<Builder>,
        cond: &hir::Cond<hir::Inferred>,
    ) -> Result<Value> {
        let test_value = self.eval_expr(fcx, b, &cond.test_expr)?;

        match test_value {
            Value::Const(any_ref) => {
                let bool_ref = any_ref.downcast_ref::<boxed::Bool>().unwrap();

                if bool_ref.as_bool() {
                    self.eval_expr(fcx, b, &cond.true_expr)
                } else {
                    self.eval_expr(fcx, b, &cond.false_expr)
                }
            }
            dynamic_value => {
                if let Some(b) = b {
                    self.build_cond(fcx, b, &dynamic_value, cond)
                } else {
                    panic!("need builder for dynamic cond");
                }
            }
        }
    }

    fn build_cond_branch(
        &mut self,
        fcx: &mut FunCtx,
        branch_expr: &hir::Expr<hir::Inferred>,
    ) -> Result<BuiltCondBranch> {
        let b = Builder::new();

        let mut some_b = Some(b);
        let result = self.eval_expr(fcx, &mut some_b, branch_expr);
        let b = some_b.unwrap();

        Ok(BuiltCondBranch { b, result })
    }

    fn build_cond(
        &mut self,
        fcx: &mut FunCtx,
        b: &mut Builder,
        test_value: &Value,
        cond: &hir::Cond<hir::Inferred>,
    ) -> Result<Value> {
        use crate::mir::equality::values_statically_equal;
        use crate::mir::value::build_reg::value_to_reg;
        use crate::mir::value::plan_phi::*;
        use crate::mir::value::types::{known_record_cons_for_value, possible_type_tags_for_value};
        use arret_runtime::abitype;

        let span = cond.span;
        let test_reg = value_to_reg(self, b, span, test_value, &abitype::ABIType::Bool);

        let mut built_true = self.build_cond_branch(fcx, &cond.true_expr)?;
        let mut built_false = self.build_cond_branch(fcx, &cond.false_expr)?;

        let output_value;
        let reg_phi;

        match (built_true.result, built_false.result) {
            (Ok(true_value), Ok(false_value)) => {
                if values_statically_equal(self, &true_value, &false_value) == Some(true) {
                    output_value = true_value;
                    reg_phi = None;
                } else {
                    let phi_abi_type = plan_phi_abi_type(&true_value, &false_value);

                    let true_result_reg =
                        value_to_reg(self, &mut built_true.b, span, &true_value, &phi_abi_type);

                    let false_result_reg =
                        value_to_reg(self, &mut built_false.b, span, &false_value, &phi_abi_type);

                    let output_reg = b.alloc_local();

                    let possible_type_tags = possible_type_tags_for_value(&true_value)
                        | possible_type_tags_for_value(&false_value);

                    let true_record_cons = known_record_cons_for_value(self, &true_value);
                    let false_record_cons = known_record_cons_for_value(self, &false_value);
                    let known_record_cons = if true_record_cons == false_record_cons {
                        true_record_cons.cloned()
                    } else {
                        None
                    };

                    let reg_value = value::RegValue {
                        reg: output_reg,
                        abi_type: phi_abi_type.clone(),
                        possible_type_tags,
                        known_record_cons,
                    };

                    output_value = reg_value.into();

                    reg_phi = Some(ops::RegPhi {
                        output_reg: output_reg.into(),
                        true_result_reg: true_result_reg.into(),
                        false_result_reg: false_result_reg.into(),
                    });
                }
            }
            (Ok(true_value), Err(Error::Diverged)) => {
                output_value = true_value;
                reg_phi = None;
            }
            (Err(Error::Diverged), Ok(false_value)) => {
                output_value = false_value;
                reg_phi = None;
            }
            (Err(true_error), _) => {
                return Err(true_error);
            }
            (_, Err(false_error)) => {
                return Err(false_error);
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
        fcx: &mut FunCtx,
        fun_expr: hir::Fun<hir::Inferred>,
        source_name: Option<&DataStr>,
    ) -> Value {
        use crate::mir::closure;

        let closure =
            closure::calculate_closure(&fcx.local_values, &fun_expr.body_expr, source_name);

        Value::ArretFun(value::ArretFun::new(
            source_name.cloned(),
            fcx.mono_ty_args.clone(),
            closure,
            fun_expr,
        ))
    }

    pub fn arret_fun_to_jit_boxed(
        &mut self,
        arret_fun: &value::ArretFun,
    ) -> Option<Gc<boxed::FunThunk>> {
        // If we have non-const (i.e. "free") values in our closure we can't be const
        if !arret_fun.closure().free_values.is_empty() {
            return None;
        }

        let entry = self.jit_thunk_for_arret_fun(arret_fun);

        let closure = boxed::NIL_INSTANCE.as_any_ref();
        let new_boxed = boxed::FunThunk::new(self, closure, entry);

        let arret_fun_value = Value::ArretFun(arret_fun.clone());
        self.thunk_fun_values
            .insert(new_boxed.as_ptr(), arret_fun_value);
        Some(new_boxed)
    }

    fn jit_thunk_for_arret_fun(&mut self, arret_fun: &value::ArretFun) -> boxed::ThunkEntry {
        // Create a dynamic thunk to this Arret function if it doesn't exist
        if let Some(thunk) = self.arret_fun_thunks.get(&arret_fun.id()) {
            return *thunk;
        }

        let thunk = unsafe {
            use std::mem;

            let wanted_abi = PolymorphABI::thunk_abi();
            let ops_fun = self
                .ops_for_arret_fun(&arret_fun, wanted_abi)
                .expect("error during arret fun boxing");

            let address = self.thunk_jit.compile_fun(
                &self.private_funs,
                self.runtime_task.heap_mut().type_info_mut().interner_mut(),
                &ops_fun,
            );

            mem::transmute(address as usize)
        };

        self.arret_fun_thunks.insert(arret_fun.id(), thunk);
        thunk
    }

    /// Returns a private fun ID for the wanted Arret fun and ABI
    ///
    /// This will return a cached ID if available
    fn id_for_arret_fun(
        &mut self,
        arret_fun: &value::ArretFun,
        wanted_abi: PolymorphABI,
    ) -> ops::PrivateFunId {
        let arret_fun_key = ArretFunKey {
            arret_fun_id: arret_fun.id(),
            polymorph_abi: wanted_abi.clone(),
        };

        if let Some(private_fun_id) = self.arret_funs.get(&arret_fun_key) {
            return *private_fun_id;
        }

        // Allocate and track our private fun ID before we actually build the fun.
        // This is to prevent a compile-time loop if this fun ends up recursing.
        let private_fun_id = self.private_fun_id_counter.alloc();
        self.arret_funs.insert(arret_fun_key, private_fun_id);

        let ops_fun = self
            .ops_for_arret_fun(arret_fun, wanted_abi)
            .expect("error during arret fun boxing");
        self.private_funs.insert(private_fun_id, ops_fun);

        private_fun_id
    }

    pub fn evaled_record_class_for_cons(
        &mut self,
        record_cons: &record::ConsId,
    ) -> &EvaledRecordClass {
        use crate::mir::specific_abi_type::specific_abi_type_for_ty_ref;

        if self.record_class_for_cons.contains_key(record_cons) {
            return &self.record_class_for_cons[record_cons];
        }

        let field_abi_types = record_cons
            .fields()
            .iter()
            .map(|field| specific_abi_type_for_ty_ref(field.ty_ref()))
            .collect();

        let record_struct =
            ops::RecordStruct::new(record_cons.ty_cons_name().clone(), field_abi_types);
        let registered_record_struct = self.thunk_jit.register_record_struct(
            &record_struct,
            self.runtime_task.heap_mut().type_info_mut().class_map_mut(),
        );

        let evaled_record_class = EvaledRecordClass {
            jit_record_class_id: registered_record_struct.record_class_id,
            jit_data_layout: registered_record_struct.data_layout,
            record_struct,
        };

        self.cons_for_jit_record_class_id.insert(
            registered_record_struct.record_class_id,
            record_cons.clone(),
        );

        self.record_class_for_cons
            .entry(record_cons.clone())
            .or_insert(evaled_record_class)
    }

    pub fn cons_for_jit_record_class_id(
        &self,
        record_class_id: boxed::RecordClassId,
    ) -> Option<&record::ConsId> {
        self.cons_for_jit_record_class_id.get(&record_class_id)
    }

    pub fn arret_fun_to_thunk_reg(
        &mut self,
        b: &mut Builder,
        span: Span,
        arret_fun: &value::ArretFun,
    ) -> BuiltReg {
        use crate::mir::closure;
        use crate::mir::ops::*;
        use arret_runtime::abitype;

        let wanted_abi = PolymorphABI::thunk_abi();
        let private_fun_id = self.id_for_arret_fun(arret_fun, wanted_abi);

        let closure_reg = closure::save_to_closure_reg(self, b, span, arret_fun.closure());

        if let Some(closure_reg) = closure_reg {
            b.push_reg(
                span,
                OpKind::AllocBoxedFunThunk,
                BoxFunThunkOp {
                    closure_reg: closure_reg.into(),
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
                    closure_reg: outer_closure_reg.into(),
                    callee: ops::Callee::PrivateFun(private_fun_id),
                },
            )
        }
    }

    pub fn arret_fun_to_callback_reg(
        &mut self,
        b: &mut Builder,
        span: Span,
        arret_fun: &value::ArretFun,
        entry_point_abi: &CallbackEntryPointABIType,
    ) -> BuiltReg {
        use crate::mir::closure;
        use crate::mir::ops::*;
        use arret_runtime::abitype;

        let wanted_abi = entry_point_abi.clone().into();
        let private_fun_id = self.id_for_arret_fun(arret_fun, wanted_abi);

        let closure_reg = closure::save_to_closure_reg(self, b, span, arret_fun.closure())
            .unwrap_or_else(|| {
                let nil_reg = b.push_reg(span, OpKind::ConstBoxedNil, ());
                b.cast_boxed(span, nil_reg, abitype::BoxedABIType::Any)
            });

        b.push_reg(
            span,
            OpKind::MakeCallback,
            MakeCallbackOp {
                closure_reg: closure_reg.into(),
                callee: ops::Callee::PrivateFun(private_fun_id),
            },
        )
    }

    pub(super) fn ops_for_arret_fun(
        &mut self,
        arret_fun: &value::ArretFun,
        wanted_abi: PolymorphABI,
    ) -> Result<ops::Fun> {
        use crate::hir::destruc::poly_for_list_destruc;
        use crate::mir::arg_list::{build_load_arg_list_value, LoadedArgList};
        use crate::mir::closure;
        use crate::mir::optimise::optimise_fun;
        use crate::mir::ret_value::build_value_ret;

        let mut b = Builder::new();
        let fun_expr = arret_fun.fun_expr();
        let span = fun_expr.span;

        let param_list_poly = poly_for_list_destruc(&arret_fun.fun_expr().params);
        let LoadedArgList {
            closure_reg,
            param_regs,
            arg_list_value,
        } = build_load_arg_list_value(self, &mut b, &wanted_abi, &param_list_poly);

        // Start by taking the type args from the fun's enclosing environment
        let mut fcx = FunCtx::with_mono_ty_args(arret_fun.env_ty_args().clone());

        // And loading its closure
        closure::load_from_closure_param(
            &mut b,
            span,
            &mut fcx.local_values,
            arret_fun.closure(),
            closure_reg,
        );

        // Try to refine our polymorphic type variables based on our requested op ABI
        let mut stx = ty::select::SelectCtx::new(&fun_expr.pvars, &fun_expr.tvars);

        let fun_param_poly = hir::destruc::poly_for_list_destruc(&fun_expr.params);
        let wanted_abi_poly = wanted_abi.param_ty_ref();
        stx.add_evidence(&fun_param_poly.into(), &wanted_abi_poly.into());

        let ty_args = stx.into_poly_ty_args();

        let mut some_b = Some(b);
        let app_result = self.inline_arret_fun_app(
            &fcx,
            &mut some_b,
            span,
            arret_fun,
            ApplyArgs {
                ty_args: &ty_args,
                list_value: arg_list_value,
            },
            inliner::ApplyStack::new(),
        );
        let mut b = some_b.unwrap();

        build_value_ret(self, &mut b, span, app_result, &wanted_abi.ops_abi.ret);

        Ok(optimise_fun(ops::Fun {
            span: arret_fun.fun_expr().span,
            source_name: arret_fun.source_name().clone(),

            abi: wanted_abi.ops_abi,
            param_regs,
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
        use crate::mir::ret_value::build_value_ret;
        use arret_runtime::abitype;

        let wanted_abi = entry_point_abi.into();

        let mut b = Builder::new();

        let LoadedArgList {
            closure_reg,
            param_regs,
            arg_list_value,
        } = build_load_arg_list_value(
            self,
            &mut b,
            &wanted_abi,
            &ty::List::new_uniform(Ty::Any.into()),
        );

        let fun_reg_value =
            value::RegValue::new(closure_reg.unwrap(), abitype::BoxedABIType::Any.into());

        let result_value =
            self.build_reg_fun_thunk_app(&mut b, EMPTY_SPAN, &fun_reg_value, &arg_list_value);

        build_value_ret(
            self,
            &mut b,
            EMPTY_SPAN,
            Ok(result_value),
            &wanted_abi.ops_abi.ret,
        );

        optimise_fun(ops::Fun {
            span: EMPTY_SPAN,
            source_name: Some("callback_to_thunk_adapter".into()),

            abi: wanted_abi.ops_abi,
            param_regs,
            ops: b.into_ops(),
        })
    }

    pub fn thunk_reg_to_callback_reg(
        &mut self,
        b: &mut Builder,
        span: Span,
        thunk_reg_abi_type: &arret_runtime::abitype::BoxedABIType,
        thunk_reg: BuiltReg,
        entry_point_abi: &CallbackEntryPointABIType,
    ) -> BuiltReg {
        use crate::mir::ops::*;
        use arret_runtime::abitype;

        // Closures are of type `Any`
        let closure_reg = b.cast_boxed_cond(
            EMPTY_SPAN,
            thunk_reg_abi_type,
            thunk_reg,
            abitype::BoxedABIType::Any,
        );

        let private_fun_id = self.private_fun_id_counter.alloc();
        let ops_fun = self.ops_for_callback_to_thunk_adapter(entry_point_abi.clone());
        self.private_funs.insert(private_fun_id, ops_fun);

        b.push_reg(
            span,
            OpKind::MakeCallback,
            MakeCallbackOp {
                closure_reg: closure_reg.into(),
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

        let mut fcx = FunCtx::new();

        // Don't pass a builder; we should never generate ops based on a def
        let source_name = Self::destruc_source_name(&destruc);
        let value =
            self.consume_expr_with_source_name(&mut fcx, &mut None, value_expr, source_name)?;

        Self::destruc_value(&mut None, &mut self.global_values, &destruc, value);
        Ok(())
    }

    pub fn should_collect(&self) -> bool {
        self.runtime_task.heap().should_collect()
    }

    /// Collect any boxed values that are no longer reachable
    pub fn collect_garbage(&mut self) {
        use arret_runtime::boxed::collect;
        use std::mem;

        let old_heap = mem::replace(self.runtime_task.heap_mut(), boxed::Heap::empty());
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
            .filter_map(|(fun_thunk, value)| unsafe {
                weak_pass
                    .new_heap_ref_for(Gc::new(fun_thunk))
                    .map(|new_fun_thunk| (new_fun_thunk.as_ptr(), value))
            })
            .collect();

        *self.runtime_task.heap_mut() = weak_pass.into_new_heap();
    }

    fn eval_expr_with_source_name(
        &mut self,
        fcx: &mut FunCtx,
        b: &mut Option<Builder>,
        expr: &Expr,
        source_name: Option<&DataStr>,
    ) -> Result<Value> {
        use crate::mir::value::types::value_with_arret_ty;

        use crate::hir::ExprKind;
        let value = match &expr.kind {
            ExprKind::Lit(literal) => Ok(self.eval_lit(literal)),
            ExprKind::Do(exprs) => self.eval_do(fcx, b, &exprs),
            ExprKind::Fun(fun_expr) => {
                Ok(self.eval_arret_fun(fcx, fun_expr.as_ref().clone(), source_name))
            }
            ExprKind::RustFun(rust_fun) => Ok(Value::RustFun(Rc::new(rust_fun.as_ref().clone()))),
            ExprKind::TyPred(_, test_ty) => Ok(Value::TyPred(test_ty.clone())),
            ExprKind::EqPred(_) => Ok(Value::EqPred),
            ExprKind::RecordCons(_, record_cons) => Ok(Value::RecordCons(record_cons.clone())),
            ExprKind::FieldAccessor(field_accessor) => Ok(Value::FieldAccessor(
                field_accessor.record_cons.clone(),
                field_accessor.field_index,
            )),
            ExprKind::Ref(_, var_id) => Ok(self.eval_ref(fcx, *var_id)),
            ExprKind::Let(hir_let) => self.eval_let(fcx, b, hir_let),
            ExprKind::App(app) => self.eval_app(fcx, b, &expr.result_ty, app),
            ExprKind::MacroExpand(span, expr) => self
                .eval_expr(fcx, b, expr)
                .map_err(|err| err.with_macro_invocation_span(*span)),
            ExprKind::Cond(cond) => self.eval_cond(fcx, b, cond),
        }?;

        // Annotate this value with the expression's result type as it passes through
        Ok(value_with_arret_ty(self, value, || {
            fcx.monomorphise(&expr.result_ty)
        }))
    }

    pub fn eval_expr(
        &mut self,
        fcx: &mut FunCtx,
        b: &mut Option<Builder>,
        expr: &Expr,
    ) -> Result<Value> {
        self.eval_expr_with_source_name(fcx, b, expr, None)
    }

    fn consume_expr_with_source_name(
        &mut self,
        fcx: &mut FunCtx,
        b: &mut Option<Builder>,
        expr: Expr,
        source_name: Option<&DataStr>,
    ) -> Result<Value> {
        use crate::hir::ExprKind;
        match expr.kind {
            ExprKind::Fun(fun_expr) => Ok(self.eval_arret_fun(fcx, *fun_expr, source_name)),
            ExprKind::RustFun(rust_fun) => Ok(Value::RustFun(rust_fun.into())),
            _ => self.eval_expr_with_source_name(fcx, b, &expr, source_name),
        }
    }

    pub fn consume_expr(
        &mut self,
        fcx: &mut FunCtx,
        b: &mut Option<Builder>,
        expr: Expr,
    ) -> Result<Value> {
        self.consume_expr_with_source_name(fcx, b, expr, None)
    }

    /// Evaluates the main function of a program
    pub fn eval_main_fun(&mut self, main_var_id: hir::VarId) -> Result<()> {
        let mut fcx = FunCtx::new();
        let main_value = self.eval_ref(&fcx, main_var_id);
        let empty_list_value = Value::List(Box::new([]), None);

        self.eval_value_app(
            &mut fcx,
            &mut None,
            EMPTY_SPAN,
            &Ty::unit().into(),
            &main_value,
            ApplyArgs {
                ty_args: &TyArgs::empty(),
                list_value: empty_list_value,
            },
        )?;

        Ok(())
    }

    /// Builds the main function of the program
    pub fn into_built_program(mut self, main_var_id: hir::VarId) -> Result<BuiltProgram> {
        use arret_runtime::abitype;

        let fcx = FunCtx::new();
        let main_value = self.eval_ref(&fcx, main_var_id);

        let main_arret_fun = if let Value::ArretFun(main_arret_fun) = main_value {
            main_arret_fun
        } else {
            unimplemented!("Non-Arret main!");
        };

        let main_abi = PolymorphABI {
            ops_abi: ops::OpsABI {
                params: Box::new([]),
                ret: abitype::RetABIType::Void,
            },
            // Main is a top-level function; it can't capture
            has_closure: false,
            has_rest: false,
        };

        let main = self.ops_for_arret_fun(&main_arret_fun, main_abi)?;

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

impl AsInterner for EvalHirCtx {
    fn as_interner(&self) -> &Interner {
        self.runtime_task.heap().type_info().interner()
    }
}
