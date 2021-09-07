use std::collections::HashMap;
use std::sync::Arc;
use std::{alloc, ffi, panic};

use arret_runtime::boxed;
use arret_runtime::boxed::prelude::*;
use arret_runtime::boxed::refs::Gc;
use arret_runtime::callback::EntryPointAbiType as CallbackEntryPointAbiType;
use arret_runtime::intern::{AsInterner, Interner};

use arret_runtime::abitype;
use arret_runtime_syntax::reader;
use arret_syntax::datum::{DataStr, Datum};
use arret_syntax::span::Span;

use crate::codegen;
use crate::context::ModuleId;
use crate::hir;
use crate::mir::builder::{Builder, BuiltReg, TryToBuilder};
use crate::mir::error::{Error, Result};
use crate::mir::inliner;
use crate::mir::ops;
use crate::mir::polymorph::PolymorphAbi;
use crate::mir::value;
use crate::mir::value::synthetic_fun::SyntheticFuns;
use crate::mir::value::types::TypeHint;
use crate::mir::{Expr, Value};
use crate::rfi;
use crate::source::EMPTY_SPAN;
use crate::ty;
use crate::ty::purity;
use crate::ty::purity::Purity;
use crate::ty::record;
use crate::ty::ty_args::TyArgs;
use crate::ty::Ty;

#[derive(PartialEq, Eq, Hash)]
struct RustFunKey {
    symbol: &'static str,
    polymorph_abi: PolymorphAbi,
}

#[derive(PartialEq, Eq, Hash)]
struct ArretFunKey {
    arret_fun_id: value::ArretFunId,
    polymorph_abi: PolymorphAbi,
}

#[derive(PartialEq, Eq)]
pub struct EvaledRecordClass {
    pub jit_record_class_id: boxed::RecordClassId,
    pub jit_data_layout: Option<alloc::Layout>,
    pub record_struct: ops::RecordStructId,
}

pub struct EvalHirCtx {
    runtime_task: arret_runtime::task::Task,
    global_values: HashMap<hir::ExportId, Value>,

    private_fun_id_counter: ops::PrivateFunIdCounter,
    private_funs: HashMap<ops::PrivateFunId, ops::Fun>,
    rust_funs: HashMap<RustFunKey, ops::PrivateFunId>,
    arret_funs: HashMap<ArretFunKey, ops::PrivateFunId>,
    synthetic_funs: SyntheticFuns,

    rust_fun_thunks: HashMap<usize, boxed::ThunkEntry>,
    arret_fun_thunks: HashMap<value::ArretFunId, boxed::ThunkEntry>,

    // This uses pointers because `FunThunk` is always inequal to itself
    thunk_fun_values: HashMap<*const boxed::FunThunk, Value>,
    thunk_jit: codegen::jit::JitCtx,

    pub(super) record_class_for_cons: HashMap<record::ConsId, EvaledRecordClass>,
    cons_for_jit_record_class_id: HashMap<boxed::RecordClassId, record::ConsId>,
}

/// Context for performing a tail call in `(recur)`
struct TailCallCtx {
    self_abi: PolymorphAbi,
    captures_reg: Option<BuiltReg>,
}

struct RecurSelf<'af> {
    arret_fun: &'af value::ArretFun,

    /// Return ABI type of expected by tail calls, if they're allowed
    tail_call_ctx: Option<TailCallCtx>,
}

pub struct FunCtx<'rs> {
    /// Optional module to find local variables in
    ///
    /// If this isn't specified the function cannot refer to other top-level definitions in the
    /// same module.
    module_id: Option<ModuleId>,

    mono_ty_args: TyArgs<ty::Mono>,
    local_values: HashMap<hir::LocalId, Value>,
    recur_self: Option<Box<RecurSelf<'rs>>>,

    pub(super) inliner_stack: inliner::ApplyStack,
}

impl<'sv> FunCtx<'sv> {
    pub fn new(module_id: Option<ModuleId>) -> FunCtx<'static> {
        FunCtx {
            module_id,

            mono_ty_args: TyArgs::empty(),
            local_values: HashMap::new(),
            recur_self: None,

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
        matches!(
            self.main.ops.as_ref(),
            [ops::Op {
                kind: ops::OpKind::RetVoid,
                ..
            }]
        )
    }
}

#[derive(Clone)]
pub(super) struct ApplyArgs<'tyargs> {
    ty_args: &'tyargs TyArgs<ty::Poly>,
    pub(super) list_value: Value,
}

fn merge_apply_purity_into_scope(
    scope: &HashMap<purity::PVarId, purity::Ref>,
    apply_purities: &HashMap<purity::PVarId, purity::Ref>,
    subst_with: &TyArgs<ty::Mono>,
) -> HashMap<purity::PVarId, purity::Ref> {
    use crate::ty::subst;

    scope
        .iter()
        .map(|(pvar, v)| (pvar.clone(), v.clone()))
        .chain(apply_purities.iter().map(|(pvar, poly_purity)| {
            let subst_purity = subst::monomorphise_purity(subst_with, poly_purity);
            (pvar.clone(), subst_purity)
        }))
        .collect()
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

    let pvar_purities = merge_apply_purity_into_scope(
        scope.pvar_purities(),
        apply_ty_args.pvar_purities(),
        subst_with,
    );

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

impl EvalHirCtx {
    pub fn new(optimising: bool) -> EvalHirCtx {
        let thunk_jit = codegen::jit::JitCtx::new(optimising);

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

    fn destruc_scalar<F>(
        scalar: &hir::destruc::Scalar<hir::Inferred>,
        value: Value,
        insert_local: &mut F,
    ) where
        F: FnMut(hir::LocalId, Value),
    {
        if let Some(local_id) = scalar.local_id() {
            insert_local(*local_id, value);
        }
    }

    fn destruc_list<F>(
        b: &mut Option<Builder>,
        span: Span,
        list: &hir::destruc::List<hir::Inferred>,
        value: Value,
        insert_local: &mut F,
    ) where
        F: FnMut(hir::LocalId, Value),
    {
        let mut iter = value.into_unsized_list_iter();

        for fixed_destruc in list.fixed() {
            let value = iter.next_unchecked(b, span);
            Self::destruc_value(b, fixed_destruc, value, insert_local);
        }

        if let Some(rest_destruc) = list.rest() {
            Self::destruc_scalar(rest_destruc, iter.into_rest(), insert_local)
        }
    }

    fn destruc_value<F>(
        b: &mut Option<Builder>,
        destruc: &hir::destruc::Destruc<hir::Inferred>,
        value: Value,
        insert_local: &mut F,
    ) where
        F: FnMut(hir::LocalId, Value),
    {
        use crate::hir::destruc::Destruc;

        match destruc {
            Destruc::Scalar(_, scalar) => Self::destruc_scalar(scalar, value, insert_local),
            Destruc::List(span, list) => Self::destruc_list(b, *span, list, value, insert_local),
        }
    }

    fn destruc_source_name(destruc: &hir::destruc::Destruc<hir::Inferred>) -> Option<&DataStr> {
        use crate::hir::destruc::Destruc;

        match destruc {
            Destruc::Scalar(_, scalar) => Some(scalar.source_name()),
            Destruc::List(_, _) => None,
        }
    }

    fn eval_local_ref(&self, fcx: &FunCtx<'_>, local_id: hir::LocalId) -> Value {
        // Try local values
        if let Some(local_value) = fcx.local_values.get(&local_id) {
            return local_value.clone();
        }

        let module_id = fcx
            .module_id
            .expect("could not fall back to global for missing local");

        // If this is a top-level def from the same module it will be a global
        fcx.local_values
            .get(&local_id)
            .unwrap_or_else(|| &self.global_values[&hir::ExportId::new(module_id, local_id)])
            .clone()
    }

    fn eval_do(
        &mut self,
        fcx: &mut FunCtx<'_>,
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
        fcx: &mut FunCtx<'_>,
        b: &mut Option<Builder>,
        hir_let: &hir::Let<hir::Inferred>,
    ) -> Result<Value> {
        let source_name = Self::destruc_source_name(&hir_let.destruc);
        let value = self.eval_expr_with_source_name(fcx, b, &hir_let.value_expr, source_name)?;

        Self::destruc_value(b, &hir_let.destruc, value, &mut |local_id, value| {
            fcx.local_values.insert(local_id, value);
        });

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
        fcx: &FunCtx<'_>,
        b: &mut Builder,
        span: Span,
        ret_ty: &ty::Ref<ty::Mono>,
        arret_fun: &value::ArretFun,
        apply_args: &ApplyArgs<'_>,
    ) -> Result<Value> {
        use crate::hir::destruc::poly_for_list_destruc;
        use crate::mir::app_purity::fun_app_purity;
        use crate::mir::arg_list::build_save_arg_list_to_regs;
        use crate::mir::env_values;
        use crate::mir::ops::*;
        use crate::mir::polymorph::polymorph_abi_for_list_ty;
        use crate::mir::ret_value::ret_reg_to_value;
        use crate::ty::subst;

        let ApplyArgs {
            list_value: arg_list_value,
            ty_args: apply_ty_args,
        } = apply_args;

        let mono_ty_args = merge_apply_ty_args_into_scope(
            arret_fun.env_ty_args(),
            apply_ty_args,
            &fcx.mono_ty_args,
        );

        let captures_reg = env_values::save_to_captures_reg(self, b, span, arret_fun.env_values());

        let param_list_poly = poly_for_list_destruc(&arret_fun.fun_expr().params);
        let param_list_mono = subst::monomorphise_list(&mono_ty_args, &param_list_poly);

        let wanted_abi =
            polymorph_abi_for_list_ty(captures_reg.is_some(), &param_list_mono, ret_ty);
        let ret_abi = wanted_abi.ret.clone();

        let mut arg_regs: Vec<RegId> = vec![];
        if let Some(captures_reg) = captures_reg {
            arg_regs.push(captures_reg.into());
        }

        arg_regs.extend(build_save_arg_list_to_regs(
            self,
            b,
            span,
            arg_list_value.clone(),
            wanted_abi.fixed_params.iter(),
            wanted_abi.rest_param.as_ref(),
        ));

        let private_fun_id = self.id_for_arret_fun(arret_fun, wanted_abi);
        let fun_expr = arret_fun.fun_expr();

        let app_purity = fun_app_purity(
            mono_ty_args.pvar_purities(),
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
        outer_fcx: &FunCtx<'_>,
        b: &mut Option<Builder>,
        span: Span,
        arret_fun: &value::ArretFun,
        apply_args: ApplyArgs<'_>,
        inliner_stack: inliner::ApplyStack,
    ) -> Result<Value> {
        let fun_expr = arret_fun.fun_expr();

        let mut inner_fcx = FunCtx {
            module_id: arret_fun.module_id(),
            mono_ty_args: merge_apply_ty_args_into_scope(
                arret_fun.env_ty_args(),
                apply_args.ty_args,
                &outer_fcx.mono_ty_args,
            ),
            local_values: outer_fcx.local_values.clone(),
            recur_self: Some(Box::new(RecurSelf {
                arret_fun,
                tail_call_ctx: None,
            })),

            inliner_stack,
        };

        Self::destruc_list(
            b,
            span,
            &fun_expr.params,
            apply_args.list_value,
            &mut |local_id, value| {
                inner_fcx.local_values.insert(local_id, value);
            },
        );

        self.eval_expr(&mut inner_fcx, b, &fun_expr.body_expr)
    }

    fn eval_arret_fun_app(
        &mut self,
        fcx: &mut FunCtx<'_>,
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

    pub fn rust_fun_to_jit_boxed(&mut self, rust_fun: Arc<rfi::Fun>) -> Gc<boxed::FunThunk> {
        let captures = boxed::NIL_INSTANCE.as_any_ref();
        let entry = self.jit_thunk_for_rust_fun(&rust_fun);
        let new_boxed = boxed::FunThunk::new(self, captures, entry);

        self.thunk_fun_values
            .insert(new_boxed.as_ptr(), Value::RustFun(rust_fun));

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

            let wanted_abi = PolymorphAbi::thunk_abi();
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
        wanted_abi: PolymorphAbi,
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

        let wanted_abi = PolymorphAbi::thunk_abi();
        let private_fun_id = self.id_for_rust_fun(rust_fun, wanted_abi);

        let nil_reg = b.push_reg(span, OpKind::ConstBoxedNil, ());
        let captures_reg = b.cast_boxed(span, nil_reg, abitype::BoxedAbiType::Any);

        b.push_reg(
            span,
            OpKind::ConstBoxedFunThunk,
            BoxFunThunkOp {
                captures_reg: captures_reg.into(),
                callee: ops::Callee::PrivateFun(private_fun_id),
            },
        )
    }

    pub fn rust_fun_to_callback_reg(
        &mut self,
        b: &mut Builder,
        span: Span,
        rust_fun: &rfi::Fun,
        entry_point_abi: &CallbackEntryPointAbiType,
    ) -> BuiltReg {
        use crate::mir::ops::*;

        let wanted_abi = entry_point_abi.clone().into();
        let private_fun_id = self.id_for_rust_fun(rust_fun, wanted_abi);

        let nil_reg = b.push_reg(span, OpKind::ConstBoxedNil, ());
        let captures_reg = b.cast_boxed(span, nil_reg, abitype::BoxedAbiType::Any);

        b.push_reg(
            span,
            OpKind::MakeCallback,
            MakeCallbackOp {
                captures_reg: captures_reg.into(),
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
        fcx: &mut FunCtx<'_>,
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

        let arret_fun_type = rust_fun.arret_fun_type();

        let mono_purities = merge_apply_purity_into_scope(
            &HashMap::new(),
            apply_ty_args.pvar_purities(),
            &fcx.mono_ty_args,
        );

        let call_purity = fun_app_purity(
            &mono_purities,
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

                let runtime_task = &mut self.runtime_task;

                let native_result = Self::call_native_fun(span, || {
                    let captures = boxed::NIL_INSTANCE.as_any_ref();
                    thunk(runtime_task, captures, boxed_arg_list)
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
            let arg_list_value = if let Some(intrinsic_name) = rust_fun.intrinsic_name() {
                match intrinsic::try_build(self, b, span, intrinsic_name, &arg_list_value)? {
                    intrinsic::BuildOutcome::None => arg_list_value,
                    intrinsic::BuildOutcome::SimplifiedArgs(simplified_arg_list_value) => {
                        simplified_arg_list_value
                    }
                    intrinsic::BuildOutcome::ReturnValue(return_value) => {
                        return Ok(return_value);
                    }
                }
            } else {
                arg_list_value
            };

            build_rust_fun_app(self, b, span, ret_ty, rust_fun, call_purity, arg_list_value)
        } else {
            panic!("Need builder for non-const function application");
        }
    }

    fn eval_const_fun_thunk_app(
        &mut self,
        fcx: &mut FunCtx<'_>,
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

        let fun_boxed_abi_type =
            if let abitype::AbiType::Boxed(ref fun_boxed_abi_type) = fun_reg_value.abi_type {
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

        let captures_reg = b.push_reg(
            span,
            OpKind::LoadBoxedFunThunkCaptures,
            fun_thunk_reg.into(),
        );
        let arg_list_reg = value_to_reg(
            self,
            b,
            span,
            arg_list_value,
            &abitype::TOP_LIST_BOXED_ABI_TYPE.into(),
        );

        let ret_reg = b.push_reg(
            span,
            OpKind::Call,
            CallOp {
                callee: Callee::BoxedFunThunk(fun_thunk_reg.into()),
                impure: true,
                args: Box::new([captures_reg.into(), arg_list_reg.into()]),
            },
        );

        value::RegValue::new(ret_reg, abitype::BoxedAbiType::Any.into()).into()
    }

    fn eval_value_app(
        &mut self,
        fcx: &mut FunCtx<'_>,
        b: &mut Option<Builder>,
        span: Span,
        ret_ty: &ty::Ref<ty::Mono>,
        fun_value: &Value,
        apply_args: ApplyArgs<'_>,
    ) -> Result<Value> {
        match fun_value {
            Value::ArretFun(arret_fun) => {
                use crate::mir::env_values;

                env_values::load_from_current_fun(&mut fcx.local_values, arret_fun.env_values());
                self.eval_arret_fun_app(fcx, b, span, ret_ty, arret_fun, apply_args)
            }
            Value::RustFun(rust_fun) => {
                self.eval_rust_fun_app(fcx, b, span, ret_ty, rust_fun, apply_args)
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
            Value::Const(boxed_fun) => {
                let fun_thunk = boxed_fun
                    .downcast_ref::<boxed::FunThunk>()
                    .expect("applying non-function box");

                self.eval_const_fun_thunk_app(fcx, b, span, ret_ty, fun_thunk, apply_args)
            }
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
        fcx: &mut FunCtx<'_>,
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

    /// Evaluates a `(recur)` within a fun body
    ///
    /// While `(recur)` is semantically equivalent to calling the fun by name it has a different
    /// implication about programmer intent. `(recur)` is used in positions where unbounded tail
    /// recursion can occur, typically to iteratively process a data structure. This is used where
    /// a loop would be used in other languages.
    ///
    /// For this reason they are evaluated quite differently from normal applies. Four things are
    /// tried in order of preference:
    ///
    /// 1. If we don't have a builder we will JIT a thunk and call into it. This is important
    ///    because our MIR evaluation is not tail recursive; we could exhaust our Rust stack if
    ///    we attempted to MIR evaluate until the end of recursion.
    ///
    /// 2. If the arg list is constant and the apply is pure then we will also evaluate through a
    ///    thunk. This is the same way Rust funs are treated.
    ///
    /// 3. If we have a `tail_call_ctx` we will built a special `TailCall` op and immediately
    ///    return its value. This maximises the chance that codegen and LLVM will be able to
    ///    optimise the tail call in to a loop. We're also able to reuse our existing captures arg
    ///    directly.
    ///
    /// 4. If we don't have a `tail_call_ctx` we will treat this as if it was an Arret fun apply.
    ///
    ///    This can happen if we're being inlined or we're inside a thunk or callback. This will
    ///    only happen for one recursion; the next one will have a `tail_call_ctx` and use one of
    ///    the above cases.
    ///
    ///    We directly build a fun app instead of attempting inlining. Inlining a recursion with a
    ///    non-constant arg list will nearly always hit the maximum inline depth and abort. This is
    ///    wasteful of compiler time.
    fn eval_recur(
        &mut self,
        fcx: &mut FunCtx<'_>,
        b: &mut Option<Builder>,
        result_ty: &ty::Ref<ty::Poly>,
        recur: &hir::Recur<hir::Inferred>,
    ) -> Result<Value> {
        use crate::mir::app_purity::fun_app_purity;

        let span = recur.span;

        let fixed_values = recur
            .fixed_arg_exprs
            .iter()
            .map(|arg| self.eval_expr(fcx, b, arg))
            .collect::<Result<Box<[Value]>>>()?;

        let rest_value = match &recur.rest_arg_expr {
            Some(rest_arg) => Some(Box::new(self.eval_expr(fcx, b, rest_arg)?)),
            None => None,
        };

        let (arret_fun, tail_call_ctx) = if let Some(recur_self) = &fcx.recur_self {
            (&recur_self.arret_fun, &recur_self.tail_call_ctx)
        } else {
            panic!("`(recur)` outside function");
        };

        let ret_ty = fcx.monomorphise(result_ty);
        let arg_list_value = Value::List(fixed_values, rest_value);

        // Determine our purity to see if we can const eval
        let recur_purity = fun_app_purity(
            fcx.mono_ty_args.pvar_purities(),
            &arret_fun.fun_expr().purity,
            &arret_fun.fun_expr().ret_ty,
        );

        // If we're impure or we have dynamic environment values we need to be evaluated at runtime
        let can_const_eval = b.is_none()
            || (recur_purity == Purity::Pure && arret_fun.env_values().free_values.is_empty());

        if can_const_eval {
            use crate::mir::value::to_const::value_to_const;

            if let Some(boxed_arg_list) = value_to_const(self, &arg_list_value) {
                let thunk = self.jit_thunk_for_arret_fun(arret_fun);
                return Self::call_native_fun(span, || {
                    let captures = boxed::NIL_INSTANCE.as_any_ref();
                    thunk(&mut self.runtime_task, captures, boxed_arg_list)
                });
            }
        }

        let some_b = if let Some(some_b) = b {
            some_b
        } else {
            panic!("failed to const eval (recur) during eval");
        };

        if let Some(tail_call_ctx) = tail_call_ctx {
            use crate::mir::arg_list::build_save_arg_list_to_regs;
            use crate::mir::ops::*;

            let self_abi = &tail_call_ctx.self_abi;

            let mut arg_regs: Vec<RegId> = vec![];
            if let Some(captures_reg) = tail_call_ctx.captures_reg {
                arg_regs.push(captures_reg.into());
            }

            arg_regs.extend(build_save_arg_list_to_regs(
                self,
                some_b,
                span,
                arg_list_value,
                self_abi.fixed_params.iter(),
                self_abi.rest_param.as_ref(),
            ));

            // All of the context for `TailCall` is implicit except the arg regs
            let ret_reg = some_b.push_reg(
                span,
                OpKind::TailCall,
                TailCallOp {
                    impure: recur_purity == Purity::Impure,
                    args: arg_regs.into_boxed_slice(),
                },
            );

            match &self_abi.ret {
                abitype::RetAbiType::Inhabited(_) => {
                    some_b.push(span, OpKind::Ret(ret_reg.into()));
                }
                abitype::RetAbiType::Never => {
                    some_b.push(span, OpKind::Unreachable);
                }
                abitype::RetAbiType::Void => {
                    some_b.push(span, OpKind::RetVoid);
                }
            }

            Err(Error::Diverged)
        } else {
            // By definition our ty args are the fun's type args pointed to themselves
            let pvar_purities = arret_fun
                .fun_expr()
                .pvars
                .iter()
                .map(|pvar| (pvar.clone(), pvar.clone().into()))
                .collect();

            let tvar_types = arret_fun
                .fun_expr()
                .tvars
                .iter()
                .map(|tvar| (tvar.clone(), tvar.clone().into()))
                .collect();

            let ty_args = TyArgs::new(pvar_purities, tvar_types);

            // We can't do a native tail call. This can happen if we're inside a thunk or callback
            // since they don't have the `FastCC` calling convention.
            self.build_arret_fun_app(
                fcx,
                some_b,
                recur.span,
                &ret_ty,
                arret_fun,
                &ApplyArgs {
                    ty_args: &ty_args,
                    list_value: arg_list_value,
                },
            )
        }
    }

    fn eval_cond(
        &mut self,
        fcx: &mut FunCtx<'_>,
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
        fcx: &mut FunCtx<'_>,
        branch_expr: &hir::Expr<hir::Inferred>,
    ) -> BuiltCondBranch {
        let b = Builder::new();

        let mut some_b = Some(b);
        let result = self.eval_expr(fcx, &mut some_b, branch_expr);
        let b = some_b.unwrap();

        BuiltCondBranch { b, result }
    }

    fn build_cond(
        &mut self,
        fcx: &mut FunCtx<'_>,
        b: &mut Builder,
        test_value: &Value,
        cond: &hir::Cond<hir::Inferred>,
    ) -> Result<Value> {
        use arret_runtime::boxed::TypeTag;

        use crate::mir::equality::values_statically_equal;
        use crate::mir::ops::{BinaryOp, OpKind};
        use crate::mir::value::build_reg::value_to_reg;
        use crate::mir::value::plan_phi::*;
        use crate::mir::value::types::{possible_type_tags_for_value, type_hint_for_value};

        let span = cond.span;
        let test_reg = value_to_reg(self, b, span, test_value, &abitype::AbiType::Bool);

        let mut built_true = self.build_cond_branch(fcx, &cond.true_expr);
        let mut built_false = self.build_cond_branch(fcx, &cond.false_expr);

        let output_value;
        let reg_phi;

        match (built_true.result, built_false.result) {
            (Ok(true_value), Ok(false_value)) => {
                let possible_true_type_tags = possible_type_tags_for_value(&true_value);
                let possible_false_type_tags = possible_type_tags_for_value(&false_value);

                if values_statically_equal(self, &true_value, &false_value) == Some(true) {
                    output_value = true_value;
                    reg_phi = None;
                } else if possible_true_type_tags == TypeTag::True.into()
                    && possible_false_type_tags == TypeTag::False.into()
                {
                    // Our output value is our test
                    // Use the unboxed value because LLVM has trouble reasoning about our boxed bools
                    let reg_value = value::RegValue::new(test_reg, abitype::AbiType::Bool);
                    output_value = reg_value.into();
                    reg_phi = None;
                } else if possible_true_type_tags == TypeTag::False.into()
                    && possible_false_type_tags == TypeTag::True.into()
                {
                    // Our output value is the negation of our test
                    let const_false_reg = b.push_reg(span, OpKind::ConstBool, false);
                    let negated_test_reg = b.push_reg(
                        span,
                        OpKind::BoolEqual,
                        BinaryOp {
                            lhs_reg: test_reg.into(),
                            rhs_reg: const_false_reg.into(),
                        },
                    );

                    let reg_value = value::RegValue::new(negated_test_reg, abitype::AbiType::Bool);
                    output_value = reg_value.into();
                    reg_phi = None;
                } else {
                    let phi_abi_type = plan_phi_abi_type(&true_value, &false_value);

                    let true_result_reg =
                        value_to_reg(self, &mut built_true.b, span, &true_value, &phi_abi_type);

                    let false_result_reg =
                        value_to_reg(self, &mut built_false.b, span, &false_value, &phi_abi_type);

                    let output_reg = b.alloc_local();

                    let possible_type_tags = possible_true_type_tags | possible_false_type_tags;

                    let true_type_hint = type_hint_for_value(self, &true_value);
                    let false_type_hint = type_hint_for_value(self, &false_value);
                    let common_type_hint = if true_type_hint == false_type_hint {
                        true_type_hint
                    } else {
                        TypeHint::None
                    };

                    let reg_value = value::RegValue {
                        reg: output_reg,
                        abi_type: phi_abi_type,
                        possible_type_tags,
                        type_hint: common_type_hint,
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

        let true_ops = built_true.b.into_ops();
        let false_ops = built_false.b.into_ops();

        // Avoid adding an empty `Cond` we'd have to optimise away later
        if reg_phi.is_some() || !true_ops.is_empty() || !false_ops.is_empty() {
            b.push(
                span,
                ops::OpKind::Cond(ops::CondOp {
                    reg_phi,
                    test_reg: test_reg.into(),
                    true_ops,
                    false_ops,
                }),
            );
        }

        Ok(output_value)
    }

    fn eval_arret_fun(
        &mut self,
        fcx: &mut FunCtx<'_>,
        fun_expr: hir::Fun<hir::Inferred>,
        source_name: Option<&DataStr>,
    ) -> Value {
        use crate::mir::env_values;

        let env_values =
            env_values::calculate_env_values(&fcx.local_values, &fun_expr.body_expr, source_name);

        Value::ArretFun(value::ArretFun::new(
            fcx.module_id,
            source_name.cloned(),
            fcx.mono_ty_args.clone(),
            env_values,
            fun_expr,
        ))
    }

    pub fn arret_fun_to_jit_boxed(
        &mut self,
        arret_fun: &value::ArretFun,
    ) -> Option<Gc<boxed::FunThunk>> {
        // If we have non-const (i.e. "free") values in our environment we can't be const
        if !arret_fun.env_values().free_values.is_empty() {
            return None;
        }

        let entry = self.jit_thunk_for_arret_fun(arret_fun);

        let captures = boxed::NIL_INSTANCE.as_any_ref();
        let new_boxed = boxed::FunThunk::new(self, captures, entry);

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

            let wanted_abi = PolymorphAbi::thunk_abi();
            let ops_fun = self.ops_for_arret_fun(arret_fun, wanted_abi);

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
        wanted_abi: PolymorphAbi,
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

        let ops_fun = self.ops_for_arret_fun(arret_fun, wanted_abi);
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
        use crate::mir::env_values;
        use crate::mir::ops::*;

        let wanted_abi = PolymorphAbi::thunk_abi();
        let private_fun_id = self.id_for_arret_fun(arret_fun, wanted_abi);

        let captures_reg = env_values::save_to_captures_reg(self, b, span, arret_fun.env_values());

        if let Some(captures_reg) = captures_reg {
            b.push_reg(
                span,
                OpKind::AllocBoxedFunThunk,
                BoxFunThunkOp {
                    captures_reg: captures_reg.into(),
                    callee: ops::Callee::PrivateFun(private_fun_id),
                },
            )
        } else {
            let nil_reg = b.push_reg(span, OpKind::ConstBoxedNil, ());
            let outer_captures_reg = b.cast_boxed(span, nil_reg, abitype::BoxedAbiType::Any);

            b.push_reg(
                span,
                OpKind::ConstBoxedFunThunk,
                BoxFunThunkOp {
                    captures_reg: outer_captures_reg.into(),
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
        entry_point_abi: &CallbackEntryPointAbiType,
    ) -> BuiltReg {
        use crate::mir::env_values;
        use crate::mir::ops::*;

        let wanted_abi = entry_point_abi.clone().into();
        let private_fun_id = self.id_for_arret_fun(arret_fun, wanted_abi);

        let captures_reg = env_values::save_to_captures_reg(self, b, span, arret_fun.env_values())
            .unwrap_or_else(|| {
                let nil_reg = b.push_reg(span, OpKind::ConstBoxedNil, ());
                b.cast_boxed(span, nil_reg, abitype::BoxedAbiType::Any)
            });

        b.push_reg(
            span,
            OpKind::MakeCallback,
            MakeCallbackOp {
                captures_reg: captures_reg.into(),
                callee: ops::Callee::PrivateFun(private_fun_id),
            },
        )
    }

    pub(super) fn ops_for_arret_fun(
        &mut self,
        arret_fun: &value::ArretFun,
        wanted_abi: PolymorphAbi,
    ) -> ops::Fun {
        use crate::hir::destruc::poly_for_list_destruc;
        use crate::mir::arg_list::{build_load_arg_list_value, LoadedArgList};
        use crate::mir::env_values;
        use crate::mir::optimise::optimise_fun;
        use crate::mir::ret_value::build_value_ret;

        let mut b = Builder::new();
        let fun_expr = arret_fun.fun_expr();
        let span = fun_expr.span;

        let param_list_poly = poly_for_list_destruc(&arret_fun.fun_expr().params);
        let LoadedArgList {
            captures_reg,
            param_regs,
            arg_list_value,
        } = build_load_arg_list_value(self, &mut b, &wanted_abi, &param_list_poly);

        // Start by loading the captures
        let mut local_values: HashMap<hir::LocalId, Value> = HashMap::new();
        let mut recur_env_values = arret_fun.env_values().clone();

        env_values::load_from_env_param(
            &mut b,
            span,
            &mut local_values,
            &mut recur_env_values,
            captures_reg,
        );

        // Our env values have been updated with its new reg IDs
        let recur_arret_fun = arret_fun.with_env_values(recur_env_values);

        // Try to refine our polymorphic type variables based on our requested op ABI
        let mut stx = ty::select::SelectCtx::new(&fun_expr.pvars, &fun_expr.tvars);

        let fun_param_poly = hir::destruc::poly_for_list_destruc(&fun_expr.params);
        let wanted_abi_poly = wanted_abi.param_ty_ref();
        stx.add_evidence(&fun_param_poly.into(), &wanted_abi_poly.into());

        let ty_args = stx.into_poly_ty_args();

        let tail_call_ctx = if wanted_abi.call_conv == ops::CallConv::FastCc {
            Some(TailCallCtx {
                self_abi: wanted_abi.clone(),
                captures_reg,
            })
        } else {
            None
        };

        // Now build a function context
        let mut fcx = FunCtx {
            module_id: arret_fun.module_id(),
            mono_ty_args: merge_apply_ty_args_into_scope(
                arret_fun.env_ty_args(),
                &ty_args,
                &TyArgs::empty(),
            ),
            local_values,
            recur_self: Some(Box::new(RecurSelf {
                arret_fun: &recur_arret_fun,
                tail_call_ctx,
            })),

            inliner_stack: inliner::ApplyStack::new(),
        };

        let mut some_b = Some(b);
        Self::destruc_list(
            &mut some_b,
            span,
            &fun_expr.params,
            arg_list_value,
            &mut |local_id, value| {
                fcx.local_values.insert(local_id, value);
            },
        );

        let app_result = self.eval_expr(&mut fcx, &mut some_b, &fun_expr.body_expr);

        let mut b = some_b.unwrap();
        build_value_ret(self, &mut b, span, app_result, &wanted_abi.ret);

        optimise_fun(ops::Fun {
            span: arret_fun.fun_expr().span,
            source_name: arret_fun.source_name().clone(),

            abi: wanted_abi.into(),
            param_regs,
            ops: b.into_ops(),
        })
    }

    /// Builds a function with a callback ABI that calls a thunk passed as its captures
    fn ops_for_callback_to_thunk_adapter(
        &mut self,
        entry_point_abi: CallbackEntryPointAbiType,
    ) -> ops::Fun {
        use crate::mir::arg_list::{build_load_arg_list_value, LoadedArgList};
        use crate::mir::optimise::optimise_fun;
        use crate::mir::ret_value::build_value_ret;

        let span = EMPTY_SPAN;
        let wanted_abi = entry_point_abi.into();

        let mut b = Builder::new();

        let LoadedArgList {
            captures_reg,
            param_regs,
            arg_list_value,
        } = build_load_arg_list_value(
            self,
            &mut b,
            &wanted_abi,
            &ty::List::new_uniform(Ty::Any.into()),
        );

        let fun_reg_value =
            value::RegValue::new(captures_reg.unwrap(), abitype::BoxedAbiType::Any.into());

        let result_value =
            self.build_reg_fun_thunk_app(&mut b, span, &fun_reg_value, &arg_list_value);

        build_value_ret(self, &mut b, span, Ok(result_value), &wanted_abi.ret);

        optimise_fun(ops::Fun {
            span,
            source_name: Some("callback_to_thunk_adapter".into()),

            abi: wanted_abi.into(),
            param_regs,
            ops: b.into_ops(),
        })
    }

    pub fn thunk_reg_to_callback_reg(
        &mut self,
        b: &mut Builder,
        span: Span,
        thunk_reg_abi_type: &arret_runtime::abitype::BoxedAbiType,
        thunk_reg: BuiltReg,
        entry_point_abi: &CallbackEntryPointAbiType,
    ) -> BuiltReg {
        use crate::mir::ops::*;

        // Captures are of type `Any`
        let captures_reg = b.cast_boxed_cond(
            span,
            thunk_reg_abi_type,
            thunk_reg,
            abitype::BoxedAbiType::Any,
        );

        let private_fun_id = self.private_fun_id_counter.alloc();
        let ops_fun = self.ops_for_callback_to_thunk_adapter(entry_point_abi.clone());
        self.private_funs.insert(private_fun_id, ops_fun);

        b.push_reg(
            span,
            OpKind::MakeCallback,
            MakeCallbackOp {
                captures_reg: captures_reg.into(),
                callee: ops::Callee::PrivateFun(private_fun_id),
            },
        )
    }

    pub fn jit_boxed_to_fun_value(&self, boxed_thunk: Gc<boxed::FunThunk>) -> Option<&Value> {
        self.thunk_fun_values.get(&boxed_thunk.as_ptr())
    }

    pub fn visit_module_defs<'a>(
        &mut self,
        module_id: ModuleId,
        defs: impl IntoIterator<Item = &'a hir::Def<hir::Inferred>>,
    ) -> Result<()> {
        for def in defs {
            let hir::Def {
                destruc,
                value_expr,
                ..
            } = def;

            let mut fcx = FunCtx::new(Some(module_id));

            // Don't pass a builder; we should never generate ops based on a def
            let source_name = Self::destruc_source_name(destruc);
            let value =
                self.eval_expr_with_source_name(&mut fcx, &mut None, value_expr, source_name)?;

            Self::destruc_value(&mut None, destruc, value, &mut |local_id, value| {
                self.global_values
                    .insert(hir::ExportId::new(module_id, local_id), value);
            });
        }

        Ok(())
    }

    pub fn consume_module_defs(
        &mut self,
        module_id: ModuleId,
        defs: impl IntoIterator<Item = hir::Def<hir::Inferred>>,
    ) -> Result<()> {
        for def in defs {
            let hir::Def {
                destruc,
                value_expr,
                ..
            } = def;

            let mut fcx = FunCtx::new(Some(module_id));

            // Don't pass a builder; we should never generate ops based on a def
            let source_name = Self::destruc_source_name(&destruc);
            let value =
                self.consume_expr_with_source_name(&mut fcx, &mut None, value_expr, source_name)?;

            Self::destruc_value(&mut None, &destruc, value, &mut |local_id, value| {
                self.global_values
                    .insert(hir::ExportId::new(module_id, local_id), value);
            });
        }

        Ok(())
    }

    pub fn should_collect(&self) -> bool {
        self.runtime_task.heap().should_collect()
    }

    /// Collect any boxed values that are no longer reachable
    pub fn collect_garbage(&mut self) {
        use arret_runtime::boxed::collect;
        use std::mem;

        let old_heap = mem::take(self.runtime_task.heap_mut());
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

        let old_thunk_fun_values = mem::take(&mut self.thunk_fun_values);
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
        fcx: &mut FunCtx<'_>,
        b: &mut Option<Builder>,
        expr: &Expr,
        source_name: Option<&DataStr>,
    ) -> Result<Value> {
        use crate::mir::value::types::value_with_arret_ty;

        use crate::hir::ExprKind;
        let value = match &expr.kind {
            ExprKind::Lit(literal) => Ok(self.eval_lit(literal)),
            ExprKind::Do(exprs) => self.eval_do(fcx, b, exprs),
            ExprKind::Fun(fun_expr) => {
                Ok(self.eval_arret_fun(fcx, fun_expr.as_ref().clone(), source_name))
            }
            ExprKind::RustFun(rust_fun) => Ok(Value::RustFun(rust_fun.clone())),
            ExprKind::TyPred(_, test_ty) => Ok(Value::TyPred(test_ty.clone())),
            ExprKind::EqPred(_) => Ok(Value::EqPred),
            ExprKind::RecordCons(_, record_cons) => Ok(Value::RecordCons(record_cons.clone())),
            ExprKind::FieldAccessor(field_accessor) => Ok(Value::FieldAccessor(
                field_accessor.record_cons.clone(),
                field_accessor.field_index,
            )),
            ExprKind::LocalRef(_, local_id) => Ok(self.eval_local_ref(fcx, *local_id)),
            ExprKind::ExportRef(_, export_id) => Ok(self.global_values[export_id].clone()),
            ExprKind::Let(hir_let) => self.eval_let(fcx, b, hir_let),
            ExprKind::App(app) => self.eval_app(fcx, b, &expr.result_ty, app),
            ExprKind::Recur(recur) => self.eval_recur(fcx, b, &expr.result_ty, recur),
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
        fcx: &mut FunCtx<'_>,
        b: &mut Option<Builder>,
        expr: &Expr,
    ) -> Result<Value> {
        self.eval_expr_with_source_name(fcx, b, expr, None)
    }

    fn consume_expr_with_source_name(
        &mut self,
        fcx: &mut FunCtx<'_>,
        b: &mut Option<Builder>,
        expr: Expr,
        source_name: Option<&DataStr>,
    ) -> Result<Value> {
        use crate::hir::ExprKind;
        match expr.kind {
            ExprKind::Fun(fun_expr) => Ok(self.eval_arret_fun(fcx, *fun_expr, source_name)),
            ExprKind::RustFun(rust_fun) => Ok(Value::RustFun(rust_fun)),
            _ => self.eval_expr_with_source_name(fcx, b, &expr, source_name),
        }
    }

    pub fn consume_expr(
        &mut self,
        fcx: &mut FunCtx<'_>,
        b: &mut Option<Builder>,
        expr: Expr,
    ) -> Result<Value> {
        self.consume_expr_with_source_name(fcx, b, expr, None)
    }

    /// Evaluates the main function of a program
    pub fn eval_main_fun(&mut self, main_export_id: hir::ExportId) -> Result<()> {
        let mut fcx = FunCtx::new(Some(main_export_id.module_id()));
        let main_value = self.eval_local_ref(&fcx, main_export_id.local_id());

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
    pub fn into_built_program(mut self, main_export_id: hir::ExportId) -> Result<BuiltProgram> {
        let fcx = FunCtx::new(Some(main_export_id.module_id()));
        let main_value = self.eval_local_ref(&fcx, main_export_id.local_id());

        let main_arret_fun = if let Value::ArretFun(main_arret_fun) = main_value {
            main_arret_fun
        } else {
            unimplemented!("Non-Arret main!");
        };

        let main_abi = PolymorphAbi {
            call_conv: ops::CallConv::Ccc,

            // Main is a top-level function; it can't capture
            has_captures: false,
            fixed_params: Box::new([]),
            rest_param: None,

            ret: abitype::RetAbiType::Void,
        };

        let main = self.ops_for_arret_fun(&main_arret_fun, main_abi);

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
