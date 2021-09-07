use std::collections::{HashMap, HashSet};
use std::sync::Arc;
use std::thread;

use codespan_reporting::diagnostic::Diagnostic;

use arret_syntax::datum::DataStr;
use arret_syntax::span::FileId;

use crate::context;
use crate::context::ModuleId;
use crate::hir;
use crate::hir::scope::Scope;
use crate::reporting::{diagnostic_for_syntax_error, errors_to_diagnostics, new_primary_label};
use crate::ty;
use crate::CompileCtx;

use crate::mir::eval_hir::EvalHirCtx;
use crate::mir::Value;
use crate::typeck::infer::{infer_module, infer_repl_expr};

/// Indicates the kind of evaluation to perform on the input
///
/// This applies to expressions; it has no effect on empty input or definitions
#[derive(Clone, Copy)]
pub enum EvalKind {
    /// Infers the type of the expression
    ///
    /// This only runs as far as type checking
    Type,

    /// Fully evaluates the expression
    Value,
}

#[derive(Debug, PartialEq)]
pub struct EvaledExprValue {
    /// Rendered type of the expression
    pub type_str: String,

    /// Rendered value of the expression
    pub value_str: String,

    /// Indicates if the type is a literal
    ///
    /// REPL implementations may want to suppress printing the type of literal values as they
    /// contain no additional information.
    pub type_is_literal: bool,
}

#[derive(Debug, PartialEq)]
pub enum EvaledLine {
    /// Line was all whitepsace
    EmptyInput,

    /// Line added new definitions
    ///
    /// All bound identifiers in the root scope are returned. This is useful for tab &
    /// autocompletion.
    Defs(Vec<DataStr>),

    /// Line was evaluated to a type with the given name
    ExprType(String),

    /// Line was evaluate to a value
    ExprValue(EvaledExprValue),
}

struct ReplEngine<'ccx> {
    root_scope: Scope<'static>,
    ccx: &'ccx CompileCtx,

    inferred_module_vars: HashMap<context::ModuleId, Arc<HashMap<hir::LocalId, ty::Ref<ty::Poly>>>>,
    seen_modules: HashSet<context::ModuleId>,

    ehx: EvalHirCtx,
}

impl<'ccx> ReplEngine<'ccx> {
    fn new(ccx: &'ccx CompileCtx) -> Self {
        Self {
            root_scope: Scope::root(),
            ccx,

            seen_modules: HashSet::new(),
            inferred_module_vars: HashMap::new(),

            ehx: EvalHirCtx::new(ccx.enable_optimisations()),
        }
    }

    /// Returns all names bound in the root scope and namespace
    fn bound_names(&self) -> Vec<DataStr> {
        self.root_scope
            .bound_idents()
            .filter_map(move |ident| {
                if ident.ns_id() == Scope::root_ns_id() {
                    Some(ident.name().clone())
                } else {
                    None
                }
            })
            .collect()
    }

    /// Visits a subtree of modules and adds any missing defs and inferred module vars
    fn visit_module_tree(
        &mut self,
        root_module: &Arc<context::Module>,
    ) -> Result<(), Vec<Diagnostic<FileId>>> {
        if self.seen_modules.contains(&root_module.module_id) {
            return Ok(());
        }

        self.seen_modules.insert(root_module.module_id);

        // Make sure our imports are first
        for import in root_module.imports.values() {
            self.visit_module_tree(import)?;
        }

        self.inferred_module_vars
            .insert(root_module.module_id, root_module.inferred_locals.clone());

        self.ehx
            .visit_module_defs(root_module.module_id, &root_module.defs)?;

        Ok(())
    }

    fn eval_line(
        &mut self,
        input: String,
        kind: EvalKind,
    ) -> Result<EvaledLine, Vec<Diagnostic<FileId>>> {
        use std::io::Write;

        use crate::hir::lowering::LoweredReplDatum;

        let source_file = self.ccx.source_loader().load_string("repl".into(), input);

        let input_data = source_file
            .parsed()
            .map_err(|err| vec![diagnostic_for_syntax_error(&err)])?;

        let input_datum = match input_data {
            [] => {
                return Ok(EvaledLine::EmptyInput);
            }
            [input_datum] => input_datum,
            _ => {
                let extra_span = input_data[1].span();

                return Err(vec![Diagnostic::error()
                    .with_message("unexpected trailing datum")
                    .with_labels(vec![new_primary_label(
                        extra_span,
                        "trailing datum",
                    )])]);
            }
        };

        let module_id = ModuleId::alloc();
        let mut child_scope = Scope::child(&self.root_scope);

        let lowered_repl_datum =
            hir::lowering::lower_repl_datum(self.ccx, &mut child_scope, input_datum)
                .map_err(errors_to_diagnostics)?;

        // Bring all the defs back in to root scope
        let exported_bindings = child_scope.into_exported_bindings();
        self.root_scope
            .import_bindings(exported_bindings, module_id);

        match lowered_repl_datum {
            LoweredReplDatum::Import(modules) => {
                for module in modules.values() {
                    self.visit_module_tree(module)?;
                }

                Ok(EvaledLine::Defs(self.bound_names()))
            }
            LoweredReplDatum::EvaluableDef(def) => {
                let inferred_module = infer_module(&self.inferred_module_vars, vec![def])
                    .map_err(errors_to_diagnostics)?;

                self.inferred_module_vars
                    .insert(module_id, Arc::new(inferred_module.inferred_locals));

                self.ehx
                    .consume_module_defs(module_id, inferred_module.defs)?;

                Ok(EvaledLine::Defs(self.bound_names()))
            }
            LoweredReplDatum::NonEvaluableDef => {
                // This was handled entirely by HIR lowering
                Ok(EvaledLine::Defs(self.bound_names()))
            }
            LoweredReplDatum::Expr(decl_expr) => {
                let node = infer_repl_expr(&self.inferred_module_vars, decl_expr)?;
                let type_str = hir::str_for_ty_ref(node.result_ty());

                match kind {
                    EvalKind::Type => Ok(EvaledLine::ExprType(type_str)),
                    EvalKind::Value => {
                        use crate::mir::eval_hir::FunCtx;
                        use arret_runtime_syntax::writer;
                        use std::str;

                        let type_is_literal = ty::props::is_literal(node.result_ty());

                        // Evaluate the expression
                        let mut fcx = FunCtx::new(None);

                        let value = self
                            .ehx
                            .consume_expr(&mut fcx, &mut None, node.into_expr())?;

                        let boxed = self
                            .ehx
                            .value_to_const(&value)
                            .expect("Received register from MIR evaluation");

                        // Write the result to a string
                        let mut output_buf: Vec<u8> = vec![];
                        writer::write_boxed(&mut output_buf, &self.ehx, boxed).unwrap();

                        // Just `#fn` isn't very useful, even with a type. Add the source name.
                        match value {
                            Value::ArretFun(arret_fun) => {
                                if let Some(source_name) = arret_fun.source_name() {
                                    write!(&mut output_buf, "/{}", source_name).unwrap();
                                }
                            }
                            Value::RustFun(rust_fun) => {
                                write!(&mut output_buf, "/{}", rust_fun.symbol()).unwrap();
                            }
                            _ => {}
                        }

                        let value_str = str::from_utf8(output_buf.as_slice()).unwrap().to_owned();

                        Ok(EvaledLine::ExprValue(EvaledExprValue {
                            type_str,
                            value_str,
                            type_is_literal,
                        }))
                    }
                }
            }
        }
    }
}

pub struct ReplCtx {
    send_line: crossbeam_channel::Sender<(String, EvalKind)>,
    receive_result: crossbeam_channel::Receiver<Result<EvaledLine, Vec<Diagnostic<FileId>>>>,
}

#[derive(Debug)]
pub struct EngineDisconnected;

impl ReplCtx {
    /// Creates a new `ReplCtx`
    ///
    /// This will launch a REPL engine thread which can asynchronously evaluate lines.
    pub fn new(ccx: Arc<CompileCtx>) -> Self {
        let (send_line, receive_line) = crossbeam_channel::unbounded();
        let (send_result, receive_result) = crossbeam_channel::unbounded();

        thread::spawn(move || {
            let mut engine = ReplEngine::new(&ccx);

            for (input, kind) in receive_line.iter() {
                let result = engine.eval_line(input, kind);
                send_result.send(result).unwrap();

                if engine.ehx.should_collect() {
                    engine.ehx.collect_garbage();
                }
            }
        });

        Self {
            send_line,
            receive_result,
        }
    }

    /// Sends a line to be evaluated by the REPL engine
    ///
    /// This is asynchronous and an unlimited number of lines can be sent before reading their
    /// results. This allows the calling thread to remain responsive to user input while evaluation,
    /// garbage collection, etc occurs.
    pub fn send_line(&self, input: String, kind: EvalKind) -> Result<(), EngineDisconnected> {
        self.send_line
            .send((input, kind))
            .map_err(|_| EngineDisconnected)
    }

    /// Receives the next result from the REPL engine
    ///
    /// These will be returned in the order they were submitted with `send_line`.
    pub fn receive_result(&self) -> Result<EvaledLine, Vec<Diagnostic<FileId>>> {
        self.receive_result.recv().unwrap()
    }
}

#[cfg(test)]
mod test {
    use super::*;

    fn eval_line_sync(
        rcx: &mut ReplCtx,
        input: String,
        kind: EvalKind,
    ) -> Result<EvaledLine, Vec<Diagnostic<FileId>>> {
        rcx.send_line(input, kind).unwrap();
        rcx.receive_result()
    }

    fn assert_defs(rcx: &mut ReplCtx, line: &'static str) {
        match eval_line_sync(rcx, line.to_owned(), EvalKind::Value).unwrap() {
            EvaledLine::Defs(_) => {}
            other => {
                panic!("Expected defs, got {:?}", other);
            }
        }
    }

    fn assert_empty(rcx: &mut ReplCtx, line: &'static str) {
        assert_eq!(
            EvaledLine::EmptyInput,
            eval_line_sync(rcx, line.to_owned(), EvalKind::Value).unwrap()
        );
    }

    fn assert_expr(
        rcx: &mut ReplCtx,
        expected_value: &'static str,
        expected_type: &'static str,
        line: &'static str,
    ) {
        assert_eq!(
            EvaledLine::ExprType(expected_type.to_owned()),
            eval_line_sync(rcx, line.to_owned(), EvalKind::Type).unwrap()
        );

        match eval_line_sync(rcx, line.into(), EvalKind::Value).unwrap() {
            EvaledLine::ExprValue(EvaledExprValue {
                value_str,
                type_str,
                ..
            }) => {
                assert_eq!(value_str, expected_value.to_owned());
                assert_eq!(type_str, expected_type.to_owned());
            }
            other => {
                panic!("unexpected REPL result: {:?}", other);
            }
        }
    }

    #[test]
    fn basic_session() {
        use crate::codegen::test::initialise_test_llvm;
        use crate::PackagePaths;

        initialise_test_llvm();

        let ccx = Arc::new(CompileCtx::new(PackagePaths::test_paths(None), true));
        let mut rcx = ReplCtx::new(ccx);

        assert_empty(&mut rcx, "       ");
        assert_empty(&mut rcx, "; COMMENT!");

        assert_expr(&mut rcx, "1", "Int", "1");

        eval_line_sync(
            &mut rcx,
            "(import [stdlib base])".to_owned(),
            EvalKind::Value,
        )
        .expect(
            "unable to load stdlib library; you may need to `cargo build` before running tests",
        );

        // Make sure we can references vars from the imported module
        assert_expr(&mut rcx, "true", "true", "(int? 5)");

        // Make sure we can redefine
        assert_defs(&mut rcx, "(def x 'first)");
        assert_defs(&mut rcx, "(def x 'second)");
        assert_expr(&mut rcx, "second", "'second", "x");

        // `(do)` at the expression level
        assert_expr(&mut rcx, "baz", "'baz", "(do 'foo 'bar 'baz)");

        // Polymorphic capturing closures
        assert_defs(
            &mut rcx,
            "(def return-constant (fn #{T} ([x T]) (fn () -> T x)))",
        );

        assert_defs(&mut rcx, "(def return-one (return-constant 1))");
        assert_defs(&mut rcx, "(def return-two (return-constant 'two))");

        assert_expr(&mut rcx, "1", "Int", "(return-one)");
        assert_expr(&mut rcx, "two", "'two", "(return-two)");
    }
}
