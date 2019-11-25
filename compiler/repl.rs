use arret_syntax::datum::DataStr;

use codespan::FileName;
use codespan_reporting::{Diagnostic, Label};

use crate::hir;
use crate::hir::scope::Scope;
use crate::reporting::{diagnostic_for_syntax_error, errors_to_diagnostics};
use crate::ty;
use crate::CompileCtx;
use crate::SourceLoader;

use crate::mir::eval_hir::EvalHirCtx;
use crate::mir::Value;
use crate::typeck::infer::InferCtx;

pub struct ReplCtx<'ccx> {
    scope: Scope<'static>,
    source_loader: &'ccx SourceLoader,
    lcx: hir::lowering::LoweringCtx<'ccx>,
    icx: InferCtx,
    ehx: EvalHirCtx,
}

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
    EmptyInput,
    Defs,
    ExprType(String),
    ExprValue(EvaledExprValue),
}

impl<'ccx> ReplCtx<'ccx> {
    pub fn new(ccx: &'ccx CompileCtx) -> Self {
        let scope = Scope::new_repl();

        ReplCtx {
            scope,
            source_loader: ccx.source_loader(),
            lcx: hir::lowering::LoweringCtx::new(ccx),
            icx: InferCtx::new(),
            ehx: EvalHirCtx::new(ccx.enable_optimisations()),
        }
    }

    /// Returns all names bound in the root scope and namespace
    pub fn bound_names(&self) -> impl Iterator<Item = &DataStr> {
        self.scope.bound_idents().filter_map(move |ident| {
            if ident.ns_id() == Scope::root_ns_id() {
                Some(ident.name())
            } else {
                None
            }
        })
    }

    pub fn eval_line(
        &mut self,
        input: String,
        kind: EvalKind,
    ) -> Result<EvaledLine, Vec<Diagnostic>> {
        use crate::hir::lowering::LoweredReplDatum;
        use std::io::Write;
        let source_file = self
            .source_loader
            .load_string(FileName::Virtual("repl".into()), input.into());

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

                return Err(vec![Diagnostic::new_error("unexpected trailing datum")
                    .with_label(
                        Label::new_primary(extra_span).with_message("trailing datum"),
                    )]);
            }
        };

        if self.ehx.should_collect() {
            self.ehx.collect_garbage();
        }

        match self
            .lcx
            .lower_repl_datum(&mut self.scope, input_datum)
            .map_err(errors_to_diagnostics)?
        {
            LoweredReplDatum::Defs(defs) => {
                for recursive_defs in defs {
                    let inferred_defs = self
                        .icx
                        .infer_defs(recursive_defs)
                        .map_err(errors_to_diagnostics)?;

                    for inferred_def in inferred_defs {
                        self.ehx.consume_def(inferred_def)?;
                    }
                }

                Ok(EvaledLine::Defs)
            }
            LoweredReplDatum::Expr(decl_expr) => {
                let node = self.icx.infer_expr(decl_expr)?;
                let type_str = hir::str_for_ty_ref(node.result_ty());

                match kind {
                    EvalKind::Type => Ok(EvaledLine::ExprType(type_str)),
                    EvalKind::Value => {
                        use crate::mir::eval_hir::FunCtx;
                        use arret_runtime_syntax::writer;
                        use std::str;

                        let type_is_literal = ty::props::is_literal(node.result_ty());

                        // Evaluate the expression
                        let mut fcx = FunCtx::new();

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

#[cfg(test)]
mod test {
    use super::*;

    fn assert_defs(rcx: &mut ReplCtx<'_>, line: &'static str) {
        assert_eq!(
            EvaledLine::Defs,
            rcx.eval_line(line.to_owned(), EvalKind::Value).unwrap()
        );
    }

    fn assert_empty(rcx: &mut ReplCtx<'_>, line: &'static str) {
        assert_eq!(
            EvaledLine::EmptyInput,
            rcx.eval_line(line.to_owned(), EvalKind::Value).unwrap()
        );
    }

    fn assert_expr(
        rcx: &mut ReplCtx<'_>,
        expected_value: &'static str,
        expected_type: &'static str,
        line: &'static str,
    ) {
        assert_eq!(
            EvaledLine::ExprType(expected_type.to_owned()),
            rcx.eval_line(line.to_owned(), EvalKind::Type).unwrap()
        );

        match rcx.eval_line(line.into(), EvalKind::Value).unwrap() {
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

        let ccx = CompileCtx::new(PackagePaths::test_paths(None), true);
        let mut rcx = ReplCtx::new(&ccx);

        assert_empty(&mut rcx, "       ");
        assert_empty(&mut rcx, "; COMMENT!");

        assert_expr(&mut rcx, "1", "Int", "1");

        rcx.eval_line("(import [stdlib base])".to_owned(), EvalKind::Value)
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
