use arret_syntax::datum::DataStr;

use codespan::FileName;
use codespan_reporting::{Diagnostic, Label};

use crate::hir;
use crate::hir::scope::Scope;
use crate::reporting::{diagnostic_for_syntax_error, errors_to_diagnostics};
use crate::CompileCtx;
use crate::SourceLoader;

use crate::mir::eval_hir::EvalHirCtx;
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
pub enum EvaledLine {
    EmptyInput,
    Defs,
    Expr(String),
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

        let ns_datum = hir::ns::NsDatum::from_syntax_datum(input_datum);

        match self
            .lcx
            .lower_repl_datum(&mut self.scope, ns_datum)
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

                match kind {
                    EvalKind::Type => {
                        let poly_str = hir::str_for_ty_ref(node.result_ty());
                        Ok(EvaledLine::Expr(poly_str))
                    }
                    EvalKind::Value => {
                        use crate::mir::eval_hir::FunCtx;
                        use arret_runtime_syntax::writer;
                        use std::str;

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
                        let output_str = str::from_utf8(output_buf.as_slice()).unwrap().to_owned();

                        Ok(EvaledLine::Expr(output_str))
                    }
                }
            }
        }
    }
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn basic_session() {
        use crate::{initialise_llvm, PackagePaths};

        initialise_llvm(false);

        let ccx = CompileCtx::new(PackagePaths::test_paths(None), true);
        let mut repl_ctx = ReplCtx::new(&ccx);

        macro_rules! assert_empty {
            ($line:expr) => {
                assert_eq!(
                    EvaledLine::EmptyInput,
                    repl_ctx
                        .eval_line($line.to_owned(), EvalKind::Value)
                        .unwrap()
                );
            };
        }

        macro_rules! assert_defs {
            ($line:expr) => {
                assert_eq!(
                    EvaledLine::Defs,
                    repl_ctx
                        .eval_line($line.to_owned(), EvalKind::Value)
                        .unwrap()
                );
            };
        }

        macro_rules! assert_type {
            ($expected:expr, $line:expr) => {
                assert_eq!(
                    EvaledLine::Expr($expected.to_owned()),
                    repl_ctx
                        .eval_line($line.to_owned(), EvalKind::Type)
                        .unwrap()
                );
            };
        }

        macro_rules! assert_value {
            ($expected:expr, $line:expr) => {
                assert_eq!(
                    EvaledLine::Expr($expected.to_owned()),
                    repl_ctx
                        .eval_line($line.to_owned(), EvalKind::Value)
                        .unwrap()
                );
            };
        }

        assert_empty!("       ");
        assert_empty!("; COMMENT!");

        assert_type!("Int", "1");
        assert_value!("1", "1");

        repl_ctx
            .eval_line("(import [stdlib base])".to_owned(), EvalKind::Value)
            .expect(
                "unable to load stdlib library; you may need to `cargo build` before running tests",
            );

        // Make sure we can references vars from the imported module
        assert_type!("true", "(int? 5)");
        assert_value!("true", "(int? 5)");

        // Make sure we can redefine
        assert_defs!("(def x 'first)");
        assert_defs!("(def x 'second)");
        assert_type!("'second", "x");
        assert_value!("second", "x");

        // `(do)` at the expression level
        assert_type!("'baz", "(do 'foo 'bar 'baz)");
        assert_value!("baz", "(do 'foo 'bar 'baz)");

        // Polymorphic capturing closures
        assert_defs!("(def return-constant (fn #{T} ([x T]) (fn () -> T x)))");
        assert_defs!("(def return-one (return-constant 1))");
        assert_defs!("(def return-two (return-constant 'two))");

        assert_type!("Int", "(return-one)");
        assert_value!("1", "(return-one)");

        assert_type!("'two", "(return-two)");
        assert_value!("two", "(return-two)");
    }
}
