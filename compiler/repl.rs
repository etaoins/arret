use crate::error::Error;
use crate::hir;
use crate::hir::scope::Scope;
use crate::{PackagePaths, SourceKind, SourceLoader};

use crate::mir::eval_hir::EvalHirCtx;
use crate::typeck::infer::InferCtx;

pub struct ReplCtx<'pp, 'sl> {
    scope: Scope,
    ns_id: hir::ns::NsId,
    lcx: hir::lowering::LoweringCtx<'pp, 'sl>,
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

impl<'pp, 'sl> ReplCtx<'pp, 'sl> {
    pub fn new(
        package_paths: &'pp PackagePaths,
        source_loader: &'sl mut SourceLoader,
    ) -> ReplCtx<'pp, 'sl> {
        let ns_id = Scope::root_ns_id();
        let scope = Scope::new_repl();

        ReplCtx {
            scope,
            ns_id,
            lcx: hir::lowering::LoweringCtx::new(package_paths, source_loader),
            icx: InferCtx::new(),
            ehx: EvalHirCtx::new(),
        }
    }

    pub fn source_loader(&self) -> &SourceLoader {
        self.lcx.source_loader()
    }

    /// Returns all names bound in the root scope and namespace
    pub fn bound_names(&self) -> impl Iterator<Item = &str> {
        let ns_id = self.ns_id;

        self.scope.bound_idents().filter_map(move |ident| {
            if ident.ns_id() == ns_id {
                Some(ident.name())
            } else {
                None
            }
        })
    }

    pub fn eval_line(&mut self, input: String, kind: EvalKind) -> Result<EvaledLine, Error> {
        use crate::hir::lowering::LoweredReplDatum;

        let source_loader = self.lcx.source_loader_mut();
        let source_id = source_loader.load_string(SourceKind::Repl, input);
        let source_file = source_loader.source_file(source_id);

        let mut input_data = source_file.parse()?;

        let input_datum = match input_data.len() {
            0 => {
                return Ok(EvaledLine::EmptyInput);
            }
            1 => input_data.pop().unwrap(),
            _ => {
                use crate::hir::error::{Error, ErrorKind};
                let extra_span = input_data[1].span();

                return Err(Error(vec![Box::new(Error::new(
                    extra_span,
                    ErrorKind::IllegalArg("unexpected trailing datum"),
                ))]));
            }
        };

        // TODO: Do this for now to catch GC bugs. This should be done only when required and in
        // a background thread.
        self.ehx.collect_garbage();

        let ns_datum = hir::ns::NsDatum::from_syntax_datum(self.ns_id, input_datum);

        match self.lcx.lower_repl_datum(&mut self.scope, ns_datum)? {
            LoweredReplDatum::Defs(defs) => {
                for recursive_defs in defs {
                    let inferred_defs = self.icx.infer_defs(recursive_defs)?;

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
                        use crate::ty;
                        use crate::ty::purity;

                        let poly_str = hir::str_for_poly(
                            &purity::PVars::new(),
                            &ty::TVars::new(),
                            node.poly_type(),
                        );
                        Ok(EvaledLine::Expr(poly_str))
                    }
                    EvalKind::Value => {
                        use crate::mir::eval_hir::DefCtx;
                        use runtime_syntax::writer;
                        use std::str;

                        // Evaluate the expression
                        let mut dcx = DefCtx::new();

                        let value = self
                            .ehx
                            .consume_expr(&mut dcx, &mut None, node.into_expr())?;

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
        use crate::initialise_llvm;
        initialise_llvm(false);

        let package_paths = PackagePaths::test_paths();
        let mut source_loader = SourceLoader::new();
        let mut repl_ctx = ReplCtx::new(&package_paths, &mut source_loader);

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
            .unwrap();

        // Make sure we can references vars from the imported module
        assert_type!("true", "(int? 5)");
        assert_value!("true", "(int? 5)");

        // Make sure we can redefine
        assert_defs!("(def x 'first)");
        assert_defs!("(def x 'second)");
        assert_type!("'second", "x");
        assert_value!("second", "x");

        // Make sure we can handle `(do)` at the module level with recursive defs
        assert_defs!("(do (def x y) (def y 2))");
        assert_type!("Int", "x");
        assert_value!("2", "x");

        // And `(do)` at the expression level
        assert_type!("'baz", "(do 'foo 'bar 'baz)");
        assert_value!("baz", "(do 'foo 'bar 'baz)");

        // Polymorphic capturing closures
        assert_defs!("(def return-constant (fn #{T} ([x : T]) (fn () -> T x)))");
        assert_defs!("(def return-one (return-constant 1))");
        assert_defs!("(def return-two (return-constant 'two))");

        assert_type!("Int", "(return-one)");
        assert_value!("1", "(return-one)");

        assert_type!("'two", "(return-two)");
        assert_value!("two", "(return-two)");
    }
}
