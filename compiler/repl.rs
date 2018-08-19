use syntax;
use syntax::parser::data_from_str_with_span_offset;

use crate::hir;
use crate::hir::scope::Scope;
use crate::reporting::Reportable;
use crate::{PackagePaths, SourceKind, SourceLoader};

use crate::mir::partial_eval::PartialEvalCtx;
use crate::typeck;
use crate::typeck::infer::InferCtx;

#[cfg_attr(test, derive(Debug))]
pub struct Error(Vec<Box<dyn Reportable>>);

impl Error {
    pub fn reports(&self) -> impl Iterator<Item = &Box<dyn Reportable>> {
        self.0.iter()
    }
}

impl From<syntax::error::Error> for Error {
    fn from(syntax_err: syntax::error::Error) -> Error {
        Error(vec![Box::new(syntax_err)])
    }
}

impl From<Vec<hir::error::Error>> for Error {
    fn from(hir_errs: Vec<hir::error::Error>) -> Error {
        Error(
            hir_errs
                .into_iter()
                .map(|hir_err| Box::new(hir_err) as Box<dyn Reportable>)
                .collect(),
        )
    }
}

impl From<typeck::error::Error> for Error {
    fn from(type_err: typeck::error::Error) -> Error {
        Error(vec![Box::new(type_err)])
    }
}
impl From<Vec<typeck::error::Error>> for Error {
    fn from(type_errs: Vec<typeck::error::Error>) -> Error {
        Error(
            type_errs
                .into_iter()
                .map(|type_err| Box::new(type_err) as Box<dyn Reportable>)
                .collect(),
        )
    }
}

pub struct ReplCtx<'pp, 'sl> {
    scope: Scope,
    ns_id: hir::ns::NsId,
    lcx: hir::lowering::LoweringCtx<'pp, 'sl>,
    icx: InferCtx,
    pcx: PartialEvalCtx,
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
        let scope = Scope::new_repl();
        let ns_id = hir::ns::NsId::new(0);

        ReplCtx {
            scope,
            ns_id,
            lcx: hir::lowering::LoweringCtx::new(package_paths, source_loader),
            icx: InferCtx::new(),
            pcx: PartialEvalCtx::new(),
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

        let mut input_data =
            data_from_str_with_span_offset(source_file.source(), source_file.span_offset())?;

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

        let ns_datum = hir::ns::NsDatum::from_syntax_datum(self.ns_id, input_datum);

        match self.lcx.lower_repl_datum(&mut self.scope, ns_datum)? {
            LoweredReplDatum::Defs(defs) => {
                let lcx = &self.lcx;

                for recursive_defs in defs {
                    let inferred_defs =
                        self.icx
                            .infer_defs(lcx.pvars(), lcx.tvars(), recursive_defs)?;

                    for inferred_def in inferred_defs {
                        self.pcx.eval_def(inferred_def);
                    }
                }

                Ok(EvaledLine::Defs)
            }
            LoweredReplDatum::Expr(decl_expr) => {
                let lcx = &self.lcx;
                let node = self.icx.infer_expr(lcx.pvars(), lcx.tvars(), decl_expr)?;

                match kind {
                    EvalKind::Type => {
                        let poly_str =
                            hir::str_for_poly(lcx.pvars(), lcx.tvars(), node.poly_type());
                        Ok(EvaledLine::Expr(poly_str))
                    }
                    EvalKind::Value => {
                        use runtime_syntax::writer;
                        use std::str;

                        // Perform partial evaluation on the expression
                        // TODO: Shouldn't need to `.into_owned()` here
                        let value = self.pcx.eval_expr(node.expr()).into_owned();
                        let boxed = self.pcx.value_to_boxed(&value);

                        // Write the result to a string
                        let mut output_buf: Vec<u8> = vec![];
                        writer::write_boxed(&mut output_buf, &self.pcx, boxed).unwrap();
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
        let package_paths = PackagePaths::default();
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
            .eval_line(
                "(import (only [stdlib base] quote def do int?))".to_owned(),
                EvalKind::Value,
            ).unwrap();

        // Make sure we can references vars from the imported module
        assert_type!("true", "(int? 5)");
        // TODO: This isn't implemented although it should be
        // assert_value!("true", "(int? 5)");

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
    }
}
