use syntax;
use syntax::parser::data_from_str_with_span_offset;

use hir;
use hir::scope::Scope;
use reporting::Reportable;
use {PackagePaths, SourceLoader};

use typeck;
use typeck::infer::InferCtx;

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
}

#[derive(Debug, PartialEq)]
pub enum EvaledLine {
    EmptyInput,
    Defs(usize),
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

    pub fn eval_line(&mut self, input: String) -> Result<EvaledLine, Error> {
        use hir::lowering::LoweredReplDatum;

        let mut input_data = {
            let source_loader = self.lcx.source_loader_mut();
            let source_id = source_loader.load_string("<repl input>".into(), input);
            let source_file = source_loader.source_file(source_id);

            data_from_str_with_span_offset(source_file.source(), source_file.span_offset())?
        };

        let input_datum = match input_data.len() {
            0 => {
                return Ok(EvaledLine::EmptyInput);
            }
            1 => input_data.pop().unwrap(),
            _ => {
                use hir::error::{Error, ErrorKind};
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
                let inferred_defs = self.icx.infer_defs(lcx.pvars(), lcx.tvars(), defs)?;

                Ok(EvaledLine::Defs(inferred_defs.len()))
            }
            LoweredReplDatum::Expr(decl_expr) => {
                let lcx = &self.lcx;
                let node = self.icx.infer_expr(lcx.pvars(), lcx.tvars(), decl_expr)?;
                let poly_str = hir::str_for_poly(lcx.pvars(), lcx.tvars(), node.poly_type());

                Ok(EvaledLine::Expr(poly_str))
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

        assert_eq!(
            EvaledLine::EmptyInput,
            repl_ctx.eval_line("   ".to_owned()).unwrap()
        );

        assert_eq!(
            EvaledLine::EmptyInput,
            repl_ctx.eval_line("; COMMENT!".to_owned()).unwrap()
        );

        assert_eq!(
            EvaledLine::Expr("Int".to_owned()),
            repl_ctx.eval_line("1".to_owned()).unwrap()
        );

        repl_ctx
            .eval_line("(import (only [stdlib base] quote def do int?))".to_owned())
            .unwrap();

        // Make sure we can references vars from the imported module
        assert_eq!(
            EvaledLine::Expr("true".to_owned()),
            repl_ctx.eval_line("(int? 5)".to_owned()).unwrap()
        );

        // Make sure we can redefine
        assert_eq!(
            EvaledLine::Defs(1),
            repl_ctx.eval_line("(def x 'first)".to_owned()).unwrap()
        );

        assert_eq!(
            EvaledLine::Defs(1),
            repl_ctx.eval_line("(def x 'second)".to_owned()).unwrap()
        );

        assert_eq!(
            EvaledLine::Expr("'second".to_owned()),
            repl_ctx.eval_line("x".to_owned()).unwrap()
        );

        // Make sure we can handle `(do)` at the module level with recursive defs
        assert_eq!(
            EvaledLine::Defs(2),
            repl_ctx
                .eval_line("(do (def x y) (def y 2))".to_owned())
                .unwrap()
        );

        assert_eq!(
            EvaledLine::Expr("Int".to_owned()),
            repl_ctx.eval_line("x".to_owned()).unwrap()
        );

        // And `(do)` at the expression level
        assert_eq!(
            EvaledLine::Expr("'baz".to_owned()),
            repl_ctx
                .eval_line("(do 'foo 'bar 'baz)".to_owned())
                .unwrap()
        );
    }
}
