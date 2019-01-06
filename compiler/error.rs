use crate::hir;
use crate::mir;
use crate::typeck;

use crate::reporting::Reportable;

#[cfg_attr(test, derive(Debug))]
pub struct Error(pub Vec<Box<dyn Reportable>>);

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

impl From<mir::error::Error> for Error {
    fn from(mir_err: mir::error::Error) -> Error {
        match mir_err {
            mir::error::Error::Panic(mir_panic) => Error(vec![Box::new(mir_panic)]),
            other => panic!(
                "attempted to convert internal MIR error {:?} to external error",
                other
            ),
        }
    }
}
