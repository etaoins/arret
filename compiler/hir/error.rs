use std::{error, fmt, io, path, result};

use codespan_reporting::{Diagnostic, Label};

use arret_syntax::datum::DataStr;
use arret_syntax::error::Error as SyntaxError;
use arret_syntax::span::Span;

use crate::reporting::{diagnostic_for_syntax_error, LocTrace};

#[derive(Debug, PartialEq, Clone)]
pub enum ErrorKind {
    PrimRef,
    TyRef,
    MacroRef(DataStr),
    UnboundIdent(DataStr),
    WrongArgCount(usize),
    IllegalArg(&'static str),
    NoMainFun,
    ExpectedSym,
    DefOutsideBody,
    ExportOutsideModule,
    NonDefInsideModule,
    ExportInsideRepl,
    PackageNotFound,
    ModuleNotFound(Box<path::Path>),
    NoMacroRule,
    DuplicateDef(Option<Span>, DataStr),
    MultipleZeroOrMoreMatch(Span),
    NoVecDestruc,
    ValueAsTy,
    UserError(DataStr),
    ReadError(Box<path::Path>),
    SyntaxError(SyntaxError),
    RustFunError(Box<str>),
    KeywordDestruc,
    BadListDestruc,
    BadRestDestruc,
}

#[derive(Debug, PartialEq, Clone)]
pub struct Error {
    loc_trace: LocTrace,
    kind: ErrorKind,
}

pub type Result<T, E = Error> = result::Result<T, E>;

impl Error {
    pub fn new(span: Span, kind: ErrorKind) -> Error {
        Error {
            loc_trace: span.into(),
            kind,
        }
    }

    pub fn kind(&self) -> &ErrorKind {
        &self.kind
    }

    pub fn from_module_io(span: Span, path: &path::Path, error: &io::Error) -> Error {
        match error.kind() {
            io::ErrorKind::NotFound => Error::new(span, ErrorKind::ModuleNotFound(path.into())),
            _ => Error::new(span, ErrorKind::ReadError(path.into())),
        }
    }

    pub fn with_macro_invocation_span(self, span: Span) -> Error {
        Error {
            loc_trace: self.loc_trace.with_macro_invocation(span),
            ..self
        }
    }
}

impl From<Error> for Diagnostic {
    fn from(error: Error) -> Diagnostic {
        let origin = error.loc_trace.origin();

        let diagnostic = match error.kind() {
            ErrorKind::PrimRef => Diagnostic::new_error("cannot take the value of a primitive")
                .with_label(Label::new_primary(origin).with_message("value expected")),

            ErrorKind::TyRef => Diagnostic::new_error("cannot take the value of a type")
                .with_label(Label::new_primary(origin).with_message("value expected")),

            ErrorKind::MacroRef(ref sym) => {
                Diagnostic::new_error(format!("cannot take the value of macro `{}`", sym))
                    .with_label(Label::new_primary(origin).with_message("value expected"))
            }

            ErrorKind::UnboundIdent(ref ident) => {
                Diagnostic::new_error(format!("unable to resolve `{}`", ident))
                    .with_label(Label::new_primary(origin).with_message("not found in this scope"))
            }

            ErrorKind::WrongArgCount(expected) => {
                let label_message = if *expected == 1 {
                    "expected 1 argument".to_owned()
                } else {
                    format!("expected {} arguments", expected)
                };

                Diagnostic::new_error(format!("wrong argument count; expected {}", expected))
                    .with_label(Label::new_primary(origin).with_message(label_message))
            }

            ErrorKind::IllegalArg(description) => {
                // TODO: This makes it hard to give rich diagnostics. This should be deprecated.
                Diagnostic::new_error(*description).with_label(Label::new_primary(origin))
            }
            ErrorKind::ExpectedSym => Diagnostic::new_error("expected symbol")
                .with_label(Label::new_primary(origin).with_message("expected symbol")),

            ErrorKind::DefOutsideBody => Diagnostic::new_error("definition outside module body")
                .with_label(Label::new_primary(origin).with_message("not at top-level of module")),

            ErrorKind::DuplicateDef(first_def_span, ref ident) => {
                let diagnostic =
                    Diagnostic::new_error(format!("duplicate definition of`{}`", ident))
                        .with_label(
                            Label::new_primary(origin).with_message("second definition here"),
                        );

                if let Some(first_def_span) = first_def_span {
                    diagnostic.with_label(
                        Label::new_secondary(*first_def_span).with_message("first definition here"),
                    )
                } else {
                    diagnostic
                }
            }

            ErrorKind::ExportOutsideModule => Diagnostic::new_error(
                "(export) outside of module body",
            )
            .with_label(Label::new_primary(origin).with_message("not at top-level of module")),

            ErrorKind::NonDefInsideModule => {
                Diagnostic::new_error("value at top-level of module body").with_label(
                    Label::new_primary(origin)
                        .with_message("(import), (export) or definition expected"),
                )
            }

            ErrorKind::ExportInsideRepl => {
                Diagnostic::new_error("export not supported within REPL")
                    .with_label(Label::new_primary(origin).with_message("export not supported"))
            }

            ErrorKind::PackageNotFound => Diagnostic::new_error("package not found")
                .with_label(Label::new_primary(origin).with_message("at this import")),

            ErrorKind::ModuleNotFound(ref filename) => Diagnostic::new_error(format!(
                "module not found at `{}`",
                filename.to_string_lossy()
            ))
            .with_label(Label::new_primary(origin).with_message("at this import")),

            ErrorKind::NoMacroRule => Diagnostic::new_error("no matching macro rule")
                .with_label(Label::new_primary(origin).with_message("at this macro invocation")),

            ErrorKind::MultipleZeroOrMoreMatch(first_zero_or_more_span) => {
                Diagnostic::new_error("multiple zero or more matches in the same sequence")
                    .with_label(
                        Label::new_primary(origin).with_message("second zero or more match"),
                    )
                    .with_label(
                        Label::new_secondary(*first_zero_or_more_span)
                            .with_message("first zero or more match"),
                    )
            }

            ErrorKind::NoVecDestruc => Diagnostic::new_error(
                "vectors can only be used in a destructure in the form `[name Type]`",
            )
            .with_label(Label::new_primary(origin).with_message("unexpected vector")),

            ErrorKind::ValueAsTy => Diagnostic::new_error("value cannot be used as a type")
                .with_label(Label::new_primary(origin).with_message("type expected")),

            ErrorKind::UserError(ref message) => Diagnostic::new_error(message.as_ref())
                .with_label(Label::new_primary(origin).with_message("user error raised here")),

            ErrorKind::ReadError(ref filename) => {
                Diagnostic::new_error(format!("error reading `{}`", filename.to_string_lossy()))
                    .with_label(Label::new_primary(origin).with_message("at this import"))
            }

            ErrorKind::SyntaxError(ref err) => {
                // Just proxy this
                return diagnostic_for_syntax_error(err);
            }

            ErrorKind::RustFunError(ref message) => Diagnostic::new_error(
                "error loading RFI module",
            )
            .with_label(Label::new_primary(origin).with_message(message.clone().into_string())),

            ErrorKind::NoMainFun => {
                Diagnostic::new_error("no main! function defined in root module").with_label(
                    Label::new_primary(origin).with_message("main! function expected in this file"),
                )
            }

            ErrorKind::KeywordDestruc => {
                Diagnostic::new_error("keywords cannot be used as variable names")
                    .with_label(Label::new_primary(origin).with_message("expected symbol"))
            }

            ErrorKind::BadListDestruc => Diagnostic::new_error("unsupported destructuring binding")
                .with_label(
                    Label::new_primary(origin)
                        .with_message("expected variable name, list or `[name Type]`"),
                ),

            ErrorKind::BadRestDestruc => Diagnostic::new_error("unsupported rest destructuring")
                .with_label(
                    Label::new_primary(origin)
                        .with_message("expected variable name or `[name Type]`"),
                ),
        };

        error.loc_trace.label_macro_invocation(diagnostic)
    }
}

impl error::Error for Error {}

impl fmt::Display for Error {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let diagnostic: Diagnostic = self.clone().into();
        f.write_str(&diagnostic.message)
    }
}

impl From<SyntaxError> for Error {
    fn from(err: SyntaxError) -> Error {
        Error::new(err.span(), ErrorKind::SyntaxError(err))
    }
}

impl From<Error> for Vec<Error> {
    fn from(error: Error) -> Vec<Error> {
        vec![error]
    }
}
