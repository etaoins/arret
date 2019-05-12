use std::{error, fmt, io, path, result};

use codespan_reporting::{Diagnostic, Label};

use arret_syntax::datum::DataStr;
use arret_syntax::error::Error as SyntaxError;
use arret_syntax::span::Span;

use crate::reporting::{diagnostic_for_syntax_error, LocTrace};

#[derive(Debug, PartialEq, Clone)]
pub enum ErrorKind {
    ExpectedValue(&'static str),
    ExpectedTy(&'static str),
    ExpectedTyCons(&'static str),
    ExpectedSym(&'static str),
    ExpectedParamList(&'static str),
    UnboundIdent(DataStr),
    WrongArgCount(usize),
    IllegalArg(&'static str),
    NoMainFun,
    DefOutsideBody,
    ExportOutsideModule,
    NonDefInsideModule,
    ExportInsideRepl,
    PackageNotFound,
    ModuleNotFound(Box<path::Path>),
    NoMacroRule(Box<[Span]>),
    DuplicateDef(Option<Span>, DataStr),
    MultipleZeroOrMoreMatch(Span),
    NoVecDestruc,
    UserError(DataStr),
    ReadError(Box<path::Path>),
    SyntaxError(SyntaxError),
    RustFunError(Box<str>),
    BadListDestruc,
    BadRestDestruc,
    NoBindingVec,
    BindingsNotVec(&'static str),
    UnevenBindingVec,
    UnsupportedLiteralType,
    VarPurityBound,
    NoParamDecl,
    UnsupportedImportFilter,
    MacroMultiPatternRef(Box<[Span]>),
    MacroNoPatternRef,
    MacroNoTemplateVars,
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
            ErrorKind::ExpectedValue(found) => {
                Diagnostic::new_error(format!("cannot take the value of a {}", found))
                    .with_label(Label::new_primary(origin).with_message("expected value"))
            }

            ErrorKind::ExpectedTy(found) => {
                Diagnostic::new_error(format!("{} cannot be used as a type", found))
                    .with_label(Label::new_primary(origin).with_message("expected type"))
            }

            ErrorKind::ExpectedTyCons(found) => {
                Diagnostic::new_error(format!("{} cannot be used as a type constructor", found))
                    .with_label(
                        Label::new_primary(origin).with_message("expected type constructor"),
                    )
            }

            ErrorKind::ExpectedSym(found) => {
                Diagnostic::new_error(format!("expected symbol, found {}", found))
                    .with_label(Label::new_primary(origin).with_message("expected symbol"))
            }

            ErrorKind::ExpectedParamList(found) => Diagnostic::new_error(format!(
                "expected parameter declaration list, found {}",
                found
            ))
            .with_label(Label::new_primary(origin).with_message("expected parameter list")),

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

            ErrorKind::NoMacroRule(pattern_spans) => {
                Diagnostic::new_error("no matching macro rule")
                    .with_label(Label::new_primary(origin).with_message("at this macro invocation"))
                    .with_labels(pattern_spans.iter().map(|pattern_span| {
                        Label::new_secondary(*pattern_span).with_message("unmatched macro rule")
                    }))
            }

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

            ErrorKind::NoBindingVec => Diagnostic::new_error("binding vector expected")
                .with_label(Label::new_primary(origin).with_message("expected vector argument")),

            ErrorKind::BindingsNotVec(found) => {
                Diagnostic::new_error(format!("binding vector expected, found {}", found))
                    .with_label(Label::new_primary(origin).with_message("vector expected"))
            }

            ErrorKind::UnevenBindingVec => {
                Diagnostic::new_error("binding vector must have an even number of forms")
                    .with_label(Label::new_primary(origin).with_message("extra binding form"))
            }

            ErrorKind::UnsupportedLiteralType => Diagnostic::new_error("unsupported literal type")
                .with_label(
                    Label::new_primary(origin)
                        .with_message("expected boolean, symbol, keyword, list or vector"),
                ),

            ErrorKind::VarPurityBound => {
                Diagnostic::new_error("purity variables cannot be bound by other variables")
                    .with_label(Label::new_primary(origin).with_message("expected `->` or `->!`"))
            }

            ErrorKind::NoParamDecl => Diagnostic::new_error("parameter declaration missing")
                .with_label(
                    Label::new_primary(origin).with_message("expected parameter list argument"),
                ),

            ErrorKind::UnsupportedImportFilter => {
                Diagnostic::new_error("unsupported import filter").with_label(
                    Label::new_primary(origin).with_message(
                        "expected `only`, `except`, `rename`, `prefix` or `prefixed`",
                    ),
                )
            }

            ErrorKind::MacroMultiPatternRef(sub_var_spans) => Diagnostic::new_error(
                "subtemplate references macro variables from multiple subpatterns",
            )
            .with_label(
                Label::new_primary(origin)
                    .with_message("subtemplate references multiple subpatterns"),
            )
            .with_labels(sub_var_spans.iter().map(|sub_var_span| {
                Label::new_secondary(*sub_var_span).with_message("referenced macro variable")
            })),

            ErrorKind::MacroNoTemplateVars => {
                Diagnostic::new_error("subtemplate does not include any macro variables")
                    .with_label(
                        Label::new_primary(origin)
                            .with_message("subtemplate includes no variables"),
                    )
            }

            ErrorKind::MacroNoPatternRef => Diagnostic::new_error(
                "subtemplate does not reference macro variables from any subpattern",
            )
            .with_label(
                Label::new_primary(origin)
                    .with_message("subtemplate does not reference subpatterns"),
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
