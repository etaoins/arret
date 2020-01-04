use std::{error, fmt, io, iter, path, result};

use codespan_reporting::diagnostic::Diagnostic;

use arret_syntax::datum::DataStr;
use arret_syntax::error::Error as SyntaxError;
use arret_syntax::span::Span;

use crate::hir::types::{str_for_purity, str_for_ty_ref};
use crate::reporting::{diagnostic_for_syntax_error, new_label, LocTrace};
use crate::ty;
use crate::ty::purity;

#[derive(Debug, PartialEq, Clone)]
pub struct ExpectedSym {
    pub found: &'static str,
    pub usage: &'static str,
}

#[derive(Debug, PartialEq, Clone)]
pub struct PolyArgIsNotTy {
    pub arg_type: ty::Ref<ty::Poly>,
    pub param_bound: ty::Ref<ty::Poly>,
    pub param_span: Span,
}

#[derive(Debug, PartialEq, Clone)]
pub struct PolyArgIsNotPure {
    pub arg_purity: purity::Ref,
    pub param_span: Span,
}

#[derive(Debug, PartialEq, Clone)]
pub struct ExpectedPolyPurityArg {
    pub found: &'static str,
    pub param_span: Span,
}

#[derive(Debug, PartialEq, Clone)]
pub enum ErrorKind {
    ExpectedValue(&'static str),
    ExpectedTy(&'static str),
    ExpectedTyCons(&'static str),
    ExpectedParamList(&'static str),
    ExpectedPolyVarsDecl(&'static str),
    ExpectedMacroSpecList(&'static str),
    ExpectedMacroRuleVec(&'static str),
    ExpectedMacroRulePatternList(&'static str),
    ExpectedMacroEllipsisEscape(&'static str),
    ExpectedCompileErrorString(&'static str),
    ExpectedImportFilterKeyword(&'static str),
    ExpectedImportRenameMap(&'static str),
    ExpectedRecordTyConsDecl(&'static str),
    ExpectedRecordValueConsDecl(&'static str),
    ExpectedRecordFieldDecl(&'static str),
    ExpectedSym(Box<ExpectedSym>),
    UnboundIdent(DataStr),
    WrongArgCount(usize),
    WrongCondArgCount,
    WrongDefLikeArgCount(&'static str),
    WrongDefRecordArgCount,
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
    BadPolyVarDecl,
    UnsupportedLiteralType,
    VarPurityBound,
    NoParamDecl,
    NoPolyVarsDecl,
    UnsupportedImportFilter,
    MacroMultiPatternRef(Box<[Span]>),
    MacroNoPatternRef,
    MacroNoTemplateVars,
    MacroBadEllipsis,
    MacroBadSetPattern,
    WrongMacroRuleVecCount(usize),
    NoMacroType,
    BadMacroType,
    BadImportSet,
    NonFunPolyTy,
    ShortModuleName,
    AnonymousPolymorphicParam,
    PolyArgIsNotTy(Box<PolyArgIsNotTy>),
    PolyArgIsNotPure(Box<PolyArgIsNotPure>),
    ExpectedPolyPurityArg(Box<ExpectedPolyPurityArg>),
    UnusedPolyPurityParam(purity::PVarId),
    UnusedPolyTyParam(ty::TVarId),
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
        let Error { loc_trace, kind } = error;
        let origin = loc_trace.origin();

        let diagnostic = match kind {
            ErrorKind::ExpectedValue(found) => Diagnostic::new_error(
                format!("cannot take the value of a {}", found),
                new_label(origin, "expected value"),
            ),

            ErrorKind::ExpectedTy(found) => Diagnostic::new_error(
                format!("{} cannot be used as a type", found),
                new_label(origin, "expected type"),
            ),

            ErrorKind::ExpectedTyCons(found) => Diagnostic::new_error(
                format!("{} cannot be used as a type constructor", found),
                new_label(origin, "expected type constructor"),
            ),

            ErrorKind::ExpectedSym(details) => {
                let ExpectedSym { found, usage } = *details;

                Diagnostic::new_error(
                    format!("expected symbol, found {}", found),
                    new_label(origin, format!("expected {}", usage)),
                )
            }

            ErrorKind::ExpectedParamList(found) => Diagnostic::new_error(
                format!("expected parameter declaration list, found {}", found),
                new_label(origin, "expected parameter list"),
            ),

            ErrorKind::ExpectedPolyVarsDecl(found) => Diagnostic::new_error(
                format!("expected polymorphic variable set, found {}", found),
                new_label(origin, "expected polymorphic variable set"),
            ),

            ErrorKind::ExpectedMacroSpecList(found) => Diagnostic::new_error(
                format!("expected macro specification list, found {}", found),
                new_label(origin, "expected `(macro-rules ...)`"),
            ),

            ErrorKind::ExpectedMacroRuleVec(found) => Diagnostic::new_error(
                format!("expected macro rule vector, found {}", found),
                new_label(origin, "expected `[pattern template]`"),
            ),

            ErrorKind::ExpectedMacroRulePatternList(found) => Diagnostic::new_error(
                format!("expected macro rule pattern list, found {}", found),
                new_label(origin, "expected macro rule pattern list"),
            ),

            ErrorKind::ExpectedMacroEllipsisEscape(found) => Diagnostic::new_error(
                format!("expected macro symbol to escape, found {}", found),
                new_label(origin, "expected symbol"),
            ),

            ErrorKind::ExpectedCompileErrorString(found) => Diagnostic::new_error(
                format!("expected error message string, found {}", found),
                new_label(origin, "expected string"),
            ),

            ErrorKind::ExpectedImportFilterKeyword(found) => Diagnostic::new_error(
                format!("expected import filter keyword, found {}", found),
                new_label(
                    origin,
                    "expected `:only`, `:exclude`, `:rename`, `:prefix` or `:prefixed`",
                ),
            ),

            ErrorKind::ExpectedImportRenameMap(found) => Diagnostic::new_error(
                format!("expected identifier rename map, found {}", found),
                new_label(origin, "expected identifier rename map"),
            ),

            ErrorKind::ExpectedRecordTyConsDecl(found) => Diagnostic::new_error(
                format!(
                    "expected record type constuctor declaration, found {}",
                    found
                ),
                new_label(origin, "expected symbol or polymorphic constructor list"),
            ),

            ErrorKind::ExpectedRecordValueConsDecl(found) => Diagnostic::new_error(
                format!(
                    "expected record value constructor declaration, found {}",
                    found
                ),
                new_label(origin, "expected record field list"),
            ),

            ErrorKind::ExpectedRecordFieldDecl(found) => Diagnostic::new_error(
                format!("expected record field declaration, found {}", found),
                new_label(origin, "expected record field declaration"),
            ),

            ErrorKind::UnboundIdent(ref ident) => {
                let diagnostic = Diagnostic::new_error(
                    format!("unable to resolve `{}`", ident),
                    new_label(origin, "not found in this scope"),
                );

                if ident.as_ref() == "nil" {
                    diagnostic.with_notes(vec![
                        "Arret does not have a distinct `nil` value; consider using `()` instead"
                            .to_owned(),
                    ])
                } else {
                    diagnostic
                }
            }

            ErrorKind::WrongArgCount(expected) => {
                let label_message = if expected == 1 {
                    "expected 1 argument".to_owned()
                } else {
                    format!("expected {} arguments", expected)
                };

                Diagnostic::new_error(
                    format!("wrong argument count; expected {}", expected),
                    new_label(origin, label_message),
                )
            }

            ErrorKind::WrongCondArgCount => Diagnostic::new_error(
                "wrong argument count; expected 3",
                new_label(origin, "expected `(if test-expr true-expr false-expr)`"),
            ),

            ErrorKind::WrongDefLikeArgCount(name) => Diagnostic::new_error(
                "wrong argument count; expected 2",
                new_label(origin, format!("expected `({} name definition)`", name)),
            ),

            ErrorKind::WrongDefRecordArgCount => Diagnostic::new_error(
                "wrong argument count; expected 2",
                new_label(
                    origin,
                    "expected `(defrecord ty-cons-decl value-cons-decl)`",
                ),
            ),

            ErrorKind::WrongMacroRuleVecCount(found) => Diagnostic::new_error(
                format!(
                    "expected macro rule vector with 2 elements, found {}",
                    found
                ),
                new_label(origin, "expected `[pattern template]`"),
            ),

            ErrorKind::DefOutsideBody => Diagnostic::new_error(
                "definition outside module body",
                new_label(origin, "not at top-level of module"),
            ),

            ErrorKind::DuplicateDef(first_def_span, ref ident) => {
                let diagnostic = Diagnostic::new_error(
                    format!("duplicate definition of `{}`", ident),
                    new_label(origin, "second definition here"),
                );

                if let Some(first_def_span) = first_def_span {
                    diagnostic.with_secondary_labels(iter::once(new_label(
                        first_def_span,
                        "first definition here",
                    )))
                } else {
                    diagnostic
                }
            }

            ErrorKind::ExportOutsideModule => Diagnostic::new_error(
                "(export) outside of module body",
                new_label(origin, "not at top-level of module"),
            ),

            ErrorKind::NonDefInsideModule => Diagnostic::new_error(
                "value at top-level of module body",
                new_label(origin, "(import), (export) or definition expected"),
            ),

            ErrorKind::ExportInsideRepl => Diagnostic::new_error(
                "export not supported within REPL",
                new_label(origin, "export not supported"),
            ),

            ErrorKind::PackageNotFound => {
                Diagnostic::new_error("package not found", new_label(origin, "at this import"))
            }

            ErrorKind::ModuleNotFound(ref filename) => Diagnostic::new_error(
                format!("module not found at `{}`", filename.to_string_lossy()),
                new_label(origin, "at this import"),
            ),

            ErrorKind::NoMacroRule(pattern_spans) => Diagnostic::new_error(
                "no matching macro rule",
                new_label(origin, "at this macro invocation"),
            )
            .with_secondary_labels(
                pattern_spans
                    .iter()
                    .map(|pattern_span| new_label(*pattern_span, "unmatched macro rule")),
            ),

            ErrorKind::MultipleZeroOrMoreMatch(first_zero_or_more_span) => Diagnostic::new_error(
                "multiple zero or more matches in the same sequence",
                new_label(origin, "second zero or more match"),
            )
            .with_secondary_labels(iter::once(new_label(
                first_zero_or_more_span,
                "first zero or more match",
            ))),

            ErrorKind::NoVecDestruc => Diagnostic::new_error(
                "vectors can only be used in a destructure in the form `[name Type]`",
                new_label(origin, "unexpected vector"),
            ),

            ErrorKind::UserError(ref message) => Diagnostic::new_error(
                message.as_ref(),
                new_label(origin, "user error raised here"),
            ),

            ErrorKind::ReadError(ref filename) => Diagnostic::new_error(
                format!("error reading `{}`", filename.to_string_lossy()),
                new_label(origin, "at this import"),
            ),

            ErrorKind::SyntaxError(ref err) => {
                // Just proxy this
                return diagnostic_for_syntax_error(err);
            }

            ErrorKind::RustFunError(ref message) => Diagnostic::new_error(
                "error loading RFI module",
                new_label(origin, message.clone().into_string()),
            ),

            ErrorKind::BadListDestruc => Diagnostic::new_error(
                "unsupported destructuring binding",
                new_label(origin, "expected variable name, list or `[name Type]`"),
            ),

            ErrorKind::BadRestDestruc => Diagnostic::new_error(
                "unsupported rest destructuring",
                new_label(origin, "expected variable name or `[name Type]`"),
            ),

            ErrorKind::NoBindingVec => Diagnostic::new_error(
                "binding vector expected",
                new_label(origin, "expected vector argument"),
            ),

            ErrorKind::BindingsNotVec(found) => Diagnostic::new_error(
                format!("binding vector expected, found {}", found),
                new_label(origin, "vector expected"),
            ),

            ErrorKind::UnevenBindingVec => Diagnostic::new_error(
                "binding vector must have an even number of forms",
                new_label(origin, "extra binding form"),
            ),

            ErrorKind::BadPolyVarDecl => Diagnostic::new_error(
                "bad polymorphic variable declaration",
                new_label(
                    origin,
                    "expected polymorphic variable name or `[name Bound]`",
                ),
            ),

            ErrorKind::UnsupportedLiteralType => Diagnostic::new_error(
                "unsupported literal type",
                new_label(origin, "expected boolean, symbol, keyword, list or vector"),
            ),

            ErrorKind::VarPurityBound => Diagnostic::new_error(
                "purity variables cannot be bound by other variables",
                new_label(origin, "expected `->` or `->!`"),
            ),

            ErrorKind::NoParamDecl => Diagnostic::new_error(
                "parameter declaration missing",
                new_label(origin, "expected parameter list argument"),
            ),

            ErrorKind::NoPolyVarsDecl => Diagnostic::new_error(
                "polymorphic variable declaration missing",
                new_label(origin, "expected polymorphic variable set argument"),
            ),

            ErrorKind::UnsupportedImportFilter => Diagnostic::new_error(
                "unsupported import filter",
                new_label(
                    origin,
                    "expected `:only`, `:exclude`, `:rename`, `:prefix` or `:prefixed`",
                ),
            ),

            ErrorKind::MacroMultiPatternRef(sub_var_spans) => Diagnostic::new_error(
                "subtemplate references macro variables from multiple subpatterns",
                new_label(origin, "subtemplate references multiple subpatterns"),
            )
            .with_secondary_labels(
                sub_var_spans
                    .iter()
                    .map(|sub_var_span| new_label(*sub_var_span, "referenced macro variable")),
            ),

            ErrorKind::MacroNoTemplateVars => Diagnostic::new_error(
                "subtemplate does not include any macro variables",
                new_label(origin, "subtemplate includes no variables"),
            ),

            ErrorKind::MacroNoPatternRef => Diagnostic::new_error(
                "subtemplate does not reference macro variables from any subpattern",
                new_label(origin, "subtemplate does not reference subpatterns"),
            ),

            ErrorKind::MacroBadEllipsis => Diagnostic::new_error(
                "unexpected ellipsis in macro rule",
                new_label(origin, "expected `var ...`"),
            ),

            ErrorKind::MacroBadSetPattern => Diagnostic::new_error(
                "set patterns must either be empty or a zero or more match",
                new_label(origin, "expected `#{}` or `#{var ...}`"),
            ),

            ErrorKind::NoMacroType => Diagnostic::new_error(
                "missing macro type",
                new_label(origin, "expected `(macro-rules ...)`"),
            ),

            ErrorKind::BadMacroType => Diagnostic::new_error(
                "unsupported macro type",
                new_label(origin, "expected `macro-rules`"),
            ),

            ErrorKind::BadImportSet => Diagnostic::new_error(
                "bad import set",
                new_label(origin, "expected module name vector or applied filter"),
            ),

            ErrorKind::NonFunPolyTy => Diagnostic::new_error(
                "polymorphism on non-function type",
                new_label(origin, "expected function type"),
            ),

            ErrorKind::ShortModuleName => Diagnostic::new_error(
                "module name requires a least two components",
                new_label(origin, "expected vector of 2 or more symbols"),
            ),

            ErrorKind::AnonymousPolymorphicParam => Diagnostic::new_error(
                "polymorphic parameters must have a name",
                new_label(origin, "expected polymorphic parameter name"),
            ),

            ErrorKind::PolyArgIsNotTy(boxed_details) => {
                let PolyArgIsNotTy {
                    arg_type,
                    param_bound,
                    param_span,
                } = *boxed_details;

                Diagnostic::new_error(
                    "mismatched types",
                    new_label(
                        origin,
                        format!(
                            "`{}` does not satisfy the lower bound of `{}`",
                            str_for_ty_ref(&arg_type),
                            str_for_ty_ref(&param_bound)
                        ),
                    ),
                )
                .with_secondary_labels(iter::once(new_label(
                    param_span,
                    "type parameter declared here",
                )))
            }

            ErrorKind::PolyArgIsNotPure(boxed_details) => {
                let PolyArgIsNotPure {
                    arg_purity,
                    param_span,
                } = *boxed_details;
                Diagnostic::new_error(
                    "mismatched purities",
                    new_label(
                        origin,
                        format!("`{}` is not pure", str_for_purity(&arg_purity)),
                    ),
                )
                .with_secondary_labels(iter::once(new_label(
                    param_span,
                    "purity parameter declared here",
                )))
            }

            ErrorKind::ExpectedPolyPurityArg(boxed_details) => {
                let ExpectedPolyPurityArg { found, param_span } = *boxed_details;

                Diagnostic::new_error(
                    format!("{} cannot be used as a purity", found),
                    new_label(origin, "expected purity"),
                )
                .with_secondary_labels(iter::once(new_label(
                    param_span,
                    "purity parameter declared here",
                )))
            }

            ErrorKind::UnusedPolyPurityParam(pvar) => Diagnostic::new_error(
                format!(
                    "unused polymorphic purity parameter `{}`",
                    pvar.source_name()
                ),
                new_label(pvar.span(), "purity parameter declared here"),
            ),

            ErrorKind::UnusedPolyTyParam(tvar) => Diagnostic::new_error(
                format!("unused polymorphic type parameter `{}`", tvar.source_name()),
                new_label(tvar.span(), "type parameter declared here"),
            ),
        };

        loc_trace.label_macro_invocation(diagnostic)
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
