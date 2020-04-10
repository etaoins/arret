use std::{error, fmt, io, iter, path, result};

use codespan_reporting::diagnostic::Diagnostic;

use arret_syntax::datum::DataStr;
use arret_syntax::error::Error as SyntaxError;
use arret_syntax::span::{FileId, Span};

use crate::hir::types::{str_for_purity, str_for_ty_ref};
use crate::reporting::{
    diagnostic_for_syntax_error, new_primary_label, new_secondary_label, LocTrace,
};
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

impl From<Error> for Diagnostic<FileId> {
    fn from(error: Error) -> Diagnostic<FileId> {
        let Error { loc_trace, kind } = error;
        let origin = loc_trace.origin();

        let diagnostic = match kind {
            ErrorKind::ExpectedValue(found) => Diagnostic::error()
                .with_message(format!("cannot take the value of a {}", found))
                .with_labels(vec![new_primary_label(origin, "expected value")]),

            ErrorKind::ExpectedTy(found) => Diagnostic::error()
                .with_message(format!("{} cannot be used as a type", found))
                .with_labels(vec![new_primary_label(origin, "expected type")]),

            ErrorKind::ExpectedTyCons(found) => Diagnostic::error()
                .with_message(format!("{} cannot be used as a type constructor", found))
                .with_labels(vec![new_primary_label(origin, "expected type constructor")]),

            ErrorKind::ExpectedSym(details) => {
                let ExpectedSym { found, usage } = *details;

                Diagnostic::error()
                    .with_message(format!("expected symbol, found {}", found))
                    .with_labels(vec![new_primary_label(
                        origin,
                        format!("expected {}", usage),
                    )])
            }

            ErrorKind::ExpectedParamList(found) => Diagnostic::error()
                .with_message(format!(
                    "expected parameter declaration list, found {}",
                    found
                ))
                .with_labels(vec![new_primary_label(origin, "expected parameter list")]),

            ErrorKind::ExpectedPolyVarsDecl(found) => Diagnostic::error()
                .with_message(format!(
                    "expected polymorphic variable set, found {}",
                    found
                ))
                .with_labels(vec![new_primary_label(
                    origin,
                    "expected polymorphic variable set",
                )]),

            ErrorKind::ExpectedMacroSpecList(found) => Diagnostic::error()
                .with_message(format!(
                    "expected macro specification list, found {}",
                    found
                ))
                .with_labels(vec![new_primary_label(
                    origin,
                    "expected `(macro-rules ...)`",
                )]),

            ErrorKind::ExpectedMacroRuleVec(found) => Diagnostic::error()
                .with_message(format!("expected macro rule vector, found {}", found))
                .with_labels(vec![new_primary_label(
                    origin,
                    "expected `[pattern template]`",
                )]),

            ErrorKind::ExpectedMacroRulePatternList(found) => Diagnostic::error()
                .with_message(format!("expected macro rule pattern list, found {}", found))
                .with_labels(vec![new_primary_label(
                    origin,
                    "expected macro rule pattern list",
                )]),

            ErrorKind::ExpectedMacroEllipsisEscape(found) => Diagnostic::error()
                .with_message(format!("expected macro symbol to escape, found {}", found))
                .with_labels(vec![new_primary_label(origin, "expected symbol")]),

            ErrorKind::ExpectedCompileErrorString(found) => Diagnostic::error()
                .with_message(format!("expected error message string, found {}", found))
                .with_labels(vec![new_primary_label(origin, "expected string")]),

            ErrorKind::ExpectedImportFilterKeyword(found) => Diagnostic::error()
                .with_message(format!("expected import filter keyword, found {}", found))
                .with_labels(vec![new_primary_label(
                    origin,
                    "expected `:only`, `:exclude`, `:rename`, `:prefix` or `:prefixed`",
                )]),

            ErrorKind::ExpectedImportRenameMap(found) => Diagnostic::error()
                .with_message(format!("expected identifier rename map, found {}", found))
                .with_labels(vec![new_primary_label(
                    origin,
                    "expected identifier rename map",
                )]),

            ErrorKind::ExpectedRecordTyConsDecl(found) => Diagnostic::error()
                .with_message(format!(
                    "expected record type constuctor declaration, found {}",
                    found
                ))
                .with_labels(vec![new_primary_label(
                    origin,
                    "expected symbol or polymorphic constructor list",
                )]),

            ErrorKind::ExpectedRecordValueConsDecl(found) => Diagnostic::error()
                .with_message(format!(
                    "expected record value constructor declaration, found {}",
                    found
                ))
                .with_labels(vec![new_primary_label(
                    origin,
                    "expected record field list",
                )]),

            ErrorKind::ExpectedRecordFieldDecl(found) => Diagnostic::error()
                .with_message(format!(
                    "expected record field declaration, found {}",
                    found
                ))
                .with_labels(vec![new_primary_label(
                    origin,
                    "expected record field declaration",
                )]),

            ErrorKind::UnboundIdent(ref ident) => {
                let diagnostic = Diagnostic::error()
                    .with_message(format!("unable to resolve `{}`", ident))
                    .with_labels(vec![new_primary_label(origin, "not found in this scope")]);

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

                Diagnostic::error()
                    .with_message(format!("wrong argument count; expected {}", expected))
                    .with_labels(vec![new_primary_label(origin, label_message)])
            }

            ErrorKind::WrongCondArgCount => Diagnostic::error()
                .with_message("wrong argument count; expected 3")
                .with_labels(vec![new_primary_label(
                    origin,
                    "expected `(if test-expr true-expr false-expr)`",
                )]),

            ErrorKind::WrongDefLikeArgCount(name) => Diagnostic::error()
                .with_message("wrong argument count; expected 2")
                .with_labels(vec![new_primary_label(
                    origin,
                    format!("expected `({} name definition)`", name),
                )]),

            ErrorKind::WrongDefRecordArgCount => Diagnostic::error()
                .with_message("wrong argument count; expected 2")
                .with_labels(vec![new_primary_label(
                    origin,
                    "expected `(defrecord ty-cons-decl value-cons-decl)`",
                )]),

            ErrorKind::WrongMacroRuleVecCount(found) => Diagnostic::error()
                .with_message(format!(
                    "expected macro rule vector with 2 elements, found {}",
                    found
                ))
                .with_labels(vec![new_primary_label(
                    origin,
                    "expected `[pattern template]`",
                )]),

            ErrorKind::DefOutsideBody => Diagnostic::error()
                .with_message("definition outside module body")
                .with_labels(vec![new_primary_label(
                    origin,
                    "not at top-level of module",
                )]),

            ErrorKind::DuplicateDef(first_def_span, ref ident) => {
                let diagnostic = Diagnostic::error()
                    .with_message(format!("duplicate definition of `{}`", ident));

                let primary_label = new_primary_label(origin, "second definition here");

                if let Some(first_def_span) = first_def_span {
                    let secondary_label =
                        new_secondary_label(first_def_span, "first definition here");

                    diagnostic.with_labels(vec![primary_label, secondary_label])
                } else {
                    diagnostic.with_labels(vec![primary_label])
                }
            }

            ErrorKind::ExportOutsideModule => Diagnostic::error()
                .with_message("(export) outside of module body")
                .with_labels(vec![new_primary_label(
                    origin,
                    "not at top-level of module",
                )]),

            ErrorKind::NonDefInsideModule => Diagnostic::error()
                .with_message("value at top-level of module body")
                .with_labels(vec![new_primary_label(
                    origin,
                    "(import), (export) or definition expected",
                )]),

            ErrorKind::ExportInsideRepl => Diagnostic::error()
                .with_message("export not supported within REPL")
                .with_labels(vec![new_primary_label(origin, "export not supported")]),

            ErrorKind::PackageNotFound => Diagnostic::error()
                .with_message("package not found")
                .with_labels(vec![new_primary_label(origin, "at this import")]),

            ErrorKind::ModuleNotFound(ref filename) => Diagnostic::error()
                .with_message(format!(
                    "module not found at `{}`",
                    filename.to_string_lossy()
                ))
                .with_labels(vec![new_primary_label(origin, "at this import")]),

            ErrorKind::NoMacroRule(pattern_spans) => Diagnostic::error()
                .with_message("no matching macro rule")
                .with_labels(
                    iter::once(new_primary_label(origin, "at this macro invocation"))
                        .chain(pattern_spans.iter().map(|pattern_span| {
                            new_secondary_label(*pattern_span, "unmatched macro rule")
                        }))
                        .collect(),
                ),

            ErrorKind::MultipleZeroOrMoreMatch(first_zero_or_more_span) => Diagnostic::error()
                .with_message("multiple zero or more matches in the same sequence")
                .with_labels(vec![
                    new_primary_label(origin, "second zero or more match"),
                    new_secondary_label(first_zero_or_more_span, "first zero or more match"),
                ]),

            ErrorKind::NoVecDestruc => Diagnostic::error()
                .with_message("vectors can only be used in a destructure in the form `[name Type]`")
                .with_labels(vec![new_primary_label(origin, "unexpected vector")]),

            ErrorKind::UserError(ref message) => Diagnostic::error()
                .with_message(message.as_ref())
                .with_labels(vec![new_primary_label(origin, "user error raised here")]),

            ErrorKind::ReadError(ref filename) => Diagnostic::error()
                .with_message(format!("error reading `{}`", filename.to_string_lossy()))
                .with_labels(vec![new_primary_label(origin, "at this import")]),

            ErrorKind::SyntaxError(ref err) => {
                // Just proxy this
                return diagnostic_for_syntax_error(err);
            }

            ErrorKind::RustFunError(ref message) => Diagnostic::error()
                .with_message("error loading RFI module")
                .with_labels(vec![new_primary_label(
                    origin,
                    message.clone().into_string(),
                )]),

            ErrorKind::BadListDestruc => Diagnostic::error()
                .with_message("unsupported destructuring binding")
                .with_labels(vec![new_primary_label(
                    origin,
                    "expected variable name, list or `[name Type]`",
                )]),

            ErrorKind::BadRestDestruc => Diagnostic::error()
                .with_message("unsupported rest destructuring")
                .with_labels(vec![new_primary_label(
                    origin,
                    "expected variable name or `[name Type]`",
                )]),

            ErrorKind::NoBindingVec => Diagnostic::error()
                .with_message("binding vector expected")
                .with_labels(vec![new_primary_label(origin, "expected vector argument")]),

            ErrorKind::BindingsNotVec(found) => Diagnostic::error()
                .with_message(format!("binding vector expected, found {}", found))
                .with_labels(vec![new_primary_label(origin, "vector expected")]),

            ErrorKind::UnevenBindingVec => Diagnostic::error()
                .with_message("binding vector must have an even number of forms")
                .with_labels(vec![new_primary_label(origin, "extra binding form")]),

            ErrorKind::BadPolyVarDecl => Diagnostic::error()
                .with_message("bad polymorphic variable declaration")
                .with_labels(vec![new_primary_label(
                    origin,
                    "expected polymorphic variable name or `[name Bound]`",
                )]),

            ErrorKind::UnsupportedLiteralType => Diagnostic::error()
                .with_message("unsupported literal type")
                .with_labels(vec![new_primary_label(
                    origin,
                    "expected boolean, symbol, keyword, list or vector",
                )]),

            ErrorKind::VarPurityBound => Diagnostic::error()
                .with_message("purity variables cannot be bound by other variables")
                .with_labels(vec![new_primary_label(origin, "expected `->` or `->!`")]),

            ErrorKind::NoParamDecl => Diagnostic::error()
                .with_message("parameter declaration missing")
                .with_labels(vec![new_primary_label(
                    origin,
                    "expected parameter list argument",
                )]),

            ErrorKind::NoPolyVarsDecl => Diagnostic::error()
                .with_message("polymorphic variable declaration missing")
                .with_labels(vec![new_primary_label(
                    origin,
                    "expected polymorphic variable set argument",
                )]),

            ErrorKind::UnsupportedImportFilter => Diagnostic::error()
                .with_message("unsupported import filter")
                .with_labels(vec![new_primary_label(
                    origin,
                    "expected `:only`, `:exclude`, `:rename`, `:prefix` or `:prefixed`",
                )]),

            ErrorKind::MacroMultiPatternRef(sub_var_spans) => Diagnostic::error()
                .with_message("subtemplate references macro variables from multiple subpatterns")
                .with_labels(
                    iter::once(new_primary_label(
                        origin,
                        "subtemplate references multiple subpatterns",
                    ))
                    .chain(sub_var_spans.iter().map(|sub_var_span| {
                        new_secondary_label(*sub_var_span, "referenced macro variable")
                    }))
                    .collect(),
                ),

            ErrorKind::MacroNoTemplateVars => Diagnostic::error()
                .with_message("subtemplate does not include any macro variables")
                .with_labels(vec![new_primary_label(
                    origin,
                    "subtemplate includes no variables",
                )]),

            ErrorKind::MacroNoPatternRef => Diagnostic::error()
                .with_message("subtemplate does not reference macro variables from any subpattern")
                .with_labels(vec![new_primary_label(
                    origin,
                    "subtemplate does not reference subpatterns",
                )]),

            ErrorKind::MacroBadEllipsis => Diagnostic::error()
                .with_message("unexpected ellipsis in macro rule")
                .with_labels(vec![new_primary_label(origin, "expected `var ...`")]),

            ErrorKind::MacroBadSetPattern => Diagnostic::error()
                .with_message("set patterns must either be empty or a zero or more match")
                .with_labels(vec![new_primary_label(
                    origin,
                    "expected `#{}` or `#{var ...}`",
                )]),

            ErrorKind::NoMacroType => Diagnostic::error()
                .with_message("missing macro type")
                .with_labels(vec![new_primary_label(
                    origin,
                    "expected `(macro-rules ...)`",
                )]),

            ErrorKind::BadMacroType => Diagnostic::error()
                .with_message("unsupported macro type")
                .with_labels(vec![new_primary_label(origin, "expected `macro-rules`")]),

            ErrorKind::BadImportSet => Diagnostic::error()
                .with_message("bad import set")
                .with_labels(vec![new_primary_label(
                    origin,
                    "expected module name vector or applied filter",
                )]),

            ErrorKind::NonFunPolyTy => Diagnostic::error()
                .with_message("polymorphism on non-function type")
                .with_labels(vec![new_primary_label(origin, "expected function type")]),

            ErrorKind::ShortModuleName => Diagnostic::error()
                .with_message("module name requires a least two components")
                .with_labels(vec![new_primary_label(
                    origin,
                    "expected vector of 2 or more symbols",
                )]),

            ErrorKind::AnonymousPolymorphicParam => Diagnostic::error()
                .with_message("polymorphic parameters must have a name")
                .with_labels(vec![new_primary_label(
                    origin,
                    "expected polymorphic parameter name",
                )]),

            ErrorKind::PolyArgIsNotTy(boxed_details) => {
                let PolyArgIsNotTy {
                    arg_type,
                    param_bound,
                    param_span,
                } = *boxed_details;

                Diagnostic::error()
                    .with_message("mismatched types")
                    .with_labels(vec![
                        new_primary_label(
                            origin,
                            format!(
                                "`{}` does not satisfy the lower bound of `{}`",
                                str_for_ty_ref(&arg_type),
                                str_for_ty_ref(&param_bound)
                            ),
                        ),
                        new_secondary_label(param_span, "type parameter declared here"),
                    ])
            }

            ErrorKind::PolyArgIsNotPure(boxed_details) => {
                let PolyArgIsNotPure {
                    arg_purity,
                    param_span,
                } = *boxed_details;
                Diagnostic::error()
                    .with_message("mismatched purities")
                    .with_labels(vec![
                        new_primary_label(
                            origin,
                            format!("`{}` is not pure", str_for_purity(&arg_purity)),
                        ),
                        new_secondary_label(param_span, "purity parameter declared here"),
                    ])
            }

            ErrorKind::ExpectedPolyPurityArg(boxed_details) => {
                let ExpectedPolyPurityArg { found, param_span } = *boxed_details;

                Diagnostic::error()
                    .with_message(format!("{} cannot be used as a purity", found))
                    .with_labels(vec![
                        new_primary_label(origin, "expected purity"),
                        new_secondary_label(param_span, "purity parameter declared here"),
                    ])
            }

            ErrorKind::UnusedPolyPurityParam(pvar) => Diagnostic::error()
                .with_message(format!(
                    "unused polymorphic purity parameter `{}`",
                    pvar.source_name()
                ))
                .with_labels(vec![new_primary_label(
                    pvar.span(),
                    "purity parameter declared here",
                )]),

            ErrorKind::UnusedPolyTyParam(tvar) => Diagnostic::error()
                .with_message(format!(
                    "unused polymorphic type parameter `{}`",
                    tvar.source_name()
                ))
                .with_labels(vec![new_primary_label(
                    tvar.span(),
                    "type parameter declared here",
                )]),
        };

        loc_trace.label_macro_invocation(diagnostic)
    }
}

impl error::Error for Error {}

impl fmt::Display for Error {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let diagnostic: Diagnostic<FileId> = self.clone().into();
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
