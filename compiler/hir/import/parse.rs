use std::result;

use arret_syntax::datum::{DataStr, Datum};
use arret_syntax::span::Span;

use crate::hir::error::{Error, ErrorKind};
use crate::hir::loader::ModuleName;
use crate::hir::util::{expect_ident, expect_spanned_ident};

type Result<T> = result::Result<T, Error>;

pub enum ParsedImportSet {
    Module(Span, ModuleName),
    Filter(ParsedFilter, Box<ParsedImportSet>),
}

impl ParsedImportSet {
    pub fn spanned_module_name(&self) -> (Span, &ModuleName) {
        match self {
            ParsedImportSet::Module(span, module_name) => (*span, module_name),
            ParsedImportSet::Filter(_, inner_import) => inner_import.spanned_module_name(),
        }
    }

    pub fn module_name(&self) -> &ModuleName {
        self.spanned_module_name().1
    }
}

pub enum ParsedFilter {
    Only(Box<[(Span, DataStr)]>),
    Exclude(Box<[(Span, DataStr)]>),
    Rename(Box<[((Span, DataStr), DataStr)]>),
    Prefix(DataStr),
}

fn parse_module_name(span: Span, name: &[Datum]) -> Result<ModuleName> {
    if name.len() < 2 {
        return Err(Error::new(span, ErrorKind::ShortModuleName));
    }

    let mut name_idents = name
        .iter()
        .map(|datum| Ok(expect_ident(datum, "module name component")?.clone()));

    let package_name = name_idents.next().unwrap()?;
    let terminal_name = name_idents.next_back().unwrap()?;
    let name_components = name_idents.collect::<result::Result<Vec<_>, Error>>()?;

    Ok(ModuleName::new(
        package_name,
        name_components,
        terminal_name.clone(),
    ))
}

fn parse_filter(
    apply_span: Span,
    filter_datum: &Datum,
    filter_input: &ParsedImportSet,
    arg_data: &[Datum],
) -> Result<ParsedFilter> {
    let (filter_span, filter_name) = match filter_datum {
        Datum::Sym(span, name) if name.starts_with(':') => (span, name),
        _ => {
            return Err(Error::new(
                filter_datum.span(),
                ErrorKind::ExpectedImportFilterKeyword(filter_datum.description()),
            ));
        }
    };

    match filter_name.as_ref() {
        ":only" => {
            let only_spanned_names = arg_data
                .iter()
                .map(|arg_datum| {
                    expect_spanned_ident(arg_datum, "identifier to include")
                        .map(|(span, ident)| (span, ident.clone()))
                })
                .collect::<Result<Box<[_]>>>()?;

            Ok(ParsedFilter::Only(only_spanned_names))
        }
        ":exclude" => {
            let exclude_spanned_names = arg_data
                .iter()
                .map(|arg_datum| {
                    expect_spanned_ident(arg_datum, "identifier to exclude")
                        .map(|(span, ident)| (span, ident.clone()))
                })
                .collect::<Result<Box<[_]>>>()?;

            Ok(ParsedFilter::Exclude(exclude_spanned_names))
        }
        ":rename" => match arg_data {
            [Datum::Map(_, vs)] => {
                let rename_spanned_names = vs
                    .iter()
                    .map(|(from_datum, to_datum)| {
                        let (from_span, from_ident) =
                            expect_spanned_ident(from_datum, "identifier to rename from")?;
                        let to_ident = expect_ident(to_datum, "identifier to rename to")?;

                        Ok(((from_span, from_ident.clone()), to_ident.clone()))
                    })
                    .collect::<Result<Box<[_]>>>()?;

                Ok(ParsedFilter::Rename(rename_spanned_names))
            }
            [other] => Err(Error::new(
                other.span(),
                ErrorKind::ExpectedImportRenameMap(other.description()),
            )),
            _ => Err(Error::new(apply_span, ErrorKind::WrongArgCount(2))),
        },
        ":prefix" => match arg_data {
            [prefix_datum] => {
                let prefix_ident = expect_ident(prefix_datum, "identifier prefix")?;
                Ok(ParsedFilter::Prefix(prefix_ident.clone()))
            }
            _ => Err(Error::new(apply_span, ErrorKind::WrongArgCount(2))),
        },
        ":prefixed" => {
            if !arg_data.is_empty() {
                return Err(Error::new(apply_span, ErrorKind::WrongArgCount(1)));
            }

            Ok(ParsedFilter::Prefix(
                format!("{}/", filter_input.module_name().terminal_name()).into(),
            ))
        }
        _ => Err(Error::new(*filter_span, ErrorKind::UnsupportedImportFilter)),
    }
}

/// Parses the passed import set datum
///
/// This produces an AST without performing the import itself.
pub fn parse_import_set(import_set_datum: &Datum) -> Result<ParsedImportSet> {
    let span = import_set_datum.span();

    match import_set_datum {
        Datum::Vector(_, vs) => {
            let module_name = parse_module_name(span, vs.as_ref())?;
            Ok(ParsedImportSet::Module(span, module_name))
        }
        Datum::List(_, vs) if vs.len() >= 2 => {
            let filter_datum = &vs[0];

            let inner_import_datum = &vs[1];
            let filter_input = parse_import_set(inner_import_datum)?;

            let filter = parse_filter(span, filter_datum, &filter_input, &vs[2..])?;
            Ok(ParsedImportSet::Filter(filter, Box::new(filter_input)))
        }
        _ => Err(Error::new(span, ErrorKind::BadImportSet)),
    }
}
