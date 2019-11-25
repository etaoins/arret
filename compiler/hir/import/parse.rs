use std::result;

use arret_syntax::datum::DataStr;
use arret_syntax::span::Span;

use crate::hir::error::{Error, ErrorKind};
use crate::hir::loader::ModuleName;
use crate::hir::ns::{Ident, NsDataIter, NsDatum};
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

fn parse_module_name(span: Span, name_iter: NsDataIter) -> Result<ModuleName> {
    if name_iter.len() < 2 {
        return Err(Error::new(span, ErrorKind::ShortModuleName));
    }

    let mut name_idents =
        name_iter.map(|datum| expect_ident(datum, "module name component").map(Ident::into_name));

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
    filter_datum: NsDatum,
    filter_input: &ParsedImportSet,
    mut arg_iter: NsDataIter,
) -> Result<ParsedFilter> {
    let (filter_span, filter_name) = if let NsDatum::Keyword(span, name) = filter_datum {
        (span, name)
    } else {
        return Err(Error::new(
            filter_datum.span(),
            ErrorKind::ExpectedImportFilterKeyword(filter_datum.description()),
        ));
    };

    match filter_name.as_ref() {
        ":only" => {
            let only_spanned_names = arg_iter
                .map(|arg_datum| {
                    expect_spanned_ident(arg_datum, "identifier to include")
                        .map(|(span, ident)| (span, ident.into_name()))
                })
                .collect::<Result<Box<[_]>>>()?;

            Ok(ParsedFilter::Only(only_spanned_names))
        }
        ":exclude" => {
            let exclude_spanned_names = arg_iter
                .map(|arg_datum| {
                    expect_spanned_ident(arg_datum, "identifier to exclude")
                        .map(|(span, ident)| (span, ident.into_name()))
                })
                .collect::<Result<Box<[_]>>>()?;

            Ok(ParsedFilter::Exclude(exclude_spanned_names))
        }
        ":rename" => {
            if arg_iter.len() != 1 {
                return Err(Error::new(apply_span, ErrorKind::WrongArgCount(2)));
            }

            let arg_datum = arg_iter.next().unwrap();
            if let NsDatum::Map(_, vs) = arg_datum {
                let rename_spanned_names = vs
                    .into_vec()
                    .into_iter()
                    .map(|(from_datum, to_datum)| {
                        let (from_span, from_ident) =
                            expect_spanned_ident(from_datum, "identifier to rename from")?;
                        let to_ident = expect_ident(to_datum, "identifier to rename to")?;

                        Ok(((from_span, from_ident.into_name()), to_ident.into_name()))
                    })
                    .collect::<Result<Box<[_]>>>()?;

                Ok(ParsedFilter::Rename(rename_spanned_names))
            } else {
                Err(Error::new(
                    arg_datum.span(),
                    ErrorKind::ExpectedImportRenameMap(arg_datum.description()),
                ))
            }
        }
        ":prefix" => {
            if arg_iter.len() != 1 {
                return Err(Error::new(apply_span, ErrorKind::WrongArgCount(2)));
            }

            let prefix_datum = arg_iter.next().unwrap();
            let prefix_ident = expect_ident(prefix_datum, "identifier prefix")?;

            Ok(ParsedFilter::Prefix(prefix_ident.into_name()))
        }
        ":prefixed" => {
            if arg_iter.len() != 0 {
                return Err(Error::new(apply_span, ErrorKind::WrongArgCount(1)));
            }

            Ok(ParsedFilter::Prefix(
                format!("{}/", filter_input.module_name().terminal_name()).into(),
            ))
        }
        _ => Err(Error::new(filter_span, ErrorKind::UnsupportedImportFilter)),
    }
}

/// Parses the passed import set datum
///
/// This produces an AST without performing the import itself.
pub fn parse_import_set(import_set_datum: NsDatum) -> Result<ParsedImportSet> {
    let span = import_set_datum.span();

    match import_set_datum {
        NsDatum::Vector(_, vs) => {
            let module_name = parse_module_name(span, vs.into_vec().into_iter())?;
            Ok(ParsedImportSet::Module(span, module_name))
        }
        NsDatum::List(_, vs) if vs.len() >= 2 => {
            let mut member_iter = vs.into_vec().into_iter();
            let filter_datum = member_iter.next().unwrap();

            let inner_import_datum = member_iter.next().unwrap();
            let filter_input = parse_import_set(inner_import_datum)?;

            let filter = parse_filter(span, filter_datum, &filter_input, member_iter)?;
            Ok(ParsedImportSet::Filter(filter, Box::new(filter_input)))
        }
        _ => Err(Error::new(span, ErrorKind::BadImportSet)),
    }
}
