use std::borrow::Cow;
use std::result;

use crate::hir::error::{Error, ErrorKind};
use crate::hir::exports::Exports;
use crate::hir::import::parse::{ParsedFilter, ParsedImportSet};

type Result<T> = result::Result<T, Vec<Error>>;

fn apply_filter(filter: ParsedFilter, exports: Cow<'_, Exports>) -> Result<Exports> {
    match filter {
        ParsedFilter::Only(only_spanned_names) => Ok(only_spanned_names
            .into_vec()
            .into_iter()
            .map(|(span, name)| {
                if let Some(binding) = exports.get(&name) {
                    Ok((name, binding.clone()))
                } else {
                    Err(Error::new(span, ErrorKind::UnboundIdent(name)))
                }
            })
            .collect::<result::Result<Exports, Error>>()?),

        ParsedFilter::Exclude(exclude_spanned_names) => {
            let mut exports = exports.into_owned();
            let mut errors = vec![];

            for (span, name) in exclude_spanned_names.into_vec().into_iter() {
                if exports.remove(&name).is_none() {
                    errors.push(Error::new(span, ErrorKind::UnboundIdent(name)));
                }
            }

            if errors.is_empty() {
                Ok(exports)
            } else {
                Err(errors)
            }
        }

        ParsedFilter::Rename(rename_spanned_names) => {
            let mut exports = exports.into_owned();
            let mut errors = vec![];

            for ((from_span, from_name), to_name) in rename_spanned_names.into_vec().into_iter() {
                match exports.remove(&from_name) {
                    Some(binding) => {
                        exports.insert(to_name, binding);
                    }
                    None => {
                        errors.push(Error::new(from_span, ErrorKind::UnboundIdent(from_name)));
                    }
                }
            }

            if errors.is_empty() {
                Ok(exports)
            } else {
                Err(errors)
            }
        }

        ParsedFilter::Prefix(prefix_name) => Ok(exports
            .iter()
            .map(|(name, binding)| (format!("{}{}", prefix_name, name).into(), binding.clone()))
            .collect()),
    }
}

/// Applies the parsed import to the passed exports
///
/// If there are no filters to apply then `exports` will be directly returned.
pub fn filter_imported_exports<'a>(
    parsed_import_set: ParsedImportSet,
    exports: Cow<'a, Exports>,
) -> Result<Cow<'a, Exports>> {
    match parsed_import_set {
        ParsedImportSet::Module(_, _) => Ok(exports),
        ParsedImportSet::Filter(filter, inner_parsed_import) => {
            let inner_exports = filter_imported_exports(*inner_parsed_import, exports)?;
            Ok(Cow::Owned(apply_filter(filter, inner_exports)?))
        }
    }
}
