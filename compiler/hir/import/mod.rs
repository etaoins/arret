mod filter;
mod parse;

use std::collections::HashMap;

use arret_syntax::datum::Datum;
use arret_syntax::span::Span;

use crate::hir::error::Error;
use crate::hir::loader::ModuleName;

pub use filter::filter_imported_exports;
pub use parse::{parse_import_set, ParsedFilter, ParsedImportSet};

pub fn try_extract_import_set(datum: &Datum) -> Option<&[Datum]> {
    if let Datum::List(_, vs) = datum {
        match vs.as_ref() {
            [Datum::Sym(_, name), import_set @ ..] if name.as_ref() == "import" => Some(import_set),
            _ => None,
        }
    } else {
        None
    }
}

/// Returns all unique imported module names for the passed module data
///
/// The value of the `HashMap` will be the first span where that module name occurs. This is
/// intended to provide a stable location for error reporting.
pub fn collect_imported_module_names<'a>(
    data: impl Iterator<Item = &'a Datum>,
) -> Result<HashMap<ModuleName, Span>, Vec<Error>> {
    let mut imported_module_names = HashMap::new();
    let mut errors = vec![];

    for datum in data {
        if let Some(arg_data) = try_extract_import_set(datum) {
            for arg_datum in arg_data {
                match parse_import_set(arg_datum) {
                    Ok(parsed_import) => {
                        let (span, module_name) = parsed_import.into_spanned_module_name();
                        imported_module_names.entry(module_name).or_insert(span);
                    }
                    Err(error) => {
                        errors.push(error);
                    }
                }
            }
        }
    }

    if !errors.is_empty() {
        return Err(errors);
    }

    Ok(imported_module_names)
}

#[cfg(test)]
mod test {
    use super::*;

    use std::borrow::Cow;
    use std::collections::HashMap;
    use std::result;

    use arret_syntax::span::t2s;

    use crate::hir::error::{Error, ErrorKind};
    use crate::hir::exports::Exports;
    use crate::hir::loader::ModuleName;
    use crate::hir::prim::Prim;
    use crate::hir::scope::Binding;

    type Result<T> = result::Result<T, Vec<Error>>;

    fn exports_for_import_set(datum: &str) -> Result<Exports> {
        use arret_syntax::parser::datum_from_str;

        let parsed_import = parse::parse_import_set(&datum_from_str(None, datum).unwrap())?;
        let (span, module_name) = parsed_import.spanned_module_name();

        if module_name == &ModuleName::new("lib".into(), vec![], "test".into()) {
            let mut exports = HashMap::new();
            exports.insert("quote".into(), Binding::Prim(Prim::Quote));
            exports.insert("if".into(), Binding::Prim(Prim::If));

            Ok(filter::filter_imported_exports(parsed_import, Cow::Owned(exports))?.into_owned())
        } else {
            Err(vec![Error::new(span, ErrorKind::PackageNotFound)])
        }
    }

    fn assert_exports_prim(exports: &Exports, name: &'static str, expected_prim: Prim) {
        match exports.get(name) {
            Some(Binding::Prim(actual_prim)) => {
                assert_eq!(actual_prim, &expected_prim);
            }
            Some(other) => {
                panic!("Non-prim binding {:?} for {}", other, name);
            }
            None => {
                panic!("Missing binding for {}", name);
            }
        }
    }

    #[test]
    fn basic_import() {
        let j = "[lib test]";
        let exports = exports_for_import_set(j).unwrap();

        assert_exports_prim(&exports, "quote", Prim::Quote);
        assert_exports_prim(&exports, "if", Prim::If);
    }

    #[test]
    fn package_not_found() {
        let j = "[not found]";
        let t = "^^^^^^^^^^^";

        let err = vec![Error::new(t2s(t), ErrorKind::PackageNotFound)];

        assert_eq!(err, exports_for_import_set(j).unwrap_err());
    }

    #[test]
    fn only_filter() {
        let j = "(:only [lib test] quote)";
        let exports = exports_for_import_set(j).unwrap();

        assert_exports_prim(&exports, "quote", Prim::Quote);
        assert_eq!(false, exports.contains_key("if"));

        let j = "(:only [lib test] quote ifz)";
        let t = "                        ^^^ ";
        let err = vec![Error::new(t2s(t), ErrorKind::UnboundIdent("ifz".into()))];

        assert_eq!(err, exports_for_import_set(j).unwrap_err());
    }

    #[test]
    fn exclude_filter() {
        let j = "(:exclude [lib test] if)";
        let exports = exports_for_import_set(j).unwrap();

        assert_exports_prim(&exports, "quote", Prim::Quote);
        assert_eq!(false, exports.contains_key("if"));

        let j = "(:exclude [lib test] ifz)";
        let t = "                     ^^^ ";
        let err = vec![Error::new(t2s(t), ErrorKind::UnboundIdent("ifz".into()))];

        assert_eq!(err, exports_for_import_set(j).unwrap_err());
    }

    #[test]
    fn rename_filter() {
        let j = "(:rename [lib test] {quote new-quote, if new-if})";
        let exports = exports_for_import_set(j).unwrap();

        assert_exports_prim(&exports, "new-quote", Prim::Quote);
        assert_exports_prim(&exports, "new-if", Prim::If);

        let j = "(:rename [lib test] {ifz new-ifz})";
        let t = "                     ^^^          ";
        let err = vec![Error::new(t2s(t), ErrorKind::UnboundIdent("ifz".into()))];

        assert_eq!(err, exports_for_import_set(j).unwrap_err());
    }

    #[test]
    fn prefix_filter() {
        let j = "(:prefix [lib test] new-)";
        let exports = exports_for_import_set(j).unwrap();

        assert_exports_prim(&exports, "new-quote", Prim::Quote);
        assert_exports_prim(&exports, "new-if", Prim::If);
    }

    #[test]
    fn prefixed_filter() {
        let j = "(:prefixed [lib test])";
        let exports = exports_for_import_set(j).unwrap();

        assert_exports_prim(&exports, "test/quote", Prim::Quote);
        assert_exports_prim(&exports, "test/if", Prim::If);
    }
}
