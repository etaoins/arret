mod filter;
mod parse;

use arret_syntax::datum::Datum;

pub use filter::filter_imported_exports;
pub use parse::{parse_import_set, ParsedFilter, ParsedImportSet};

pub fn try_extract_import_set(datum: &Datum) -> Option<&[Datum]> {
    if let Datum::List(_, vs) = datum {
        if let Some(Datum::Sym(_, name)) = vs.get(0) {
            if name.as_ref() == "import" {
                return Some(&vs[1..]);
            }
        }
    }

    None
}

#[cfg(test)]
mod test {
    use super::*;

    use std::borrow::Cow;
    use std::collections::HashMap;
    use std::result;

    use arret_syntax::span::{t2s, EMPTY_SPAN};

    use crate::hir::error::{Error, ErrorKind};
    use crate::hir::exports::Exports;
    use crate::hir::loader::ModuleName;
    use crate::hir::prim::Prim;
    use crate::hir::scope::Binding;

    type Result<T> = result::Result<T, Vec<Error>>;

    fn exports_for_import_set(datum: &str) -> Result<Exports> {
        use arret_syntax::parser::datum_from_str;

        let parsed_import = parse::parse_import_set(&datum_from_str(datum).unwrap())?;

        let (_, module_name) = parsed_import.spanned_module_name();

        if module_name == &ModuleName::new("lib".into(), vec![], "test".into()) {
            let mut exports = HashMap::new();
            exports.insert("quote".into(), Binding::Prim(Prim::Quote));
            exports.insert("if".into(), Binding::Prim(Prim::If));

            Ok(filter::filter_imported_exports(&parsed_import, Cow::Owned(exports))?.into_owned())
        } else {
            Err(vec![Error::new(EMPTY_SPAN, ErrorKind::PackageNotFound)])
        }
    }

    #[test]
    fn basic_import() {
        let j = "[lib test]";
        let exports = exports_for_import_set(j).unwrap();

        assert_eq!(exports["quote"], Binding::Prim(Prim::Quote));
        assert_eq!(exports["if"], Binding::Prim(Prim::If));
    }

    #[test]
    fn package_not_found() {
        let j = "[not found]";
        let err = vec![Error::new(EMPTY_SPAN, ErrorKind::PackageNotFound)];

        assert_eq!(err, exports_for_import_set(j).unwrap_err());
    }

    #[test]
    fn only_filter() {
        let j = "(:only [lib test] quote)";
        let exports = exports_for_import_set(j).unwrap();

        assert_eq!(exports["quote"], Binding::Prim(Prim::Quote));
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

        assert_eq!(exports["quote"], Binding::Prim(Prim::Quote));
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

        assert_eq!(exports["new-quote"], Binding::Prim(Prim::Quote));
        assert_eq!(exports["new-if"], Binding::Prim(Prim::If));

        let j = "(:rename [lib test] {ifz new-ifz})";
        let t = "                     ^^^          ";
        let err = vec![Error::new(t2s(t), ErrorKind::UnboundIdent("ifz".into()))];

        assert_eq!(err, exports_for_import_set(j).unwrap_err());
    }

    #[test]
    fn prefix_filter() {
        let j = "(:prefix [lib test] new-)";
        let exports = exports_for_import_set(j).unwrap();

        assert_eq!(exports["new-quote"], Binding::Prim(Prim::Quote));
        assert_eq!(exports["new-if"], Binding::Prim(Prim::If));
    }

    #[test]
    fn prefixed_filter() {
        let j = "(:prefixed [lib test])";
        let exports = exports_for_import_set(j).unwrap();

        assert_eq!(exports["test/quote"], Binding::Prim(Prim::Quote));
        assert_eq!(exports["test/if"], Binding::Prim(Prim::If));
    }
}
