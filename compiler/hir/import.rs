use std::result;

use syntax::datum::DataStr;
use syntax::span::Span;

use crate::hir::error::{Error, ErrorKind};
use crate::hir::exports::Exports;
use crate::hir::loader::ModuleName;
use crate::hir::ns::{Ident, NsDataIter, NsDatum};
use crate::hir::util::{expect_arg_count, expect_ident, expect_ident_and_span, expect_one_arg};

type Result<T> = result::Result<T, Vec<Error>>;

/// Input to an (import) filter
///
/// This tracks the exports and the terminal name of the original module
struct FilterInput {
    exports: Exports,

    /// The terminal name of the module the exports came from
    ///
    /// This is to support `(prefixed)`. For example, the terminal name of `[stdlib base]` would be
    /// `base` and if `(prefixed)` was used it would prepend `base/` to all of its identifiers.
    terminal_name: DataStr,
}

struct LowerImportCtx<F>
where
    F: FnMut(Span, ModuleName) -> Result<Exports>,
{
    load_module: F,
}

impl<F> LowerImportCtx<F>
where
    F: FnMut(Span, ModuleName) -> Result<Exports>,
{
    fn lower_module_import(&mut self, span: Span, name_data: Vec<NsDatum>) -> Result<FilterInput> {
        let mut name_idents = name_data
            .into_iter()
            .map(|datum| expect_ident(datum).map(Ident::into_name));

        let package_name = name_idents.next().unwrap()?;
        let terminal_name = name_idents.next_back().unwrap()?;
        let name_components = name_idents.collect::<result::Result<Vec<_>, Error>>()?;

        let module_name = ModuleName::new(package_name, name_components, terminal_name.clone());
        let exports = (self.load_module)(span, module_name)?;

        Ok(FilterInput {
            exports,
            terminal_name,
        })
    }

    fn lower_import_filter(
        &mut self,
        apply_span: Span,
        filter_name: &str,
        filter_input: FilterInput,
        arg_iter: NsDataIter,
    ) -> Result<FilterInput> {
        match filter_name {
            "only" => {
                let inner_exports = filter_input.exports;
                let only_exports = arg_iter
                    .map(|arg_datum| {
                        let (ident, span) = expect_ident_and_span(arg_datum)?;

                        if let Some(binding) = inner_exports.get(ident.name()) {
                            Ok((ident.into_name(), binding.clone()))
                        } else {
                            Err(Error::new(
                                span,
                                ErrorKind::UnboundSym(ident.into_name()),
                            ))
                        }
                    })
                    .collect::<result::Result<Exports, Error>>()?;

                Ok(FilterInput {
                    exports: only_exports,
                    terminal_name: filter_input.terminal_name,
                })
            }
            "except" => {
                let mut except_exports = filter_input.exports;
                let mut errors = vec![];

                for arg_datum in arg_iter {
                    let (ident, span) = expect_ident_and_span(arg_datum)?;

                    if except_exports.remove(ident.name()).is_none() {
                        errors.push(Error::new(
                            span,
                            ErrorKind::UnboundSym(ident.into_name()),
                        ));
                    }
                }

                if errors.is_empty() {
                    Ok(FilterInput {
                        exports: except_exports,
                        terminal_name: filter_input.terminal_name,
                    })
                } else {
                    Err(errors)
                }
            }
            "rename" => {
                let arg_datum = expect_one_arg(apply_span, arg_iter)?;

                if let NsDatum::Map(_, vs) = arg_datum {
                    let mut rename_exports = filter_input.exports;
                    let mut errors = vec![];

                    for (from_datum, to_datum) in vs.into_vec() {
                        let (from_ident, from_span) = expect_ident_and_span(from_datum)?;
                        let to_ident = expect_ident(to_datum)?;

                        match rename_exports.remove(from_ident.name()) {
                            Some(binding) => {
                                rename_exports.insert(to_ident.into_name(), binding);
                            }
                            None => {
                                errors.push(Error::new(
                                    from_span,
                                    ErrorKind::UnboundSym(from_ident.into_name()),
                                ));
                            }
                        }
                    }

                    if errors.is_empty() {
                        Ok(FilterInput {
                            exports: rename_exports,
                            terminal_name: filter_input.terminal_name,
                        })
                    } else {
                        Err(errors)
                    }
                } else {
                    Err(vec![Error::new(
                        arg_datum.span(),
                        ErrorKind::IllegalArg(
                            "(rename) expects a map of identifier renames",
                        ),
                    )])
                }
            }
            "prefix" => {
                let prefix_datum = expect_one_arg(apply_span, arg_iter)?;
                let prefix_ident = expect_ident(prefix_datum)?;

                let prefix_exports = filter_input
                    .exports
                    .into_iter()
                    .map(|(name, binding)| (format!("{}{}", prefix_ident.name(), name).into(), binding))
                    .collect();

                Ok(FilterInput {
                    exports: prefix_exports,
                    terminal_name: filter_input.terminal_name,
                })
            }
            "prefixed" => {
                expect_arg_count(apply_span, 0, arg_iter.len())?;
                let FilterInput {
                    exports,
                    terminal_name,
                } = filter_input;

                let prefixed_exports = exports
                    .into_iter()
                    .map(|(name, binding)| (format!("{}/{}", &terminal_name, name).into(), binding))
                    .collect();

                Ok(FilterInput {
                    exports: prefixed_exports,
                    terminal_name,
                })
            }
            _ => Err(vec![Error::new(
                apply_span,
                ErrorKind::IllegalArg(
                    "unknown import filter; must be `only`, `except`, `rename`, `prefix` or `prefixed`"
                ),
            )]),
        }
    }

    fn lower_import_set(&mut self, import_set_datum: NsDatum) -> Result<FilterInput> {
        let span = import_set_datum.span();
        match import_set_datum {
            NsDatum::Vector(_, vs) => {
                if vs.len() < 2 {
                    return Err(vec![Error::new(
                        span,
                        ErrorKind::IllegalArg("module name requires a least two elements"),
                    )]);
                }

                return self.lower_module_import(span, vs.into_vec());
            }
            NsDatum::List(_, vs) => {
                let mut filter_iter = vs.into_vec().into_iter();

                // Each filter requires a filter identifier and an inner import set
                if filter_iter.len() >= 2 {
                    let filter_ident = expect_ident(filter_iter.next().unwrap())?;
                    let inner_import_datum = filter_iter.next().unwrap();

                    let filter_input = self.lower_import_set(inner_import_datum)?;
                    return self.lower_import_filter(
                        span,
                        filter_ident.name(),
                        filter_input,
                        filter_iter,
                    );
                }
            }
            _ => {}
        }

        Err(vec![Error::new(
            span,
            ErrorKind::IllegalArg(
                "import set must either be a module name vector or an applied filter",
            ),
        )])
    }
}

pub fn lower_import_set<F>(import_set_datum: NsDatum, load_module: F) -> Result<Exports>
where
    F: FnMut(Span, ModuleName) -> Result<Exports>,
{
    let mut lic = LowerImportCtx { load_module };
    lic.lower_import_set(import_set_datum)
        .map(|filter_input| filter_input.exports)
}

#[cfg(test)]
mod test {
    use super::*;
    use crate::hir::prim::Prim;
    use crate::hir::scope::Binding;
    use std::collections::HashMap;
    use syntax::span::{t2s, EMPTY_SPAN};

    fn exports_for_import_set(datum: &str) -> Result<Exports> {
        use syntax::parser::datum_from_str;

        let import_set_datum = NsDatum::from_syntax_datum(&datum_from_str(datum).unwrap());

        lower_import_set(import_set_datum, |_, module_name| {
            if module_name == ModuleName::new("lib".into(), vec![], "test".into()) {
                let mut exports = HashMap::new();
                exports.insert("quote".into(), Binding::Prim(Prim::Quote));
                exports.insert("if".into(), Binding::Prim(Prim::If));

                Ok(exports)
            } else {
                Err(vec![Error::new(EMPTY_SPAN, ErrorKind::PackageNotFound)])
            }
        })
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
        let j = "(only [lib test] quote)";
        let exports = exports_for_import_set(j).unwrap();

        assert_eq!(exports["quote"], Binding::Prim(Prim::Quote));
        assert_eq!(false, exports.contains_key("if"));

        let j = "(only [lib test] quote ifz)";
        let t = "                       ^^^ ";
        let err = vec![Error::new(t2s(t), ErrorKind::UnboundSym("ifz".into()))];

        assert_eq!(err, exports_for_import_set(j).unwrap_err());
    }

    #[test]
    fn except_filter() {
        let j = "(except [lib test] if)";
        let exports = exports_for_import_set(j).unwrap();

        assert_eq!(exports["quote"], Binding::Prim(Prim::Quote));
        assert_eq!(false, exports.contains_key("if"));

        let j = "(except [lib test] ifz)";
        let t = "                   ^^^ ";
        let err = vec![Error::new(t2s(t), ErrorKind::UnboundSym("ifz".into()))];

        assert_eq!(err, exports_for_import_set(j).unwrap_err());
    }

    #[test]
    fn rename_filter() {
        let j = "(rename [lib test] {quote new-quote, if new-if})";
        let exports = exports_for_import_set(j).unwrap();

        assert_eq!(exports["new-quote"], Binding::Prim(Prim::Quote));
        assert_eq!(exports["new-if"], Binding::Prim(Prim::If));

        let j = "(rename [lib test] {ifz new-ifz})";
        let t = "                    ^^^          ";
        let err = vec![Error::new(t2s(t), ErrorKind::UnboundSym("ifz".into()))];

        assert_eq!(err, exports_for_import_set(j).unwrap_err());
    }

    #[test]
    fn prefix_filter() {
        let j = "(prefix [lib test] new-)";
        let exports = exports_for_import_set(j).unwrap();

        assert_eq!(exports["new-quote"], Binding::Prim(Prim::Quote));
        assert_eq!(exports["new-if"], Binding::Prim(Prim::If));
    }

    #[test]
    fn prefixed_filter() {
        let j = "(prefixed [lib test])";
        let exports = exports_for_import_set(j).unwrap();

        assert_eq!(exports["test/quote"], Binding::Prim(Prim::Quote));
        assert_eq!(exports["test/if"], Binding::Prim(Prim::If));
    }
}
