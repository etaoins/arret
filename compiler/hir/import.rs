use std::collections::HashMap;
use std::result;

use hir::error::{Error, ErrorKind};
use hir::loader::ModuleName;
use hir::ns::{NsDataIter, NsDatum};
use hir::scope::Binding;
use hir::util::{expect_arg_count, expect_ident, expect_ident_and_span, expect_one_arg};
use syntax::span::Span;

type Result<T> = result::Result<T, Vec<Error>>;
type Bindings = HashMap<Box<str>, Binding>;

/// Input to an (import) filter
///
/// This tracks the bindings and the terminal name of the original module
struct FilterInput {
    bindings: Bindings,

    /// The terminal name of the module the bindings came from
    ///
    /// This is to support `(prefixed)`. For example, the terminal name of `[scheme base]` would be
    /// `base` and if `(prefixed)` was used it would prepend `base/` to all of its identifiers.
    terminal_name: Box<str>,
}

struct LowerImportCtx<F>
where
    F: FnMut(Span, ModuleName) -> Result<Bindings>,
{
    load_module: F,
}

impl<F> LowerImportCtx<F>
where
    F: FnMut(Span, ModuleName) -> Result<Bindings>,
{
    fn lower_module_import(&mut self, span: Span, name_data: Vec<NsDatum>) -> Result<FilterInput> {
        let mut name_components = name_data
            .into_iter()
            .map(|datum| expect_ident(datum).map(|ident| ident.into_name()))
            .collect::<result::Result<Vec<Box<str>>, Error>>()?;

        let terminal_name = name_components.pop().unwrap();
        let module_name = ModuleName::new(name_components, terminal_name.clone());
        let bindings = (self.load_module)(span, module_name)?;

        Ok(FilterInput {
            bindings,
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
                let inner_bindings = filter_input.bindings;
                let only_bindings = arg_iter
                    .map(|arg_datum| {
                        let (ident, span) = expect_ident_and_span(arg_datum)?;

                        if let Some(binding) = inner_bindings.get(ident.name()) {
                            Ok((ident.into_name(), binding.clone()))
                        } else {
                            Err(Error::new(
                                span,
                                ErrorKind::UnboundSym(ident.into_name()),
                            ))
                        }
                    })
                    .collect::<result::Result<Bindings, Error>>()?;

                Ok(FilterInput {
                    bindings: only_bindings,
                    terminal_name: filter_input.terminal_name,
                })
            }
            "except" => {
                let mut except_bindings = filter_input.bindings;
                let mut errors = vec![];

                for arg_datum in arg_iter {
                    let (ident, span) = expect_ident_and_span(arg_datum)?;

                    if except_bindings.remove(ident.name()).is_none() {
                        errors.push(Error::new(
                            span,
                            ErrorKind::UnboundSym(ident.into_name()),
                        ));
                    }
                }

                if errors.is_empty() {
                    Ok(FilterInput {
                        bindings: except_bindings,
                        terminal_name: filter_input.terminal_name,
                    })
                } else {
                    Err(errors)
                }
            }
            "rename" => {
                let arg_datum = expect_one_arg(apply_span, arg_iter)?;

                if let NsDatum::Map(_, vs) = arg_datum {
                    let mut rename_bindings = filter_input.bindings;
                    let mut errors = vec![];

                    for (from_datum, to_datum) in vs.into_vec() {
                        let (from_ident, from_span) = expect_ident_and_span(from_datum)?;
                        let to_ident = expect_ident(to_datum)?;

                        match rename_bindings.remove(from_ident.name()) {
                            Some(binding) => {
                                rename_bindings.insert(to_ident.into_name(), binding);
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
                            bindings: rename_bindings,
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

                let prefix_bindings = filter_input
                    .bindings
                    .into_iter()
                    .map(|(name, binding)| (format!("{}{}", prefix_ident.name(), name).into_boxed_str(), binding))
                    .collect();

                Ok(FilterInput {
                    bindings: prefix_bindings,
                    terminal_name: filter_input.terminal_name,
                })
            }
            "prefixed" => {
                expect_arg_count(apply_span, 0, arg_iter.len())?;
                let FilterInput {
                    bindings,
                    terminal_name,
                } = filter_input;

                let prefixed_bindings = bindings
                    .into_iter()
                    .map(|(name, binding)| (format!("{}/{}", &terminal_name, name).into_boxed_str(), binding))
                    .collect();

                Ok(FilterInput {
                    bindings: prefixed_bindings,
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
            NsDatum::Vec(_, vs) => {
                if vs.is_empty() {
                    return Err(vec![Error::new(
                        span,
                        ErrorKind::IllegalArg("module name requires a least one element"),
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

pub fn lower_import_set<F>(import_set_datum: NsDatum, load_module: F) -> Result<Bindings>
where
    F: FnMut(Span, ModuleName) -> Result<Bindings>,
{
    let mut lic = LowerImportCtx { load_module };
    lic.lower_import_set(import_set_datum)
        .map(|filter_input| filter_input.bindings)
}

#[cfg(test)]
mod test {
    use super::*;
    use hir::ns::NsId;
    use hir::prim::Prim;
    use syntax::span::{t2s, EMPTY_SPAN};

    fn load_test_module(_: Span, module_name: ModuleName) -> Result<Bindings> {
        if module_name == ModuleName::new(vec!["lib".into()], "test".into()) {
            let mut bindings = HashMap::new();
            bindings.insert("quote".into(), Binding::Prim(Prim::Quote));
            bindings.insert("if".into(), Binding::Prim(Prim::If));

            Ok(bindings)
        } else {
            Err(vec![Error::new(EMPTY_SPAN, ErrorKind::ModuleNotFound)])
        }
    }

    fn bindings_for_import_set(datum: &str) -> Result<HashMap<Box<str>, Binding>> {
        use syntax::parser::datum_from_str;

        let test_ns_id = NsId::new(0);

        let import_set_datum =
            NsDatum::from_syntax_datum(test_ns_id, datum_from_str(datum).unwrap());

        lower_import_set(import_set_datum, load_test_module)
    }

    #[test]
    fn basic_import() {
        let j = "[lib test]";
        let bindings = bindings_for_import_set(j).unwrap();

        assert_eq!(bindings["quote"], Binding::Prim(Prim::Quote));
        assert_eq!(bindings["if"], Binding::Prim(Prim::If));
    }

    #[test]
    fn module_not_found() {
        let j = "[not found]";
        let err = vec![Error::new(EMPTY_SPAN, ErrorKind::ModuleNotFound)];

        assert_eq!(err, bindings_for_import_set(j).unwrap_err());
    }

    #[test]
    fn only_filter() {
        let j = "(only [lib test] quote)";
        let bindings = bindings_for_import_set(j).unwrap();

        assert_eq!(bindings["quote"], Binding::Prim(Prim::Quote));
        assert_eq!(false, bindings.contains_key("if"));

        let j = "(only [lib test] quote ifz)";
        let t = "                       ^^^ ";
        let err = vec![Error::new(t2s(t), ErrorKind::UnboundSym("ifz".into()))];

        assert_eq!(err, bindings_for_import_set(j).unwrap_err());
    }

    #[test]
    fn except_filter() {
        let j = "(except [lib test] if)";
        let bindings = bindings_for_import_set(j).unwrap();

        assert_eq!(bindings["quote"], Binding::Prim(Prim::Quote));
        assert_eq!(false, bindings.contains_key("if"));

        let j = "(except [lib test] ifz)";
        let t = "                   ^^^ ";
        let err = vec![Error::new(t2s(t), ErrorKind::UnboundSym("ifz".into()))];

        assert_eq!(err, bindings_for_import_set(j).unwrap_err());
    }

    #[test]
    fn rename_filter() {
        let j = "(rename [lib test] {quote new-quote, if new-if})";
        let bindings = bindings_for_import_set(j).unwrap();

        assert_eq!(bindings["new-quote"], Binding::Prim(Prim::Quote));
        assert_eq!(bindings["new-if"], Binding::Prim(Prim::If));

        let j = "(rename [lib test] {ifz new-ifz})";
        let t = "                    ^^^          ";
        let err = vec![Error::new(t2s(t), ErrorKind::UnboundSym("ifz".into()))];

        assert_eq!(err, bindings_for_import_set(j).unwrap_err());
    }

    #[test]
    fn prefix_filter() {
        let j = "(prefix [lib test] new-)";
        let bindings = bindings_for_import_set(j).unwrap();

        assert_eq!(bindings["new-quote"], Binding::Prim(Prim::Quote));
        assert_eq!(bindings["new-if"], Binding::Prim(Prim::If));
    }

    #[test]
    fn prefixed_filter() {
        let j = "(prefixed [lib test])";
        let bindings = bindings_for_import_set(j).unwrap();

        assert_eq!(bindings["test/quote"], Binding::Prim(Prim::Quote));
        assert_eq!(bindings["test/if"], Binding::Prim(Prim::If));
    }
}
