use crate::datum::Datum;
use crate::error::{Error, ErrorKind, Result};
use crate::span::Span;

struct FoundArity {
    fixed_args: u8,
    has_rest: bool,
}

/// Visits all arg literals replacing `%` with `%1` and tracking our arity
fn visit_arg_literals(found_arity: &mut FoundArity, datum: Datum) -> Result<Datum> {
    match datum {
        Datum::Sym(span, name) => {
            if name.starts_with('%') {
                match &name[1..] {
                    "" => {
                        // We need to rewrite this to %1 in case it's also referred to by that name
                        found_arity.fixed_args = std::cmp::max(found_arity.fixed_args, 1);
                        Ok(Datum::Sym(span, "%1".into()))
                    }
                    "&" => {
                        found_arity.has_rest = true;
                        Ok(Datum::Sym(span, name))
                    }
                    other => other
                        .parse::<u8>()
                        .map(|parsed_number| {
                            found_arity.fixed_args =
                                std::cmp::max(found_arity.fixed_args, parsed_number);
                            Datum::Sym(span, name)
                        })
                        .map_err(|_| Error::new(span, ErrorKind::InvalidArgLiteral)),
                }
            } else {
                Ok(Datum::Sym(span, name))
            }
        }
        Datum::List(span, content) => {
            let replaced_content = content
                .into_vec()
                .into_iter()
                .map(|body_datum| visit_arg_literals(found_arity, body_datum))
                .collect::<Result<Vec<Datum>>>()?;

            Ok(Datum::List(span, replaced_content.into()))
        }
        other => Ok(other),
    }
}

/// Converts body data from a `#()` reader macro in to an anonymous function
pub fn convert_anon_fun(outer_span: Span, body_data: impl Iterator<Item = Datum>) -> Result<Datum> {
    use std::iter;

    let mut found_arity = FoundArity {
        fixed_args: 0,
        has_rest: false,
    };

    let replaced_body = body_data
        .map(|body_datum| visit_arg_literals(&mut found_arity, body_datum))
        .collect::<Result<Vec<Datum>>>()?;

    let mut param_list: Vec<Datum> = (0..found_arity.fixed_args)
        .map(|param_index| {
            let param_ordinal = param_index + 1;
            Datum::Sym(outer_span, format!("%{}", param_ordinal).into())
        })
        .collect();

    if found_arity.has_rest {
        param_list.extend(
            iter::once(Datum::Sym(outer_span, "&".into()))
                .chain(iter::once(Datum::Sym(outer_span, "%&".into()))),
        );
    }

    let expanded_fun = vec![
        Datum::Sym(outer_span, "fn".into()),
        Datum::List(outer_span, param_list.into()),
        Datum::List(outer_span, replaced_body.into()),
    ];

    Ok(Datum::List(outer_span, expanded_fun.into()))
}

/////////

#[cfg(test)]
mod test {
    use super::*;
    use crate::parser::data_from_str;
    use crate::span::t2s;

    #[test]
    fn empty_fun() {
        let j = "";
        let t = "";

        let body_data = data_from_str(j).unwrap();
        let outer_span = t2s(t);

        let expected = Datum::List(
            outer_span,
            Box::new([
                Datum::Sym(outer_span, "fn".into()),
                Datum::List(outer_span, Box::new([])),
                Datum::List(outer_span, Box::new([])),
            ]),
        );

        assert_eq!(
            expected,
            convert_anon_fun(outer_span, body_data.into_iter()).unwrap()
        );
    }

    #[test]
    fn one_arg_fun() {
        let j = "%";
        let t = "^";

        let body_data = data_from_str(j).unwrap();
        let outer_span = t2s(t);

        let expected = Datum::List(
            outer_span,
            Box::new([
                Datum::Sym(outer_span, "fn".into()),
                Datum::List(outer_span, Box::new([Datum::Sym(outer_span, "%1".into())])),
                Datum::List(
                    outer_span,
                    Box::new([
                        // This is converted to %1
                        Datum::Sym(t2s(t), "%1".into()),
                    ]),
                ),
            ]),
        );

        assert_eq!(
            expected,
            convert_anon_fun(outer_span, body_data.into_iter()).unwrap()
        );
    }

    #[test]
    fn two_arg_fun() {
        let j = "%1 %2";
        let t = "^^^^^";
        let u = "^^   ";
        let v = "   ^^";

        let body_data = data_from_str(j).unwrap();
        let outer_span = t2s(t);

        let expected = Datum::List(
            outer_span,
            Box::new([
                Datum::Sym(outer_span, "fn".into()),
                Datum::List(
                    outer_span,
                    Box::new([
                        Datum::Sym(outer_span, "%1".into()),
                        Datum::Sym(outer_span, "%2".into()),
                    ]),
                ),
                Datum::List(
                    outer_span,
                    Box::new([
                        Datum::Sym(t2s(u), "%1".into()),
                        Datum::Sym(t2s(v), "%2".into()),
                    ]),
                ),
            ]),
        );

        assert_eq!(
            expected,
            convert_anon_fun(outer_span, body_data.into_iter()).unwrap()
        );
    }

    #[test]
    fn rest_fun() {
        let j = "%1 %&";
        let t = "^^^^^";
        let u = "^^   ";
        let v = "   ^^";

        let body_data = data_from_str(j).unwrap();
        let outer_span = t2s(t);

        let expected = Datum::List(
            outer_span,
            Box::new([
                Datum::Sym(outer_span, "fn".into()),
                Datum::List(
                    outer_span,
                    Box::new([
                        Datum::Sym(outer_span, "%1".into()),
                        Datum::Sym(outer_span, "&".into()),
                        Datum::Sym(outer_span, "%&".into()),
                    ]),
                ),
                Datum::List(
                    outer_span,
                    Box::new([
                        Datum::Sym(t2s(u), "%1".into()),
                        Datum::Sym(t2s(v), "%&".into()),
                    ]),
                ),
            ]),
        );

        assert_eq!(
            expected,
            convert_anon_fun(outer_span, body_data.into_iter()).unwrap()
        );
    }
}
