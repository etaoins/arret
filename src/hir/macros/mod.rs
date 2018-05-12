mod checker;
mod expander;
mod matcher;

use std::collections::{HashMap, HashSet};

use hir::error::{Error, ErrorKind, Result};
use hir::macros::checker::{check_rule, VarLinks};
use hir::macros::expander::expand_rule;
use hir::macros::matcher::match_rule;
use hir::ns::{Ident, NsDatum, NsIdAlloc};
use hir::prim::Prim;
use hir::scope::{Binding, Scope};
use syntax::span::Span;

#[derive(PartialEq, Eq, Debug, Hash)]
pub enum MacroVar {
    Bound(Binding),
    Unbound(String),
}

impl MacroVar {
    fn from_ident(scope: &Scope, ident: &Ident) -> MacroVar {
        match scope.get(ident) {
            Some(binding) => MacroVar::Bound(binding),
            None => MacroVar::Unbound(ident.name().clone()),
        }
    }
}

#[derive(PartialEq, Debug)]
pub struct SpecialVars {
    literals: HashSet<MacroVar>,
}

impl SpecialVars {
    fn is_literal(&self, var: &MacroVar) -> bool {
        self.literals.contains(var)
    }

    fn is_non_literal_prim(&self, var: &MacroVar, prim: Prim) -> bool {
        *var == MacroVar::Bound(Binding::Prim(prim)) && !self.is_literal(var)
    }

    fn is_wildcard(&self, var: &MacroVar) -> bool {
        self.is_non_literal_prim(var, Prim::Wildcard)
    }

    fn is_ellipsis(&self, var: &MacroVar) -> bool {
        self.is_non_literal_prim(var, Prim::Ellipsis)
    }

    fn is_ellipsis_datum(&self, scope: &Scope, datum: &NsDatum) -> bool {
        if let NsDatum::Ident(_, ident) = datum {
            let var = MacroVar::from_ident(scope, ident);
            self.is_ellipsis(&var)
        } else {
            false
        }
    }

    fn starts_with_zero_or_more(&self, scope: &Scope, data: &[NsDatum]) -> bool {
        data.get(1)
            .map(|d| self.is_ellipsis_datum(scope, d))
            .unwrap_or(false)
    }

    fn is_escaped_ellipsis(&self, scope: &Scope, data: &[NsDatum]) -> bool {
        data.len() == 2 && data.iter().all(|d| self.is_ellipsis_datum(scope, d))
    }
}

#[derive(PartialEq, Debug)]
pub struct Rule {
    pattern: Vec<NsDatum>,
    template: NsDatum,
    var_links: VarLinks,
}

#[derive(PartialEq, Debug)]
pub struct Macro {
    special_vars: SpecialVars,
    rules: Vec<Rule>,
}

#[derive(Debug)]
pub struct MatchData {
    vars: HashMap<MacroVar, NsDatum>,
    // The outside vector is the subpatterns; the inside vector contains the zero or more matches
    subpatterns: Vec<Vec<MatchData>>,
}

impl MatchData {
    fn new() -> MatchData {
        MatchData {
            vars: HashMap::new(),
            subpatterns: vec![],
        }
    }
}

impl Macro {
    pub fn new(special_vars: SpecialVars, rules: Vec<Rule>) -> Macro {
        Macro {
            special_vars,
            rules,
        }
    }
}

pub fn lower_macro_rule(
    scope: &Scope,
    self_ident: &Ident,
    special_vars: &SpecialVars,
    rule_datum: NsDatum,
) -> Result<Rule> {
    let (span, mut rule_values) = if let NsDatum::Vec(span, vs) = rule_datum {
        (span, vs)
    } else {
        return Err(Error::new(
            rule_datum.span(),
            ErrorKind::IllegalArg("expected a macro rule vector".to_owned()),
        ));
    };

    if rule_values.len() != 2 {
        return Err(Error::new(
            span,
            ErrorKind::IllegalArg("expected a macro rule vector with two elements".to_owned()),
        ));
    }

    let template = rule_values.pop().unwrap();
    let pattern_data = rule_values.pop().unwrap();

    let pattern = if let NsDatum::List(span, mut vs) = pattern_data {
        if vs.len() < 1 {
            return Err(Error::new(
                span,
                ErrorKind::IllegalArg(
                    "macro rule patterns must contain at least the name of the macro".to_owned(),
                ),
            ));
        }

        match vs.remove(0) {
            NsDatum::Ident(_, ref ident) if ident.name() == self_ident.name() => {}
            other => {
                return Err(Error::new(
                    other.span(),
                    ErrorKind::IllegalArg(
                        "macro rule patterns must start with the name of the macro".to_owned(),
                    ),
                ));
            }
        }

        vs
    } else {
        return Err(Error::new(
            pattern_data.span(),
            ErrorKind::IllegalArg("expected a macro rule pattern list".to_owned()),
        ));
    };

    let var_links = check_rule(scope, special_vars, pattern.as_slice(), &template)?;

    Ok(Rule {
        pattern,
        template,
        var_links,
    })
}

pub fn lower_macro_rules(
    scope: &Scope,
    span: Span,
    self_ident: &Ident,
    mut macro_rules_data: Vec<NsDatum>,
) -> Result<Macro> {
    if macro_rules_data.len() != 2 {
        return Err(Error::new(span, ErrorKind::WrongArgCount(2)));
    }

    let rules_datum = macro_rules_data.pop().unwrap();
    let literals_datum = macro_rules_data.pop().unwrap();

    let literals = if let NsDatum::Set(_, vs) = literals_datum {
        vs.into_iter()
            .map(|v| {
                if let NsDatum::Ident(_, ref ident) = v {
                    Ok(MacroVar::from_ident(scope, ident))
                } else {
                    Err(Error::new(
                        v.span(),
                        ErrorKind::IllegalArg("pattern literal must be a symbol".to_owned()),
                    ))
                }
            })
            .collect::<Result<HashSet<MacroVar>>>()?
    } else {
        return Err(Error::new(
            literals_datum.span(),
            ErrorKind::IllegalArg("expected set of pattern literals".to_owned()),
        ));
    };

    let rules_values = if let NsDatum::Vec(_, vs) = rules_datum {
        vs
    } else {
        return Err(Error::new(
            rules_datum.span(),
            ErrorKind::IllegalArg("expected a vector of syntax rules".to_owned()),
        ));
    };

    let special_vars = SpecialVars { literals };

    let rules = rules_values
        .into_iter()
        .map(|rule_datum| lower_macro_rule(scope, self_ident, &special_vars, rule_datum))
        .collect::<Result<Vec<Rule>>>()?;

    Ok(Macro::new(special_vars, rules))
}

pub fn expand_macro(
    ns_id_alloc: &mut NsIdAlloc,
    scope: &mut Scope,
    invocation_span: Span,
    mac: &Macro,
    arg_data: &[NsDatum],
) -> Result<NsDatum> {
    for rule in &mac.rules {
        let match_result = match_rule(scope, &mac.special_vars, rule, arg_data);

        if let Ok(match_data) = match_result {
            return Ok(expand_rule(
                ns_id_alloc,
                scope,
                &mac.special_vars,
                &match_data,
                &rule.var_links,
                &rule.template,
            ));
        }
    }

    Err(Error::new(invocation_span, ErrorKind::NoMacroRule))
}

#[cfg(test)]
mod test {
    use super::*;
    use syntax::parser::data_from_str;
    use syntax::span::t2s;

    fn macro_rules_for_str(data_str: &str) -> Result<Macro> {
        use hir::ns::NsId;

        let test_data = data_from_str(data_str).unwrap();

        let full_span = Span {
            lo: 0,
            hi: data_str.len() as u32,
        };

        let test_ns_id = NsId::new(0);
        let test_ns_data = test_data
            .into_iter()
            .map(|datum| NsDatum::from_syntax_datum(test_ns_id, datum))
            .collect::<Vec<NsDatum>>();

        let self_ident = Ident::new(test_ns_id, "self".to_owned());
        lower_macro_rules(&Scope::new_empty(), full_span, &self_ident, test_ns_data)
    }

    #[test]
    fn wrong_arg_count() {
        let j = "#{} [] []";
        let t = "^^^^^^^^^";

        let err = Error::new(t2s(t), ErrorKind::WrongArgCount(2));
        assert_eq!(err, macro_rules_for_str(j).unwrap_err());
    }

    #[test]
    fn non_set_literals() {
        let j = "[] []";
        let t = "^^   ";

        let err = Error::new(
            t2s(t),
            ErrorKind::IllegalArg("expected set of pattern literals".to_owned()),
        );
        assert_eq!(err, macro_rules_for_str(j).unwrap_err());
    }

    #[test]
    fn non_symbol_literal() {
        let j = "#{one 2} []";
        let t = "      ^    ";

        let err = Error::new(
            t2s(t),
            ErrorKind::IllegalArg("pattern literal must be a symbol".to_owned()),
        );
        assert_eq!(err, macro_rules_for_str(j).unwrap_err());
    }

    #[test]
    fn empty_rules() {
        let j = "#{} []";

        let special_vars = SpecialVars {
            literals: HashSet::new(),
        };

        let expected = Macro::new(special_vars, vec![]);
        assert_eq!(expected, macro_rules_for_str(j).unwrap());
    }

    #[test]
    fn rule_with_non_vector() {
        let j = "#{} [1]";
        let t = "     ^ ";

        let err = Error::new(
            t2s(t),
            ErrorKind::IllegalArg("expected a macro rule vector".to_owned()),
        );
        assert_eq!(err, macro_rules_for_str(j).unwrap_err());
    }

    #[test]
    fn rule_with_not_enough_elements() {
        let j = "#{} [[(self)]]";
        let t = "     ^^^^^^^^ ";

        let err = Error::new(
            t2s(t),
            ErrorKind::IllegalArg("expected a macro rule vector with two elements".to_owned()),
        );
        assert_eq!(err, macro_rules_for_str(j).unwrap_err());
    }

    #[test]
    fn rule_with_non_list_pattern() {
        let j = "#{} [[self 1]]";
        let t = "      ^^^^    ";

        let err = Error::new(
            t2s(t),
            ErrorKind::IllegalArg("expected a macro rule pattern list".to_owned()),
        );
        assert_eq!(err, macro_rules_for_str(j).unwrap_err());
    }

    #[test]
    fn rule_with_empty_pattern_list() {
        let j = "#{} [[() 1]]";
        let t = "      ^^    ";

        let err = Error::new(
            t2s(t),
            ErrorKind::IllegalArg(
                "macro rule patterns must contain at least the name of the macro".to_owned(),
            ),
        );
        assert_eq!(err, macro_rules_for_str(j).unwrap_err());
    }

    #[test]
    fn rule_with_non_self_pattern() {
        let j = "#{} [[(notself) 1]]";
        let t = "       ^^^^^^^     ";

        let err = Error::new(
            t2s(t),
            ErrorKind::IllegalArg(
                "macro rule patterns must start with the name of the macro".to_owned(),
            ),
        );
        assert_eq!(err, macro_rules_for_str(j).unwrap_err());
    }
}
