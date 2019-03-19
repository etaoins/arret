mod checker;
mod expander;
mod matcher;

use std::collections::HashMap;

use syntax::span::Span;

use crate::hir::error::{Error, ErrorKind, Result};
use crate::hir::macros::checker::{check_rule, VarLinks};
use crate::hir::macros::expander::expand_rule;
use crate::hir::macros::matcher::match_rule;
use crate::hir::ns::{Ident, NsDatum};
use crate::hir::prim::Prim;
use crate::hir::scope::{Binding, Scope};

use crate::id_type::RcId;

#[derive(PartialEq, Debug)]
pub struct Rule {
    pattern: Vec<NsDatum>,
    template: NsDatum,
    var_links: VarLinks,
}

#[derive(Debug)]
pub struct Macro {
    rules: Vec<Rule>,
}

pub type MacroId = RcId<Macro>;

#[derive(Debug)]
pub struct MatchData<'data> {
    vars: HashMap<&'data Ident, &'data NsDatum>,
    // The outside vector is the subpatterns; the inside vector contains the zero or more matches
    subpatterns: Vec<Vec<MatchData<'data>>>,
}

impl<'data> MatchData<'data> {
    fn new() -> MatchData<'data> {
        MatchData {
            vars: HashMap::new(),
            subpatterns: vec![],
        }
    }
}

impl Macro {
    pub fn new(rules: Vec<Rule>) -> Macro {
        Macro { rules }
    }
}

fn is_ellipsis_datum(scope: &Scope, datum: &NsDatum) -> bool {
    if let NsDatum::Ident(_, ident) = datum {
        scope.get(ident) == Some(&Binding::Prim(Prim::Ellipsis))
    } else {
        false
    }
}

fn starts_with_zero_or_more(scope: &Scope, data: &[NsDatum]) -> bool {
    data.get(1)
        .map(|d| is_ellipsis_datum(scope, d))
        .unwrap_or(false)
}

fn is_escaped_ellipsis(scope: &Scope, data: &[NsDatum]) -> bool {
    data.len() == 2 && data.iter().all(|d| is_ellipsis_datum(scope, d))
}

fn lower_macro_rule_datum(scope: &Scope, rule_datum: NsDatum) -> Result<Rule> {
    let (span, mut rule_values) = if let NsDatum::Vector(span, vs) = rule_datum {
        (span, vs.into_vec())
    } else {
        return Err(Error::new(
            rule_datum.span(),
            ErrorKind::IllegalArg("expected a macro rule vector"),
        ));
    };

    if rule_values.len() != 2 {
        return Err(Error::new(
            span,
            ErrorKind::IllegalArg("expected a macro rule vector with two elements"),
        ));
    }

    let template = rule_values.pop().unwrap();
    let pattern_datum = rule_values.pop().unwrap();

    let pattern = if let NsDatum::List(_, vs) = pattern_datum {
        vs.into_vec()
    } else {
        return Err(Error::new(
            pattern_datum.span(),
            ErrorKind::IllegalArg("expected a macro rule pattern list"),
        ));
    };

    let var_links = check_rule(scope, pattern.as_slice(), &template)?;

    Ok(Rule {
        pattern,
        template,
        var_links,
    })
}

pub fn lower_macro_rules(scope: &Scope, macro_rules_data: Vec<NsDatum>) -> Result<Macro> {
    let rules = macro_rules_data
        .into_iter()
        .map(|rule_datum| lower_macro_rule_datum(scope, rule_datum))
        .collect::<Result<Vec<Rule>>>()?;

    Ok(Macro::new(rules))
}

pub fn expand_macro(
    scope: &mut Scope,
    invocation_span: Span,
    mac: &Macro,
    arg_data: &[NsDatum],
) -> Result<NsDatum> {
    for rule in &mac.rules {
        let match_result = match_rule(scope, rule, arg_data);

        if let Ok(match_data) = match_result {
            return Ok(expand_rule(
                scope,
                &match_data,
                &rule.var_links,
                &rule.template,
            ));
        }
    }

    Err(Error::new(invocation_span, ErrorKind::NoMacroRule))
}
