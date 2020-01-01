mod expander;
mod linker;
mod matcher;

use std::sync::Arc;

use arret_syntax::span::Span;

use crate::hir::error::{Error, ErrorKind, Result};
use crate::hir::macros::expander::expand_rule;
use crate::hir::macros::linker::{link_rule_vars, VarLinks};
use crate::hir::macros::matcher::match_rule;
use crate::hir::ns::{Ident, NsDatum};
use crate::hir::scope::Scope;

#[derive(Debug)]
pub struct Rule {
    pattern_span: Span,
    pattern: Box<[NsDatum]>,
    template: NsDatum,
    var_links: VarLinks,
}

#[derive(Debug)]
pub struct Macro {
    rules: Box<[Rule]>,
}

impl Macro {
    pub fn new(rules: Box<[Rule]>) -> Arc<Self> {
        Arc::new(Self { rules })
    }
}

fn starts_with_zero_or_more(data: &[NsDatum]) -> bool {
    if let Some(NsDatum::Ident(_, ident)) = data.get(1) {
        ident.is_ellipsis()
    } else {
        false
    }
}

fn get_escaped_ident(data: &[NsDatum]) -> Option<&Ident> {
    match data {
        [NsDatum::Ident(_, ellipsis_ident), NsDatum::Ident(_, escaped_ident)]
            if ellipsis_ident.is_ellipsis() =>
        {
            Some(escaped_ident)
        }
        _ => None,
    }
}

fn lower_macro_rule_datum(
    scope: &Scope<'_>,
    self_ident: &Ident,
    rule_datum: NsDatum,
) -> Result<Rule> {
    let (span, mut rule_values) = if let NsDatum::Vector(span, vs) = rule_datum {
        (span, vs.into_vec())
    } else {
        return Err(Error::new(
            rule_datum.span(),
            ErrorKind::ExpectedMacroRuleVec(rule_datum.description()),
        ));
    };

    if rule_values.len() != 2 {
        return Err(Error::new(
            span,
            ErrorKind::WrongMacroRuleVecCount(rule_values.len()),
        ));
    }

    let template = rule_values.pop().unwrap();
    let pattern_datum = rule_values.pop().unwrap();

    let (pattern_span, pattern) = if let NsDatum::List(span, vs) = pattern_datum {
        (span, vs)
    } else {
        return Err(Error::new(
            pattern_datum.span(),
            ErrorKind::ExpectedMacroRulePatternList(pattern_datum.description()),
        ));
    };

    let var_links = link_rule_vars(scope, self_ident, &pattern, &template)?;

    Ok(Rule {
        pattern_span,
        pattern,
        template,
        var_links,
    })
}

pub fn lower_macro_rules(
    scope: &Scope<'_>,
    self_ident: &Ident,
    macro_rules_data: Vec<NsDatum>,
) -> Result<Arc<Macro>> {
    let rules = macro_rules_data
        .into_iter()
        .map(|rule_datum| lower_macro_rule_datum(scope, self_ident, rule_datum))
        .collect::<Result<Box<[Rule]>>>()?;

    Ok(Macro::new(rules))
}

pub fn expand_macro<'s, 'p>(
    scope: &'s mut Scope<'p>,
    invocation_span: Span,
    mac: &Arc<Macro>,
    arg_data: &[NsDatum],
) -> Result<NsDatum> {
    for rule in mac.rules.iter() {
        let match_result = match_rule(rule, arg_data);

        if let Ok(match_data) = match_result {
            return Ok(expand_rule(
                scope,
                mac,
                &match_data,
                &rule.var_links,
                &rule.template,
            ));
        }
    }

    Err(Error::new(
        invocation_span,
        ErrorKind::NoMacroRule(mac.rules.iter().map(|rule| rule.pattern_span).collect()),
    ))
}
