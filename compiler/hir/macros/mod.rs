mod expander;
mod linker;
mod matcher;

use syntax::span::Span;

use crate::hir::error::{Error, ErrorKind, Result};
use crate::hir::macros::expander::expand_rule;
use crate::hir::macros::linker::{link_rule_vars, VarLinks};
use crate::hir::macros::matcher::match_rule;
use crate::hir::ns::{Ident, NsDatum};
use crate::hir::scope::Scope;

use crate::id_type::ArcId;

#[derive(PartialEq, Debug)]
pub struct Rule {
    pattern: Box<[NsDatum]>,
    template: NsDatum,
    var_links: VarLinks,
}

#[derive(Debug)]
pub struct Macro {
    rules: Box<[Rule]>,
}

pub type MacroId = ArcId<Macro>;

impl Macro {
    pub fn new(rules: Box<[Rule]>) -> Macro {
        Macro { rules }
    }
}

fn starts_with_zero_or_more(data: &[NsDatum]) -> bool {
    if let Some(NsDatum::Ident(_, ident)) = data.get(1) {
        ident.name() == "..."
    } else {
        false
    }
}

fn get_escaped_ident(data: &[NsDatum]) -> Option<&Ident> {
    match data {
        [NsDatum::Ident(_, ellipsis_ident), NsDatum::Ident(_, escaped_ident)]
            if ellipsis_ident.name() == "..." =>
        {
            Some(escaped_ident)
        }
        _ => None,
    }
}

fn lower_macro_rule_datum(scope: &Scope, self_ident: &Ident, rule_datum: NsDatum) -> Result<Rule> {
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
        vs
    } else {
        return Err(Error::new(
            pattern_datum.span(),
            ErrorKind::IllegalArg("expected a macro rule pattern list"),
        ));
    };

    let var_links = link_rule_vars(scope, self_ident, &pattern, &template)?;

    Ok(Rule {
        pattern,
        template,
        var_links,
    })
}

pub fn lower_macro_rules(
    scope: &Scope,
    self_ident: &Ident,
    macro_rules_data: Vec<NsDatum>,
) -> Result<Macro> {
    let rules = macro_rules_data
        .into_iter()
        .map(|rule_datum| lower_macro_rule_datum(scope, self_ident, rule_datum))
        .collect::<Result<Box<[Rule]>>>()?;

    Ok(Macro::new(rules))
}

pub fn expand_macro(
    scope: &mut Scope,
    invocation_span: Span,
    mac: &MacroId,
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

    Err(Error::new(invocation_span, ErrorKind::NoMacroRule))
}
