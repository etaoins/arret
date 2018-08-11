mod checker;
mod expander;
mod matcher;

use std::collections::{HashMap, HashSet};

use crate::hir::error::{Error, ErrorKind, Result};
use crate::hir::macros::checker::{check_rule, VarLinks};
use crate::hir::macros::expander::expand_rule;
use crate::hir::macros::matcher::match_rule;
use crate::hir::ns::{Ident, NsDatum};
use crate::hir::prim::Prim;
use crate::hir::scope::{Binding, Scope};
use crate::hir::util::expect_ident;
use syntax::span::Span;

#[derive(PartialEq, Eq, Debug, Hash)]
pub enum MacroVar {
    Bound(Binding),
    Unbound(Box<str>),
}

impl MacroVar {
    fn from_ident(scope: &Scope, ident: &Ident) -> MacroVar {
        match scope.get(ident) {
            Some(binding) => MacroVar::Bound(binding),
            None => MacroVar::Unbound(ident.name().into()),
        }
    }

    fn from_owned_ident(scope: &Scope, ident: Ident) -> MacroVar {
        match scope.get(&ident) {
            Some(binding) => MacroVar::Bound(binding),
            None => MacroVar::Unbound(ident.into_name()),
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

fn lower_macro_rule_datum(
    scope: &Scope,
    special_vars: &SpecialVars,
    rule_datum: NsDatum,
) -> Result<Rule> {
    let (span, mut rule_values) = if let NsDatum::Vec(span, vs) = rule_datum {
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

    let var_links = check_rule(scope, special_vars, pattern.as_slice(), &template)?;

    Ok(Rule {
        pattern,
        template,
        var_links,
    })
}

pub fn lower_macro_rules(scope: &Scope, macro_rules_data: Vec<NsDatum>) -> Result<Macro> {
    let literals;
    let mut macro_rules_iter;

    // Peak at our first datum to see if it's a Set
    if let Some(NsDatum::Set(_, _)) = macro_rules_data.get(0) {
        macro_rules_iter = macro_rules_data.into_iter();
        literals = if let NsDatum::Set(_, vs) = macro_rules_iter.next().unwrap() {
            vs.into_vec()
                .into_iter()
                .map(|v| Ok(MacroVar::from_owned_ident(scope, expect_ident(v)?)))
                .collect::<Result<HashSet<MacroVar>>>()?
        } else {
            unreachable!("Shouldn't be here")
        };
    } else {
        macro_rules_iter = macro_rules_data.into_iter();
        literals = HashSet::new()
    };

    let special_vars = SpecialVars { literals };
    let rules = macro_rules_iter
        .map(|rule_datum| lower_macro_rule_datum(scope, &special_vars, rule_datum))
        .collect::<Result<Vec<Rule>>>()?;

    Ok(Macro::new(special_vars, rules))
}

pub fn expand_macro(
    scope: &mut Scope,
    invocation_span: Span,
    mac: &Macro,
    arg_data: &[NsDatum],
) -> Result<NsDatum> {
    for rule in &mac.rules {
        let match_result = match_rule(scope, &mac.special_vars, rule, arg_data);

        if let Ok(match_data) = match_result {
            return Ok(expand_rule(
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
