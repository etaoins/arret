use std::collections::HashSet;

use hir::macros::{MacroVar, SpecialVars};
use hir::error::{Error, Result};
use hir::scope::{Ident, NsValue, Scope};
use syntax::span::{Span, EMPTY_SPAN};

#[derive(PartialEq, Debug)]
pub struct VarLinks {
    pattern_idx: usize,
    subtemplates: Vec<VarLinks>,
}

impl VarLinks {
    pub fn pattern_idx(&self) -> usize {
        self.pattern_idx
    }

    pub fn subtemplates(&self) -> &Vec<VarLinks> {
        &self.subtemplates
    }
}

#[derive(Debug)]
struct FoundVars {
    span: Span,
    vars: HashSet<MacroVar>,
    subs: Vec<FoundVars>,
}

impl FoundVars {
    fn new(span: Span) -> FoundVars {
        FoundVars {
            span,
            vars: HashSet::new(),
            subs: vec![],
        }
    }
}

struct FindVarsContext<'a> {
    scope: &'a Scope,
    special_vars: &'a SpecialVars,
}

impl<'a> FindVarsContext<'a> {
    fn new(scope: &'a Scope, special_vars: &'a SpecialVars) -> FindVarsContext<'a> {
        FindVarsContext {
            scope,
            special_vars,
        }
    }

    fn visit_ident(&mut self, pattern_vars: &mut FoundVars, ident: &Ident) {
        let macro_var = MacroVar::from_ident(&self.scope, ident);

        if self.special_vars.is_literal(&macro_var) || self.special_vars.is_wildcard(&macro_var) {
            // Not a variable
            return;
        }

        pattern_vars.vars.insert(macro_var);
    }

    fn visit_zero_or_more(&mut self, pattern_vars: &mut FoundVars, pattern: &NsValue) {
        let mut sub_vars = FoundVars::new(pattern.span());
        self.visit_datum(&mut sub_vars, pattern);

        pattern_vars.subs.push(sub_vars);
    }

    fn visit_datum(&mut self, pattern_vars: &mut FoundVars, pattern: &NsValue) {
        match pattern {
            &NsValue::Ident(_, ref ident) => self.visit_ident(pattern_vars, ident),
            &NsValue::List(_, ref vs) => self.visit_slice(pattern_vars, vs),
            &NsValue::Vector(_, ref vs) => self.visit_slice(pattern_vars, vs),
            _ => {
                // Can't contain a pattern var
            }
        };
    }

    fn visit_slice(&mut self, pattern_vars: &mut FoundVars, mut patterns: &[NsValue]) {
        while !patterns.is_empty() {
            if self.special_vars
                .starts_with_zero_or_more(&self.scope, patterns)
            {
                self.visit_zero_or_more(pattern_vars, &patterns[0]);
                patterns = &patterns[2..];
            } else {
                self.visit_datum(pattern_vars, &patterns[0]);
                patterns = &patterns[1..];
            }
        }
    }
}

fn link_found_vars(
    pattern_idx: usize,
    pattern_vars: &FoundVars,
    template_vars: &FoundVars,
) -> Result<VarLinks> {
    let subtemplates = template_vars
        .subs
        .iter()
        .map(|subtemplate_vars| {
            if subtemplate_vars.vars.is_empty() {
                return Err(Error::IllegalArg(
                    template_vars.span,
                    "Subtemplate does not include any macro variables".to_owned(),
                ));
            }

            // Find possible indices for subpatterns in our pattern
            let possible_indices = pattern_vars
                .subs
                .iter()
                .enumerate()
                .filter(|&(_, pv)| !pv.vars.is_disjoint(&subtemplate_vars.vars))
                .collect::<Vec<(usize, &FoundVars)>>();

            let (pattern_idx, subpattern_vars) = if possible_indices.len() == 0 {
                return Err(Error::IllegalArg(
                    template_vars.span,
                    "Subtemplate does not reference macro variables from any subpattern".to_owned(),
                ));
            } else if possible_indices.len() > 1 {
                return Err(Error::IllegalArg(
                    template_vars.span,
                    "Subtemplate references macro variables from multiple subpatterns".to_owned(),
                ));
            } else {
                possible_indices[0]
            };

            // Iterate over our subpatterns
            link_found_vars(pattern_idx, subpattern_vars, subtemplate_vars)
        })
        .collect::<Result<Vec<VarLinks>>>()?;

    Ok(VarLinks {
        pattern_idx,
        subtemplates,
    })
}

pub fn link_vars(
    scope: &Scope,
    special_vars: &SpecialVars,
    patterns: &[NsValue],
    template: &NsValue,
) -> Result<VarLinks> {
    let mut fvcx = FindVarsContext::new(scope, special_vars);

    // We don't need to report the root span for the pattern
    let mut pattern_vars = FoundVars::new(EMPTY_SPAN);
    fvcx.visit_slice(&mut pattern_vars, patterns);

    let mut template_vars = FoundVars::new(template.span());
    fvcx.visit_datum(&mut template_vars, template);

    link_found_vars(0, &pattern_vars, &template_vars)
}
