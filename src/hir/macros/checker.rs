use std::collections::{HashMap, HashSet};
use std::result;

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

/// Tracks which type of input is being provided to FindVarsContext
#[derive(Clone, Copy, PartialEq)]
enum FindVarsInputType {
    Pattern,
    Template,
}

struct FindVarsContext<'a> {
    scope: &'a Scope,
    special_vars: &'a SpecialVars,
    input_type: FindVarsInputType,
    unbound_var_spans: Option<HashMap<String, Span>>,
}

type FindVarsResult = result::Result<(), Error>;

impl<'a> FindVarsContext<'a> {
    fn new(
        scope: &'a Scope,
        special_vars: &'a SpecialVars,
        input_type: FindVarsInputType,
    ) -> FindVarsContext<'a> {
        let unbound_var_spans = if input_type == FindVarsInputType::Template {
            // Duplicate bound vars are allowed in the template as they must all resolve to the
            // same value.
            None
        } else {
            // This tracks the name of unbound variables and where they were first used (for error
            // reporting)
            Some(HashMap::<String, Span>::new())
        };

        FindVarsContext {
            scope,
            special_vars,
            input_type,
            unbound_var_spans,
        }
    }

    fn visit_ident(
        &mut self,
        pattern_vars: &mut FoundVars,
        span: Span,
        ident: &Ident,
    ) -> FindVarsResult {
        let macro_var = MacroVar::from_ident(&self.scope, ident);

        if self.special_vars.is_literal(&macro_var) || self.special_vars.is_wildcard(&macro_var) {
            // Not a variable
            return Ok(());
        }

        if let Some(ref mut unbound_var_spans) = self.unbound_var_spans {
            if let MacroVar::Unbound(ref name) = macro_var {
                if let Some(old_span) = unbound_var_spans.insert(name.clone(), span) {
                    return Err(Error::DuplicateMacroVar(
                        self.scope.span_to_error_loc(span),
                        name.clone(),
                        old_span,
                    ));
                }
            }
        }

        pattern_vars.vars.insert(macro_var);
        Ok(())
    }

    fn visit_zero_or_more(
        &mut self,
        pattern_vars: &mut FoundVars,
        pattern: &NsValue,
    ) -> FindVarsResult {
        let mut sub_vars = FoundVars::new(pattern.span());
        self.visit_datum(&mut sub_vars, pattern)?;

        pattern_vars.subs.push(sub_vars);
        Ok(())
    }

    fn visit_datum(&mut self, pattern_vars: &mut FoundVars, pattern: &NsValue) -> FindVarsResult {
        match pattern {
            &NsValue::Ident(span, ref ident) => self.visit_ident(pattern_vars, span, ident),
            &NsValue::List(_, ref vs) => self.visit_seq(pattern_vars, vs),
            &NsValue::Vec(_, ref vs) => self.visit_seq(pattern_vars, vs),
            &NsValue::Set(span, ref vs) => self.visit_set(pattern_vars, span, vs),
            _ => {
                // Can't contain a pattern var
                Ok(())
            }
        }
    }

    fn visit_seq(
        &mut self,
        pattern_vars: &mut FoundVars,
        mut patterns: &[NsValue],
    ) -> FindVarsResult {
        let mut zero_or_more_span: Option<Span> = None;

        while !patterns.is_empty() {
            if self.special_vars
                .starts_with_zero_or_more(&self.scope, patterns)
            {
                let pattern = &patterns[0];

                // Make sure we don't have multiple zero or more matches in the same slice
                if self.input_type == FindVarsInputType::Pattern {
                    match zero_or_more_span {
                        Some(old_span) => {
                            // We've already had a zero-or-more match!
                            return Err(Error::MultipleZeroOrMoreMatch(
                                self.scope.span_to_error_loc(pattern.span()),
                                old_span,
                            ));
                        }
                        None => {
                            zero_or_more_span = Some(pattern.span());
                        }
                    }
                }

                self.visit_zero_or_more(pattern_vars, pattern)?;
                patterns = &patterns[2..];
            } else {
                self.visit_datum(pattern_vars, &patterns[0])?;
                patterns = &patterns[1..];
            }
        }

        Ok(())
    }

    fn visit_set(
        &mut self,
        pattern_vars: &mut FoundVars,
        span: Span,
        patterns: &[NsValue],
    ) -> FindVarsResult {
        if self.input_type == FindVarsInputType::Template {
            // Sets are expanded exactly as seq
            return self.visit_seq(pattern_vars, patterns);
        }

        match patterns.len() {
            0 => Ok(()),
            2 if self.special_vars
                .starts_with_zero_or_more(&self.scope, patterns) =>
            {
                self.visit_zero_or_more(pattern_vars, &patterns[0])
            }
            _ => Err(Error::IllegalArg(
                self.scope.span_to_error_loc(span),
                "Set patterns must either be empty or a zero or more match".to_owned(),
            )),
        }
    }
}

fn link_found_vars(
    scope: &Scope,
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
                    scope.span_to_error_loc(template_vars.span),
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
                    scope.span_to_error_loc(template_vars.span),
                    "Subtemplate does not reference macro variables from any subpattern".to_owned(),
                ));
            } else if possible_indices.len() > 1 {
                return Err(Error::IllegalArg(
                    scope.span_to_error_loc(template_vars.span),
                    "Subtemplate references macro variables from multiple subpatterns".to_owned(),
                ));
            } else {
                possible_indices[0]
            };

            // Iterate over our subpatterns
            link_found_vars(scope, pattern_idx, subpattern_vars, subtemplate_vars)
        })
        .collect::<Result<Vec<VarLinks>>>()?;

    Ok(VarLinks {
        pattern_idx,
        subtemplates,
    })
}

pub fn check_rule(
    scope: &Scope,
    special_vars: &SpecialVars,
    patterns: &[NsValue],
    template: &NsValue,
) -> Result<VarLinks> {
    let mut fpvcx = FindVarsContext::new(scope, special_vars, FindVarsInputType::Pattern);
    // We don't need to report the root span for the pattern
    let mut pattern_vars = FoundVars::new(EMPTY_SPAN);
    fpvcx.visit_seq(&mut pattern_vars, patterns)?;

    let mut ftvcx = FindVarsContext::new(scope, special_vars, FindVarsInputType::Template);
    let mut template_vars = FoundVars::new(template.span());
    ftvcx.visit_datum(&mut template_vars, template)?;

    link_found_vars(scope, 0, &pattern_vars, &template_vars)
}
