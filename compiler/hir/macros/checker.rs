use std::collections::{HashMap, HashSet};
use std::result;

use hir::error::{Error, ErrorKind, Result};
use hir::macros::{MacroVar, SpecialVars};
use hir::ns::{Ident, NsDatum};
use hir::scope::Scope;
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

/// Tracks which type of input is being provided to `FindVarsCtx`
#[derive(Clone, Copy, PartialEq)]
enum FindVarsInputType {
    Pattern,
    Template,
}

struct FindVarsCtx<'scope, 'svars> {
    scope: &'scope Scope,
    special_vars: &'svars SpecialVars,
    input_type: FindVarsInputType,
    unbound_var_spans: Option<HashMap<Box<str>, Span>>,
}

type FindVarsResult = result::Result<(), Error>;

impl<'scope, 'svars> FindVarsCtx<'scope, 'svars> {
    fn new(
        scope: &'scope Scope,
        special_vars: &'svars SpecialVars,
        input_type: FindVarsInputType,
    ) -> FindVarsCtx<'scope, 'svars> {
        let unbound_var_spans = if input_type == FindVarsInputType::Template {
            // Duplicate bound vars are allowed in the template as they must all resolve to the
            // same value.
            None
        } else {
            // This tracks the name of unbound variables and where they were first used (for error
            // reporting)
            Some(HashMap::<Box<str>, Span>::new())
        };

        FindVarsCtx {
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
        let macro_var = MacroVar::from_ident(self.scope, ident);

        if self.special_vars.is_literal(&macro_var) || self.special_vars.is_wildcard(&macro_var) {
            // Not a variable
            return Ok(());
        }

        if self.special_vars.is_ellipsis(&macro_var) {
            return Err(Error::new(
                span,
                ErrorKind::IllegalArg("ellipsis can only be used as part of a zero or more match"),
            ));
        }

        if let Some(ref mut unbound_var_spans) = self.unbound_var_spans {
            if let MacroVar::Unbound(ref name) = macro_var {
                if let Some(old_span) = unbound_var_spans.insert(name.clone(), span) {
                    return Err(Error::new(span, ErrorKind::DuplicateDef(old_span)));
                }
            }
        }

        pattern_vars.vars.insert(macro_var);
        Ok(())
    }

    fn visit_zero_or_more(
        &mut self,
        pattern_vars: &mut FoundVars,
        pattern: &NsDatum,
    ) -> FindVarsResult {
        let mut sub_vars = FoundVars::new(pattern.span());
        self.visit_datum(&mut sub_vars, pattern)?;

        pattern_vars.subs.push(sub_vars);
        Ok(())
    }

    fn visit_datum(&mut self, pattern_vars: &mut FoundVars, pattern: &NsDatum) -> FindVarsResult {
        match pattern {
            NsDatum::Ident(span, ident) => self.visit_ident(pattern_vars, *span, ident),
            NsDatum::List(_, vs) => self.visit_list(pattern_vars, vs),
            NsDatum::Vec(_, vs) => self.visit_seq(pattern_vars, vs),
            NsDatum::Set(span, vs) => self.visit_set(pattern_vars, *span, vs),
            _ => {
                // Can't contain a pattern var
                Ok(())
            }
        }
    }

    fn visit_seq(
        &mut self,
        pattern_vars: &mut FoundVars,
        mut patterns: &[NsDatum],
    ) -> FindVarsResult {
        let mut zero_or_more_span: Option<Span> = None;

        while !patterns.is_empty() {
            if self
                .special_vars
                .starts_with_zero_or_more(self.scope, patterns)
            {
                let pattern = &patterns[0];

                // Make sure we don't have multiple zero or more matches in the same slice
                if self.input_type == FindVarsInputType::Pattern {
                    match zero_or_more_span {
                        Some(old_span) => {
                            // We've already had a zero-or-more match!
                            return Err(Error::new(
                                pattern.span(),
                                ErrorKind::MultipleZeroOrMoreMatch(old_span),
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

    fn visit_list(&mut self, pattern_vars: &mut FoundVars, patterns: &[NsDatum]) -> FindVarsResult {
        if self.input_type == FindVarsInputType::Template
            && self.special_vars.is_escaped_ellipsis(self.scope, patterns)
        {
            Ok(())
        } else {
            self.visit_seq(pattern_vars, patterns)
        }
    }

    fn visit_set(
        &mut self,
        pattern_vars: &mut FoundVars,
        span: Span,
        patterns: &[NsDatum],
    ) -> FindVarsResult {
        if self.input_type == FindVarsInputType::Template {
            // Sets are expanded exactly as seq
            return self.visit_seq(pattern_vars, patterns);
        }

        match patterns.len() {
            0 => Ok(()),
            2 if self
                .special_vars
                .starts_with_zero_or_more(self.scope, patterns) =>
            {
                self.visit_zero_or_more(pattern_vars, &patterns[0])
            }
            _ => Err(Error::new(
                span,
                ErrorKind::IllegalArg("set patterns must either be empty or a zero or more match"),
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
                return Err(Error::new(
                    template_vars.span,
                    ErrorKind::IllegalArg("subtemplate does not include any macro variables"),
                ));
            }

            // Find possible indices for subpatterns in our pattern
            let possible_indices = pattern_vars
                .subs
                .iter()
                .enumerate()
                .filter(|(_, pv)| !pv.vars.is_disjoint(&subtemplate_vars.vars))
                .collect::<Vec<(usize, &FoundVars)>>();

            if possible_indices.is_empty() {
                return Err(Error::new(
                    template_vars.span,
                    ErrorKind::IllegalArg(
                        "subtemplate does not reference macro variables from any subpattern",
                    ),
                ));
            } else if possible_indices.len() > 1 {
                return Err(Error::new(
                    template_vars.span,
                    ErrorKind::IllegalArg(
                        "subtemplate references macro variables from multiple subpatterns",
                    ),
                ));
            }

            // Iterate over our subpatterns
            let (pattern_idx, subpattern_vars) = possible_indices[0];
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
    patterns: &[NsDatum],
    template: &NsDatum,
) -> Result<VarLinks> {
    let mut fpvcx = FindVarsCtx::new(scope, special_vars, FindVarsInputType::Pattern);
    // We don't need to report the root span for the pattern
    let mut pattern_vars = FoundVars::new(EMPTY_SPAN);
    fpvcx.visit_seq(&mut pattern_vars, patterns)?;

    let mut ftvcx = FindVarsCtx::new(scope, special_vars, FindVarsInputType::Template);
    let mut template_vars = FoundVars::new(template.span());
    ftvcx.visit_datum(&mut template_vars, template)?;

    link_found_vars(scope, 0, &pattern_vars, &template_vars)
}
