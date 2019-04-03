use std::collections::HashMap;
use std::result;

use syntax::span::{Span, EMPTY_SPAN};

use crate::hir::error::{Error, ErrorKind, Result};
use crate::hir::macros::{get_escaped_ident, starts_with_zero_or_more};
use crate::hir::ns::{Ident, NsDatum};
use crate::hir::scope::{Binding, Scope};

/// Indicates the meaning of a given ident in the template
#[derive(PartialEq, Debug)]
pub enum TemplateIdent {
    /// Refers to a template variable with a given index
    SubpatternVar(usize),
    /// Ident of the macro being expanded
    SelfIdent,
    /// Ident bound to the given binding
    Bound(Binding),
    /// Unbound ident
    Unbound,
}

/// Precomputed links from variables in the template to the pattern
#[derive(PartialEq, Debug)]
pub struct VarLinks {
    subpattern_index: usize,
    template_idents: Box<[TemplateIdent]>,
    subtemplates: Box<[VarLinks]>,
}

impl VarLinks {
    /// Index of the subpattern for this subtemplate
    pub fn subpattern_index(&self) -> usize {
        self.subpattern_index
    }

    /// Returns the template ident for the given index
    pub fn template_ident(&self, i: usize) -> &TemplateIdent {
        &self.template_idents[i]
    }

    /// Links for our subtemplates in visit order
    pub fn subtemplates(&self) -> &[VarLinks] {
        &self.subtemplates
    }
}

#[derive(Debug)]
struct FoundVars<'data> {
    span: Span,
    idents: Vec<&'data Ident>,
    subs: Vec<FoundVars<'data>>,
}

impl<'data> FoundVars<'data> {
    fn new(span: Span) -> Self {
        FoundVars {
            span,
            idents: vec![],
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

struct FindVarsCtx<'data> {
    input_type: FindVarsInputType,
    var_spans: Option<HashMap<&'data Ident, Span>>,
}

type FindVarsResult = result::Result<(), Error>;

impl<'data> FindVarsCtx<'data> {
    fn new(input_type: FindVarsInputType) -> Self {
        let var_spans = if input_type == FindVarsInputType::Template {
            // Duplicate vars are allowed in the template as they must all resolve to the same
            // value.
            None
        } else {
            // This tracks the name of variables and where they were first used (for error
            // reporting)
            Some(HashMap::<&'data Ident, Span>::new())
        };

        FindVarsCtx {
            input_type,
            var_spans,
        }
    }

    fn visit_ident(
        &mut self,
        pattern_vars: &mut FoundVars<'data>,
        span: Span,
        ident: &'data Ident,
    ) -> FindVarsResult {
        if ident.name() == "_" {
            // This is a wildcard
            return Ok(());
        }

        if ident.name() == "..." {
            return Err(Error::new(
                span,
                ErrorKind::IllegalArg("ellipsis can only be used as part of a zero or more match"),
            ));
        }

        if let Some(ref mut var_spans) = self.var_spans {
            if let Some(old_span) = var_spans.insert(ident, span) {
                return Err(Error::new(
                    span,
                    ErrorKind::DuplicateDef(old_span, ident.name().into()),
                ));
            }
        }

        pattern_vars.idents.push(ident);
        Ok(())
    }

    fn visit_zero_or_more(
        &mut self,
        pattern_vars: &mut FoundVars<'data>,
        pattern: &'data NsDatum,
    ) -> FindVarsResult {
        let mut sub_vars = FoundVars::new(pattern.span());
        self.visit_datum(&mut sub_vars, pattern)?;

        pattern_vars.subs.push(sub_vars);
        Ok(())
    }

    fn visit_datum(
        &mut self,
        pattern_vars: &mut FoundVars<'data>,
        pattern: &'data NsDatum,
    ) -> FindVarsResult {
        match pattern {
            NsDatum::Ident(span, ident) => self.visit_ident(pattern_vars, *span, ident),
            NsDatum::List(_, vs) => self.visit_list(pattern_vars, vs),
            NsDatum::Vector(_, vs) => self.visit_seq(pattern_vars, vs),
            NsDatum::Set(span, vs) => self.visit_set(pattern_vars, *span, vs),
            _ => {
                // Can't contain a pattern var
                Ok(())
            }
        }
    }

    fn visit_seq(
        &mut self,
        pattern_vars: &mut FoundVars<'data>,
        mut patterns: &'data [NsDatum],
    ) -> FindVarsResult {
        let mut zero_or_more_span: Option<Span> = None;

        while !patterns.is_empty() {
            if starts_with_zero_or_more(patterns) {
                let pattern = &patterns[0];

                // Make sure we don't have multiple zero or more matches in the same slice
                if self.input_type == FindVarsInputType::Pattern {
                    if let Some(old_span) = zero_or_more_span.replace(pattern.span()) {
                        // We've already had a zero-or-more match
                        return Err(Error::new(
                            pattern.span(),
                            ErrorKind::MultipleZeroOrMoreMatch(old_span),
                        ));
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

    fn visit_list(
        &mut self,
        pattern_vars: &mut FoundVars<'data>,
        patterns: &'data [NsDatum],
    ) -> FindVarsResult {
        if get_escaped_ident(patterns).is_some() {
            // This isn't actually a list
            Ok(())
        } else {
            self.visit_seq(pattern_vars, patterns)
        }
    }

    fn visit_set(
        &mut self,
        pattern_vars: &mut FoundVars<'data>,
        span: Span,
        patterns: &'data [NsDatum],
    ) -> FindVarsResult {
        if self.input_type == FindVarsInputType::Template {
            // Sets are expanded the same way as any other sequence
            return self.visit_seq(pattern_vars, patterns);
        }

        match patterns.len() {
            0 => Ok(()),
            2 if starts_with_zero_or_more(patterns) => {
                self.visit_zero_or_more(pattern_vars, &patterns[0])
            }
            _ => Err(Error::new(
                span,
                ErrorKind::IllegalArg("set patterns must either be empty or a zero or more match"),
            )),
        }
    }
}

fn link_template_ident(
    scope: &Scope,
    self_ident: &Ident,
    template_ident: &Ident,
    pattern_idents: &[&Ident],
) -> TemplateIdent {
    // First, see if this corresponds to a var in the pattern
    if let Some(subpattern_index) = pattern_idents
        .iter()
        .position(|pattern_ident| *pattern_ident == template_ident)
    {
        TemplateIdent::SubpatternVar(subpattern_index)
    } else if template_ident == self_ident {
        TemplateIdent::SelfIdent
    } else if let Some(binding) = scope.get(template_ident) {
        TemplateIdent::Bound(binding.clone())
    } else {
        TemplateIdent::Unbound
    }
}

fn link_found_vars(
    scope: &Scope,
    self_ident: &Ident,
    subpattern_index: usize,
    pattern_vars: &FoundVars<'_>,
    template_vars: &FoundVars<'_>,
) -> Result<VarLinks> {
    let template_idents = template_vars
        .idents
        .iter()
        .map(|template_ident| {
            link_template_ident(scope, self_ident, template_ident, &pattern_vars.idents)
        })
        .collect();

    let subtemplates = template_vars
        .subs
        .iter()
        .map(|subtemplate_vars| {
            if subtemplate_vars.idents.is_empty() {
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
                .filter(|(_, subpattern_vars)| {
                    subpattern_vars
                        .idents
                        .iter()
                        .any(|subpattern_var| subtemplate_vars.idents.contains(subpattern_var))
                })
                .collect::<Vec<(usize, &FoundVars<'_>)>>();

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
            let (pattern_index, subpattern_vars) = possible_indices[0];
            link_found_vars(
                scope,
                self_ident,
                pattern_index,
                subpattern_vars,
                subtemplate_vars,
            )
        })
        .collect::<Result<Box<[VarLinks]>>>()?;

    Ok(VarLinks {
        subpattern_index,
        template_idents,
        subtemplates,
    })
}

pub fn link_rule_vars(
    scope: &Scope,
    self_ident: &Ident,
    patterns: &[NsDatum],
    template: &NsDatum,
) -> Result<VarLinks> {
    let mut fpvcx = FindVarsCtx::new(FindVarsInputType::Pattern);

    // We don't need to report the root span for the pattern
    let mut pattern_vars = FoundVars::new(EMPTY_SPAN);
    fpvcx.visit_seq(&mut pattern_vars, patterns)?;

    let mut ftvcx = FindVarsCtx::new(FindVarsInputType::Template);
    let mut template_vars = FoundVars::new(template.span());
    ftvcx.visit_datum(&mut template_vars, template)?;

    link_found_vars(scope, self_ident, 0, &pattern_vars, &template_vars)
}
