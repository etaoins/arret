use hir::macros::checker::VarLinks;
use hir::macros::{MacroVar, MatchData, SpecialVars};
use hir::ns::{Ident, NsDatum, NsId};
use hir::scope::Scope;
use std::collections::HashMap;
use syntax::span::Span;

struct ExpandCursor<'data, 'links> {
    match_data: &'data MatchData,
    var_links: &'links VarLinks,
    subtemplate_idx: usize,
}

struct ExpandCtx<'scope, 'svars> {
    scope: &'scope mut Scope,
    special_vars: &'svars SpecialVars,
    ns_mapping: HashMap<NsId, NsId>,
}

impl<'scope, 'svars> ExpandCtx<'scope, 'svars> {
    fn new(
        scope: &'scope mut Scope,
        special_vars: &'svars SpecialVars,
    ) -> ExpandCtx<'scope, 'svars> {
        ExpandCtx {
            scope,
            special_vars,
            ns_mapping: HashMap::new(),
        }
    }

    fn expand_ident(&mut self, cursor: &ExpandCursor, span: Span, ident: &Ident) -> NsDatum {
        let macro_var = MacroVar::from_ident(self.scope, ident);

        if let Some(replacement) = cursor.match_data.vars.get(&macro_var) {
            return replacement.clone();
        }

        // Rescope this ident
        let old_ns_id = ident.ns_id();

        let scope = &mut self.scope;
        let new_ns_id = self
            .ns_mapping
            .entry(old_ns_id)
            .or_insert_with(|| scope.alloc_ns_id());

        let new_ident = ident.with_ns_id(*new_ns_id);
        // This should succeed because we should only map each ident once
        scope.rebind(span, ident, &new_ident).unwrap();

        NsDatum::Ident(span, new_ident)
    }

    fn expand_zero_or_more(
        &mut self,
        cursor: &mut ExpandCursor,
        template: &NsDatum,
    ) -> Vec<NsDatum> {
        // Find our subpattern index from our subtemplate index
        let subtemplate_idx = cursor.subtemplate_idx;
        let subvar_links = &cursor.var_links.subtemplates()[subtemplate_idx];
        let subpattern_idx = subvar_links.pattern_idx();
        let submatches = &cursor.match_data.subpatterns[subpattern_idx];

        cursor.subtemplate_idx += 1;

        submatches
            .iter()
            .map(|m| {
                // Build a new cursor pointing to our subpattern
                let mut subcursor = ExpandCursor {
                    match_data: m,
                    var_links: subvar_links,
                    subtemplate_idx: 0,
                };

                self.expand_datum(&mut subcursor, template)
            })
            .collect()
    }

    fn expand_slice(
        &mut self,
        cursor: &mut ExpandCursor,
        mut templates: &[NsDatum],
    ) -> Box<[NsDatum]> {
        let mut result: Vec<NsDatum> = vec![];

        while !templates.is_empty() {
            if self
                .special_vars
                .starts_with_zero_or_more(self.scope, templates)
            {
                let mut expanded = self.expand_zero_or_more(cursor, &templates[0]);
                result.append(&mut expanded);

                // Skip the ellipsis as well
                templates = &templates[2..];
            } else {
                let expanded = self.expand_datum(cursor, &templates[0]);
                result.push(expanded);

                templates = &templates[1..];
            }
        }

        result.into_boxed_slice()
    }

    fn expand_list(
        &mut self,
        cursor: &mut ExpandCursor,
        span: Span,
        templates: &[NsDatum],
    ) -> NsDatum {
        if self.special_vars.is_escaped_ellipsis(self.scope, templates) {
            templates[1].clone()
        } else {
            NsDatum::List(span, self.expand_slice(cursor, templates))
        }
    }

    fn expand_datum(&mut self, cursor: &mut ExpandCursor, template: &NsDatum) -> NsDatum {
        match template {
            NsDatum::Ident(span, ident) => self.expand_ident(cursor, *span, ident),
            NsDatum::List(span, vs) => self.expand_list(cursor, *span, vs),
            NsDatum::Vec(span, vs) => NsDatum::Vec(*span, self.expand_slice(cursor, vs)),
            NsDatum::Set(span, vs) => NsDatum::Set(*span, self.expand_slice(cursor, vs)),
            other => other.clone(),
        }
    }
}

pub fn expand_rule(
    scope: &mut Scope,
    special_vars: &SpecialVars,
    match_data: &MatchData,
    var_links: &VarLinks,
    template: &NsDatum,
) -> NsDatum {
    let mut mcx = ExpandCtx::new(scope, special_vars);

    let mut cursor = ExpandCursor {
        match_data,
        var_links,
        subtemplate_idx: 0,
    };

    mcx.expand_datum(&mut cursor, template)
}
