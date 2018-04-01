use hir::scope::Scope;
use hir::ns::{Ident, NsDatum, NsId, NsIdAlloc};
use hir::macros::{MacroVar, MatchData, SpecialVars};
use hir::macros::checker::VarLinks;
use std::collections::HashMap;
use syntax::span::Span;

struct ExpandCursor<'a> {
    match_data: &'a MatchData,
    var_links: &'a VarLinks,
    subtemplate_idx: usize,
}

struct ExpandContext<'a> {
    ns_id_alloc: &'a mut NsIdAlloc,
    scope: &'a mut Scope,
    special_vars: &'a SpecialVars,
    ns_mapping: HashMap<NsId, NsId>,
}

impl<'a> ExpandContext<'a> {
    fn new(
        ns_id_alloc: &'a mut NsIdAlloc,
        scope: &'a mut Scope,
        special_vars: &'a SpecialVars,
    ) -> ExpandContext<'a> {
        ExpandContext {
            ns_id_alloc,
            scope,
            special_vars,
            ns_mapping: HashMap::new(),
        }
    }

    fn expand_ident(&mut self, cursor: &ExpandCursor, span: Span, ident: &Ident) -> NsDatum {
        let macro_var = MacroVar::from_ident(&self.scope, ident);

        if let Some(replacement) = cursor.match_data.vars.get(&macro_var) {
            return replacement.clone();
        }

        // TODO: Always allocate an NsId even if we never use it to get around the borrow checker
        let alloced_ns_id = self.ns_id_alloc.alloc();

        // Rescope this ident
        let old_ns_id = ident.ns_id();
        let new_ns_id = self.ns_mapping.entry(old_ns_id).or_insert(alloced_ns_id);

        let new_ident = ident.with_ns_id(*new_ns_id);
        self.scope.rebind(ident, &new_ident);

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

        cursor.subtemplate_idx = cursor.subtemplate_idx + 1;

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
    ) -> Vec<NsDatum> {
        let mut result: Vec<NsDatum> = vec![];

        while !templates.is_empty() {
            if self.special_vars
                .starts_with_zero_or_more(&self.scope, templates)
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

        result
    }

    fn expand_datum(&mut self, cursor: &mut ExpandCursor, template: &NsDatum) -> NsDatum {
        match template {
            &NsDatum::Ident(span, ref ident) => self.expand_ident(cursor, span, ident),
            &NsDatum::List(span, ref vs) => NsDatum::List(span, self.expand_slice(cursor, vs)),
            &NsDatum::Vec(span, ref vs) => NsDatum::Vec(span, self.expand_slice(cursor, vs)),
            &NsDatum::Set(span, ref vs) => NsDatum::Set(span, self.expand_slice(cursor, vs)),
            other => other.clone(),
        }
    }
}

pub fn expand_rule(
    ns_id_alloc: &mut NsIdAlloc,
    scope: &mut Scope,
    special_vars: &SpecialVars,
    match_data: MatchData,
    var_links: &VarLinks,
    template: &NsDatum,
) -> NsDatum {
    let mut mcx = ExpandContext::new(ns_id_alloc, scope, special_vars);

    let mut cursor = ExpandCursor {
        match_data: &match_data,
        var_links,
        subtemplate_idx: 0,
    };

    mcx.expand_datum(&mut cursor, template)
}
