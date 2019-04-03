use std::collections::HashMap;

use syntax::span::Span;

use crate::hir::macros::linker::{TemplateIdent, VarLinks};
use crate::hir::macros::matcher::MatchData;
use crate::hir::macros::{get_escaped_ident, starts_with_zero_or_more, MacroId};
use crate::hir::ns::{Ident, NsDatum, NsId};
use crate::hir::scope::{Binding, Scope};

struct ExpandCursor<'data, 'links> {
    match_data: &'data MatchData<'data>,
    var_links: &'links VarLinks,

    ident_index: usize,
    subtemplate_index: usize,
}

struct ExpandCtx<'scope, 'parent> {
    scope: &'scope mut Scope<'parent>,
    ns_mapping: HashMap<NsId, NsId>,
}

impl<'scope, 'parent> ExpandCtx<'scope, 'parent> {
    fn new(scope: &'scope mut Scope<'parent>) -> Self {
        ExpandCtx {
            scope,
            ns_mapping: HashMap::new(),
        }
    }

    fn expand_ident(
        &mut self,
        self_mac: &MacroId,
        cursor: &mut ExpandCursor<'_, '_>,
        span: Span,
        ident: &Ident,
    ) -> NsDatum {
        let binding = if ident.name() != "_" {
            let template_ident = cursor.var_links.template_ident(cursor.ident_index);
            cursor.ident_index += 1;

            match template_ident {
                TemplateIdent::SubpatternVar(var_index) => {
                    return cursor.match_data.var(*var_index).clone();
                }
                TemplateIdent::SelfIdent => Some(Binding::Macro(self_mac.clone())),
                TemplateIdent::Bound(binding) => Some(binding.clone()),
                TemplateIdent::Unbound => None,
            }
        } else {
            None
        };

        // Re-scope this ident
        let old_ns_id = ident.ns_id();
        let scope = &mut self.scope;
        let new_ns_id = self
            .ns_mapping
            .entry(old_ns_id)
            .or_insert_with(|| scope.alloc_ns_id());

        let new_ident = ident.with_ns_id(*new_ns_id);

        if let Some(binding) = binding {
            scope.replace_binding(span, new_ident.clone(), binding);
        };

        NsDatum::Ident(span, new_ident)
    }

    fn expand_zero_or_more(
        &mut self,
        self_mac: &MacroId,
        cursor: &mut ExpandCursor<'_, '_>,
        template: &NsDatum,
    ) -> Vec<NsDatum> {
        // Find our subpattern index from our subtemplate index
        let subtemplate_index = cursor.subtemplate_index;
        let subvar_links = &cursor.var_links.subtemplates()[subtemplate_index];
        let subpattern_index = subvar_links.subpattern_index();
        let submatches = &cursor.match_data.subpattern(subpattern_index);

        cursor.subtemplate_index += 1;

        submatches
            .iter()
            .map(|m| {
                // Build a new cursor pointing to our subpattern
                let mut subcursor = ExpandCursor {
                    match_data: m,
                    var_links: subvar_links,

                    ident_index: 0,
                    subtemplate_index: 0,
                };

                self.expand_datum(self_mac, &mut subcursor, template)
            })
            .collect()
    }

    fn expand_slice(
        &mut self,
        self_mac: &MacroId,
        cursor: &mut ExpandCursor<'_, '_>,
        mut templates: &[NsDatum],
    ) -> Box<[NsDatum]> {
        let mut result: Vec<NsDatum> = vec![];

        while !templates.is_empty() {
            if starts_with_zero_or_more(templates) {
                let mut expanded = self.expand_zero_or_more(self_mac, cursor, &templates[0]);
                result.append(&mut expanded);

                // Skip the ellipsis as well
                templates = &templates[2..];
            } else {
                let expanded = self.expand_datum(self_mac, cursor, &templates[0]);
                result.push(expanded);

                templates = &templates[1..];
            }
        }

        result.into_boxed_slice()
    }

    fn expand_list(
        &mut self,
        self_mac: &MacroId,
        cursor: &mut ExpandCursor<'_, '_>,
        span: Span,
        templates: &[NsDatum],
    ) -> NsDatum {
        if let Some(ident) = get_escaped_ident(templates) {
            NsDatum::Ident(span, ident.clone())
        } else {
            NsDatum::List(span, self.expand_slice(self_mac, cursor, templates))
        }
    }

    fn expand_datum(
        &mut self,
        self_mac: &MacroId,
        cursor: &mut ExpandCursor<'_, '_>,
        template: &NsDatum,
    ) -> NsDatum {
        match template {
            NsDatum::Ident(span, ident) => self.expand_ident(self_mac, cursor, *span, ident),
            NsDatum::List(span, vs) => self.expand_list(self_mac, cursor, *span, vs),
            NsDatum::Vector(span, vs) => {
                NsDatum::Vector(*span, self.expand_slice(self_mac, cursor, vs))
            }
            NsDatum::Set(span, vs) => NsDatum::Set(*span, self.expand_slice(self_mac, cursor, vs)),
            other => other.clone(),
        }
    }
}

pub fn expand_rule<'scope, 'parent, 'data>(
    scope: &'scope mut Scope<'parent>,
    self_mac: &MacroId,
    match_data: &'data MatchData<'data>,
    var_links: &VarLinks,
    template: &NsDatum,
) -> NsDatum {
    let mut mcx = ExpandCtx::new(scope);

    let mut cursor = ExpandCursor {
        match_data,
        var_links,

        ident_index: 0,
        subtemplate_index: 0,
    };

    mcx.expand_datum(self_mac, &mut cursor, template)
}
