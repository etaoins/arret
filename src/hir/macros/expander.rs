use hir::scope::{Ident, NsId, NsIdAllocator, NsValue, Scope};
use hir::macros::{MacroVar, MatchData, SpecialVars};
use std::collections::HashMap;
use syntax::span::Span;

pub struct ExpandContext<'a> {
    ns_id_allocator: &'a mut NsIdAllocator,
    scope: Scope,
    special_vars: &'a SpecialVars,
    ns_mapping: HashMap<NsId, NsId>,
}

impl<'a> ExpandContext<'a> {
    pub fn new(
        ns_id_allocator: &'a mut NsIdAllocator,
        scope: &Scope,
        special_vars: &'a SpecialVars,
    ) -> ExpandContext<'a> {
        ExpandContext {
            ns_id_allocator,
            scope: Scope::new_child(scope),
            special_vars,
            ns_mapping: HashMap::new(),
        }
    }

    fn expand_ident(&mut self, match_data: &MatchData, span: Span, ident: &Ident) -> NsValue {
        let macro_var = MacroVar::from_ident(&self.scope, ident);

        if let Some(replacement) = match_data.vars.get(&macro_var) {
            return replacement.clone();
        }

        // TODO: Always allocate an NsId even if we never use it to get around the borrow checker
        let alloced_ns_id = self.ns_id_allocator.alloc();

        // Rescope this ident
        let old_ns_id = ident.ns_id();
        let new_ns_id = self.ns_mapping.entry(old_ns_id).or_insert(alloced_ns_id);

        let new_ident = ident.with_ns_id(*new_ns_id);
        self.scope.rebind(ident, &new_ident);

        NsValue::Ident(span, new_ident)
    }

    fn expand_zero_or_more(&mut self, match_data: &MatchData, template: &NsValue) -> Vec<NsValue> {
        // TODO: Find correct subpattern
        let matches = &match_data.subpatterns[0];
        println!("{:?}", matches);

        matches
            .iter()
            .map(|m| self.expand_datum(m, template))
            .collect()
    }

    fn expand_slice(&mut self, match_data: &MatchData, mut templates: &[NsValue]) -> Vec<NsValue> {
        let mut result: Vec<NsValue> = vec![];

        loop {
            if templates.is_empty() {
                break;
            }

            if let Some(&NsValue::Ident(_, ref next_ident)) = templates.get(1) {
                let next_var = MacroVar::from_ident(&self.scope, next_ident);

                if self.special_vars.is_zero_or_more(&next_var) {
                    let mut expanded = self.expand_zero_or_more(match_data, &templates[0]);
                    result.append(&mut expanded);

                    // Skip the ellipsis as well
                    templates = &templates[2..];
                    continue;
                }
            }

            let expanded = self.expand_datum(match_data, &templates[0]);
            result.push(expanded);

            templates = &templates[1..];
        }

        result
    }

    fn expand_datum(&mut self, match_data: &MatchData, template: &NsValue) -> NsValue {
        match template {
            &NsValue::Ident(span, ref ident) => self.expand_ident(match_data, span, ident),
            &NsValue::List(span, ref vs) => NsValue::List(span, self.expand_slice(match_data, vs)),
            &NsValue::Vector(span, ref vs) => {
                NsValue::Vector(span, self.expand_slice(match_data, vs))
            }
            other => other.clone(),
        }
    }

    pub fn expand_template(
        mut self,
        match_data: &MatchData,
        template: &NsValue,
    ) -> (Scope, NsValue) {
        let expanded_datum = self.expand_datum(match_data, template);
        (self.scope, expanded_datum)
    }
}
