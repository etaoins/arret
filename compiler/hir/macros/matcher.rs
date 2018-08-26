use std::result;

use crate::hir::macros::{MacroVar, MatchData, Rule, SpecialVars};
use crate::hir::ns::{Ident, NsDatum};
use crate::hir::scope::Scope;

struct MatchCtx<'scope, 'data, 'svars> {
    scope: &'scope Scope,
    special_vars: &'svars SpecialVars,
    match_data: MatchData<'data>,
}

type Result<T> = result::Result<T, ()>;
type MatchVisitResult = result::Result<(), ()>;

impl<'scope, 'data, 'svars> MatchCtx<'scope, 'data, 'svars> {
    fn new(
        scope: &'scope Scope,
        special_vars: &'svars SpecialVars,
    ) -> MatchCtx<'scope, 'data, 'svars> {
        MatchCtx {
            scope,
            special_vars,
            match_data: MatchData::new(),
        }
    }

    fn visit_ident(&mut self, pattern_ident: &Ident, arg: &'data NsDatum) -> MatchVisitResult {
        let pattern_var = MacroVar::from_ident(self.scope, pattern_ident);

        if self.special_vars.is_wildcard(&pattern_var) {
            // This is a wildcard; just discard
            Ok(())
        } else if self.special_vars.is_literal(&pattern_var) {
            // The arg must be the exact same pattern variable
            if let NsDatum::Ident(_, arg_ident) = arg {
                if pattern_var == MacroVar::from_ident(self.scope, arg_ident) {
                    return Ok(());
                }
            }

            Err(())
        } else {
            self.match_data.vars.insert(pattern_var, arg);
            Ok(())
        }
    }

    // TODO: Floats, maps
    fn visit_datum(&mut self, pattern: &NsDatum, arg: &'data NsDatum) -> MatchVisitResult {
        match (pattern, arg) {
            (NsDatum::Ident(_, pattern_ident), arg) => self.visit_ident(pattern_ident, arg),
            (NsDatum::List(_, pvs), NsDatum::List(_, avs)) => self.visit_slice(pvs, avs),
            (NsDatum::Vector(_, pvs), NsDatum::Vector(_, avs)) => self.visit_slice(pvs, avs),
            (NsDatum::Set(_, pvs), NsDatum::Set(_, avs)) => self.visit_slice(pvs, avs),
            (NsDatum::Bool(_, pv), NsDatum::Bool(_, av)) if pv == av => Ok(()),
            (NsDatum::Int(_, pv), NsDatum::Int(_, av)) if pv == av => Ok(()),
            (NsDatum::Char(_, pv), NsDatum::Char(_, av)) if pv == av => Ok(()),
            (NsDatum::Str(_, pv), NsDatum::Str(_, av)) if pv == av => Ok(()),
            _ => Err(()),
        }
    }

    fn visit_zero_or_more(
        &mut self,
        pattern: &NsDatum,
        args: &'data [NsDatum],
    ) -> MatchVisitResult {
        let submatch_data = args
            .iter()
            .map(|arg| {
                let mut subcontext = MatchCtx {
                    scope: self.scope,
                    special_vars: self.special_vars,
                    match_data: MatchData::new(),
                };

                subcontext.visit_datum(pattern, arg)?;
                Ok(subcontext.match_data)
            }).collect::<result::Result<Vec<MatchData<'data>>, ()>>()?;

        self.match_data.subpatterns.push(submatch_data);
        Ok(())
    }

    fn visit_slice(
        &mut self,
        mut patterns: &[NsDatum],
        mut args: &'data [NsDatum],
    ) -> MatchVisitResult {
        loop {
            if self
                .special_vars
                .starts_with_zero_or_more(self.scope, patterns)
            {
                let rest_patterns_len = patterns.len() - 2;

                if args.len() < rest_patterns_len {
                    // Cannot match
                    return Err(());
                }

                let (zero_or_more_args, rest_args) = args.split_at(args.len() - rest_patterns_len);
                self.visit_zero_or_more(&patterns[0], zero_or_more_args)?;

                patterns = &patterns[2..];
                args = rest_args;
            } else {
                let (pattern, arg) = match (patterns.first(), args.first()) {
                    (Some(pattern), Some(arg)) => (pattern, arg),
                    (None, None) => {
                        // Patterns and args ran out at the same time
                        return Ok(());
                    }
                    _ => {
                        // Mismatched lengths
                        return Err(());
                    }
                };

                self.visit_datum(pattern, arg)?;

                patterns = &patterns[1..];
                args = &args[1..];
            }
        }
    }

    fn visit_rule(mut self, rule: &Rule, arg_data: &'data [NsDatum]) -> Result<MatchData<'data>> {
        self.visit_slice(rule.pattern.as_slice(), arg_data)?;
        Ok(self.match_data)
    }
}

pub fn match_rule<'data>(
    scope: &Scope,
    special_vars: &SpecialVars,
    rule: &Rule,
    arg_data: &'data [NsDatum],
) -> Result<MatchData<'data>> {
    let mcx = MatchCtx::new(scope, special_vars);
    mcx.visit_rule(rule, arg_data)
}
