use std::result;

use hir::scope::{Ident, NsDatum, Scope};
use hir::macros::{MacroVar, MatchData, Rule, SpecialVars};

struct MatchContext<'a> {
    scope: &'a Scope,
    special_vars: &'a SpecialVars,
    match_data: MatchData,
}

type Result<T> = result::Result<T, ()>;
type MatchVisitResult = result::Result<(), ()>;

impl<'a> MatchContext<'a> {
    fn new(scope: &'a Scope, special_vars: &'a SpecialVars) -> MatchContext<'a> {
        MatchContext {
            scope,
            special_vars,
            match_data: MatchData::new(),
        }
    }

    fn visit_ident(&mut self, pattern_ident: &Ident, arg: &NsDatum) -> MatchVisitResult {
        let pattern_var = MacroVar::from_ident(self.scope, pattern_ident);

        if self.special_vars.is_wildcard(&pattern_var) {
            // This is a wildcard; just discard
            Ok(())
        } else if self.special_vars.is_literal(&pattern_var) {
            // The arg must be the exact same pattern variable
            if let &NsDatum::Ident(_, ref arg_ident) = arg {
                if pattern_var == MacroVar::from_ident(self.scope, arg_ident) {
                    return Ok(());
                }
            }

            Err(())
        } else {
            self.match_data.vars.insert(pattern_var, arg.clone());
            Ok(())
        }
    }

    // TODO: Floats, maps
    fn visit_datum(&mut self, pattern: &NsDatum, arg: &NsDatum) -> MatchVisitResult {
        match (pattern, arg) {
            (&NsDatum::Ident(_, ref pattern_ident), arg) => self.visit_ident(pattern_ident, arg),
            (&NsDatum::List(_, ref pvs), &NsDatum::List(_, ref avs)) => self.visit_slice(pvs, avs),
            (&NsDatum::Vec(_, ref pvs), &NsDatum::Vec(_, ref avs)) => self.visit_slice(pvs, avs),
            (&NsDatum::Set(_, ref pvs), &NsDatum::Set(_, ref avs)) => self.visit_slice(pvs, avs),
            (&NsDatum::Bool(_, pv), &NsDatum::Bool(_, av)) if pv == av => Ok(()),
            (&NsDatum::Int(_, pv), &NsDatum::Int(_, av)) if pv == av => Ok(()),
            (&NsDatum::Char(_, pv), &NsDatum::Char(_, av)) if pv == av => Ok(()),
            (&NsDatum::Str(_, ref pv), &NsDatum::Str(_, ref av)) if pv == av => Ok(()),
            _ => Err(()),
        }
    }

    fn visit_zero_or_more(&mut self, pattern: &NsDatum, args: &[NsDatum]) -> MatchVisitResult {
        let submatch_data = args.iter()
            .map(|arg| {
                let mut subcontext = MatchContext {
                    scope: self.scope,
                    special_vars: self.special_vars,
                    match_data: MatchData::new(),
                };

                subcontext.visit_datum(pattern, arg)?;
                Ok(subcontext.match_data)
            })
            .collect::<result::Result<Vec<MatchData>, ()>>()?;

        self.match_data.subpatterns.push(submatch_data);
        Ok(())
    }

    fn visit_slice(&mut self, mut patterns: &[NsDatum], mut args: &[NsDatum]) -> MatchVisitResult {
        loop {
            if self.special_vars
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

    fn visit_rule(mut self, rule: &Rule, arg_data: &[NsDatum]) -> Result<MatchData> {
        self.visit_slice(rule.pattern.as_slice(), arg_data)?;
        Ok(self.match_data)
    }
}

pub fn match_rule(
    scope: &Scope,
    special_vars: &SpecialVars,
    rule: &Rule,
    arg_data: &[NsDatum],
) -> Result<MatchData> {
    let mcx = MatchContext::new(scope, special_vars);
    mcx.visit_rule(rule, arg_data)
}
