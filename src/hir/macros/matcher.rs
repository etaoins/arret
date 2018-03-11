use std::result;

use hir::scope::{Ident, NsValue, Scope};
use hir::macros::{MacroVar, MatchData, Rule, SpecialVars};

pub struct MatchContext<'a> {
    scope: &'a Scope,
    special_vars: &'a SpecialVars,
    match_data: MatchData,
}

type MatchVisitResult = result::Result<(), ()>;

impl<'a> MatchContext<'a> {
    pub fn new(scope: &'a Scope, special_vars: &'a SpecialVars) -> MatchContext<'a> {
        MatchContext {
            scope,
            special_vars,
            match_data: MatchData::new(),
        }
    }

    fn visit_ident(&mut self, pattern_ident: &Ident, arg: &NsValue) -> MatchVisitResult {
        if pattern_ident.name() == "_" {
            // This is a wildcard
            return Ok(());
        }

        let pattern_var = MacroVar::from_ident(self.scope, pattern_ident);

        if self.special_vars.is_literal(&pattern_var) {
            // The arg must be the exact same pattern variable
            if let &NsValue::Ident(_, ref arg_ident) = arg {
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

    // TODO: Floats, sets, maps
    fn visit_datum(&mut self, pattern: &NsValue, arg: &NsValue) -> MatchVisitResult {
        match (pattern, arg) {
            (&NsValue::Ident(_, ref pattern_ident), arg) => self.visit_ident(pattern_ident, arg),
            (&NsValue::List(_, ref pvs), &NsValue::List(_, ref avs)) => self.visit_slice(pvs, avs),
            (&NsValue::Vector(_, ref pvs), &NsValue::Vector(_, ref avs)) => {
                self.visit_slice(pvs, avs)
            }
            (&NsValue::Bool(_, pv), &NsValue::Bool(_, av)) if pv == av => Ok(()),
            (&NsValue::Int(_, pv), &NsValue::Int(_, av)) if pv == av => Ok(()),
            (&NsValue::Char(_, pv), &NsValue::Char(_, av)) if pv == av => Ok(()),
            (&NsValue::String(_, ref pv), &NsValue::String(_, ref av)) if pv == av => Ok(()),
            _ => Err(()),
        }
    }

    fn visit_zero_or_more(&mut self, pattern: &NsValue, args: &[NsValue]) -> MatchVisitResult {
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

    fn visit_slice(&mut self, mut patterns: &[NsValue], mut args: &[NsValue]) -> MatchVisitResult {
        loop {
            match (patterns.first(), args.first()) {
                (Some(pattern), Some(arg)) => {
                    if let Some(&NsValue::Ident(_, ref next_ident)) = patterns.get(1) {
                        let next_var = MacroVar::from_ident(self.scope, next_ident);

                        if self.special_vars.is_zero_or_more(&next_var) {
                            let rest_patterns_len = patterns.len() - 2;

                            if args.len() < rest_patterns_len {
                                // Cannot match
                                return Err(());
                            }

                            let (zero_or_more_args, rest_args) =
                                args.split_at(args.len() - rest_patterns_len);

                            self.visit_zero_or_more(pattern, zero_or_more_args)?;
                            patterns = &patterns[2..];
                            args = rest_args;
                            continue;
                        }
                    }

                    self.visit_datum(pattern, arg)?
                }
                (None, None) => {
                    return Ok(());
                }
                _ => return Err(()),
            }

            patterns = &patterns[1..];
            args = &args[1..];
        }
    }

    pub fn visit_rule(
        mut self,
        rule: &Rule,
        arg_data: &[NsValue],
    ) -> result::Result<MatchData, ()> {
        self.visit_slice(rule.pattern.as_slice(), arg_data)?;
        Ok(self.match_data)
    }
}
