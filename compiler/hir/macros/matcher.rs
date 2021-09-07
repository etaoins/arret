use std::result;

use crate::hir::macros::{get_escaped_ident, starts_with_zero_or_more, Rule};
use crate::hir::ns::{Ident, NsDatum};

#[derive(Debug)]
pub struct MatchData<'data> {
    vars: Vec<&'data NsDatum>,
    // The outside vector is the subpatterns; the inside slice contains the zero or more matches
    subpatterns: Vec<Box<[MatchData<'data>]>>,
}

impl<'data> MatchData<'data> {
    fn new() -> MatchData<'data> {
        MatchData {
            vars: vec![],
            subpatterns: vec![],
        }
    }

    pub fn var(&self, i: usize) -> &'data NsDatum {
        self.vars[i]
    }

    pub fn subpattern(&self, i: usize) -> &[MatchData<'data>] {
        &self.subpatterns[i]
    }
}

struct MatchCtx<'data> {
    match_data: MatchData<'data>,
}

type Result<T> = result::Result<T, ()>;

impl<'data> MatchCtx<'data> {
    fn new() -> Self {
        MatchCtx {
            match_data: MatchData::new(),
        }
    }

    fn match_ident(&mut self, pattern_ident: &'data Ident, arg: &'data NsDatum) -> bool {
        if pattern_ident.is_underscore() {
            // This is a wildcard; just discard
        } else {
            self.match_data.vars.push(arg);
        }

        true
    }

    // TODO: Maps
    #[allow(clippy::float_cmp)]
    fn match_datum(&mut self, pattern: &'data NsDatum, arg: &'data NsDatum) -> bool {
        match (pattern, arg) {
            (NsDatum::Ident(_, pattern_ident), arg) => self.match_ident(pattern_ident, arg),
            (NsDatum::Keyword(_, pv), NsDatum::Keyword(_, av)) => pv == av,
            (NsDatum::List(_, pvs), NsDatum::List(_, avs)) => self.match_slice(pvs, avs),
            (NsDatum::Vector(_, pvs), NsDatum::Vector(_, avs)) => self.match_slice(pvs, avs),
            (NsDatum::Set(_, pvs), NsDatum::Set(_, avs)) => self.match_slice(pvs, avs),
            (NsDatum::Bool(_, pv), NsDatum::Bool(_, av)) => pv == av,
            (NsDatum::Int(_, pv), NsDatum::Int(_, av)) => pv == av,
            // Don't match NaNs against other NaNs. This is consistent with `=`.
            (NsDatum::Float(_, pv), NsDatum::Float(_, av)) => pv == av,
            (NsDatum::Char(_, pv), NsDatum::Char(_, av)) => pv == av,
            (NsDatum::Str(_, pv), NsDatum::Str(_, av)) => pv == av,
            (NsDatum::List(_, pv), NsDatum::Ident(_, arg)) => {
                if let Some(escaped_ident) = get_escaped_ident(pv) {
                    escaped_ident.name() == arg.name()
                } else {
                    false
                }
            }
            _ => false,
        }
    }

    fn match_zero_or_more(&mut self, pattern: &'data NsDatum, args: &'data [NsDatum]) -> bool {
        let submatch_result = args
            .iter()
            .map(|arg| {
                let mut subcontext = MatchCtx {
                    match_data: MatchData::new(),
                };

                if !subcontext.match_datum(pattern, arg) {
                    Err(())
                } else {
                    Ok(subcontext.match_data)
                }
            })
            .collect::<result::Result<Box<[MatchData<'data>]>, ()>>();

        match submatch_result {
            Ok(submatch_data) => {
                self.match_data.subpatterns.push(submatch_data);
                true
            }
            Err(()) => false,
        }
    }

    fn match_slice(&mut self, mut patterns: &'data [NsDatum], mut args: &'data [NsDatum]) -> bool {
        loop {
            if starts_with_zero_or_more(patterns) {
                let rest_patterns_len = patterns.len() - 2;

                if args.len() < rest_patterns_len {
                    // Cannot match
                    break false;
                }

                let (zero_or_more_args, rest_args) = args.split_at(args.len() - rest_patterns_len);
                if !self.match_zero_or_more(&patterns[0], zero_or_more_args) {
                    break false;
                }

                patterns = &patterns[2..];
                args = rest_args;
            } else {
                let (pattern, arg) = match (patterns.first(), args.first()) {
                    (Some(pattern), Some(arg)) => (pattern, arg),
                    (None, None) => {
                        // Patterns and args ran out at the same time
                        break true;
                    }
                    _ => {
                        // Mismatched lengths
                        break false;
                    }
                };

                if !self.match_datum(pattern, arg) {
                    break false;
                }

                patterns = &patterns[1..];
                args = &args[1..];
            }
        }
    }

    fn visit_rule(
        mut self,
        rule: &'data Rule,
        arg_data: &'data [NsDatum],
    ) -> Result<MatchData<'data>> {
        if self.match_slice(&rule.pattern, arg_data) {
            Ok(self.match_data)
        } else {
            Err(())
        }
    }
}

pub fn match_rule<'data>(
    rule: &'data Rule,
    arg_data: &'data [NsDatum],
) -> Result<MatchData<'data>> {
    let mcx = MatchCtx::new();
    mcx.visit_rule(rule, arg_data)
}
