use std::collections::{HashMap, HashSet};
use std::result;

use syntax::span::Span;
use hir::scope::{Binding, Ident, NsId, NsValue, Scope};
use hir::error::{Error, Result};

#[derive(Clone, PartialEq, Debug)]
pub struct Rule {
    pattern: Vec<NsValue>,
    template: NsValue,
}

#[derive(Clone, PartialEq, Debug)]
pub struct Macro {
    self_ident: Ident,
    literals: HashSet<String>,
    rules: Vec<Rule>,
}

impl Macro {
    pub fn new(self_ident: Ident, literals: HashSet<String>, rules: Vec<Rule>) -> Macro {
        Macro {
            self_ident,
            literals,
            rules,
        }
    }
}

pub fn lower_macro_rules(
    span: Span,
    self_ident: Ident,
    macro_rules_data: &[NsValue],
) -> Result<Macro> {
    if macro_rules_data.len() != 2 {
        return Err(Error::WrongArgCount(span, 2));
    }

    let literals_datum = &macro_rules_data[0];
    let rules_datum = &macro_rules_data[1];

    let literals = match literals_datum {
        &NsValue::Set(_, ref vs) => vs.iter()
            .map(|v| match v {
                &NsValue::Ident(_, ref ident) => Ok(ident.name().clone()),
                other => Err(Error::IllegalArg(
                    other.span(),
                    "Pattern literal must be a symbol".to_owned(),
                )),
            })
            .collect::<Result<HashSet<String>>>()?,
        other => {
            return Err(Error::IllegalArg(
                other.span(),
                "Expected set of pattern literals".to_owned(),
            ));
        }
    };

    let rules_data = match rules_datum {
        &NsValue::Vector(_, ref vs) => vs,
        other => {
            return Err(Error::IllegalArg(
                other.span(),
                "Expected a vector of syntax rules".to_owned(),
            ));
        }
    };

    let split_rules_data = rules_data
        .iter()
        .map(|rule_datum| match rule_datum {
            &NsValue::Vector(span, ref vs) => {
                if vs.len() != 2 {
                    Err(Error::IllegalArg(
                        span,
                        "Expected a macro rule vector with two elements".to_owned(),
                    ))
                } else {
                    Ok((&vs[0], &vs[1]))
                }
            }
            other => Err(Error::IllegalArg(
                other.span(),
                "Expected a macro rule vector".to_owned(),
            )),
        })
        .collect::<Result<Vec<(&NsValue, &NsValue)>>>()?;

    let rules = split_rules_data
        .iter()
        .map(|&(full_pattern_data, template)| {
            let pattern = match full_pattern_data {
                &NsValue::List(span, ref vs) => {
                    if vs.len() < 1 {
                        return Err(Error::IllegalArg(
                            span,
                            "Macro rule patterns must contain at least the name of the macro"
                                .to_owned(),
                        ));
                    }

                    let (first, pattern_params) = vs.split_first().unwrap();

                    match first {
                        &NsValue::Ident(_, ref ident) if ident.name() == self_ident.name() => {}
                        other => {
                            return Err(Error::IllegalArg(
                                other.span(),
                                "Macro rule patterns must start with the name of the macro"
                                    .to_owned(),
                            ));
                        }
                    }

                    pattern_params
                }
                other => {
                    return Err(Error::IllegalArg(
                        other.span(),
                        "Expected a macro rule pattern list".to_owned(),
                    ));
                }
            };

            Ok(Rule {
                pattern: pattern.iter().map(|datum| datum.clone()).collect(),
                template: template.clone(),
            })
        })
        .collect::<Result<Vec<Rule>>>()?;

    Ok(Macro::new(self_ident, literals, rules))
}

#[derive(PartialEq, Eq, Hash)]
enum MacroVar {
    Bound(Binding),
    Unbound(String),
}

impl MacroVar {
    fn from_ident(scope: &Scope, ident: &Ident) -> MacroVar {
        match scope.get(ident) {
            Some(binding) => MacroVar::Bound(binding),
            None => MacroVar::Unbound(ident.name().clone()),
        }
    }
}

struct MatchData {
    vars: HashMap<MacroVar, NsValue>,
}

type MatchVisitResult = result::Result<(), ()>;

impl MatchData {
    fn visit_datum(&mut self, scope: &Scope, pattern: &NsValue, arg: &NsValue) -> MatchVisitResult {
        match (pattern, arg) {
            (&NsValue::Ident(_, ref pattern_ident), arg) => {
                let pattern_var = MacroVar::from_ident(scope, pattern_ident);
                self.vars.insert(pattern_var, arg.clone());
                Ok(())
            }
            _ => Err(()),
        }
    }

    fn visit_slice(
        &mut self,
        scope: &Scope,
        mut patterns: &[NsValue],
        mut args: &[NsValue],
    ) -> MatchVisitResult {
        loop {
            match (patterns.first(), args.first()) {
                (Some(pattern), Some(arg)) => self.visit_datum(scope, pattern, arg)?,
                (None, None) => {
                    return Ok(());
                }
                _ => return Err(()),
            }

            patterns = &patterns[1..];
            args = &args[1..];
        }
    }

    fn from_macro_rule(
        scope: &Scope,
        rule: &Rule,
        arg_data: &[NsValue],
    ) -> result::Result<MatchData, ()> {
        let mut match_data = MatchData {
            vars: HashMap::new(),
        };

        match_data.visit_slice(scope, rule.pattern.as_slice(), arg_data)?;

        Ok(match_data)
    }
}

struct ExpandContext {
    scope: Scope,
    match_data: MatchData,
    ns_mapping: HashMap<NsId, NsId>,
}

impl ExpandContext {
    fn new(scope: &Scope, match_data: MatchData) -> ExpandContext {
        ExpandContext {
            scope: Scope::new_child(scope),
            match_data,
            ns_mapping: HashMap::new(),
        }
    }

    fn expand_template(self, template: &NsValue) -> (Scope, NsValue) {
        (self.scope, template.clone())
    }
}

pub fn expand_macro(
    scope: &Scope,
    span: Span,
    mac: &Macro,
    arg_data: Vec<NsValue>,
) -> Result<(Scope, NsValue)> {
    for rule in mac.rules.iter() {
        if let Ok(match_data) = MatchData::from_macro_rule(scope, rule, arg_data.as_slice()) {
            let ecx = ExpandContext::new(scope, match_data);

            return Ok(ecx.expand_template(&rule.template));
        }
    }

    Err(Error::NoMacroRule(span))
}

#[cfg(test)]
use syntax::parser::data_from_str;
#[cfg(test)]
use syntax::span::t2s;

#[cfg(test)]
fn test_ns_id() -> NsId {
    NsId::new(0)
}

#[cfg(test)]
fn self_ident() -> Ident {
    Ident::new(test_ns_id(), "self".to_owned())
}

#[cfg(test)]
fn macro_rules_for_str(data_str: &str) -> Result<Macro> {
    let test_data = data_from_str(data_str).unwrap();

    let full_span = Span {
        lo: 0,
        hi: data_str.len() as u32,
    };

    let test_ns_data = test_data
        .into_iter()
        .map(|datum| NsValue::from_value(datum, test_ns_id()))
        .collect::<Vec<NsValue>>();

    lower_macro_rules(full_span, self_ident(), test_ns_data.as_slice())
}

#[test]
fn wrong_arg_count() {
    let j = "#{} [] []";
    let t = "^^^^^^^^^";

    let err = Error::WrongArgCount(t2s(t), 2);
    assert_eq!(err, macro_rules_for_str(j).unwrap_err());
}

#[test]
fn non_set_literals() {
    let j = "[] []";
    let t = "^^   ";

    let err = Error::IllegalArg(t2s(t), "Expected set of pattern literals".to_owned());
    assert_eq!(err, macro_rules_for_str(j).unwrap_err());
}

#[test]
fn non_symbol_literal() {
    let j = "#{one 2} []";
    let t = "      ^    ";

    let err = Error::IllegalArg(t2s(t), "Pattern literal must be a symbol".to_owned());
    assert_eq!(err, macro_rules_for_str(j).unwrap_err());
}

#[test]
fn empty_rules() {
    let j = "#{} []";

    let expected = Macro::new(self_ident(), HashSet::new(), vec![]);
    assert_eq!(expected, macro_rules_for_str(j).unwrap());
}

#[test]
fn trivial_rules() {
    let j = "#{a b} [[(self x) x]]";
    let t = "               ^     ";
    let u = "                  ^  ";

    let mut literals = HashSet::new();
    literals.insert("a".to_owned());
    literals.insert("b".to_owned());

    let pattern = vec![
        NsValue::Ident(t2s(t), Ident::new(test_ns_id(), "x".to_owned())),
    ];
    let template = NsValue::Ident(t2s(u), Ident::new(test_ns_id(), "x".to_owned()));
    let rules = vec![Rule { pattern, template }];

    let expected = Macro::new(self_ident(), literals, rules);
    assert_eq!(expected, macro_rules_for_str(j).unwrap());
}

#[test]
fn rule_with_non_vector() {
    let j = "#{} [1]";
    let t = "     ^ ";

    let err = Error::IllegalArg(t2s(t), "Expected a macro rule vector".to_owned());
    assert_eq!(err, macro_rules_for_str(j).unwrap_err());
}

#[test]
fn rule_with_not_enough_elements() {
    let j = "#{} [[(self)]]";
    let t = "     ^^^^^^^^ ";

    let err = Error::IllegalArg(
        t2s(t),
        "Expected a macro rule vector with two elements".to_owned(),
    );
    assert_eq!(err, macro_rules_for_str(j).unwrap_err());
}

#[test]
fn rule_with_non_list_pattern() {
    let j = "#{} [[self 1]]";
    let t = "      ^^^^    ";

    let err = Error::IllegalArg(t2s(t), "Expected a macro rule pattern list".to_owned());
    assert_eq!(err, macro_rules_for_str(j).unwrap_err());
}

#[test]
fn rule_with_empty_pattern_list() {
    let j = "#{} [[() 1]]";
    let t = "      ^^    ";

    let err = Error::IllegalArg(
        t2s(t),
        "Macro rule patterns must contain at least the name of the macro".to_owned(),
    );
    assert_eq!(err, macro_rules_for_str(j).unwrap_err());
}

#[test]
fn rule_with_non_self_pattern() {
    let j = "#{} [[(notself) 1]]";
    let t = "       ^^^^^^^     ";

    let err = Error::IllegalArg(
        t2s(t),
        "Macro rule patterns must start with the name of the macro".to_owned(),
    );
    assert_eq!(err, macro_rules_for_str(j).unwrap_err());
}
