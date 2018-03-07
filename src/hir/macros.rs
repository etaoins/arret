use std::collections::{HashMap, HashSet};
use std::result;

use syntax::span::Span;
use hir::scope::{Binding, Ident, NsId, NsIdAllocator, NsValue, Scope};
use hir::error::{Error, Result};

#[derive(PartialEq, Eq, Debug, Hash)]
pub enum MacroVar {
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

#[derive(PartialEq, Debug)]
pub struct Rule {
    pattern: Vec<NsValue>,
    template: NsValue,
}

#[derive(PartialEq, Debug)]
pub struct Macro {
    self_ident: Ident,
    literals: HashSet<MacroVar>,
    rules: Vec<Rule>,
}

impl Macro {
    pub fn new(self_ident: Ident, literals: HashSet<MacroVar>, rules: Vec<Rule>) -> Macro {
        Macro {
            self_ident,
            literals,
            rules,
        }
    }
}

pub fn lower_macro_rule(self_ident: &Ident, rule_datum: NsValue) -> Result<Rule> {
    let (span, mut rule_values) = if let NsValue::Vector(span, vs) = rule_datum {
        (span, vs)
    } else {
        return Err(Error::IllegalArg(
            rule_datum.span(),
            "Expected a macro rule vector".to_owned(),
        ));
    };

    if rule_values.len() != 2 {
        return Err(Error::IllegalArg(
            span,
            "Expected a macro rule vector with two elements".to_owned(),
        ));
    }

    let template = rule_values.pop().unwrap();
    let pattern_data = rule_values.pop().unwrap();

    let pattern = if let NsValue::List(span, mut vs) = pattern_data {
        if vs.len() < 1 {
            return Err(Error::IllegalArg(
                span,
                "Macro rule patterns must contain at least the name of the macro".to_owned(),
            ));
        }

        match vs.remove(0) {
            NsValue::Ident(_, ref ident) if ident.name() == self_ident.name() => {}
            other => {
                return Err(Error::IllegalArg(
                    other.span(),
                    "Macro rule patterns must start with the name of the macro".to_owned(),
                ));
            }
        }

        vs
    } else {
        return Err(Error::IllegalArg(
            pattern_data.span(),
            "Expected a macro rule pattern list".to_owned(),
        ));
    };

    Ok(Rule { pattern, template })
}

pub fn lower_macro_rules(
    scope: &Scope,
    span: Span,
    self_ident: &Ident,
    mut macro_rules_data: Vec<NsValue>,
) -> Result<Macro> {
    if macro_rules_data.len() != 2 {
        return Err(Error::WrongArgCount(span, 2));
    }

    let rules_datum = macro_rules_data.pop().unwrap();
    let literals_datum = macro_rules_data.pop().unwrap();

    let literals = if let NsValue::Set(_, vs) = literals_datum {
        vs.into_iter()
            .map(|v| {
                if let NsValue::Ident(_, ref ident) = v {
                    Ok(MacroVar::from_ident(scope, ident))
                } else {
                    Err(Error::IllegalArg(
                        v.span(),
                        "Pattern literal must be a symbol".to_owned(),
                    ))
                }
            })
            .collect::<Result<HashSet<MacroVar>>>()?
    } else {
        return Err(Error::IllegalArg(
            literals_datum.span(),
            "Expected set of pattern literals".to_owned(),
        ));
    };

    let rules_values = if let NsValue::Vector(_, vs) = rules_datum {
        vs
    } else {
        return Err(Error::IllegalArg(
            rules_datum.span(),
            "Expected a vector of syntax rules".to_owned(),
        ));
    };

    let rules = rules_values
        .into_iter()
        .map(|rule_datum| lower_macro_rule(self_ident, rule_datum))
        .collect::<Result<Vec<Rule>>>()?;

    Ok(Macro::new(self_ident.clone(), literals, rules))
}

struct MatchData {
    vars: HashMap<MacroVar, NsValue>,
}

struct MatchContext<'a> {
    scope: &'a Scope,
    literals: &'a HashSet<MacroVar>,
    match_data: MatchData,
}

type MatchVisitResult = result::Result<(), ()>;

impl<'a> MatchContext<'a> {
    fn new(scope: &'a Scope, literals: &'a HashSet<MacroVar>) -> MatchContext<'a> {
        MatchContext {
            scope,
            literals,
            match_data: MatchData {
                vars: HashMap::new(),
            },
        }
    }

    fn visit_ident(&mut self, pattern_ident: &Ident, arg: &NsValue) -> MatchVisitResult {
        if pattern_ident.name() == "_" {
            // This is a wildcard
            return Ok(());
        }

        let pattern_var = MacroVar::from_ident(self.scope, pattern_ident);

        if self.literals.contains(&pattern_var) {
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

    fn visit_datum(&mut self, pattern: &NsValue, arg: &NsValue) -> MatchVisitResult {
        match (pattern, arg) {
            (&NsValue::Ident(_, ref pattern_ident), arg) => self.visit_ident(pattern_ident, arg),
            (&NsValue::List(_, ref pvs), &NsValue::List(_, ref avs)) => self.visit_slice(pvs, avs),
            (&NsValue::Vector(_, ref pvs), &NsValue::Vector(_, ref avs)) => {
                self.visit_slice(pvs, avs)
            }
            _ => Err(()),
        }
    }

    fn visit_slice(&mut self, mut patterns: &[NsValue], mut args: &[NsValue]) -> MatchVisitResult {
        loop {
            match (patterns.first(), args.first()) {
                (Some(pattern), Some(arg)) => self.visit_datum(pattern, arg)?,
                (None, None) => {
                    return Ok(());
                }
                _ => return Err(()),
            }

            patterns = &patterns[1..];
            args = &args[1..];
        }
    }

    fn visit_rule(mut self, rule: &Rule, arg_data: &[NsValue]) -> result::Result<MatchData, ()> {
        self.visit_slice(rule.pattern.as_slice(), arg_data)?;
        Ok(self.match_data)
    }
}

struct ExpandContext<'a> {
    ns_id_allocator: &'a mut NsIdAllocator,
    scope: Scope,
    match_data: MatchData,
    ns_mapping: HashMap<NsId, NsId>,
}

impl<'a> ExpandContext<'a> {
    fn new(
        ns_id_allocator: &'a mut NsIdAllocator,
        scope: &Scope,
        match_data: MatchData,
    ) -> ExpandContext<'a> {
        ExpandContext {
            ns_id_allocator,
            scope: Scope::new_child(scope),
            match_data,
            ns_mapping: HashMap::new(),
        }
    }

    fn expand_ident(&mut self, span: Span, ident: &Ident) -> NsValue {
        let macro_var = MacroVar::from_ident(&self.scope, ident);

        if let Some(replacement) = self.match_data.vars.get(&macro_var) {
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

    fn expand_datum(&mut self, template: &NsValue) -> NsValue {
        match template {
            &NsValue::Ident(span, ref ident) => self.expand_ident(span, ident),
            &NsValue::List(span, ref vs) => {
                NsValue::List(span, vs.iter().map(|v| self.expand_datum(v)).collect())
            }
            &NsValue::Vector(span, ref vs) => {
                NsValue::Vector(span, vs.iter().map(|v| self.expand_datum(v)).collect())
            }
            other => other.clone(),
        }
    }

    fn expand_template(mut self, template: &NsValue) -> (Scope, NsValue) {
        let expanded_datum = self.expand_datum(template);
        (self.scope, expanded_datum)
    }
}

pub fn expand_macro(
    ns_id_allocator: &mut NsIdAllocator,
    scope: &Scope,
    span: Span,
    mac: &Macro,
    arg_data: Vec<NsValue>,
) -> Result<(Scope, NsValue)> {
    for rule in mac.rules.iter() {
        let mut mcx = MatchContext::new(scope, &mac.literals);
        if let Ok(match_data) = mcx.visit_rule(rule, arg_data.as_slice()) {
            let ecx = ExpandContext::new(ns_id_allocator, scope, match_data);

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

    lower_macro_rules(&Scope::new_empty(), full_span, &self_ident(), test_ns_data)
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
    literals.insert(MacroVar::Unbound("a".to_owned()));
    literals.insert(MacroVar::Unbound("b".to_owned()));

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
