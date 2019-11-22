pub mod escape;
pub mod names;

use std::collections::{BTreeMap, HashMap};
use std::rc::Rc;

use arret_runtime::intern;

use crate::codegen::analysis::escape::Captures;
use crate::mir::ops;

pub struct AnalysedMod<'of> {
    private_funs: HashMap<ops::PrivateFunId, AnalysedFun<'of>>,
    entry_fun: AnalysedFun<'of>,
    global_interned_names: BTreeMap<Rc<str>, intern::InternedSym>,
}

pub struct AnalysedFun<'of> {
    pub ops_fun: &'of ops::Fun,
    pub captures: Captures,
}

impl<'of> AnalysedMod<'of> {
    pub fn new(
        private_funs: &'of HashMap<ops::PrivateFunId, ops::Fun>,
        entry_fun: &'of ops::Fun,
    ) -> AnalysedMod<'of> {
        // This also determines which private funs are used; private_fun_captures won't contain
        // entries for unused funs
        let escape::ProgramCaptures {
            private_fun_captures,
            entry_fun_captures,
        } = escape::calc_program_captures(private_funs, entry_fun);

        let private_funs = private_fun_captures
            .into_iter()
            .map(|(private_fun_id, captures)| {
                (
                    private_fun_id,
                    AnalysedFun {
                        ops_fun: &private_funs[&private_fun_id],
                        captures,
                    },
                )
            })
            .collect();

        let global_interned_names =
            names::calc_program_global_interned_names(&private_funs, entry_fun);

        AnalysedMod {
            private_funs,
            entry_fun: AnalysedFun {
                ops_fun: entry_fun,
                captures: entry_fun_captures,
            },
            global_interned_names,
        }
    }

    pub fn private_funs(&self) -> impl Iterator<Item = (&ops::PrivateFunId, &AnalysedFun<'of>)> {
        self.private_funs.iter()
    }

    pub fn entry_fun(&self) -> &AnalysedFun<'of> {
        &self.entry_fun
    }

    pub fn global_interned_names(&self) -> &BTreeMap<Rc<str>, intern::InternedSym> {
        &self.global_interned_names
    }
}
