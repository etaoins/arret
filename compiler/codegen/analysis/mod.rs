pub mod escape;
pub mod needs_task;

use crate::codegen::analysis::escape::Captures;
use crate::mir::ops;
use std::collections::HashMap;

pub struct AnalysedMod<'of> {
    private_funs: &'of [ops::Fun],
    entry_fun: &'of ops::Fun,

    private_fun_captures: HashMap<ops::PrivateFunId, Captures>,
    entry_fun_captures: Captures,
}

impl<'of> AnalysedMod<'of> {
    pub fn new(private_funs: &'of [ops::Fun], entry_fun: &'of ops::Fun) -> AnalysedMod<'of> {
        let mut private_fun_captures = HashMap::new();
        let entry_fun_captures =
            escape::calc_fun_captures(private_funs, &mut private_fun_captures, entry_fun);

        AnalysedMod {
            private_funs,
            entry_fun,

            private_fun_captures,
            entry_fun_captures,
        }
    }

    pub fn private_fun(&self, private_fun_id: ops::PrivateFunId) -> &'of ops::Fun {
        &self.private_funs[private_fun_id.to_usize()]
    }

    pub fn private_fun_captures(&self, private_fun_id: ops::PrivateFunId) -> &Captures {
        &self.private_fun_captures[&private_fun_id]
    }

    pub fn entry_fun(&self) -> &'of ops::Fun {
        self.entry_fun
    }

    pub fn entry_fun_captures(&self) -> &Captures {
        &self.entry_fun_captures
    }
}
