pub mod escape;
pub mod needs_task;

use std::collections::HashMap;

use crate::codegen::alloc::AllocAtom;
use crate::codegen::analysis::escape::Captures;
use crate::mir::ops;

pub struct AnalysedMod<'of> {
    private_funs: &'of HashMap<ops::PrivateFunId, ops::Fun>,
    entry_fun: &'of ops::Fun,

    private_fun_captures: HashMap<ops::PrivateFunId, Captures>,
    entry_fun_captures: Captures,

    private_fun_alloc_plans: HashMap<ops::PrivateFunId, Vec<AllocAtom<'of>>>,
    entry_fun_alloc_plan: Vec<AllocAtom<'of>>,
}

impl<'of> AnalysedMod<'of> {
    pub fn new(
        private_funs: &'of HashMap<ops::PrivateFunId, ops::Fun>,
        entry_fun: &'of ops::Fun,
    ) -> AnalysedMod<'of> {
        use crate::codegen::alloc::plan::plan_allocs;

        // This also determines which private funs are used; private_fun_captures won't contain
        // entries for unused funs
        let escape::ProgramCaptures {
            private_fun_captures,
            entry_fun_captures,
        } = escape::calc_program_captures(private_funs, entry_fun);

        let private_fun_alloc_plans: HashMap<ops::PrivateFunId, _> = private_fun_captures
            .iter()
            .map(|(private_fun_id, _)| {
                let alloc_plan = plan_allocs(
                    &private_fun_captures[private_fun_id],
                    &private_funs[private_fun_id].ops,
                );

                (*private_fun_id, alloc_plan)
            })
            .collect();

        let entry_fun_alloc_plan = plan_allocs(&entry_fun_captures, &entry_fun.ops);

        AnalysedMod {
            private_funs,
            entry_fun,

            private_fun_captures,
            entry_fun_captures,

            private_fun_alloc_plans,
            entry_fun_alloc_plan,
        }
    }

    pub fn private_fun(&self, private_fun_id: ops::PrivateFunId) -> &'of ops::Fun {
        &self.private_funs[&private_fun_id]
    }

    pub fn private_fun_captures(&self, private_fun_id: ops::PrivateFunId) -> &Captures {
        &self.private_fun_captures[&private_fun_id]
    }

    pub fn private_fun_alloc_plan(
        &self,
        private_fun_id: ops::PrivateFunId,
    ) -> &Vec<AllocAtom<'of>> {
        &self.private_fun_alloc_plans[&private_fun_id]
    }

    pub fn entry_fun(&self) -> &'of ops::Fun {
        self.entry_fun
    }

    pub fn entry_fun_captures(&self) -> &Captures {
        &self.entry_fun_captures
    }

    pub fn entry_fun_alloc_plan(&self) -> &Vec<AllocAtom<'of>> {
        &self.entry_fun_alloc_plan
    }
}
