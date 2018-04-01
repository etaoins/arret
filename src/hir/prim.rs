use std::collections::HashMap;
use hir::scope::Binding;

macro_rules! prims {
    ( $( ($n:expr, $i:ident) ),* ) => {
        #[derive(Clone, Copy, PartialEq, Eq, Debug, Hash)]
        pub enum Prim {
            $($i,)*
        }

        pub fn insert_prim_exports(exports: &mut HashMap<String, Binding>) {
            $(
                exports.insert($n.to_owned(), Binding::Prim(Prim::$i));
            )*
        }
    }
}

prims!(
    ("def", Def),
    ("fn", Fun),
    ("if", If),
    ("quote", Quote),
    ("import", Import),
    ("export", Export),
    ("defmacro", DefMacro),
    ("...", Ellipsis),
    ("_", Wildcard),
    ("macro-rules", MacroRules),
    (":", TyColon),
    ("deftype", DefType),
    ("type-predicate", TyPred)
);
