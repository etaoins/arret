use hir::scope::Binding;
use std::collections::HashMap;

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
    ("let", Let),
    ("fn", Fun),
    ("if", If),
    ("quote", Quote),
    ("import", Import),
    ("export", Export),
    ("defmacro", DefMacro),
    ("letmacro", LetMacro),
    ("...", Ellipsis),
    ("_", Wildcard),
    ("macro-rules", MacroRules),
    (":", TyColon),
    ("deftype", DefType),
    ("lettype", LetType),
    ("type-predicate", TyPred),
    ("compile-error", CompileError)
);
