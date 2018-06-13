use hir::scope::Binding;
use std::collections::HashMap;

macro_rules! prims {
    ( $( ($n:expr, $i:ident) ),* ) => {
        #[derive(Clone, Copy, PartialEq, Eq, Debug, Hash)]
        pub enum Prim {
            $($i,)*
        }

        pub fn insert_prim_exports(exports: &mut HashMap<Box<str>, Binding>) {
            $(
                // `(import)` is magically added to every scope. If we add it again we will trigger
                // duplicate definition errors if someone imports `[risp internal primitives]`.
                if $n != "import" {
                    exports.insert($n.into(), Binding::Prim(Prim::$i));
                }
            )*
        }
    }
}

prims!(
    ("def", Def),
    ("let", Let),
    ("fn", Fun),
    ("if", If),
    ("do", Do),
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
