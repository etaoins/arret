use crate::hir::scope::Binding;

macro_rules! export_prims {
    ( $( ($n:expr, $i:ident) ),* ) => {
        #[derive(Clone, Copy, PartialEq, Eq, Debug, Hash)]
        pub enum Prim {
            // `(import)` is magically added to every scope. If we add it again in `PRIM_EXPORTS`
            // we will trigger duplicate definition errors if someone imports the prim modules.
            Import,
            $($i,)*
        }

        pub const PRIM_EXPORTS: &[(&str, Binding)] = &[
            $(
                ($n, Binding::Prim(Prim::$i))
            ),*
        ];
    }
}

export_prims!(
    ("def", Def),
    ("let", Let),
    ("fn", Fun),
    ("if", If),
    ("do", Do),
    ("quote", Quote),
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
