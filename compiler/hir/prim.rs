use crate::hir::scope::Binding;

macro_rules! export_prims {
    ( $( ($n:expr, $i:ident) ),* ) => {
        #[derive(Clone, Copy, PartialEq, Eq, Debug, Hash)]
        pub enum Prim {
            // `(import)` is magically added to every scope. If we add it again in `PRIM_EXPORTS`
            // we will trigger duplicate definition errors if someone imports the prim modules.
            // Note that `(import)` is actually parsed specially; this is here to catch duplicate
            // definitions.
            ImportPlaceholder,
            $($i,)*
        }

        pub const PRIM_EXPORTS: &[(&str, Binding)] = &[
            // This is a pseudo-primitive. Unlike normal primitives we can take the value of `=`.
            ("=", Binding::EqPred),
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
    ("recur", Recur),
    ("quote", Quote),
    ("export", Export),
    ("defmacro", DefMacro),
    ("letmacro", LetMacro),
    ("macro-rules", MacroRules),
    ("deftype", DefType),
    ("lettype", LetType),
    ("defrecord", DefRecord),
    ("letrecord", LetRecord),
    ("compile-error", CompileError),
    ("All", All)
);
