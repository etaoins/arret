use std::collections::HashMap;
use hir::{Expr, Var};
use syntax::value::Value;
use syntax::span::t2s;
use syntax::parser::datum_from_str;

macro_rules! primitives {
    ( $( ($n:expr, $i:ident) ),* ) => {
        enum Primitive {
            $($i,)*
        }

        fn primitive_bindings<'tcx>() -> Bindings<'tcx> {
            let mut bindings: Bindings<'tcx> = HashMap::new();
            $(bindings.insert($n, &Binding::Primitive(Primitive::$i));)*
            bindings
        }
    }
}

enum Binding<'tcx> {
    Var(&'tcx Var<'tcx>),
    Primitive(Primitive),
}

type Bindings<'tcx> = HashMap<&'tcx str, &'tcx Binding<'tcx>>;

primitives!(("def", Def), ("fn", Fun), ("if", If), ("quote", Quote));

struct Scope<'tcx> {
    parent: Option<&'tcx Scope<'tcx>>,
    bindings: Bindings<'tcx>,
}

impl<'tcx> Scope<'tcx> {
    fn primitive_scope() -> Scope<'tcx> {
        Scope {
            parent: None,
            bindings: primitive_bindings(),
        }
    }
}

struct LoweringContext {
    next_inst_id: usize,
}

impl LoweringContext {
    fn new() -> LoweringContext {
        LoweringContext { next_inst_id: 0 }
    }

    fn lower_module<'tcx>(mut self, datum: Value) -> Expr<'tcx> {
        match datum {
            lit @ Value::Bool(_, _) => Expr::Lit(lit),
            _ => unimplemented!("Unsupported datum"),
        }
    }
}

#[test]
fn self_quoting_datum() {
    let lcx = LoweringContext::new();

    let j = "false";
    let t = "^^^^^";

    let expected = Expr::Lit(Value::Bool(t2s(t), false));
    let datum = datum_from_str(j).unwrap();

    assert_eq!(expected, lcx.lower_module(datum));
}
