use arret_syntax::datum::Datum;

use arret_runtime::boxed;
use arret_runtime::boxed::prelude::*;
use arret_runtime::boxed::refs::Gc;

/// Places a syntax datum on a box heap
pub fn box_syntax_datum(heap: &mut impl boxed::AsHeap, datum: &Datum) -> Gc<boxed::Any> {
    match datum {
        Datum::Bool(_, value) => boxed::Bool::singleton_ref(*value).as_any_ref(),
        Datum::Int(_, val) => boxed::Int::new(heap, *val).as_any_ref(),
        Datum::Float(_, val) => boxed::Float::new(heap, *val).as_any_ref(),
        Datum::Char(_, val) => boxed::Char::new(heap, *val).as_any_ref(),
        Datum::Str(_, val) => boxed::Str::new(heap, val.as_ref()).as_any_ref(),
        Datum::Sym(_, val) => boxed::Sym::new(heap, val.as_ref()).as_any_ref(),
        Datum::List(_, vs) => {
            boxed::List::from_values(heap, vs.iter(), box_syntax_datum).as_any_ref()
        }
        Datum::Vector(_, vs) => {
            boxed::Vector::from_values(heap, vs.iter(), box_syntax_datum).as_any_ref()
        }
        Datum::Set(_, vs) => {
            boxed::Set::from_values(heap, vs.iter(), box_syntax_datum).as_any_ref()
        }
        Datum::Map(_, vs) => boxed::Map::from_values(heap, vs.iter(), |heap, (key, value)| {
            (box_syntax_datum(heap, key), box_syntax_datum(heap, value))
        })
        .as_any_ref(),
    }
}

// This is indirectly tested by `writer`
