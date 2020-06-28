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
            let boxed_elems = vs
                .iter()
                .map(|elem| box_syntax_datum(heap, elem))
                .collect::<Vec<Gc<boxed::Any>>>();

            boxed::List::new(heap, boxed_elems.into_iter()).as_any_ref()
        }
        Datum::Vector(_, vs) => {
            let boxed_elems = vs
                .iter()
                .map(|elem| box_syntax_datum(heap, elem))
                .collect::<Vec<Gc<boxed::Any>>>();

            boxed::Vector::new(heap, boxed_elems.into_iter()).as_any_ref()
        }
        Datum::Set(_, vs) => {
            let boxed_elems = vs
                .iter()
                .map(|elem| box_syntax_datum(heap, elem))
                .collect::<Vec<Gc<boxed::Any>>>();

            boxed::Set::new(heap, boxed_elems.into_iter()).as_any_ref()
        }
        Datum::Map(_, vs) => {
            let boxed_elems = vs
                .iter()
                .map(|(key, value)| (box_syntax_datum(heap, key), box_syntax_datum(heap, value)))
                .collect::<Vec<(Gc<boxed::Any>, Gc<boxed::Any>)>>();

            boxed::Map::new(heap, boxed_elems.into_iter()).as_any_ref()
        }
    }
}

// This is indirectly tested by `writer`
