use crate::boxed::prelude::*;

use std::io::prelude::*;

use arret_runtime::boxed;
use arret_runtime::boxed::refs::Gc;

pub fn pretty_print(write: &mut dyn Write, heap: &impl AsHeap, value: Gc<boxed::Any>) {
    match value.as_subtype() {
        boxed::AnySubtype::Str(string) => {
            write.write_all(string.as_str().as_bytes()).unwrap();
        }
        boxed::AnySubtype::Char(c) => {
            let mut buffer = [0; 4];
            write
                .write_all(c.value().encode_utf8(&mut buffer).as_bytes())
                .unwrap();
        }
        boxed::AnySubtype::Sym(sym) => {
            write
                .write_all(sym.name(heap.as_heap()).as_bytes())
                .unwrap();
        }
        _ => {
            arret_runtime_syntax::writer::write_boxed(write, heap.as_heap(), value).unwrap();
        }
    }
}
