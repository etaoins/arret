use arret_syntax::parser::datum_from_str;

use arret_runtime::binding::*;
use arret_runtime::boxed;
use arret_runtime::boxed::refs::Gc;
use arret_runtime::task::Task;

use arret_runtime_syntax::reader;

#[arret_rfi_derive::rust_fun("(Str -> Any)")]
pub fn stdlib_read_str(task: &mut Task, edn_str: Gc<boxed::Str>) -> Gc<boxed::Any> {
    let parsed_syntax = datum_from_str(edn_str.as_str()).unwrap();
    reader::box_syntax_datum(task, &parsed_syntax)
}
