use std::io::{Result, Write};

use arret_runtime::boxed;
use arret_runtime::boxed::prelude::*;
use arret_runtime::boxed::refs::Gc;
use arret_runtime::intern::InternedSym;

macro_rules! process_escaped_chars {
    ($w:ident, $source:ident, $( $pattern:pat => $escape:expr ),*) => {
        // Try to write sequential unescaped characters in chunks
        // This is especially important if $w isn't buffered
        let mut last_escape_end = 0;
        for (index, c) in $source.char_indices() {
            match c {
                $(
                    $pattern => {
                        $w.write_all(&$source.as_bytes()[last_escape_end..index])?;
                        last_escape_end = index + c.len_utf8();
                        ($escape)?;
                    }
                ),* ,
                _ => {}
            };
        }

        $w.write_all(&$source.as_bytes()[last_escape_end..])?;
    }
}

fn write_escaped_str(w: &mut dyn Write, source: &str) -> Result<()> {
    process_escaped_chars!(w, source,
        '\t' => write!(w, "\\t"),
        '\r' => write!(w, "\\r"),
        '\n' => write!(w, "\\n"),
        '\\' => write!(w, "\\\\"),
        '"' => write!(w, "\\\""),
        c @ '\u{0}'..='\u{19}' => write!(w, "\\x{:X};", c as u32)
    );

    Ok(())
}

fn write_boxed_seq(
    w: &mut dyn Write,
    heap: &impl AsHeap,
    elems: impl Iterator<Item = Gc<boxed::Any>>,
) -> Result<()> {
    let mut has_prev = false;
    for elem in elems {
        if has_prev {
            write!(w, " ")?;
        } else {
            has_prev = true;
        }

        write_boxed(w, heap, elem)?;
    }

    Ok(())
}

fn write_char(w: &mut dyn Write, c: char) -> Result<()> {
    match c {
        '\n' => write!(w, "\\newline"),
        '\r' => write!(w, "\\return"),
        ' ' => write!(w, "\\space"),
        '\t' => write!(w, "\\tab"),
        '\u{21}'..='\u{126}' => write!(w, "\\{}", c),
        other => write!(w, "\\u{:04X}", other as u32),
    }
}

#[allow(clippy::float_cmp)]
fn write_float(w: &mut dyn Write, f: f64) -> Result<()> {
    if f.is_nan() {
        write!(w, "##NaN")
    } else if f.is_infinite() {
        if f.is_sign_positive() {
            write!(w, "##Inf")
        } else {
            write!(w, "##-Inf")
        }
    } else if f == 0.0 && f.is_sign_negative() {
        write!(w, "-0.0")
    } else if (f as i64 as f64) == f {
        // This is has no fractional part; force a .0 to mark it as a float
        write!(w, "{:.1}", f)
    } else {
        write!(w, "{:.}", f)
    }
}

fn write_interned_sym(
    w: &mut dyn Write,
    heap: &impl AsHeap,
    interned_sym: InternedSym,
) -> Result<()> {
    // TODO: We don't support quoted/raw symbols as EDN doesn't
    // This assumes the symbol is a valid identifier
    write!(
        w,
        "{}",
        heap.as_heap()
            .type_info()
            .interner()
            .unintern(&interned_sym)
    )
}

fn write_record(w: &mut dyn Write, heap: &impl AsHeap, record: &boxed::Record) -> Result<()> {
    use boxed::FieldValue;

    // TODO: Print our source name
    write!(w, "#record(")?;

    let mut has_prev = false;
    for field in record.field_values(heap.as_heap()) {
        if has_prev {
            write!(w, " ")?;
        } else {
            has_prev = true;
        }

        match field {
            FieldValue::Bool(true) => write!(w, "true")?,
            FieldValue::Bool(false) => write!(w, "false")?,
            FieldValue::Char(c) => write_char(w, c)?,
            FieldValue::Float(f) => write_float(w, f)?,
            FieldValue::Int(i) => write!(w, "{}", i)?,
            FieldValue::InternedSym(interned_sym) => write_interned_sym(w, heap, interned_sym)?,
            FieldValue::Boxed(boxed) => write_boxed(w, heap, boxed)?,
            FieldValue::Callback(_) => write!(w, "#fn")?,
        }
    }

    write!(w, ")")
}

/// Writes a representation of the passed box to the writer
pub fn write_boxed(w: &mut dyn Write, heap: &impl AsHeap, any_ref: Gc<boxed::Any>) -> Result<()> {
    use arret_runtime::boxed::AnySubtype;

    match any_ref.as_subtype() {
        AnySubtype::True(_) => write!(w, "true"),
        AnySubtype::False(_) => write!(w, "false"),
        AnySubtype::Nil(_) => write!(w, "()"),
        AnySubtype::Int(int_ref) => write!(w, "{}", int_ref.value()),
        AnySubtype::Sym(sym) => write_interned_sym(w, heap, sym.interned()),
        AnySubtype::Float(float_ref) => write_float(w, float_ref.value()),
        AnySubtype::Pair(list) => {
            write!(w, "(")?;
            write_boxed_seq(w, heap, list.as_list_ref().iter())?;
            write!(w, ")")
        }
        AnySubtype::Vector(vec) => {
            write!(w, "[")?;
            write_boxed_seq(w, heap, vec.iter().cloned())?;
            write!(w, "]")
        }
        AnySubtype::Char(char_ref) => write_char(w, char_ref.value()),
        AnySubtype::Str(s) => {
            write!(w, "\"")?;
            write_escaped_str(w, s.as_str())?;
            write!(w, "\"")
        }
        AnySubtype::FunThunk(_) => write!(w, "#fn"),
        AnySubtype::Record(record) => write_record(w, heap, record),
    }
}

#[cfg(test)]
mod test {
    use super::*;

    fn string_for_boxed(heap: &boxed::Heap, any_ref: Gc<boxed::Any>) -> String {
        use std::str;

        let mut output_buf: Vec<u8> = vec![];
        write_boxed(&mut output_buf, heap, any_ref).unwrap();
        str::from_utf8(output_buf.as_slice()).unwrap().to_owned()
    }

    fn assert_write(heap: &mut boxed::Heap, expected: &'static str, any_ref: Gc<boxed::Any>) {
        use crate::reader;
        use arret_syntax::parser::datum_from_str;

        let first_output = string_for_boxed(heap, any_ref);
        assert_eq!(expected, first_output);

        // Try to round trip this to make sure our output and tests are sane
        let reparsed_syntax = datum_from_str(&first_output).unwrap();
        let reboxed_ref = reader::box_syntax_datum(heap, &reparsed_syntax);

        let second_output = string_for_boxed(heap, reboxed_ref);
        assert_eq!(expected, second_output);
    }

    #[test]
    fn bools() {
        let mut heap = boxed::Heap::empty();
        assert_write(&mut heap, "false", boxed::FALSE_INSTANCE.as_any_ref());
        assert_write(&mut heap, "true", boxed::TRUE_INSTANCE.as_any_ref());
    }

    #[test]
    fn ints() {
        let mut heap = boxed::Heap::empty();

        let boxed_zero = boxed::Int::new(&mut heap, 0);
        assert_write(&mut heap, "0", boxed_zero.as_any_ref());

        let boxed_positive = boxed::Int::new(&mut heap, 120);
        assert_write(&mut heap, "120", boxed_positive.as_any_ref());

        let boxed_negative = boxed::Int::new(&mut heap, -120);
        assert_write(&mut heap, "-120", boxed_negative.as_any_ref());
    }

    #[test]
    fn floats() {
        let mut heap = boxed::Heap::empty();

        let test_floats = [
            ("0.0", 0.0),
            ("-0.0", -0.0),
            ("120.0", 120.0),
            ("0.25", 0.25),
            ("-120.0", -120.0),
            ("9007199254740992.0", 9_007_199_254_740_992.0),
            ("##NaN", std::f64::NAN),
            ("##Inf", std::f64::INFINITY),
            ("##-Inf", std::f64::NEG_INFINITY),
        ];

        for (expected, f) in &test_floats {
            let boxed_float = boxed::Float::new(&mut heap, *f);
            assert_write(&mut heap, expected, boxed_float.as_any_ref());
        }
    }

    #[test]
    fn sym() {
        let mut heap = boxed::Heap::empty();

        let boxed_foo = boxed::Sym::new(&mut heap, "foo");
        assert_write(&mut heap, "foo", boxed_foo.as_any_ref());

        let boxed_bar = boxed::Sym::new(&mut heap, "bar");
        assert_write(&mut heap, "bar", boxed_bar.as_any_ref());
    }

    #[test]
    fn lists() {
        let mut heap = boxed::Heap::empty();

        let empty_list = boxed::List::from_values(&mut heap, [].iter().cloned(), boxed::Int::new);
        assert_write(&mut heap, "()", empty_list.as_any_ref());

        let one_list = boxed::List::from_values(&mut heap, [1].iter().cloned(), boxed::Int::new);
        assert_write(&mut heap, "(1)", one_list.as_any_ref());

        let three_list =
            boxed::List::from_values(&mut heap, [1, 2, 3].iter().cloned(), boxed::Int::new);
        assert_write(&mut heap, "(1 2 3)", three_list.as_any_ref());
    }

    #[test]
    fn vectors() {
        let mut heap = boxed::Heap::empty();

        let empty_vector =
            boxed::Vector::from_values(&mut heap, [].iter().cloned(), boxed::Int::new);
        assert_write(&mut heap, "[]", empty_vector.as_any_ref());

        let one_vector =
            boxed::Vector::from_values(&mut heap, [1].iter().cloned(), boxed::Int::new);
        assert_write(&mut heap, "[1]", one_vector.as_any_ref());

        let three_vector =
            boxed::Vector::from_values(&mut heap, [1, 2, 3].iter().cloned(), boxed::Int::new);
        assert_write(&mut heap, "[1 2 3]", three_vector.as_any_ref());
    }

    #[test]
    fn chars() {
        let mut heap = boxed::Heap::empty();

        let test_chars = [
            ("\\newline", '\n'),
            ("\\return", '\r'),
            ("\\space", ' '),
            ("\\tab", '\t'),
            ("\\a", 'a'),
            ("\\A", 'A'),
            ("\\(", '('),
            ("\\u03BB", '\u{03bb}'),
        ];

        for (expected, c) in &test_chars {
            let boxed_char = boxed::Char::new(&mut heap, *c);
            assert_write(&mut heap, expected, boxed_char.as_any_ref());
        }
    }

    #[test]
    fn strings() {
        let mut heap = boxed::Heap::empty();

        let test_strings = [
            (r#""""#, ""),
            (r#""Hello, world!""#, "Hello, world!"),
            (r#""Hello\"World""#, "Hello\"World"),
            (r#""Hello\\World""#, "Hello\\World"),
            (r#""Tab\t""#, "Tab\t"),
            (r#""\n\nnewline""#, "\n\nnewline"),
            (r#""carriage: \r""#, "carriage: \r"),
            (r#""lλ""#, "lλ"),
            (r#""\x0;null!""#, "\u{0}null!"),
            (
                r#""The word \"recursion\" has many meanings.""#,
                r#"The word "recursion" has many meanings."#,
            ),
        ];

        for (expected, s) in &test_strings {
            let boxed_char = boxed::Str::new(&mut heap, *s);
            assert_write(&mut heap, expected, boxed_char.as_any_ref());
        }
    }
}
