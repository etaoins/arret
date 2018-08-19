use std::io::{Result, Write};

use runtime::boxed;
use runtime::boxed::prelude::*;
use runtime::boxed::refs::Gc;

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

/// Writes a representation of the passed box to the writer
pub fn write_boxed(w: &mut dyn Write, heap: &impl AsHeap, any_ref: Gc<boxed::Any>) -> Result<()> {
    use runtime::boxed::AnySubtype;

    match any_ref.as_subtype() {
        AnySubtype::True(_) => write!(w, "true"),
        AnySubtype::False(_) => write!(w, "false"),
        AnySubtype::Nil(_) => write!(w, "()"),
        AnySubtype::Int(int_ref) => write!(w, "{}", int_ref.value()),
        AnySubtype::Sym(sym) => {
            // TODO: We don't support quoted/raw symbols as EDN doesn't
            // This assumes the symbol is a valid identifier
            let interner = heap.as_heap().interner();
            write!(w, "{}", sym.name(interner))
        }
        AnySubtype::Float(_) => {
            unimplemented!("No format for floats defined");
        }
        AnySubtype::TopPair(list) => {
            write!(w, "(")?;
            write_boxed_seq(w, heap, list.as_pair().as_list().iter())?;
            write!(w, ")")
        }
        AnySubtype::TopVector(vec) => {
            write!(w, "[")?;
            write_boxed_seq(w, heap, vec.as_vector().iter().cloned())?;
            write!(w, "]")
        }
        AnySubtype::Char(c) => match c.value() {
            '\n' => write!(w, "\\newline"),
            '\r' => write!(w, "\\return"),
            ' ' => write!(w, "\\space"),
            '\t' => write!(w, "\\tab"),
            '\u{21}'..='\u{126}' => write!(w, "\\{}", c.value()),
            other => write!(w, "\\u{:04X}", other as u32),
        },
        AnySubtype::Str(s) => {
            write!(w, "\"");
            write_escaped_str(w, s.as_str())?;
            write!(w, "\"")
        }
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
        use syntax::parser::datum_from_str;

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
        let mut heap = boxed::Heap::new();
        assert_write(&mut heap, "false", boxed::FALSE_INSTANCE.as_any_ref());
        assert_write(&mut heap, "true", boxed::TRUE_INSTANCE.as_any_ref());
    }

    #[test]
    fn ints() {
        let mut heap = boxed::Heap::new();

        let boxed_zero = boxed::Int::new(&mut heap, 0);
        assert_write(&mut heap, "0", boxed_zero.as_any_ref());

        let boxed_positive = boxed::Int::new(&mut heap, 120);
        assert_write(&mut heap, "120", boxed_positive.as_any_ref());
    }

    #[test]
    fn sym() {
        let mut heap = boxed::Heap::new();

        let boxed_foo = boxed::Sym::new(&mut heap, "foo");
        assert_write(&mut heap, "foo", boxed_foo.as_any_ref());

        let boxed_bar = boxed::Sym::new(&mut heap, "bar");
        assert_write(&mut heap, "bar", boxed_bar.as_any_ref());
    }

    #[test]
    fn lists() {
        let mut heap = boxed::Heap::new();

        let empty_list = boxed::List::<boxed::Int>::from_values(&mut heap, [].iter().cloned());
        assert_write(&mut heap, "()", empty_list.as_any_ref());

        let one_list = boxed::List::<boxed::Int>::from_values(&mut heap, [1].iter().cloned());
        assert_write(&mut heap, "(1)", one_list.as_any_ref());

        let three_list =
            boxed::List::<boxed::Int>::from_values(&mut heap, [1, 2, 3].iter().cloned());
        assert_write(&mut heap, "(1 2 3)", three_list.as_any_ref());
    }

    #[test]
    fn vectors() {
        let mut heap = boxed::Heap::new();

        let empty_vector = boxed::Vector::<boxed::Int>::from_values(&mut heap, [].iter().cloned());
        assert_write(&mut heap, "[]", empty_vector.as_any_ref());

        let one_vector = boxed::Vector::<boxed::Int>::from_values(&mut heap, [1].iter().cloned());
        assert_write(&mut heap, "[1]", one_vector.as_any_ref());

        let three_vector =
            boxed::Vector::<boxed::Int>::from_values(&mut heap, [1, 2, 3].iter().cloned());
        assert_write(&mut heap, "[1 2 3]", three_vector.as_any_ref());
    }

    #[test]
    fn chars() {
        let mut heap = boxed::Heap::new();

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
        let mut heap = boxed::Heap::new();

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
