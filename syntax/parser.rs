use crate::datum::Datum;
use crate::error::{Error, ErrorKind, ExpectedContent, Result};
use crate::span::Span;
use std;

pub fn data_from_str_with_span_offset(s: &str, span_offset: usize) -> Result<Vec<Datum>> {
    let mut parser = Parser::from_str(s, span_offset);
    parser.parse_data()
}

pub struct Parser<'de> {
    input: &'de str,
    consumed_bytes: usize,
}

fn is_whitespace(c: char) -> bool {
    match c {
        ',' | ' ' | '\n' | '\t' | '\r' => true,
        _ => false,
    }
}

pub fn is_identifier_char(c: char) -> bool {
    match c {
        'A'..='Z' | 'a'..='z' | '0'..='9' |
        // Punctuation allowed at beginning of an identiifer
        '.' | '*' | '+' | '!' | '-' | '_' | '?' | '$' | '%' | '&' | '=' | '<' | '>' | ':' |
        // Punctuation allowed anywhere
        '#' |
        // We don't support namespacing so we treat this as a normal char
        '/' => true,
        _ => false,
    }
}

fn u64_to_positive_i64(span: Span, i: u64) -> Result<i64> {
    if i > (1 << 63) - 1 {
        Err(Error::new(span, ErrorKind::IntegerOverflow))
    } else {
        Ok(i as i64)
    }
}

impl<'de> Parser<'de> {
    fn from_str(input: &'de str, consumed_bytes: usize) -> Self {
        Parser {
            input,
            consumed_bytes,
        }
    }

    fn eof_err(&self, ec: ExpectedContent) -> Error {
        let eof_pos = (self.consumed_bytes + self.input.len()) as u32;

        Error::new(
            Span {
                lo: eof_pos,
                hi: eof_pos,
            },
            ErrorKind::Eof(ec),
        )
    }

    fn peek_char(&mut self, ec: ExpectedContent) -> Result<char> {
        self.input.chars().next().ok_or_else(|| self.eof_err(ec))
    }

    fn peek_nth_char(&mut self, i: usize, ec: ExpectedContent) -> Result<char> {
        self.input.chars().nth(i).ok_or_else(|| self.eof_err(ec))
    }

    fn eat_bytes(&mut self, count: usize) {
        self.input = &self.input[count..];
        self.consumed_bytes += count;
    }

    fn consume_char(&mut self, ec: ExpectedContent) -> Result<char> {
        let mut char_indices = self.input.char_indices();

        match char_indices.next() {
            Some((_, c)) => {
                let next_index = char_indices
                    .next()
                    .map(|t| t.0)
                    .unwrap_or_else(|| self.input.len());
                self.eat_bytes(next_index);

                Ok(c)
            }

            None => Err(self.eof_err(ec)),
        }
    }

    fn skip_until_non_whitespace(&mut self, ec: ExpectedContent) -> Result<char> {
        loop {
            self.consume_until(|c| !is_whitespace(c) || c == ';' || c == '#');

            match self.peek_char(ec)? {
                ';' => {
                    self.consume_until(|c| c == '\n');
                }
                '#' => {
                    match self.peek_nth_char(1, ec) {
                        Ok('_') => {
                            // Discard the #_ and the following datum
                            self.eat_bytes(2);
                            self.parse_datum()?;
                        }
                        _ => {
                            break Ok('#');
                        }
                    }
                }
                other => {
                    break Ok(other);
                }
            }
        }
    }

    fn consume_until<T>(&mut self, predicate: T) -> (Span, &str)
    where
        T: FnMut(char) -> bool,
    {
        let lo = self.consumed_bytes as u32;
        let last_index = self
            .input
            .find(predicate)
            .unwrap_or_else(|| self.input.len());
        let (consumed, remaining_input) = self.input.split_at(last_index);

        self.input = remaining_input;
        self.consumed_bytes += last_index;

        let span = Span {
            lo,
            hi: (self.consumed_bytes as u32),
        };

        (span, consumed)
    }

    fn capture_span<F, R>(&mut self, block: F) -> (Span, R)
    where
        F: FnOnce(&mut Parser<'_>) -> R,
    {
        let lo = self.consumed_bytes as u32;
        let result = block(self);
        let hi = self.consumed_bytes as u32;

        (Span { lo, hi }, result)
    }

    fn parse_int(&mut self) -> Result<Datum> {
        let (span, digits) = self.consume_until(|c| !c.is_ascii_digit());

        u64::from_str_radix(digits, 10)
            .map_err(|_| Error::new(span, ErrorKind::IntegerOverflow))
            .and_then(|content| u64_to_positive_i64(span, content))
            .map(|i| Datum::Int(span, i))
    }

    fn parse_negative_or_symbol(&mut self) -> Result<Datum> {
        match self.peek_nth_char(1, ExpectedContent::Identifier) {
            Ok(digit) if digit.is_ascii_digit() => {
                // Let the `-` through as the first character
                let mut is_leading_minus = true;
                let (span, digits) = self.consume_until(|c| {
                    if is_leading_minus {
                        is_leading_minus = false;
                        false
                    } else {
                        !c.is_ascii_digit()
                    }
                });

                let value = i64::from_str_radix(digits, 10)
                    .map_err(|_| Error::new(span, ErrorKind::IntegerOverflow))?;

                Ok(Datum::Int(span, value))
            }
            Ok(_)
            | Err(Error {
                kind: ErrorKind::Eof(_),
                ..
            }) => self.parse_identifier(),
            Err(other) => Err(other),
        }
    }

    fn parse_char(&mut self) -> Result<Datum> {
        let (span, c) = self.capture_span(|s| {
            // Consume the \
            s.eat_bytes(1);

            // Consume the characer name
            let (span, char_name) =
                s.consume_until(|c| c == ')' || c == ']' || c == '}' || is_whitespace(c));

            let mut char_name_chars = char_name.chars();
            if let Some(first_char) = char_name_chars.next() {
                if char_name_chars.next().is_none() {
                    // There is only a single character; return it
                    return Ok(first_char);
                }

                if first_char == 'u' {
                    // This is a hex code point
                    let hex_string = &char_name[1..];
                    let code_point = u32::from_str_radix(hex_string, 16)
                        .map_err(|_| Error::new(span, ErrorKind::UnsupportedChar))?;

                    return std::char::from_u32(code_point)
                        .ok_or_else(|| Error::new(span, ErrorKind::InvalidCodePoint));
                }
            }

            match char_name {
                "newline" => Ok('\n'),
                "return" => Ok('\r'),
                "space" => Ok(' '),
                "tab" => Ok('\t'),
                _ => Err(Error::new(span, ErrorKind::UnsupportedChar)),
            }
        });

        c.map(|c| Datum::Char(span, c))
    }

    fn parse_dispatch(&mut self) -> Result<Datum> {
        // Consume the #
        // This means we need to adjust our spans below to cover it for reporting
        self.eat_bytes(1);

        match self.peek_char(ExpectedContent::Dispatch)? {
            '{' => self.parse_set(),
            _ => {
                let (span, _) = self.capture_span(|s| s.consume_char(ExpectedContent::Dispatch));
                let adj_span = span.with_lo(span.lo - 1);

                Err(Error::new(adj_span, ErrorKind::UnsupportedDispatch))
            }
        }
    }

    fn parse_seq<F>(&mut self, terminator: char, make_ec: F) -> Result<Box<[Datum]>>
    where
        F: FnOnce(Span) -> ExpectedContent,
    {
        // Consume the opening bracket
        let (open_bracket_span, _) = self.capture_span(|s| {
            s.eat_bytes(1);
        });
        let ec = make_ec(open_bracket_span);

        let mut content = Vec::new();

        // Keep eating datums until we hit the terminator
        loop {
            let next_char = self.skip_until_non_whitespace(ec)?;
            if next_char == terminator {
                // End of the sequence
                self.eat_bytes(1);
                break Ok(content.into_boxed_slice());
            } else {
                content.push(self.parse_datum_starting_with(next_char)?);
            }
        }
    }

    fn parse_list(&mut self) -> Result<Datum> {
        let (outer_span, contents) = self.capture_span(|s| s.parse_seq(')', ExpectedContent::List));

        contents.map(|contents| Datum::List(outer_span, contents))
    }

    fn parse_vector(&mut self) -> Result<Datum> {
        let (outer_span, contents) =
            self.capture_span(|s| s.parse_seq(']', ExpectedContent::Vector));

        contents.map(|contents| Datum::Vector(outer_span, contents))
    }

    fn parse_map(&mut self) -> Result<Datum> {
        // First get the contents without splitting pairwise
        let (span, unpaired_contents) =
            self.capture_span(|s| s.parse_seq('}', ExpectedContent::Map));

        let unpaired_contents = unpaired_contents?;

        if unpaired_contents.len() % 2 == 1 {
            return Err(Error::new(span, ErrorKind::UnevenMap));
        }

        let contents = unpaired_contents
            .chunks(2)
            .map(|chunk| (chunk[0].clone(), chunk[1].clone()))
            .collect::<Vec<(Datum, Datum)>>()
            .into_boxed_slice();

        Ok(Datum::Map(span, contents))
    }

    fn parse_set(&mut self) -> Result<Datum> {
        let (outer_span, contents) = self.capture_span(|s| s.parse_seq('}', ExpectedContent::Set));

        // Cover the # in our span
        let adj_span = outer_span.with_lo(outer_span.lo - 1);
        contents.map(|contents| Datum::Set(adj_span, contents))
    }

    fn parse_quote_escape(&mut self) -> Result<char> {
        let escape_lo = self.consumed_bytes as u32;

        match self.consume_char(ExpectedContent::QuoteEscape)? {
            't' => Ok('\t'),
            'r' => Ok('\r'),
            'n' => Ok('\n'),
            '\\' => Ok('\\'),
            '"' => Ok('"'),
            'x' => {
                let (span, hex_string) = self.consume_until(|c| c == ';');
                let code_point = u32::from_str_radix(hex_string, 16);

                let code_point =
                    code_point.map_err(|_| Error::new(span, ErrorKind::UnsupportedChar))?;

                if self.consume_char(ExpectedContent::CodePoint)? != ';' {
                    return Err(Error::new(span, ErrorKind::UnsupportedChar));
                }

                std::char::from_u32(code_point)
                    .ok_or_else(|| Error::new(span, ErrorKind::InvalidCodePoint))
            }
            _ => {
                let span = Span {
                    lo: escape_lo,
                    hi: self.consumed_bytes as u32,
                };
                Err(Error::new(span, ErrorKind::UnsupportedStringEscape))
            }
        }
    }

    fn parse_string(&mut self) -> Result<Datum> {
        let (span, contents) = self.capture_span(|s| {
            let (open_quote_span, _) = s.capture_span(|s| {
                // Eat the opening quote
                s.eat_bytes(1);
            });

            let mut contents = String::new();
            loop {
                match s.consume_char(ExpectedContent::String(open_quote_span))? {
                    '"' => {
                        return Ok(contents);
                    }
                    '\\' => contents.push(s.parse_quote_escape()?),
                    other => {
                        contents.push(other);
                    }
                }
            }
        });
        contents.map(|contents| Datum::Str(span, contents.into_boxed_str()))
    }

    fn parse_identifier(&mut self) -> Result<Datum> {
        let (span, content) = self.consume_until(|c| !is_identifier_char(c));

        if content.is_empty() {
            let (span, next_char) =
                self.capture_span(|s| s.consume_char(ExpectedContent::Identifier));
            return Err(Error::new(span, ErrorKind::UnexpectedChar(next_char?)));
        }

        match content {
            "true" => Ok(Datum::Bool(span, true)),
            "false" => Ok(Datum::Bool(span, false)),
            _ => Ok(Datum::Sym(span, content.into())),
        }
    }

    fn parse_symbol_shorthand(&mut self, expansion: &str) -> Result<Datum> {
        let (outer_span, (shorthand_span, quoted_datum)) = self.capture_span(|s| {
            let (shorthand_span, _) = s.capture_span(|s| {
                // Discard the shorthand. Note this must be ASCII.
                s.eat_bytes(1);
            });

            (shorthand_span, s.parse_datum())
        });

        quoted_datum.map(|quoted_datum| {
            Datum::List(
                outer_span,
                Box::new([Datum::Sym(shorthand_span, expansion.into()), quoted_datum]),
            )
        })
    }

    fn parse_datum_starting_with(&mut self, c: char) -> Result<Datum> {
        match c {
            '(' => self.parse_list(),
            '[' => self.parse_vector(),
            '{' => self.parse_map(),
            '0'..='9' => self.parse_int(),
            '-' => self.parse_negative_or_symbol(),
            '\'' => self.parse_symbol_shorthand("quote"),
            '"' => self.parse_string(),
            '\\' => self.parse_char(),
            '#' => self.parse_dispatch(),
            _ => self.parse_identifier(),
        }
    }

    fn parse_datum(&mut self) -> Result<Datum> {
        let start_char = self.skip_until_non_whitespace(ExpectedContent::Datum)?;
        self.parse_datum_starting_with(start_char)
    }

    fn parse_data(&mut self) -> Result<Vec<Datum>> {
        let mut datum_vec = Vec::new();

        // Keep eating datums until we hit EOF
        loop {
            match self.parse_datum() {
                Ok(datum) => {
                    datum_vec.push(datum);
                }
                Err(err) => {
                    if let ErrorKind::Eof(_) = err.kind() {
                        break Ok(datum_vec);
                    } else {
                        break Err(err);
                    }
                }
            }
        }
    }
}

/////////

pub fn datum_from_str(s: &str) -> Result<Datum> {
    let mut parser = Parser::from_str(s, 0);
    parser.parse_datum()
}

pub fn data_from_str(s: &str) -> Result<Vec<Datum>> {
    data_from_str_with_span_offset(s, 0)
}

#[cfg(test)]
mod test {
    use super::*;
    use crate::span::t2s;

    fn whole_str_span(v: &str) -> Span {
        Span {
            lo: 0,
            hi: v.len() as u32,
        }
    }
    #[test]
    fn bool_datum() {
        let j = "false";
        let t = "^^^^^";
        let expected = Datum::Bool(t2s(t), false);

        assert_eq!(expected, datum_from_str(j).unwrap());

        let j = "true";
        let t = "^^^^";
        let expected = Datum::Bool(t2s(t), true);
        assert_eq!(expected, datum_from_str(j).unwrap());

        let j = "     false";
        let t = "     ^^^^^";
        let expected = Datum::Bool(t2s(t), false);
        assert_eq!(expected, datum_from_str(j).unwrap());

        let j = "\ttrue\t";
        let t = "\t^^^^\t";
        let expected = Datum::Bool(t2s(t), true);
        assert_eq!(expected, datum_from_str(j).unwrap());

        let j = " trueorfalse  ";
        let t = " ^^^^^^^^^^^  ";
        let expected = Datum::Sym(t2s(t), "trueorfalse".into());
        assert_eq!(expected, datum_from_str(j).unwrap());
    }

    #[test]
    fn list_datum() {
        let j = "() ; with a comment";
        let t = "^^                 ";
        let expected = Datum::List(t2s(t), Box::new([]));
        assert_eq!(expected, datum_from_str(j).unwrap());

        let j = "( true   false )";
        let t = "^^^^^^^^^^^^^^^^";
        let u = "  ^^^^          ";
        let v = "         ^^^^^  ";

        let expected = Datum::List(
            t2s(t),
            Box::new([Datum::Bool(t2s(u), true), Datum::Bool(t2s(v), false)]),
        );
        assert_eq!(expected, datum_from_str(j).unwrap());

        let j = "(1, 2, (3))";
        let t = "^^^^^^^^^^^";
        let u = " ^         ";
        let v = "    ^      ";
        let w = "       ^^^ ";
        let x = "        ^  ";

        let expected = Datum::List(
            t2s(t),
            Box::new([
                Datum::Int(t2s(u), 1),
                Datum::Int(t2s(v), 2),
                Datum::List(t2s(w), Box::new([Datum::Int(t2s(x), 3)])),
            ]),
        );
        assert_eq!(expected, datum_from_str(j).unwrap());

        let j = "(true";
        let t = "    >";
        let u = "^    ";
        let err = Error::new(t2s(t), ErrorKind::Eof(ExpectedContent::List(t2s(u))));
        assert_eq!(err, datum_from_str(j).unwrap_err());

        let j = ")";
        let t = "^";
        let err = Error::new(t2s(t), ErrorKind::UnexpectedChar(')'));
        assert_eq!(err, datum_from_str(j).unwrap_err());
    }

    #[test]
    fn vector_datum() {
        let j = "  []";
        let t = "  ^^";
        let expected = Datum::Vector(t2s(t), Box::new([]));
        assert_eq!(expected, datum_from_str(j).unwrap());

        let j = "[ true   (true false) ]";
        let t = "^^^^^^^^^^^^^^^^^^^^^^^";
        let u = "  ^^^^                 ";
        let v = "         ^^^^^^^^^^^^  ";
        let w = "          ^^^^         ";
        let x = "               ^^^^^   ";

        let expected = Datum::Vector(
            t2s(t),
            Box::new([
                Datum::Bool(t2s(u), true),
                Datum::List(
                    t2s(v),
                    Box::new([Datum::Bool(t2s(w), true), Datum::Bool(t2s(x), false)]),
                ),
            ]),
        );
        assert_eq!(expected, datum_from_str(j).unwrap());

        let j = "[true []";
        let t = "       >";
        let u = "^       ";
        let err = Error::new(t2s(t), ErrorKind::Eof(ExpectedContent::Vector(t2s(u))));
        assert_eq!(err, datum_from_str(j).unwrap_err());

        let j = "]";
        let t = "^";
        let err = Error::new(t2s(t), ErrorKind::UnexpectedChar(']'));
        assert_eq!(err, datum_from_str(j).unwrap_err());
    }

    #[test]
    fn symbol_datum() {
        for &test_symbol in &[
            "HELLO",
            "HELLO123",
            "predicate?",
            "mutate!",
            "from->to",
            "!$%&*+-./:<=>?",
            "+",
            "-",
        ] {
            let s = whole_str_span(test_symbol);
            let expected = Datum::Sym(s, test_symbol.into());

            assert_eq!(expected, datum_from_str(test_symbol).unwrap());
        }
    }

    #[test]
    fn keyword_symbol_datum() {
        for &test_symbol in &[":HELLO", ":HELLO123", ":predicate?", ":mutate!"] {
            let s = whole_str_span(test_symbol);
            let expected = Datum::Sym(s, test_symbol.into());

            assert_eq!(expected, datum_from_str(test_symbol).unwrap());
        }
    }

    #[test]
    fn string_datum() {
        let test_strings = [
            (r#""""#, ""),
            (r#""Hello, world!""#, "Hello, world!"),
            (r#""Hello\"World""#, "Hello\"World"),
            (r#""Hello\\World""#, "Hello\\World"),
            (r#""Tab\t""#, "Tab\t"),
            (r#""\nnewline""#, "\nnewline"),
            (r#""carriage: \r""#, "carriage: \r"),
            (r#""Space\x20;Bar""#, "Space Bar"),
            (r#""l\x03BB;""#, "l\u{03bb}"),
            (r#""\x0;null!""#, "\u{0000}null!"),
            (
                r#""The word \"recursion\" has many meanings.""#,
                r#"The word "recursion" has many meanings."#,
            ),
        ];

        for (test_string, expected_contents) in &test_strings {
            let s = whole_str_span(test_string);
            let expected = Datum::Str(s, (*expected_contents).into());

            assert_eq!(expected, datum_from_str(test_string).unwrap());
        }

        let j = r#" "foo "#;
        let t = r#"     >"#;
        let u = r#" ^    "#;
        let err = Error::new(t2s(t), ErrorKind::Eof(ExpectedContent::String(t2s(u))));
        assert_eq!(err, datum_from_str(j).unwrap_err());

        let j = r#""\p""#;
        let t = r#"  ^ "#;
        let err = Error::new(t2s(t), ErrorKind::UnsupportedStringEscape);
        assert_eq!(err, datum_from_str(j).unwrap_err());
    }

    #[test]
    fn char_datum() {
        let test_chars = [
            ("\\newline", '\u{0a}'),
            ("\\return", '\u{0d}'),
            ("\\space", '\u{20}'),
            ("\\tab", '\u{09}'),
            ("\\a", 'a'),
            ("\\A", 'A'),
            ("\\(", '('),
            ("\\☃", '\u{2603}'),
            ("\\u03BB", '\u{03bb}'),
        ];

        for (j, expected_char) in &test_chars {
            let s = whole_str_span(j);
            let expected = Datum::Char(s, *expected_char);

            assert_eq!(expected, datum_from_str(j).unwrap());
        }

        let j = r#"\SPACE"#;
        let t = r#" ^^^^^"#;
        let err = Error::new(t2s(t), ErrorKind::UnsupportedChar);
        assert_eq!(err, datum_from_str(j).unwrap_err());

        let j = r#"\u110000"#;
        let t = r#" ^^^^^^^"#;
        let err = Error::new(t2s(t), ErrorKind::InvalidCodePoint);
        assert_eq!(err, datum_from_str(j).unwrap_err());

        let j = r#"[\newline]"#;
        let t = r#" ^^^^^^^^ "#;
        let expected = Datum::Vector(whole_str_span(j), Box::new([Datum::Char(t2s(t), '\n')]));
        assert_eq!(expected, datum_from_str(j).unwrap());
    }

    #[allow(clippy::unreadable_literal)]
    #[test]
    fn int_datum() {
        let test_ints = [
            ("0", 0),
            ("000", 0),
            ("1000", 1000),
            ("-1000", -1000),
            ("9223372036854775807", 9223372036854775807),
            ("-9223372036854775808", -9223372036854775808),
        ];

        for &(j, expected_int) in &test_ints {
            let s = whole_str_span(j);
            let expected = Datum::Int(s, expected_int);

            assert_eq!(expected, datum_from_str(j).unwrap());
        }

        let j = "10223372036854775807";
        let t = "^^^^^^^^^^^^^^^^^^^^";
        let err = Error::new(t2s(t), ErrorKind::IntegerOverflow);
        assert_eq!(err, datum_from_str(j).unwrap_err());

        let j = "-10223372036854775807";
        let t = "^^^^^^^^^^^^^^^^^^^^^";
        let err = Error::new(t2s(t), ErrorKind::IntegerOverflow);
        assert_eq!(err, datum_from_str(j).unwrap_err());

        let j = "4545894549584910223372036854775807";
        let t = "^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^";
        let err = Error::new(t2s(t), ErrorKind::IntegerOverflow);
        assert_eq!(err, datum_from_str(j).unwrap_err());
    }

    #[test]
    fn map_datum() {
        let j = "{}";
        let t = "^^";
        let expected = Datum::Map(t2s(t), Box::new([]));
        assert_eq!(expected, datum_from_str(j).unwrap());

        let j = "{ 1,2 ,, 3  4}";
        let t = "^^^^^^^^^^^^^^";
        let u = "  ^           ";
        let v = "    ^         ";
        let w = "         ^    ";
        let x = "            ^ ";

        let expected_contents = Box::new([
            (Datum::Int(t2s(u), 1), Datum::Int(t2s(v), 2)),
            (Datum::Int(t2s(w), 3), Datum::Int(t2s(x), 4)),
        ]);
        let expected = Datum::Map(t2s(t), expected_contents);

        assert_eq!(expected, datum_from_str(j).unwrap());

        let j = "{1 {2 3}}";
        let t = "^^^^^^^^^";
        let u = " ^       ";
        let v = "   ^^^^^ ";
        let w = "    ^    ";
        let x = "      ^  ";

        let inner_contents = Box::new([(Datum::Int(t2s(w), 2), Datum::Int(t2s(x), 3))]);
        let inner = Datum::Map(t2s(v), inner_contents);

        let outer_contents = Box::new([(Datum::Int(t2s(u), 1), inner)]);
        let expected = Datum::Map(t2s(t), outer_contents);

        assert_eq!(expected, datum_from_str(j).unwrap());

        let j = "{1}";
        let t = "^^^";
        let err = Error::new(t2s(t), ErrorKind::UnevenMap);
        assert_eq!(err, datum_from_str(j).unwrap_err());
    }

    #[test]
    fn set_datum() {
        let j = "#{}";
        let t = "^^^";
        let expected = Datum::Set(t2s(t), Box::new([]));
        assert_eq!(expected, datum_from_str(j).unwrap());

        let j = "#{ 1 2  3 4}";
        let t = "^^^^^^^^^^^^";
        let u = "   ^        ";
        let v = "     ^      ";
        let w = "        ^   ";
        let x = "          ^ ";

        let expected_contents = Box::new([
            Datum::Int(t2s(u), 1),
            Datum::Int(t2s(v), 2),
            Datum::Int(t2s(w), 3),
            Datum::Int(t2s(x), 4),
        ]);
        let expected = Datum::Set(t2s(t), expected_contents);

        assert_eq!(expected, datum_from_str(j).unwrap());

        let j = "#{1 #{2 3}}";
        let t = "^^^^^^^^^^^";
        let u = "  ^        ";
        let v = "    ^^^^^^ ";
        let w = "      ^    ";
        let x = "        ^  ";

        let inner_contents = Box::new([(Datum::Int(t2s(w), 2)), (Datum::Int(t2s(x), 3))]);
        let inner = Datum::Set(t2s(v), inner_contents);

        let outer_contents = Box::new([Datum::Int(t2s(u), 1), inner]);
        let expected = Datum::Set(t2s(t), outer_contents);

        assert_eq!(expected, datum_from_str(j).unwrap());
    }

    #[test]
    fn quote_shorthand() {
        let j = "'foo";
        let t = "^^^^";
        let u = "^   ";
        let v = " ^^^";

        let expected = Datum::List(
            t2s(t),
            Box::new([
                Datum::Sym(t2s(u), "quote".into()),
                Datum::Sym(t2s(v), "foo".into()),
            ]),
        );
        assert_eq!(expected, datum_from_str(j).unwrap());

        let j = "' (1 2 3)";
        let t = "^^^^^^^^^";
        let u = "^        ";
        let v = "  ^^^^^^^";
        let w = "   ^     ";
        let x = "     ^   ";
        let y = "       ^ ";

        let expected = Datum::List(
            t2s(t),
            Box::new([
                Datum::Sym(t2s(u), "quote".into()),
                Datum::List(
                    t2s(v),
                    Box::new([
                        Datum::Int(t2s(w), 1),
                        Datum::Int(t2s(x), 2),
                        Datum::Int(t2s(y), 3),
                    ]),
                ),
            ]),
        );
        assert_eq!(expected, datum_from_str(j).unwrap());

        let j = "'";
        let t = ">";
        let err = Error::new(t2s(t), ErrorKind::Eof(ExpectedContent::Datum));
        assert_eq!(err, datum_from_str(j).unwrap_err());
    }

    #[test]
    fn unsupported_dispatch() {
        let j = r#"#loop"#;
        let t = r#"^^   "#;
        let err = Error::new(t2s(t), ErrorKind::UnsupportedDispatch);

        assert_eq!(err, datum_from_str(j).unwrap_err());

        let j = "#";
        let t = ">";
        let err = Error::new(t2s(t), ErrorKind::Eof(ExpectedContent::Dispatch));

        assert_eq!(err, datum_from_str(j).unwrap_err());
    }

    #[test]
    fn datum_comment() {
        let j = "(Hello #_(you jerk))";
        let t = "^^^^^^^^^^^^^^^^^^^^";
        let u = " ^^^^^              ";

        let expected = Datum::List(t2s(t), Box::new([Datum::Sym(t2s(u), "Hello".into())]));
        assert_eq!(expected, datum_from_str(j).unwrap());

        let j = "(Hello #_  you jerk)";
        let t = "^^^^^^^^^^^^^^^^^^^^";
        let u = " ^^^^^              ";
        let v = "               ^^^^ ";

        let expected = Datum::List(
            t2s(t),
            Box::new([
                Datum::Sym(t2s(u), "Hello".into()),
                Datum::Sym(t2s(v), "jerk".into()),
            ]),
        );
        assert_eq!(expected, datum_from_str(j).unwrap());
    }

    #[test]
    fn multiple_data() {
        let j = " 1  #_two 3  ";
        let t = " ^           ";
        let u = "          ^  ";

        let expected = vec![Datum::Int(t2s(t), 1), Datum::Int(t2s(u), 3)];
        assert_eq!(expected, data_from_str(j).unwrap());

        let j = "(true)))";
        let t = "      ^ ";
        let err = Error::new(t2s(t), ErrorKind::UnexpectedChar(')'));
        assert_eq!(err, data_from_str(j).unwrap_err());
    }
}
