use crate::datum::Datum;
use crate::error::{Error, ErrorKind, Result, WithinContext};
use crate::span::{ByteIndex, FileId, Span};

pub fn data_from_str_with_span_offset(
    file_id: Option<FileId>,
    s: &str,
    span_offset: ByteIndex,
) -> Result<Vec<Datum>> {
    Parser::from_str(file_id, s, span_offset).parse_data()
}

pub fn data_from_str(file_id: Option<FileId>, s: &str) -> Result<Vec<Datum>> {
    data_from_str_with_span_offset(file_id, s, 0)
}

pub fn datum_from_str_with_span_offset(
    file_id: Option<FileId>,
    s: &str,
    span_offset: ByteIndex,
) -> Result<Datum> {
    Parser::from_str(file_id, s, span_offset).parse_datum()
}

pub fn datum_from_str(file_id: Option<FileId>, s: &str) -> Result<Datum> {
    datum_from_str_with_span_offset(file_id, s, 0)
}

fn is_whitespace(c: char) -> bool {
    matches!(c, ',' | ' ' | '\n' | '\t' | '\r')
}

pub fn is_identifier_char(c: char) -> bool {
    matches!(c,
        'A'..='Z' | 'a'..='z' | '0'..='9' |
        // Punctuation allowed at beginning of an identifier
        '.' | '*' | '+' | '!' | '-' | '_' | '?' | '$' | '%' | '&' | '=' | '<' | '>' | ':' |
        // Punctuation allowed anywhere
        '#' |
        // We don't support namespacing so we treat this as a normal char
        '/'
    )
}

pub struct Parser<'input> {
    file_id: Option<FileId>,
    input: &'input str,
    consumed_bytes: ByteIndex,
}

impl<'input> Parser<'input> {
    fn from_str(file_id: Option<FileId>, input: &'input str, span_offset: ByteIndex) -> Self {
        Parser {
            file_id,
            input,
            consumed_bytes: span_offset,
        }
    }

    fn eof_err(&self, within: WithinContext) -> Error {
        let eof_pos = self.consumed_bytes + (self.input.len() as ByteIndex);

        Error::new(
            Span::new(self.file_id, eof_pos, eof_pos),
            ErrorKind::Eof(within),
        )
    }

    fn peek_char(&mut self, within: WithinContext) -> Result<char> {
        self.input
            .chars()
            .next()
            .ok_or_else(|| self.eof_err(within))
    }

    fn peek_nth_char(&mut self, i: usize, within: WithinContext) -> Result<char> {
        self.input
            .chars()
            .nth(i)
            .ok_or_else(|| self.eof_err(within))
    }

    fn eat_bytes(&mut self, count: usize) {
        self.input = &self.input[count..];
        self.consumed_bytes += count as ByteIndex;
    }

    fn consume_char(&mut self, within: WithinContext) -> Result<char> {
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

            None => Err(self.eof_err(within)),
        }
    }

    fn skip_until_non_whitespace(&mut self, within: WithinContext) -> Result<char> {
        loop {
            self.consume_while(is_whitespace);

            match self.peek_char(within)? {
                ';' => {
                    self.consume_until(|c| c == '\n');
                }
                '#' => {
                    match self.peek_nth_char(1, within) {
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
        let start = self.consumed_bytes;
        let last_index = self
            .input
            .find(predicate)
            .unwrap_or_else(|| self.input.len());
        let (consumed, remaining_input) = self.input.split_at(last_index);

        self.input = remaining_input;
        self.consumed_bytes += last_index as ByteIndex;

        (
            Span::new(self.file_id, start, self.consumed_bytes),
            consumed,
        )
    }

    fn consume_while<T>(&mut self, mut predicate: T) -> (Span, &str)
    where
        T: FnMut(char) -> bool,
    {
        self.consume_until(|c| !predicate(c))
    }

    fn capture_span<F, R>(&mut self, block: F) -> (Span, R)
    where
        F: FnOnce(&mut Parser<'_>) -> R,
    {
        let start = self.consumed_bytes;
        let result = block(self);
        let end = self.consumed_bytes;

        (Span::new(self.file_id, start, end), result)
    }

    fn parse_num(&mut self) -> Result<Datum> {
        enum State {
            Sign,
            Whole,
            Fractional,
        }

        let mut state: State = State::Sign;

        let (span, digits) = self.consume_while(|c| match state {
            State::Sign => match c {
                '+' | '-' | '0'..='9' => {
                    state = State::Whole;
                    true
                }
                _ => false,
            },
            State::Whole => match c {
                '.' => {
                    state = State::Fractional;
                    true
                }
                '0'..='9' => true,
                _ => false,
            },
            State::Fractional => matches!(c, '0'..='9'),
        });

        match state {
            State::Sign => Err(Error::new(span, ErrorKind::InvalidFloat)),

            State::Whole => digits
                .parse::<i64>()
                .map_err(|_| Error::new(span, ErrorKind::IntegerOverflow))
                .map(|i| Datum::Int(span, i)),

            State::Fractional => digits
                .parse::<f64>()
                .map_err(|_| Error::new(span, ErrorKind::InvalidFloat))
                .map(|f| Datum::Float(span, f)),
        }
    }

    fn parse_symbolic_float(&mut self) -> Result<Datum> {
        let (span, symbolic_name) = self.consume_while(is_identifier_char);

        let float_value = match symbolic_name {
            "#NaN" => std::f64::NAN,
            "#Inf" => std::f64::INFINITY,
            "#-Inf" => std::f64::NEG_INFINITY,
            _ => {
                return Err(Error::new(span, ErrorKind::UnsupportedDispatch));
            }
        };

        Ok(Datum::Float(
            // Cover the initial #
            Span::new(self.file_id, span.start() - 1, span.end()),
            float_value,
        ))
    }

    fn parse_signed_num_or_symbol(&mut self) -> Result<Datum> {
        match self.peek_nth_char(1, WithinContext::Identifier) {
            Ok(digit) if digit.is_ascii_digit() => self.parse_num(),
            Ok(_)
            | Err(Error {
                kind: ErrorKind::Eof(_),
                ..
            }) => self.parse_identifier(WithinContext::Identifier),
            Err(other) => Err(other),
        }
    }

    fn parse_char(&mut self) -> Result<Datum> {
        let (span, c) = self.capture_span(|s| {
            // Consume the \
            s.eat_bytes(1);

            // Consume the character name
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

        match self.peek_char(WithinContext::Dispatch)? {
            '{' => self.parse_set(),
            '(' => self.parse_anon_fun(),
            '#' => self.parse_symbolic_float(),
            _ => {
                let (span, _) = self.capture_span(|s| s.consume_char(WithinContext::Dispatch));

                Err(Error::new(
                    Span::new(self.file_id, span.start() - 1, span.end()),
                    ErrorKind::UnsupportedDispatch,
                ))
            }
        }
    }

    fn parse_seq<F>(&mut self, terminator: char, make_ec: F) -> Result<Vec<Datum>>
    where
        F: FnOnce(Span) -> WithinContext,
    {
        // Consume the opening bracket
        let (open_bracket_span, _) = self.capture_span(|s| {
            s.eat_bytes(1);
        });
        let ec = make_ec(open_bracket_span);

        let mut content = Vec::new();

        // Keep eating data until we hit the terminator
        loop {
            let next_char = self.skip_until_non_whitespace(ec)?;
            if next_char == terminator {
                // End of the sequence
                self.eat_bytes(1);
                break Ok(content);
            } else {
                content.push(self.parse_datum_starting_with(next_char, ec)?);
            }
        }
    }

    fn parse_list(&mut self) -> Result<Datum> {
        let (outer_span, contents) = self.capture_span(|s| s.parse_seq(')', WithinContext::List));

        contents.map(|contents| Datum::List(outer_span, contents.into()))
    }

    fn parse_vector(&mut self) -> Result<Datum> {
        let (outer_span, contents) = self.capture_span(|s| s.parse_seq(']', WithinContext::Vector));

        contents.map(|contents| Datum::Vector(outer_span, contents.into()))
    }

    fn parse_map(&mut self) -> Result<Datum> {
        // First get the contents without splitting pairwise
        let (span, unpaired_contents) = self.capture_span(|s| s.parse_seq('}', WithinContext::Map));

        let unpaired_contents = unpaired_contents?;
        if unpaired_contents.len() % 2 == 1 {
            return Err(Error::new(span, ErrorKind::UnevenMap));
        }

        let mut paired_contents = Vec::with_capacity(unpaired_contents.len() / 2);
        let mut unpaired_contents_iter = unpaired_contents.into_iter();
        while let Some(key) = unpaired_contents_iter.next() {
            let value = unpaired_contents_iter.next().unwrap();
            paired_contents.push((key, value));
        }

        Ok(Datum::Map(span, paired_contents.into_boxed_slice()))
    }

    fn parse_set(&mut self) -> Result<Datum> {
        let (outer_span, contents) = self.capture_span(|s| s.parse_seq('}', WithinContext::Set));

        contents.map(|contents| {
            Datum::Set(
                // Cover the # in our span
                Span::new(self.file_id, outer_span.start() - 1, outer_span.end()),
                contents.into(),
            )
        })
    }

    fn parse_anon_fun(&mut self) -> Result<Datum> {
        use crate::anon_fun::convert_anon_fun;

        let (outer_span, body_contents) =
            self.capture_span(|s| s.parse_seq(')', WithinContext::List));

        let body_contents = body_contents?;

        convert_anon_fun(
            // Cover the # in our span
            Span::new(self.file_id, outer_span.start() - 1, outer_span.end()),
            body_contents.into_iter(),
        )
    }

    fn parse_quote_escape(&mut self) -> Result<char> {
        let escape_start = self.consumed_bytes as ByteIndex;

        match self.consume_char(WithinContext::QuoteEscape)? {
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

                if self.consume_char(WithinContext::CodePoint)? != ';' {
                    return Err(Error::new(span, ErrorKind::UnsupportedChar));
                }

                std::char::from_u32(code_point)
                    .ok_or_else(|| Error::new(span, ErrorKind::InvalidCodePoint))
            }
            _ => Err(Error::new(
                Span::new(self.file_id, escape_start, self.consumed_bytes),
                ErrorKind::UnsupportedStringEscape,
            )),
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
                let (_, unescaped_contents) = s.consume_until(|c| c == '"' || c == '\\');
                contents.push_str(unescaped_contents);

                match s.consume_char(WithinContext::String(open_quote_span))? {
                    '"' => {
                        return Ok(contents);
                    }
                    '\\' => contents.push(s.parse_quote_escape()?),
                    _ => {
                        unreachable!("Shouldn't be here");
                    }
                }
            }
        });
        contents.map(|contents| Datum::Str(span, contents.into()))
    }

    fn parse_identifier(&mut self, within: WithinContext) -> Result<Datum> {
        let (span, content) = self.consume_while(is_identifier_char);

        if content.is_empty() {
            let (span, next_char) = self.capture_span(|s| s.consume_char(within));
            return Err(Error::new(
                span,
                ErrorKind::UnexpectedChar(next_char?, within),
            ));
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

    fn parse_datum_starting_with(&mut self, c: char, within: WithinContext) -> Result<Datum> {
        match c {
            '(' => self.parse_list(),
            '[' => self.parse_vector(),
            '{' => self.parse_map(),
            '0'..='9' => self.parse_num(),
            '-' | '+' => self.parse_signed_num_or_symbol(),
            '\'' => self.parse_symbol_shorthand("quote"),
            '"' => self.parse_string(),
            '\\' => self.parse_char(),
            '#' => self.parse_dispatch(),
            _ => self.parse_identifier(within),
        }
    }

    fn parse_datum(&mut self) -> Result<Datum> {
        let ec = WithinContext::Datum;

        let start_char = self.skip_until_non_whitespace(ec)?;
        self.parse_datum_starting_with(start_char, ec)
    }

    fn parse_data(&mut self) -> Result<Vec<Datum>> {
        let mut datum_vec = Vec::new();

        // Keep eating datums until we hit EOF
        loop {
            match self.parse_datum() {
                Ok(datum) => {
                    datum_vec.push(datum);
                }
                Err(err) if err.kind() == &ErrorKind::Eof(WithinContext::Datum) => {
                    break Ok(datum_vec)
                }
                Err(err) => break Err(err),
            }
        }
    }
}

/////////

#[allow(clippy::many_single_char_names)]
#[cfg(test)]
mod test {
    use super::*;
    use crate::span::t2s;

    fn whole_str_span(v: &str) -> Span {
        Span::new(None, 0, v.len() as ByteIndex)
    }

    #[test]
    fn bool_datum() {
        let j = "false";
        let t = "^^^^^";
        let expected = Datum::Bool(t2s(t), false);

        assert_eq!(expected, datum_from_str(None, j).unwrap());

        let j = "true";
        let t = "^^^^";
        let expected = Datum::Bool(t2s(t), true);
        assert_eq!(expected, datum_from_str(None, j).unwrap());

        let j = "     false";
        let t = "     ^^^^^";
        let expected = Datum::Bool(t2s(t), false);
        assert_eq!(expected, datum_from_str(None, j).unwrap());

        let j = "\ttrue\t";
        let t = "\t^^^^\t";
        let expected = Datum::Bool(t2s(t), true);
        assert_eq!(expected, datum_from_str(None, j).unwrap());

        let j = " trueorfalse  ";
        let t = " ^^^^^^^^^^^  ";
        let expected = Datum::Sym(t2s(t), "trueorfalse".into());
        assert_eq!(expected, datum_from_str(None, j).unwrap());
    }

    #[test]
    fn list_datum() {
        let j = "() ; with a comment";
        let t = "^^                 ";
        let expected = Datum::List(t2s(t), Box::new([]));
        assert_eq!(expected, datum_from_str(None, j).unwrap());

        let j = "( true   false )";
        let t = "^^^^^^^^^^^^^^^^";
        let u = "  ^^^^          ";
        let v = "         ^^^^^  ";

        let expected = Datum::List(
            t2s(t),
            Box::new([Datum::Bool(t2s(u), true), Datum::Bool(t2s(v), false)]),
        );
        assert_eq!(expected, datum_from_str(None, j).unwrap());

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
        assert_eq!(expected, datum_from_str(None, j).unwrap());

        let j = "(true";
        let t = "    >";
        let u = "^    ";
        let err = Error::new(t2s(t), ErrorKind::Eof(WithinContext::List(t2s(u))));
        assert_eq!(err, datum_from_str(None, j).unwrap_err());

        let j = ")";
        let t = "^";
        let err = Error::new(t2s(t), ErrorKind::UnexpectedChar(')', WithinContext::Datum));
        assert_eq!(err, datum_from_str(None, j).unwrap_err());

        let j = "(]";
        let t = "^ ";
        let u = " ^";
        let err = Error::new(
            t2s(u),
            ErrorKind::UnexpectedChar(']', WithinContext::List(t2s(t))),
        );
        assert_eq!(err, datum_from_str(None, j).unwrap_err());
    }

    #[test]
    fn vector_datum() {
        let j = "  []";
        let t = "  ^^";
        let expected = Datum::Vector(t2s(t), Box::new([]));
        assert_eq!(expected, datum_from_str(None, j).unwrap());

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
        assert_eq!(expected, datum_from_str(None, j).unwrap());

        let j = "[true []";
        let t = "       >";
        let u = "^       ";
        let err = Error::new(t2s(t), ErrorKind::Eof(WithinContext::Vector(t2s(u))));
        assert_eq!(err, datum_from_str(None, j).unwrap_err());

        let j = "]";
        let t = "^";
        let err = Error::new(t2s(t), ErrorKind::UnexpectedChar(']', WithinContext::Datum));
        assert_eq!(err, datum_from_str(None, j).unwrap_err());
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
            // These are nearly numbers
            ".",
            "+",
            "+.",
            "+.5",
            "-",
            "-.",
            "-.5",
        ] {
            let s = whole_str_span(test_symbol);
            let expected = Datum::Sym(s, test_symbol.into());

            assert_eq!(expected, datum_from_str(None, test_symbol).unwrap());
        }
    }

    #[test]
    fn keyword_symbol_datum() {
        for &test_symbol in &[":HELLO", ":HELLO123", ":predicate?", ":mutate!"] {
            let s = whole_str_span(test_symbol);
            let expected = Datum::Sym(s, test_symbol.into());

            assert_eq!(expected, datum_from_str(None, test_symbol).unwrap());
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

            assert_eq!(expected, datum_from_str(None, test_string).unwrap());
        }

        let j = r#" "foo "#;
        let t = r#"     >"#;
        let u = r#" ^    "#;
        let err = Error::new(t2s(t), ErrorKind::Eof(WithinContext::String(t2s(u))));
        assert_eq!(err, datum_from_str(None, j).unwrap_err());

        let j = r#""\p""#;
        let t = r#"  ^ "#;
        let err = Error::new(t2s(t), ErrorKind::UnsupportedStringEscape);
        assert_eq!(err, datum_from_str(None, j).unwrap_err());
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

            assert_eq!(expected, datum_from_str(None, j).unwrap());
        }

        let j = r#"\SPACE"#;
        let t = r#" ^^^^^"#;
        let err = Error::new(t2s(t), ErrorKind::UnsupportedChar);
        assert_eq!(err, datum_from_str(None, j).unwrap_err());

        let j = r#"\u110000"#;
        let t = r#" ^^^^^^^"#;
        let err = Error::new(t2s(t), ErrorKind::InvalidCodePoint);
        assert_eq!(err, datum_from_str(None, j).unwrap_err());

        let j = r#"[\newline]"#;
        let t = r#" ^^^^^^^^ "#;
        let expected = Datum::Vector(whole_str_span(j), Box::new([Datum::Char(t2s(t), '\n')]));
        assert_eq!(expected, datum_from_str(None, j).unwrap());
    }

    #[test]
    fn int_datum() {
        let test_ints = [
            ("0", 0),
            ("000", 0),
            ("1000", 1000),
            ("+1000", 1000),
            ("-1000", -1000),
            ("9223372036854775807", 9223372036854775807),
            ("+9223372036854775807", 9223372036854775807),
            ("-9223372036854775808", -9223372036854775808),
        ];

        for &(j, expected_int) in &test_ints {
            let s = whole_str_span(j);
            let expected = Datum::Int(s, expected_int);

            assert_eq!(expected, datum_from_str(None, j).unwrap());
        }

        let j = "10223372036854775807";
        let t = "^^^^^^^^^^^^^^^^^^^^";
        let err = Error::new(t2s(t), ErrorKind::IntegerOverflow);
        assert_eq!(err, datum_from_str(None, j).unwrap_err());

        let j = "-10223372036854775807";
        let t = "^^^^^^^^^^^^^^^^^^^^^";
        let err = Error::new(t2s(t), ErrorKind::IntegerOverflow);
        assert_eq!(err, datum_from_str(None, j).unwrap_err());

        let j = "4545894549584910223372036854775807";
        let t = "^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^";
        let err = Error::new(t2s(t), ErrorKind::IntegerOverflow);
        assert_eq!(err, datum_from_str(None, j).unwrap_err());
    }

    #[test]
    fn float_datum() {
        let test_floats = [
            ("0.", 0.0),
            ("0.0", 0.0),
            ("000.000", 0.0),
            ("+16.", 16.0),
            ("+16.5", 16.5),
            ("+016.500", 16.5),
            ("-32.", -32.0),
            ("-32.25", -32.25),
            ("-032.2500", -32.25),
            ("##Inf", std::f64::INFINITY),
            ("##-Inf", std::f64::NEG_INFINITY),
        ];

        for &(j, expected_float) in &test_floats {
            let s = whole_str_span(j);
            let expected = Datum::Float(s, expected_float);

            assert_eq!(expected, datum_from_str(None, j).unwrap());
        }

        // This can't be compared using normal equality
        if let Datum::Float(_, f) = datum_from_str(None, "##NaN").unwrap() {
            assert!(f.is_nan());
        } else {
            panic!("Expected ##NaN to parse as float");
        }
    }

    #[test]
    fn map_datum() {
        let j = "{}";
        let t = "^^";
        let expected = Datum::Map(t2s(t), Box::new([]));
        assert_eq!(expected, datum_from_str(None, j).unwrap());

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

        assert_eq!(expected, datum_from_str(None, j).unwrap());

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

        assert_eq!(expected, datum_from_str(None, j).unwrap());

        let j = "{1}";
        let t = "^^^";
        let err = Error::new(t2s(t), ErrorKind::UnevenMap);
        assert_eq!(err, datum_from_str(None, j).unwrap_err());
    }

    #[test]
    fn set_datum() {
        let j = "#{}";
        let t = "^^^";
        let expected = Datum::Set(t2s(t), Box::new([]));
        assert_eq!(expected, datum_from_str(None, j).unwrap());

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

        assert_eq!(expected, datum_from_str(None, j).unwrap());

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

        assert_eq!(expected, datum_from_str(None, j).unwrap());
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
        assert_eq!(expected, datum_from_str(None, j).unwrap());

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
        assert_eq!(expected, datum_from_str(None, j).unwrap());

        let j = "'";
        let t = ">";
        let err = Error::new(t2s(t), ErrorKind::Eof(WithinContext::Datum));
        assert_eq!(err, datum_from_str(None, j).unwrap_err());
    }

    #[test]
    fn unsupported_dispatch() {
        let j = r#"#loop"#;
        let t = r#"^^   "#;
        let err = Error::new(t2s(t), ErrorKind::UnsupportedDispatch);

        assert_eq!(err, datum_from_str(None, j).unwrap_err());

        let j = "#";
        let t = ">";
        let err = Error::new(t2s(t), ErrorKind::Eof(WithinContext::Dispatch));

        assert_eq!(err, datum_from_str(None, j).unwrap_err());
    }

    #[test]
    fn datum_comment() {
        let j = "(Hello #_(you jerk))";
        let t = "^^^^^^^^^^^^^^^^^^^^";
        let u = " ^^^^^              ";

        let expected = Datum::List(t2s(t), Box::new([Datum::Sym(t2s(u), "Hello".into())]));
        assert_eq!(expected, datum_from_str(None, j).unwrap());

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
        assert_eq!(expected, datum_from_str(None, j).unwrap());
    }

    #[test]
    fn multiple_data() {
        let j = " 1  #_two 3  ";
        let t = " ^           ";
        let u = "          ^  ";

        let expected = vec![Datum::Int(t2s(t), 1), Datum::Int(t2s(u), 3)];
        assert_eq!(expected, data_from_str(None, j).unwrap());

        let j = "(true)))";
        let t = "      ^ ";
        let err = Error::new(t2s(t), ErrorKind::UnexpectedChar(')', WithinContext::Datum));
        assert_eq!(err, data_from_str(None, j).unwrap_err());

        let j = "(true";
        let t = "    >";
        let u = "^    ";

        let err = Error::new(t2s(t), ErrorKind::Eof(WithinContext::List(t2s(u))));
        assert_eq!(err, data_from_str(None, j).unwrap_err());
    }
}
