use syntax::error::{Error, Result};
use syntax::value::{SValue, Value};
use syntax::span::Span;
use std;

pub fn datum_from_str(s: &str) -> Result<SValue> {
    let mut parser = Parser::from_str(s);

    let result = parser.parse_datum()?;

    // Allow for trailing whitespace
    let _ = parser.skip_until_non_whitespace();

    if parser.input.is_empty() {
        Ok(result)
    } else {
        let consumed = parser.consumed_bytes as u32;
        let span = Span {
            lo: consumed,
            hi: consumed + (parser.input.len() as u32),
        };

        Err(Error::TrailingCharacters(span))
    }
}

pub fn data_from_str(s: &str) -> Result<Vec<SValue>> {
    let mut parser = Parser::from_str(s);
    parser.parse_data()
}

pub struct Parser<'de> {
    input: &'de str,
    consumed_bytes: usize,
}

fn is_whitespace(c: char) -> bool {
    match c {
        ' ' | '\n' | '\t' | '\r' => true,
        _ => false,
    }
}

fn is_identifier_char(c: char) -> bool {
    match c {
        '\\'  =>
            // Literal backslash
            false,
        _ if is_whitespace(c) =>
            false,
        '#' | '(' | ')' | '[' | ']' | '{' | '}' | '\'' | '`' | ',' | '|' =>
            // Syntax character
            false,
        '\u{20}'...'\u{7f}' =>
            true,
        _ =>
            false,
    }
}

fn u64_to_positive_i64(span: Span, i: u64) -> Result<i64> {
    if i > (1 << 63) - 1 {
        Err(Error::IntegerOverflow(span))
    } else {
        Ok(i as i64)
    }
}

fn u64_to_negative_i64(span: Span, i: u64) -> Result<i64> {
    let min_i64_abs_value = 1 << 63;

    if i > min_i64_abs_value {
        Err(Error::IntegerOverflow(span))
    } else if i == min_i64_abs_value {
        Ok(i64::min_value())
    } else {
        Ok(-(i as i64))
    }
}

impl<'de> Parser<'de> {
    pub fn from_str(input: &'de str) -> Self {
        Parser {
            input,
            consumed_bytes: 0,
        }
    }

    fn peek_char(&mut self) -> Result<char> {
        self.input.chars().next().ok_or(Error::Eof)
    }

    fn peek_nth_char(&mut self, i: usize) -> Result<char> {
        self.input.chars().nth(i).ok_or(Error::Eof)
    }

    fn eat_bytes(&mut self, count: usize) {
        self.input = &self.input[count..];
        self.consumed_bytes = self.consumed_bytes + count
    }

    fn consume_char(&mut self) -> Result<char> {
        let mut char_indices = self.input.char_indices();

        match char_indices.next() {
            Some((_, c)) => {
                let next_index = char_indices.next().map(|t| t.0).unwrap_or(self.input.len());
                self.eat_bytes(next_index);

                Ok(c)
            }

            None => Err(Error::Eof),
        }
    }

    fn eat_until_end_of_block_comment(&mut self) -> Result<()> {
        let mut depth = 1;

        // Disard the #|
        self.eat_bytes(2);

        loop {
            match self.consume_char()? {
                '|' => if self.peek_char()? == '#' {
                    self.eat_bytes(1);

                    depth = depth - 1;
                    if depth == 0 {
                        return Ok(());
                    }
                },
                '#' => if self.peek_char()? == '|' {
                    // Nested comment
                    self.eat_bytes(1);
                    depth = depth + 1;
                },
                _ => {}
            }
        }
    }

    fn skip_until_non_whitespace(&mut self) -> Result<char> {
        loop {
            match self.peek_char()? {
                ' ' | '\n' | '\t' | '\r' => {
                    self.eat_bytes(1);
                }
                ';' => {
                    let _ = self.consume_until(|c| c == '\n');
                }
                '#' => {
                    match self.peek_nth_char(1) {
                        Ok(';') => {
                            // Discard the #; and the following datum
                            self.eat_bytes(2);
                            self.parse_datum()?;
                        }
                        Ok('|') => {
                            self.eat_until_end_of_block_comment()?;
                        }
                        _ => {
                            return Ok('#');
                        }
                    }
                }
                other => {
                    return Ok(other);
                }
            }
        }
    }

    fn consume_until<'a, T>(&'a mut self, predicate: T) -> (Span, &'a str)
    where
        T: Fn(char) -> bool,
    {
        let lo = self.consumed_bytes as u32;
        let last_index = self.input.find(predicate).unwrap_or(self.input.len());
        let (consumed, remaining_input) = self.input.split_at(last_index);

        self.input = remaining_input;
        self.consumed_bytes = self.consumed_bytes + last_index;

        let span = Span {
            lo,
            hi: (self.consumed_bytes as u32),
        };

        (span, consumed)
    }

    fn consume_literal(&mut self, token: &'static str) -> Result<()> {
        if self.input.starts_with(token) {
            self.eat_bytes(token.len());
            Ok(())
        } else {
            // Span to the next non-identifier. This assumes the literal should only contain
            // identifier chars
            let lo = self.consumed_bytes as u32;
            let ident_count = self.input
                .find(|c| !is_identifier_char(c))
                .unwrap_or(self.input.len());

            let span = Span {
                lo,
                hi: lo + (ident_count as u32),
            };

            Err(Error::ExpectedLiteral(span, token))
        }
    }

    fn capture_span<F, R>(&mut self, block: F) -> (Span, R)
    where
        F: FnOnce(&mut Parser) -> R,
    {
        let lo = self.consumed_bytes as u32;
        let result = block(self);
        let hi = self.consumed_bytes as u32;

        (Span { lo, hi }, result)
    }

    fn parse_int_content(&mut self) -> Result<u64> {
        let (span, digits) = self.consume_until(|c| match c {
            '0'...'9' => false,
            _ => true,
        });

        u64::from_str_radix(digits, 10).map_err(|_| Error::InvalidInteger(span))
    }

    fn parse_int(&mut self) -> Result<SValue> {
        let (span, content) = self.capture_span(|s| s.parse_int_content());

        content
            .and_then(|content| u64_to_positive_i64(span.clone(), content))
            .map(|i| SValue::Int(span, i))
    }

    fn parse_negative_or_symbol(&mut self) -> Result<SValue> {
        let (span, content) = self.capture_span(|s| {
            // Consume the -
            s.eat_bytes(1);

            // Try to parse as integer
            s.parse_int_content()
        });

        match content {
            Ok(valid_integer) => {
                u64_to_negative_i64(span.clone(), valid_integer).map(|i| SValue::Int(span, i))
            }
            Err(Error::InvalidInteger(_)) => {
                // Treat as a symbol
                let rest_of_symbol = self.parse_unenclosed_symbol_content();

                let span_with_symbol = Span {
                    lo: span.lo,
                    hi: span.lo + (rest_of_symbol.len() as u32) + 1,
                };

                Ok(SValue::Symbol(
                    span_with_symbol,
                    format!("-{}", rest_of_symbol),
                ))
            }
            Err(other_err) => Err(other_err),
        }
    }

    fn parse_char(&mut self) -> Result<char> {
        // Consume the \
        self.eat_bytes(1);

        // Take everything up to the next whitespace
        let (span, char_name) = self.consume_until(|c| is_whitespace(c));

        let mut char_name_chars = char_name.chars();
        if let Some(first_char) = char_name_chars.next() {
            if char_name_chars.next().is_none() {
                // There is only a single character; return it
                return Ok(first_char);
            }

            if first_char == 'x' {
                // This is a hex code point
                let hex_string = &char_name[1..];
                let code_point = u32::from_str_radix(hex_string, 16)
                    .map_err(|_| Error::InvalidCharLiteral(span.clone()))?;

                return std::char::from_u32(code_point).ok_or(Error::InvalidCharCodePoint(span));
            }
        }

        match char_name {
            "alarm" => Ok('\u{07}'),
            "backspace" => Ok('\u{08}'),
            "delete" => Ok('\u{7f}'),
            "escape" => Ok('\u{1b}'),
            "newline" => Ok('\u{0a}'),
            "null" => Ok('\u{00}'),
            "return" => Ok('\u{0d}'),
            "space" => Ok('\u{20}'),
            "tab" => Ok('\u{09}'),
            _ => Err(Error::InvalidCharLiteral(span)),
        }
    }

    fn parse_octo_datum(&mut self) -> Result<SValue> {
        let (span, value) = self.capture_span(|s| {
            // Consume the #
            s.eat_bytes(1);

            match s.peek_char()? {
                't' => {
                    s.consume_literal("true")?;
                    Ok(Value::Bool(true))
                }
                'f' => {
                    s.consume_literal("false")?;
                    Ok(Value::Bool(false))
                }
                '\\' => Ok(Value::Char(s.parse_char()?)),
                _ => {
                    // Cover the #
                    let invalid_lo = (s.consumed_bytes - 1) as u32;
                    let _ = s.consume_char();
                    let invalid_hi = s.consumed_bytes as u32;

                    let span = Span {
                        lo: invalid_lo,
                        hi: invalid_hi,
                    };

                    Err(Error::InvalidOctoDatum(span))
                }
            }
        });

        value.map(|v| v.to_spanned(span))
    }

    fn parse_seq(&mut self, terminator: char) -> Result<Vec<SValue>> {
        // Consume the opening bracket
        self.eat_bytes(1);

        let mut datum_vec = Vec::new();

        // Keep eating datums until we hit the terminator
        loop {
            if self.skip_until_non_whitespace()? == terminator {
                // End of the sequence
                self.eat_bytes(1);
                return Ok(datum_vec);
            } else {
                datum_vec.push(self.parse_datum()?);
            }
        }
    }

    fn parse_list(&mut self) -> Result<SValue> {
        let (outer_span, contents) = self.capture_span(|s| s.parse_seq(')'));
        contents.map(|contents| SValue::List(outer_span, contents))
    }

    fn parse_vector(&mut self) -> Result<SValue> {
        let (outer_span, contents) = self.capture_span(|s| s.parse_seq(']'));
        contents.map(|contents| SValue::Vector(outer_span, contents))
    }

    fn parse_quote_escape(&mut self) -> Result<char> {
        let escape_lo = self.consumed_bytes as u32;

        match self.consume_char()? {
            't' => Ok('\t'),
            'r' => Ok('\r'),
            'n' => Ok('\n'),
            '\\' => Ok('\\'),
            '"' => Ok('"'),
            '|' => Ok('|'),
            'x' => {
                let (span, code_point) = {
                    let (span, hex_string) = self.consume_until(|c| c == ';');
                    (span, u32::from_str_radix(hex_string, 16))
                };

                let code_point = code_point.map_err(|_| Error::InvalidCharLiteral(span.clone()))?;

                if self.consume_char()? != ';' {
                    return Err(Error::InvalidCharLiteral(span));
                }

                return std::char::from_u32(code_point).ok_or(Error::InvalidCharCodePoint(span));
            }
            _ => {
                let span = Span {
                    lo: escape_lo,
                    hi: self.consumed_bytes as u32,
                };
                Err(Error::InvalidQuoteEscape(span))
            }
        }
    }

    fn parse_enclosed_stringlike(&mut self, quote_char: char) -> Result<String> {
        // Eat the opening quote
        self.eat_bytes(1);

        let mut contents = String::new();
        loop {
            match self.consume_char()? {
                end_quote if end_quote == quote_char => {
                    return Ok(contents);
                }
                '\\' => contents.push(self.parse_quote_escape()?),
                other => {
                    contents.push(other);
                }
            }
        }
    }

    fn parse_string(&mut self) -> Result<SValue> {
        let (span, contents) = self.capture_span(|s| s.parse_enclosed_stringlike('"'));
        contents.map(|contents| SValue::String(span, contents))
    }

    fn parse_enclosed_symbol(&mut self) -> Result<SValue> {
        let (span, contents) = self.capture_span(|s| s.parse_enclosed_stringlike('|'));
        contents.map(|contents| SValue::Symbol(span, contents))
    }

    fn parse_unenclosed_symbol_content(&mut self) -> &str {
        self.consume_until(|c| !is_identifier_char(c)).1
    }

    fn parse_unenclosed_symbol(&mut self) -> Result<SValue> {
        let (span, content) = self.capture_span(|s| s.parse_unenclosed_symbol_content().to_owned());

        if content.len() == 0 {
            let (span, next_char) = self.capture_span(|s| s.consume_char());
            Err(Error::UnexpectedChar(span, next_char?))
        } else {
            Ok(SValue::Symbol(span, content))
        }
    }

    fn parse_symbol_shorthand(&mut self, expansion: &str) -> Result<SValue> {
        let (outer_span, (shorthand_span, quoted_datum)) = self.capture_span(|s| {
            let (shorthand_span, _) = s.capture_span(|s| {
                // Discard the shorthand. Note this must be ASCII.
                s.eat_bytes(1);
            });

            (shorthand_span, s.parse_datum())
        });

        quoted_datum.map(|quoted_datum| {
            SValue::List(
                outer_span,
                vec![
                    SValue::Symbol(shorthand_span, expansion.to_owned()),
                    quoted_datum,
                ],
            )
        })
    }

    fn parse_datum(&mut self) -> Result<SValue> {
        match self.skip_until_non_whitespace()? {
            '#' => self.parse_octo_datum(),
            '(' => self.parse_list(),
            '[' => self.parse_vector(),
            '0'...'9' => self.parse_int(),
            '-' => self.parse_negative_or_symbol(),
            '\'' => self.parse_symbol_shorthand("quote"),
            '"' => self.parse_string(),
            '|' => self.parse_enclosed_symbol(),
            _ => self.parse_unenclosed_symbol(),
        }
    }

    fn parse_data(&mut self) -> Result<Vec<SValue>> {
        let mut datum_vec = Vec::new();

        // Keep eating datums until we hit EOF
        loop {
            match self.skip_until_non_whitespace() {
                Ok(_) => {
                    let datum = self.parse_datum()?;
                    println!("{:?}", datum);

                    datum_vec.push(datum);
                }
                Err(Error::Eof) => return Ok(datum_vec),
                Err(e) => return Err(e),
            }
        }
    }
}

/////////

#[cfg(test)]
fn t2s(v: &str) -> Span {
    let lo = v.find('^').unwrap_or(v.len()) as u32;
    let hi = v.rfind('^').map(|i| i + 1).unwrap_or(v.len()) as u32;

    Span { lo, hi }
}

#[cfg(test)]
fn whole_str_span(v: &str) -> Span {
    Span {
        lo: 0,
        hi: v.len() as u32,
    }
}

#[test]
fn bool_datum() {
    let j = "#false";
    let t = "^^^^^^";
    let expected = SValue::Bool(t2s(t), false);

    assert_eq!(expected, datum_from_str(j).unwrap());

    let j = "#true";
    let t = "^^^^^";
    let expected = SValue::Bool(t2s(t), true);
    assert_eq!(expected, datum_from_str(j).unwrap());

    let j = "     #false";
    let t = "     ^^^^^^";
    let expected = SValue::Bool(t2s(t), false);
    assert_eq!(expected, datum_from_str(j).unwrap());

    let j = "\t#true\t";
    let t = "\t^^^^^\t";
    let expected = SValue::Bool(t2s(t), true);
    assert_eq!(expected, datum_from_str(j).unwrap());

    let j = " #true 1";
    let t = "       ^";
    let err = Error::TrailingCharacters(t2s(t));
    assert_eq!(err, datum_from_str(j).unwrap_err());

    // This is valid R7RS. We intentionally don't allow it
    let j = " #t";
    let t = "  ^";
    let err = Error::ExpectedLiteral(t2s(t), "true");
    assert_eq!(err, datum_from_str(j).unwrap_err());

    // This is valid R7RS. We intentionally don't allow it
    let j = " #f     ";
    let t = "  ^     ";
    let err = Error::ExpectedLiteral(t2s(t), "false");
    assert_eq!(err, datum_from_str(j).unwrap_err());

    let j = " #fumble     ";
    let t = "  ^^^^^^     ";
    let err = Error::ExpectedLiteral(t2s(t), "false");
    assert_eq!(err, datum_from_str(j).unwrap_err());
}

#[test]
fn list_datum() {
    let j = "() ; with a comment";
    let t = "^^                 ";
    let expected = SValue::List(t2s(t), vec![]);
    assert_eq!(expected, datum_from_str(j).unwrap());

    let j = "( #true   #false )";
    let t = "^^^^^^^^^^^^^^^^^^";
    let u = "  ^^^^^           ";
    let v = "          ^^^^^^  ";

    let expected = SValue::List(
        t2s(t),
        vec![SValue::Bool(t2s(u), true), SValue::Bool(t2s(v), false)],
    );
    assert_eq!(expected, datum_from_str(j).unwrap());

    let j = "(#true";
    let err = Error::Eof;
    assert_eq!(err, datum_from_str(j).unwrap_err());

    let j = "(#true))";
    let t = "       ^";
    let err = Error::TrailingCharacters(t2s(t));
    assert_eq!(err, datum_from_str(j).unwrap_err());

    let j = ")";
    let t = "^";
    let err = Error::UnexpectedChar(t2s(t), ')');
    assert_eq!(err, datum_from_str(j).unwrap_err());
}

#[test]
fn vector_datum() {
    let j = "  []";
    let t = "  ^^";
    let expected = SValue::Vector(t2s(t), vec![]);
    assert_eq!(expected, datum_from_str(j).unwrap());

    let j = "[ #true   (#true #false) ]";
    let t = "^^^^^^^^^^^^^^^^^^^^^^^^^^";
    let u = "  ^^^^^";
    let v = "          ^^^^^^^^^^^^^^";
    let w = "           ^^^^^";
    let x = "                 ^^^^^^";

    let expected = SValue::Vector(
        t2s(t),
        vec![
            SValue::Bool(t2s(u), true),
            SValue::List(
                t2s(v),
                vec![SValue::Bool(t2s(w), true), SValue::Bool(t2s(x), false)],
            ),
        ],
    );
    assert_eq!(expected, datum_from_str(j).unwrap());

    let j = "]";
    let t = "^";
    let err = Error::UnexpectedChar(t2s(t), ']');
    assert_eq!(err, datum_from_str(j).unwrap_err());
}

#[test]
fn unenclosed_symbol_datum() {
    for test_symbol in vec![
        "HELLO",
        "HELLO123",
        "predicate?",
        "mutate!",
        "from->to",
        "!$%&*+-./:<=>?@^_",
        "+",
        "-",
    ] {
        let s = whole_str_span(test_symbol);
        let expected = SValue::Symbol(s, test_symbol.to_owned());

        assert_eq!(expected, datum_from_str(test_symbol).unwrap());
    }
}

#[test]
fn enclosed_symbol_datum() {
    let test_symbols = vec![
        (r#"|Hello, world!|"#, "Hello, world!"),
        (r#"|\"|"#, "\""),
        (r#"|\||"#, "|"),
        (r#"|two\x20;words|"#, "two words"),
        (r#"||"#, ""),
        (r#"|0|"#, "0"),
        (r#"|\t\t|"#, "\t\t"),
    ];

    for (test_symbol, expected_contents) in test_symbols {
        let s = whole_str_span(test_symbol);
        let expected = SValue::Symbol(s, expected_contents.to_owned());

        assert_eq!(expected, datum_from_str(test_symbol).unwrap());
    }

    let j = "|foo";
    let err = Error::Eof;
    assert_eq!(err, datum_from_str(j).unwrap_err());
}

#[test]
fn string_datum() {
    let test_strings = vec![
        (r#""""#, ""),
        (r#""Hello, world!""#, "Hello, world!"),
        (r#""Hello\"World""#, "Hello\"World"),
        (r#""Hello\\World""#, "Hello\\World"),
        (r#""Hello\|World""#, "Hello|World"),
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

    for (test_string, expected_contents) in test_strings {
        let s = whole_str_span(test_string);
        let expected = SValue::String(s, expected_contents.to_owned());

        assert_eq!(expected, datum_from_str(test_string).unwrap());
    }

    let j = r#""foo"#;
    let err = Error::Eof;
    assert_eq!(err, datum_from_str(j).unwrap_err());

    let j = r#""\p""#;
    let t = r#"  ^ "#;
    let err = Error::InvalidQuoteEscape(t2s(t));
    assert_eq!(err, datum_from_str(j).unwrap_err());
}

#[test]
fn char_datum() {
    let test_chars = vec![
        ("#\\alarm", '\u{07}'),
        ("#\\backspace", '\u{08}'),
        ("#\\delete", '\u{7f}'),
        ("#\\escape", '\u{1b}'),
        ("#\\newline", '\u{0a}'),
        ("#\\null", '\u{00}'),
        ("#\\return", '\u{0d}'),
        ("#\\space", '\u{20}'),
        ("#\\tab", '\u{09}'),
        ("#\\a", 'a'),
        ("#\\A", 'A'),
        ("#\\(", '('),
        ("#\\☃", '\u{2603}'),
        ("#\\x03BB", '\u{03bb}'),
    ];

    for (j, expected_char) in test_chars {
        let s = whole_str_span(j);
        let expected = SValue::Char(s, expected_char);

        assert_eq!(expected, datum_from_str(j).unwrap());
    }

    let j = r#"#\SPACE"#;
    let t = r#"  ^^^^^"#;
    let err = Error::InvalidCharLiteral(t2s(t));
    assert_eq!(err, datum_from_str(j).unwrap_err());

    let j = r#"#\x110000"#;
    let t = r#"  ^^^^^^^"#;
    let err = Error::InvalidCharCodePoint(t2s(t));
    assert_eq!(err, datum_from_str(j).unwrap_err());
}

#[test]
fn int_datum() {
    let test_ints = vec![
        ("0", 0),
        ("000", 0),
        ("1000", 1000),
        ("-1000", -1000),
        ("9223372036854775807", 9223372036854775807),
        ("-9223372036854775808", -9223372036854775808),
    ];

    for (j, expected_int) in test_ints {
        let s = whole_str_span(j);
        let expected = SValue::Int(s, expected_int);

        assert_eq!(expected, datum_from_str(j).unwrap());
    }

    let j = "10223372036854775807";
    let t = "^^^^^^^^^^^^^^^^^^^^";
    let err = Error::IntegerOverflow(t2s(t));
    // TODO: We can only detect in the range i64::max_value() to u64::max_value()
    assert_eq!(err, datum_from_str(j).unwrap_err());

    let j = "-10223372036854775807";
    let t = "^^^^^^^^^^^^^^^^^^^^^";
    let err = Error::IntegerOverflow(t2s(t));
    // TODO: We can only detect in the range i64::min_value() to -u64::max_value()
    assert_eq!(err, datum_from_str(j).unwrap_err());

    let j = "4545894549584910223372036854775807";
    let t = "^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^";
    let err = Error::InvalidInteger(t2s(t));
    // TODO: We can only detect in the range i64::min_value() to -u64::max_value()
    assert_eq!(err, datum_from_str(j).unwrap_err());
}

#[test]
fn quote_shorthand() {
    let j = "'foo";
    let t = "^^^^";
    let u = "^   ";
    let v = " ^^^";

    let expected = SValue::List(
        t2s(t),
        vec![
            SValue::Symbol(t2s(u), "quote".to_owned()),
            SValue::Symbol(t2s(v), "foo".to_owned()),
        ],
    );
    assert_eq!(expected, datum_from_str(j).unwrap());

    let j = "' (1 2 3)";
    let t = "^^^^^^^^^";
    let u = "^        ";
    let v = "  ^^^^^^^";
    let w = "   ^     ";
    let x = "     ^   ";
    let y = "       ^ ";

    let expected = SValue::List(
        t2s(t),
        vec![
            SValue::Symbol(t2s(u), "quote".to_owned()),
            SValue::List(
                t2s(v),
                vec![
                    SValue::Int(t2s(w), 1),
                    SValue::Int(t2s(x), 2),
                    SValue::Int(t2s(y), 3),
                ],
            ),
        ],
    );
    assert_eq!(expected, datum_from_str(j).unwrap());

    let j = "'";
    let err = Error::Eof;
    assert_eq!(err, datum_from_str(j).unwrap_err());
}

#[test]
fn invalid_octodatum() {
    let j = r#"#loop"#;
    let t = r#"^^   "#;
    let err = Error::InvalidOctoDatum(t2s(t));

    assert_eq!(err, datum_from_str(j).unwrap_err());

    let j = "#";
    let err = Error::Eof;

    assert_eq!(err, datum_from_str(j).unwrap_err());
}

#[test]
fn block_comment() {
    let j = r#"
        #| This is a block comment
           This can be as many lines as it wants
           It can also contain # and |
           It can even contain a #| nested comment |# |#
        (display "LOL")
        #| Make sure we treat this as a separate comment |#"#;

    let expected = Value::List(vec![
        Value::Symbol("display".to_owned()),
        Value::String("LOL".to_owned()),
    ]);
    assert_eq!(expected, datum_from_str(j).unwrap().to_unspanned());

    let j = "#|";
    let err = Error::Eof;
    assert_eq!(err, datum_from_str(j).unwrap_err());
}

#[test]
fn datum_comment() {
    let j = "(Hello #;(you jerk))";
    let t = "^^^^^^^^^^^^^^^^^^^^";
    let u = " ^^^^^              ";

    let expected = SValue::List(t2s(t), vec![SValue::Symbol(t2s(u), "Hello".to_owned())]);
    assert_eq!(expected, datum_from_str(j).unwrap());

    let j = "(Hello #;  you jerk)";
    let t = "^^^^^^^^^^^^^^^^^^^^";
    let u = " ^^^^^              ";
    let v = "               ^^^^ ";

    let expected = SValue::List(
        t2s(t),
        vec![
            SValue::Symbol(t2s(u), "Hello".to_owned()),
            SValue::Symbol(t2s(v), "jerk".to_owned()),
        ],
    );
    assert_eq!(expected, datum_from_str(j).unwrap());
}

#[test]
fn multiple_data() {
    let j = " 1  #|two|# 3  ";
    let t = " ^             ";
    let u = "            ^  ";

    let expected = vec![SValue::Int(t2s(t), 1), SValue::Int(t2s(u), 3)];
    assert_eq!(expected, data_from_str(j).unwrap());

    let j = "(#true)))";
    let t = "       ^ ";
    let err = Error::UnexpectedChar(t2s(t), ')');
    assert_eq!(err, data_from_str(j).unwrap_err());
}
