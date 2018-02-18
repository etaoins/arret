use syntax::error::{Error, Result};
use syntax::value::Value;
use std;
use syntax::span::Span;

pub fn data_from_str(s: &str) -> Result<Vec<Value>> {
    data_from_str_with_span_offset(s, 0)
}

pub fn data_from_str_with_span_offset(s: &str, span_offset: usize) -> Result<Vec<Value>> {
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

fn is_identifier_char(c: char) -> bool {
    match c {
        'A'...'Z' | 'a'...'z' | '0'...'9' => true,
        '.' | '*' | '+' | '!' | '-' | '_' | '?' | '$' | '%' | '&' | '=' | '<' | '>' | ':' => {
            // Punctuation allowed at beginning of an identiifer
            true
        }
        '#' => {
            // Punctuation allowed anywhere
            true
        }
        '/' => {
            // We don't support namespacing so we treat this as a normal char
            true
        }
        _ => false,
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
    fn from_str(input: &'de str, consumed_bytes: usize) -> Self {
        Parser {
            input,
            consumed_bytes,
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

    fn skip_until_non_whitespace(&mut self) -> Result<char> {
        loop {
            match self.peek_char()? {
                ';' => {
                    let _ = self.consume_until(|c| c == '\n');
                }
                '#' => {
                    match self.peek_nth_char(1) {
                        Ok('_') => {
                            // Discard the #_ and the following datum
                            self.eat_bytes(2);
                            self.parse_datum()?;
                        }
                        _ => {
                            return Ok('#');
                        }
                    }
                }
                other => if is_whitespace(other) {
                    self.eat_bytes(1)
                } else {
                    return Ok(other);
                },
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

    fn parse_int(&mut self) -> Result<Value> {
        let (span, content) = self.capture_span(|s| s.parse_int_content());

        content
            .and_then(|content| u64_to_positive_i64(span.clone(), content))
            .map(|i| Value::Int(span, i))
    }

    fn parse_negative_or_symbol(&mut self) -> Result<Value> {
        let (span, content) = self.capture_span(|s| {
            // Consume the -
            s.eat_bytes(1);

            // Try to parse as integer
            s.parse_int_content()
        });

        match content {
            Ok(valid_integer) => {
                u64_to_negative_i64(span.clone(), valid_integer).map(|i| Value::Int(span, i))
            }
            Err(Error::InvalidInteger(_)) => {
                // Treat as a symbol
                let rest_of_symbol = self.parse_identifier_content();

                let span_with_symbol = Span {
                    lo: span.lo,
                    hi: span.lo + (rest_of_symbol.len() as u32) + 1,
                };

                Ok(Value::Symbol(
                    span_with_symbol,
                    format!("-{}", rest_of_symbol),
                ))
            }
            Err(other_err) => Err(other_err),
        }
    }

    fn parse_char(&mut self) -> Result<Value> {
        let (span, c) = self.capture_span(|s| {
            // Consume the \
            s.eat_bytes(1);

            // Take everything up to the next whitespace
            let (span, char_name) = s.consume_until(|c| is_whitespace(c));

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
                        .map_err(|_| Error::InvalidCharLiteral(span.clone()))?;

                    return std::char::from_u32(code_point).ok_or(Error::InvalidCharCodePoint(span));
                }
            }

            match char_name {
                "newline" => Ok('\u{0a}'),
                "return" => Ok('\u{0d}'),
                "space" => Ok('\u{20}'),
                "tab" => Ok('\u{09}'),
                _ => Err(Error::InvalidCharLiteral(span)),
            }
        });

        c.map(|c| Value::Char(span, c))
    }

    fn parse_dispatch(&mut self) -> Result<Value> {
        // Consume the #
        // This means we need to adjust our spans below to cover it for reporting
        self.eat_bytes(1);

        match self.peek_char()? {
            '{' => self.parse_set(),
            _ => {
                let (span, _) = self.capture_span(|s| s.consume_char());
                let adj_span = span.with_lo(span.lo - 1);

                Err(Error::InvalidDispatch(adj_span))
            }
        }
    }

    fn parse_seq(&mut self, terminator: char) -> Result<Vec<Value>> {
        // Consume the opening bracket
        self.eat_bytes(1);
        let mut content = Vec::new();

        // Keep eating datums until we hit the terminator
        loop {
            if self.skip_until_non_whitespace()? == terminator {
                // End of the sequence
                self.eat_bytes(1);
                return Ok(content);
            } else {
                content.push(self.parse_datum()?);
            }
        }
    }

    fn parse_list(&mut self) -> Result<Value> {
        let (outer_span, contents) = self.capture_span(|s| s.parse_seq(')'));

        contents.map(|contents| Value::List(outer_span, contents))
    }

    fn parse_vector(&mut self) -> Result<Value> {
        let (outer_span, contents) = self.capture_span(|s| s.parse_seq(']'));

        contents.map(|contents| Value::Vector(outer_span, contents))
    }

    fn parse_map(&mut self) -> Result<Value> {
        // First get the contents without splitting pairwise
        let (span, unpaired_contents) = self.capture_span(|s| s.parse_seq('}'));

        let unpaired_contents = unpaired_contents?;

        if unpaired_contents.len() % 2 == 1 {
            return Err(Error::UnevenMap(span));
        }

        let contents: Vec<(Value, Value)> = unpaired_contents
            .chunks(2)
            .map(|chunk| (chunk[0].clone(), chunk[1].clone()))
            .collect();

        Ok(Value::Map(span, contents))
    }

    fn parse_set(&mut self) -> Result<Value> {
        let (outer_span, contents) = self.capture_span(|s| s.parse_seq('}'));

        // Cover the # in our span
        let adj_span = outer_span.with_lo(outer_span.lo - 1);
        contents.map(|contents| Value::Set(adj_span, contents))
    }

    fn parse_quote_escape(&mut self) -> Result<char> {
        let escape_lo = self.consumed_bytes as u32;

        match self.consume_char()? {
            't' => Ok('\t'),
            'r' => Ok('\r'),
            'n' => Ok('\n'),
            '\\' => Ok('\\'),
            '"' => Ok('"'),
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
    fn parse_string(&mut self) -> Result<Value> {
        let (span, contents) = self.capture_span(|s| {
            // Eat the opening quote
            s.eat_bytes(1);

            let mut contents = String::new();
            loop {
                match s.consume_char()? {
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
        contents.map(|contents| Value::String(span, contents))
    }

    fn parse_identifier_content(&mut self) -> &str {
        self.consume_until(|c| !is_identifier_char(c)).1
    }

    fn parse_identifier(&mut self) -> Result<Value> {
        let (span, content) = self.capture_span(|s| s.parse_identifier_content().to_owned());

        if content.len() == 0 {
            let (span, next_char) = self.capture_span(|s| s.consume_char());
            return Err(Error::UnexpectedChar(span, next_char?));
        }

        match content.as_ref() {
            "true" => Ok(Value::Bool(span, true)),
            "false" => Ok(Value::Bool(span, false)),
            _ => Ok(Value::Symbol(span, content)),
        }
    }

    fn parse_symbol_shorthand(&mut self, expansion: &str) -> Result<Value> {
        let (outer_span, (shorthand_span, quoted_datum)) = self.capture_span(|s| {
            let (shorthand_span, _) = s.capture_span(|s| {
                // Discard the shorthand. Note this must be ASCII.
                s.eat_bytes(1);
            });

            (shorthand_span, s.parse_datum())
        });

        quoted_datum.map(|quoted_datum| {
            Value::List(
                outer_span,
                vec![
                    Value::Symbol(shorthand_span, expansion.to_owned()),
                    quoted_datum,
                ],
            )
        })
    }

    fn parse_datum(&mut self) -> Result<Value> {
        match self.skip_until_non_whitespace()? {
            '(' => self.parse_list(),
            '[' => self.parse_vector(),
            '{' => self.parse_map(),
            '0'...'9' => self.parse_int(),
            '-' => self.parse_negative_or_symbol(),
            '\'' => self.parse_symbol_shorthand("quote"),
            '"' => self.parse_string(),
            '\\' => self.parse_char(),
            '#' => self.parse_dispatch(),
            _ => self.parse_identifier(),
        }
    }

    fn parse_data(&mut self) -> Result<Vec<Value>> {
        let mut datum_vec = Vec::new();

        // Keep eating datums until we hit EOF
        loop {
            match self.skip_until_non_whitespace() {
                Ok(_) => {
                    let datum = self.parse_datum()?;
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
use syntax::span::t2s;

#[cfg(test)]
fn whole_str_span(v: &str) -> Span {
    Span {
        lo: 0,
        hi: v.len() as u32,
    }
}

#[cfg(test)]
fn datum_from_str(s: &str) -> Result<Value> {
    let mut parser = Parser::from_str(s, 0);

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

#[test]
fn bool_datum() {
    let j = "false";
    let t = "^^^^^";
    let expected = Value::Bool(t2s(t), false);

    assert_eq!(expected, datum_from_str(j).unwrap());

    let j = "true";
    let t = "^^^^";
    let expected = Value::Bool(t2s(t), true);
    assert_eq!(expected, datum_from_str(j).unwrap());

    let j = "     false";
    let t = "     ^^^^^";
    let expected = Value::Bool(t2s(t), false);
    assert_eq!(expected, datum_from_str(j).unwrap());

    let j = "\ttrue\t";
    let t = "\t^^^^\t";
    let expected = Value::Bool(t2s(t), true);
    assert_eq!(expected, datum_from_str(j).unwrap());

    let j = " true 1";
    let t = "      ^";
    let err = Error::TrailingCharacters(t2s(t));
    assert_eq!(err, datum_from_str(j).unwrap_err());

    let j = " trueorfalse  ";
    let t = " ^^^^^^^^^^^  ";
    let expected = Value::Symbol(t2s(t), "trueorfalse".to_owned());
    assert_eq!(expected, datum_from_str(j).unwrap());
}

#[test]
fn list_datum() {
    let j = "() ; with a comment";
    let t = "^^                 ";
    let expected = Value::List(t2s(t), vec![]);
    assert_eq!(expected, datum_from_str(j).unwrap());

    let j = "( true   false )";
    let t = "^^^^^^^^^^^^^^^^";
    let u = "  ^^^^          ";
    let v = "         ^^^^^  ";

    let expected = Value::List(
        t2s(t),
        vec![Value::Bool(t2s(u), true), Value::Bool(t2s(v), false)],
    );
    assert_eq!(expected, datum_from_str(j).unwrap());

    let j = "(1, 2, (3))";
    let t = "^^^^^^^^^^^";
    let u = " ^         ";
    let v = "    ^      ";
    let w = "       ^^^ ";
    let x = "        ^  ";

    let expected = Value::List(
        t2s(t),
        vec![
            Value::Int(t2s(u), 1),
            Value::Int(t2s(v), 2),
            Value::List(t2s(w), vec![Value::Int(t2s(x), 3)]),
        ],
    );
    assert_eq!(expected, datum_from_str(j).unwrap());

    let j = "(true";
    let err = Error::Eof;
    assert_eq!(err, datum_from_str(j).unwrap_err());

    let j = "(true))";
    let t = "      ^";
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
    let expected = Value::Vector(t2s(t), vec![]);
    assert_eq!(expected, datum_from_str(j).unwrap());

    let j = "[ true   (true false) ]";
    let t = "^^^^^^^^^^^^^^^^^^^^^^^";
    let u = "  ^^^^                 ";
    let v = "         ^^^^^^^^^^^^  ";
    let w = "          ^^^^         ";
    let x = "               ^^^^^   ";

    let expected = Value::Vector(
        t2s(t),
        vec![
            Value::Bool(t2s(u), true),
            Value::List(
                t2s(v),
                vec![Value::Bool(t2s(w), true), Value::Bool(t2s(x), false)],
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
fn symbol_datum() {
    for test_symbol in vec![
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
        let expected = Value::Symbol(s, test_symbol.to_owned());

        assert_eq!(expected, datum_from_str(test_symbol).unwrap());
    }
}

#[test]
fn keyword_symbol_datum() {
    for test_symbol in vec![":HELLO", ":HELLO123", ":predicate?", ":mutate!"] {
        let s = whole_str_span(test_symbol);
        let expected = Value::Symbol(s, test_symbol.to_owned());

        assert_eq!(expected, datum_from_str(test_symbol).unwrap());
    }
}

#[test]
fn string_datum() {
    let test_strings = vec![
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

    for (test_string, expected_contents) in test_strings {
        let s = whole_str_span(test_string);
        let expected = Value::String(s, expected_contents.to_owned());

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

    for (j, expected_char) in test_chars {
        let s = whole_str_span(j);
        let expected = Value::Char(s, expected_char);

        assert_eq!(expected, datum_from_str(j).unwrap());
    }

    let j = r#"\SPACE"#;
    let t = r#" ^^^^^"#;
    let err = Error::InvalidCharLiteral(t2s(t));
    assert_eq!(err, datum_from_str(j).unwrap_err());

    let j = r#"\u110000"#;
    let t = r#" ^^^^^^^"#;
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
        let expected = Value::Int(s, expected_int);

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
fn map_datum() {
    let j = "{}";
    let t = "^^";
    let expected = Value::Map(t2s(t), vec![]);
    assert_eq!(expected, datum_from_str(j).unwrap());

    let j = "{ 1,2 ,, 3  4}";
    let t = "^^^^^^^^^^^^^^";
    let u = "  ^           ";
    let v = "    ^         ";
    let w = "         ^    ";
    let x = "            ^ ";

    let mut expected_contents = Vec::<(Value, Value)>::new();
    expected_contents.push((Value::Int(t2s(u), 1), Value::Int(t2s(v), 2)));
    expected_contents.push((Value::Int(t2s(w), 3), Value::Int(t2s(x), 4)));
    let expected = Value::Map(t2s(t), expected_contents);

    assert_eq!(expected, datum_from_str(j).unwrap());

    let j = "{1 {2 3}}";
    let t = "^^^^^^^^^";
    let u = " ^       ";
    let v = "   ^^^^^ ";
    let w = "    ^    ";
    let x = "      ^  ";

    let mut inner_contents = Vec::<(Value, Value)>::new();
    inner_contents.push((Value::Int(t2s(w), 2), Value::Int(t2s(x), 3)));
    let inner = Value::Map(t2s(v), inner_contents);

    let mut outer_contents = Vec::<(Value, Value)>::new();
    outer_contents.push((Value::Int(t2s(u), 1), inner));
    let expected = Value::Map(t2s(t), outer_contents);

    assert_eq!(expected, datum_from_str(j).unwrap());

    let j = "{1}";
    let t = "^^^";
    let err = Error::UnevenMap(t2s(t));
    assert_eq!(err, datum_from_str(j).unwrap_err());
}

#[test]
fn set_datum() {
    let j = "#{}";
    let t = "^^^";
    let expected = Value::Set(t2s(t), Vec::new());
    assert_eq!(expected, datum_from_str(j).unwrap());

    let j = "#{ 1 2  3 4}";
    let t = "^^^^^^^^^^^^";
    let u = "   ^        ";
    let v = "     ^      ";
    let w = "        ^   ";
    let x = "          ^ ";

    let mut expected_contents = Vec::<Value>::new();
    expected_contents.push((Value::Int(t2s(u), 1)));
    expected_contents.push((Value::Int(t2s(v), 2)));
    expected_contents.push((Value::Int(t2s(w), 3)));
    expected_contents.push((Value::Int(t2s(x), 4)));
    let expected = Value::Set(t2s(t), expected_contents);

    assert_eq!(expected, datum_from_str(j).unwrap());

    let j = "#{1 #{2 3}}";
    let t = "^^^^^^^^^^^";
    let u = "  ^        ";
    let v = "    ^^^^^^ ";
    let w = "      ^    ";
    let x = "        ^  ";

    let mut inner_contents = Vec::<Value>::new();
    inner_contents.push((Value::Int(t2s(w), 2)));
    inner_contents.push((Value::Int(t2s(x), 3)));
    let inner = Value::Set(t2s(v), inner_contents);

    let mut outer_contents = Vec::<Value>::new();
    outer_contents.push((Value::Int(t2s(u), 1)));
    outer_contents.push(inner);
    let expected = Value::Set(t2s(t), outer_contents);

    assert_eq!(expected, datum_from_str(j).unwrap());
}

#[test]
fn quote_shorthand() {
    let j = "'foo";
    let t = "^^^^";
    let u = "^   ";
    let v = " ^^^";

    let expected = Value::List(
        t2s(t),
        vec![
            Value::Symbol(t2s(u), "quote".to_owned()),
            Value::Symbol(t2s(v), "foo".to_owned()),
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

    let expected = Value::List(
        t2s(t),
        vec![
            Value::Symbol(t2s(u), "quote".to_owned()),
            Value::List(
                t2s(v),
                vec![
                    Value::Int(t2s(w), 1),
                    Value::Int(t2s(x), 2),
                    Value::Int(t2s(y), 3),
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
fn invalid_dispatch() {
    let j = r#"#loop"#;
    let t = r#"^^   "#;
    let err = Error::InvalidDispatch(t2s(t));

    assert_eq!(err, datum_from_str(j).unwrap_err());

    let j = "#";
    let err = Error::Eof;

    assert_eq!(err, datum_from_str(j).unwrap_err());
}

#[test]
fn datum_comment() {
    let j = "(Hello #_(you jerk))";
    let t = "^^^^^^^^^^^^^^^^^^^^";
    let u = " ^^^^^              ";

    let expected = Value::List(t2s(t), vec![Value::Symbol(t2s(u), "Hello".to_owned())]);
    assert_eq!(expected, datum_from_str(j).unwrap());

    let j = "(Hello #_  you jerk)";
    let t = "^^^^^^^^^^^^^^^^^^^^";
    let u = " ^^^^^              ";
    let v = "               ^^^^ ";

    let expected = Value::List(
        t2s(t),
        vec![
            Value::Symbol(t2s(u), "Hello".to_owned()),
            Value::Symbol(t2s(v), "jerk".to_owned()),
        ],
    );
    assert_eq!(expected, datum_from_str(j).unwrap());
}

#[test]
fn multiple_data() {
    let j = " 1  #_two 3  ";
    let t = " ^           ";
    let u = "          ^  ";

    let expected = vec![Value::Int(t2s(t), 1), Value::Int(t2s(u), 3)];
    assert_eq!(expected, data_from_str(j).unwrap());

    let j = "(true)))";
    let t = "      ^ ";
    let err = Error::UnexpectedChar(t2s(t), ')');
    assert_eq!(err, data_from_str(j).unwrap_err());
}
