use std::borrow::Cow;
use std::io::prelude::*;
use std::io::BufReader;
use std::sync::Arc;
use std::{fs, path};

use ansi_term::{Colour, Style};

use arret_syntax::datum::DataStr;
use arret_syntax::span::ByteIndex;

use arret_compiler::{emit_diagnostics_to_stderr, CompileCtx};

const PROMPT: &str = "arret> ";

const TYPE_ONLY_PREFIX: &str = "/type ";
const QUIT_COMMAND: &str = "/quit";
const HELP_COMMAND: &str = "/help";

/// Maximum line length we'll provide parser hints and error highlighting for
///
/// This requires parsing the whole line and we don't support incremental reparsing. This means
/// in the worst case of pasting a large line character-by-character we'll behave O(n!) with
/// with the size of the pasted line. This seems like a reasonable cutoff where a human isn't
/// typing the input.
const MAXIMUM_PARSED_LINE_LEN: usize = 512;

/// Completions that don't map to a bound value in scope
const UNBOUND_COMPLETIONS: &[&str] = &[
    TYPE_ONLY_PREFIX,
    QUIT_COMMAND,
    HELP_COMMAND,
    "true",
    "false",
    "##NaN",
    "##Inf",
    "##-Inf",
];

fn error_for_line(mut line: &str) -> Option<arret_syntax::error::Error> {
    use arret_syntax::parser::datum_from_str_with_span_offset;

    let span_offset = if line.starts_with(TYPE_ONLY_PREFIX) {
        line = &line[TYPE_ONLY_PREFIX.len()..];
        TYPE_ONLY_PREFIX.len()
    } else {
        0
    };

    // Is this a command?
    if line.starts_with('/') ||
    // Or empty?
    line.chars().all(char::is_whitespace) ||
    // Or is too large to parse interactively?
    line.len() > MAXIMUM_PARSED_LINE_LEN
    {
        return None;
    }

    datum_from_str_with_span_offset(line, ByteIndex(span_offset as u32)).err()
}

fn expected_content_for_line(line: &str) -> Option<arret_syntax::error::ExpectedContent> {
    error_for_line(line).and_then(|error| {
        if let arret_syntax::error::ErrorKind::Eof(expected_content) = error.kind() {
            Some(*expected_content)
        } else {
            None
        }
    })
}

struct ArretHelper {
    all_names: Vec<DataStr>,
}

impl ArretHelper {
    fn new(mut bound_names: Vec<DataStr>) -> ArretHelper {
        bound_names.extend(UNBOUND_COMPLETIONS.iter().map(|unbound| (*unbound).into()));
        bound_names.sort();

        ArretHelper {
            all_names: bound_names,
        }
    }
}

impl rustyline::completion::Completer for ArretHelper {
    type Candidate = String;

    fn complete(
        &self,
        line: &str,
        pos: usize,
        _: &rustyline::Context<'_>,
    ) -> rustyline::Result<(usize, Vec<String>)> {
        use arret_syntax::parser::is_identifier_char;

        let prefix_start = line[0..pos]
            .rfind(|c| !is_identifier_char(c))
            .map(|i| i + 1)
            .unwrap_or(0);

        let prefix = &line[prefix_start..pos];

        let suffix = if line.len() > pos {
            let suffix_end = line[pos..]
                .find(|c| !is_identifier_char(c))
                .map(|i| i + pos)
                .unwrap_or_else(|| line.len());
            &line[pos..suffix_end]
        } else {
            ""
        };

        let options = self
            .all_names
            .iter()
            .filter_map(|name| {
                if name.starts_with('/') && pos != prefix.len() {
                    // Commands can only appear at the beginning of the line
                    None
                } else if name.starts_with(prefix) && name.ends_with(suffix) {
                    Some((&name[0..name.len() - suffix.len()]).to_owned())
                } else {
                    None
                }
            })
            .collect();

        Ok((prefix_start, options))
    }
}

impl rustyline::hint::Hinter for ArretHelper {
    fn hint(&self, line: &str, pos: usize, _: &rustyline::Context<'_>) -> Option<String> {
        use arret_syntax::error::ExpectedContent;
        use arret_syntax::parser::is_identifier_char;

        let expected_content = expected_content_for_line(line);

        // If we're inside a string we shouldn't try to hint identifiers
        if let Some(ExpectedContent::String(_)) = expected_content {
            return Some("\"".to_owned());
        }

        let last_ident_start = line
            .rfind(|c| !is_identifier_char(c))
            .map(|i| i + 1)
            .unwrap_or(0);

        let last_ident = &line[last_ident_start..];

        if !last_ident.is_empty() {
            for name in self.all_names.iter() {
                if name.starts_with('/') && pos != last_ident.len() {
                    // Command not at the start of the line
                    continue;
                }

                if name.starts_with(last_ident) && name.len() > last_ident.len() {
                    return Some(name[last_ident.len()..].to_owned());
                }
            }
        }

        expected_content
            .and_then(|ec| ec.expected_close_char())
            .map(|c| c.to_string())
    }
}

impl rustyline::highlight::Highlighter for ArretHelper {
    fn highlight<'l>(&self, line: &'l str, _pos: usize) -> Cow<'l, str> {
        // See if we have an error
        let error_span = error_for_line(line).and_then(|error| {
            if let arret_syntax::error::ErrorKind::Eof(ec) = error.kind() {
                // We'll already be hinting at the end of the line so point to the opening char
                ec.open_char_span()
            } else {
                Some(error.span())
            }
        });

        let error_span = if let Some(error_span) = error_span {
            error_span
        } else {
            return line.into();
        };

        let error_start = error_span.start().to_usize();
        let error_end = error_span.end().to_usize();

        let prefix = &line[0..error_start];
        let error = &line[error_start..error_end];
        let suffix = &line[error_end..];

        let error_style = Colour::Red.bold();
        format!("{}{}{}", prefix, error_style.paint(error), suffix).into()
    }

    fn highlight_prompt<'b, 's: 'b, 'p: 'b>(
        &'s self,
        prompt: &'p str,
        _default: bool,
    ) -> Cow<'b, str> {
        let prompt_style = Colour::Fixed(25); // DeepSkyBlue4 (#005faf)
        prompt_style.paint(prompt).to_string().into()
    }

    fn highlight_hint<'h>(&self, hint: &'h str) -> Cow<'h, str> {
        use arret_syntax::parser::is_identifier_char;

        if hint.chars().next().map(is_identifier_char) == Some(true) {
            // This is a name completion
            let name_style = Style::new().dimmed();
            name_style.paint(hint).to_string().into()
        } else {
            // This is an unexpected EOF hint
            let unexpected_eof_style = Colour::Red.bold();
            unexpected_eof_style.paint(hint).to_string().into()
        }
    }

    fn highlight_char(&self, line: &str, _pos: usize) -> bool {
        // Essentially any character can change highlighting:
        //
        // - Delimiters can change the structure of input
        // - Identifier characters can make char literals (e.g. \newline) change validity
        line.len() <= MAXIMUM_PARSED_LINE_LEN
    }
}

impl rustyline::Helper for ArretHelper {}

/// Gets the full path to where our REPL history should be stored
///
/// This does very little error handling as history is a "nice to have" feature
fn repl_history_path() -> Option<path::PathBuf> {
    let project_dirs = directories::ProjectDirs::from("org.arret-lang", "", "arret")?;
    let data_dir = project_dirs.data_dir();

    fs::create_dir_all(data_dir).ok()?;
    Some(data_dir.join("repl-history"))
}

enum ParsedCommand {
    EvalValue(String),
    EvalType(String),
    Quit,
    Other,
}

fn parse_command(mut line: String) -> ParsedCommand {
    match line.as_ref() {
        _ if line.starts_with(TYPE_ONLY_PREFIX) => {
            line.drain(0..TYPE_ONLY_PREFIX.len());
            ParsedCommand::EvalType(line)
        }
        HELP_COMMAND => {
            println!("Available REPL commands:");
            println!();
            println!("/help                 Prints this summary");
            println!("/type <expression>    Evaluates the type of the given expression");
            println!("/quit                 Exits the REPL");
            ParsedCommand::Other
        }
        QUIT_COMMAND => ParsedCommand::Quit,
        _ => ParsedCommand::EvalValue(line),
    }
}

pub fn interactive_loop(ccx: Arc<CompileCtx>, include_path: Option<path::PathBuf>) {
    use arret_compiler::repl::{EvalKind, EvaledExprValue, EvaledLine};
    use rustyline::error::ReadlineError;

    // Setup our REPL backend
    let repl_ctx = arret_compiler::repl::ReplCtx::new(ccx.clone());

    // Setup Rustyline
    let mut rl = rustyline::Editor::<ArretHelper>::new();

    // Import [stdlib base] so we have most useful things defined
    let initial_import = "(import [stdlib base])".to_owned();
    repl_ctx.send_line(initial_import, EvalKind::Value).unwrap();
    let mut sent_prelude_lines = 1;

    if let Some(include_path) = include_path {
        let include_file = fs::File::open(include_path).unwrap();

        // Import the include file line-by-line
        for line in BufReader::new(include_file).lines() {
            repl_ctx.send_line(line.unwrap(), EvalKind::Value).unwrap();
            sent_prelude_lines += 1
        }
    }

    // Load our history while the REPL engine is thinking
    let history_path = repl_history_path();
    if let Some(ref history_path) = history_path {
        let _ = rl.load_history(history_path);
    }

    // Collect all the responses
    for _ in 0..sent_prelude_lines {
        match repl_ctx.receive_result() {
            Ok(EvaledLine::Defs(bound_names)) => {
                rl.set_helper(Some(ArretHelper::new(bound_names)));
            }
            Ok(_) => {}
            Err(diagnostics) => emit_diagnostics_to_stderr(ccx.source_loader(), diagnostics),
        }
    }

    // Configure our styles
    let defs_style = Colour::Purple.bold();

    let expr_arrow_style = Colour::Green.bold();
    let type_style = Colour::Fixed(166); // DarkOrange3 (#d75f00)
    let type_brackets_style = Style::new().dimmed();

    loop {
        let mut history_dirty = false;
        let readline = rl.readline(&PROMPT);

        match readline {
            Ok(line) => {
                if !line.chars().all(char::is_whitespace) {
                    history_dirty = rl.add_history_entry(line.clone());
                }

                let (eval_kind, input) = match parse_command(line) {
                    ParsedCommand::EvalValue(input) => (EvalKind::Value, input),
                    ParsedCommand::EvalType(input) => (EvalKind::Type, input),
                    ParsedCommand::Quit => {
                        break;
                    }
                    ParsedCommand::Other => {
                        continue;
                    }
                };

                repl_ctx.send_line(input, eval_kind).unwrap();

                if history_dirty {
                    // Write our history while the REPL engine is thinking
                    if let Some(ref history_path) = history_path {
                        let _ = rl.save_history(&history_path);
                    }
                }

                match repl_ctx.receive_result() {
                    Ok(EvaledLine::EmptyInput) => {}
                    Ok(EvaledLine::Defs(bound_names)) => {
                        // Refresh our completions
                        rl.set_helper(Some(ArretHelper::new(bound_names)));

                        println!("{}", defs_style.paint("defined"))
                    }
                    Ok(EvaledLine::ExprType(type_str)) => {
                        println!(
                            "{} {}",
                            expr_arrow_style.paint("=>"),
                            type_style.paint(type_str)
                        );
                    }
                    Ok(EvaledLine::ExprValue(evaled_expr)) => {
                        let EvaledExprValue {
                            value_str,
                            type_str,
                            type_is_literal,
                        } = evaled_expr;

                        if type_is_literal {
                            println!(
                                // => value
                                "{} {}",
                                expr_arrow_style.paint("=>"),
                                value_str,
                            );
                        } else {
                            println!(
                                // => [value Type]
                                "{} {}{} {}{}",
                                expr_arrow_style.paint("=>"),
                                type_brackets_style.paint("["),
                                value_str,
                                type_style.paint(type_str),
                                type_brackets_style.paint("]"),
                            );
                        }
                    }
                    Err(diagnostics) => {
                        emit_diagnostics_to_stderr(ccx.source_loader(), diagnostics);
                    }
                }
            }
            Err(ReadlineError::Interrupted) | Err(ReadlineError::Eof) => break,
            Err(other) => {
                panic!("Readline error: {:?}", other);
            }
        }
    }
}
