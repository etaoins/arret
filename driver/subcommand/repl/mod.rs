mod arret_helper;
mod command;
mod history;
mod syntax;

use std::io::prelude::*;
use std::io::BufReader;
use std::sync::Arc;
use std::{fs, path};

use ansi_term::{Colour, Style};

use arret_compiler::{emit_diagnostics_to_stderr, CompileCtx};

use arret_helper::ArretHelper;
use command::{parse_command, ParsedCommand};
use history::repl_history_path;

const PROMPT: &str = "arret> ";

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
        let readline = rl.readline(PROMPT);

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
