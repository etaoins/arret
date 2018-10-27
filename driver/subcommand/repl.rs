use std::borrow::Cow;
use std::io::prelude::*;
use std::io::BufReader;
use std::{fs, path};

use ansi_term::Colour;
use directories;
use rustyline;

use compiler;

use crate::DriverConfig;

const PROMPT: &str = "arret> ";
const TYPE_ONLY_PREFIX: &str = ":type ";

struct ArretHelper {
    bound_names: Vec<String>,
}

impl ArretHelper {
    fn new<'a>(names_iter: impl Iterator<Item = &'a str>) -> ArretHelper {
        let mut all_names = names_iter.map(|s| s.to_owned()).collect::<Vec<String>>();
        all_names.sort();

        ArretHelper {
            bound_names: all_names,
        }
    }
}

fn report_all_to_stderr(source_loader: &compiler::SourceLoader, err: &compiler::error::Error) {
    use compiler::reporting::report_to_stderr;

    for reportable in err.reports() {
        report_to_stderr(source_loader, reportable.as_ref())
    }
}

impl rustyline::completion::Completer for ArretHelper {
    type Candidate = String;

    fn complete(&self, line: &str, pos: usize) -> rustyline::Result<(usize, Vec<String>)> {
        use syntax::parser::is_identifier_char;

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
            .bound_names
            .iter()
            .filter_map(|name| {
                if name.starts_with(prefix) && name.ends_with(suffix) {
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
    fn hint(&self, _line: &str, _pos: usize) -> Option<String> {
        None
    }
}

impl rustyline::highlight::Highlighter for ArretHelper {
    fn highlight_prompt<'p>(&self, prompt: &'p str) -> Cow<'p, str> {
        let prompt_style = Colour::Blue;
        prompt_style.paint(prompt).to_string().into()
    }
}

impl rustyline::Helper for ArretHelper {}

/// Gets the full path to where our REPL history should be stored
///
/// This does very little error handling as history is a "nice to have" feature
fn repl_history_path() -> Option<path::PathBuf> {
    let project_dirs = directories::ProjectDirs::from("xyz.arret-lang", "", "arret")?;
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
        ":help" => {
            println!("Available REPL commands:");
            println!();
            println!(":help                  print this summary");
            println!(":type <expression>     evaluate the type of the given expression");
            println!(":quit                  exit the REPL");
            ParsedCommand::Other
        }
        ":quit" => ParsedCommand::Quit,
        _ => ParsedCommand::EvalValue(line),
    }
}

pub fn interactive_loop(cfg: &DriverConfig, include_path: Option<path::PathBuf>) {
    use compiler::repl::{EvalKind, EvaledLine};
    use rustyline;
    use rustyline::error::ReadlineError;

    // Setup our REPL backend
    let mut source_loader = compiler::SourceLoader::new();
    let mut repl_ctx =
        compiler::repl::ReplCtx::new(&cfg.package_paths, &mut source_loader, cfg.llvm_opt);

    // Setup Rustyline
    let mut rl = rustyline::Editor::<ArretHelper>::new();

    // Import [stdlib base] so we have most useful things defined
    let initial_import = "(import [stdlib base])".to_owned();
    if let Err(err) = repl_ctx.eval_line(initial_import, EvalKind::Value) {
        report_all_to_stderr(repl_ctx.source_loader(), &err);
    }
    rl.set_helper(Some(ArretHelper::new(repl_ctx.bound_names())));

    // Process the include file if specified
    if let Some(include_path) = include_path {
        let include_file = fs::File::open(include_path).unwrap();
        for include_line in BufReader::new(include_file).lines() {
            if let Err(err) = repl_ctx.eval_line(include_line.unwrap(), EvalKind::Value) {
                report_all_to_stderr(repl_ctx.source_loader(), &err);
            }
        }
    }

    // Load our history
    let history_path = repl_history_path();
    if let Some(ref history_path) = history_path {
        let _ = rl.load_history(history_path);
    }

    // Configure our styles
    let defs_style = Colour::Purple.bold();
    let expr_arrow_style = Colour::Green.bold();

    loop {
        let readline = rl.readline(&PROMPT);

        match readline {
            Ok(line) => {
                if !line.chars().all(char::is_whitespace) {
                    rl.add_history_entry(line.clone());
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

                match repl_ctx.eval_line(input, eval_kind) {
                    Ok(EvaledLine::EmptyInput) => {}
                    Ok(EvaledLine::Defs) => {
                        // Refresh our completions
                        rl.set_helper(Some(ArretHelper::new(repl_ctx.bound_names())));

                        println!("{}", defs_style.paint("defined"))
                    }
                    Ok(EvaledLine::Expr(value)) => {
                        println!("{} {}", expr_arrow_style.paint("=>"), value);
                    }
                    Err(err) => {
                        report_all_to_stderr(repl_ctx.source_loader(), &err);
                    }
                }
            }
            Err(ReadlineError::Interrupted) | Err(ReadlineError::Eof) => break,
            Err(other) => {
                panic!("Readline error: {:?}", other);
            }
        }

        if let Some(ref history_path) = history_path {
            let _ = rl.save_history(&history_path);
        }
    }
}
