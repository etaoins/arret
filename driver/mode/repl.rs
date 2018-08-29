use std::{fs, path};

use ansi_term::Colour;
use directories;
use rustyline;

use compiler;

use crate::DriverConfig;

const PROMPT: &str = "arret> ";
const TYPE_ONLY_PREFIX: &str = ":type ";

struct Completer {
    bound_names: Vec<String>,
}

impl Completer {
    fn new<'a>(names_iter: impl Iterator<Item = &'a str>) -> Completer {
        Completer {
            bound_names: names_iter.map(|s| s.to_owned()).collect(),
        }
    }
}

impl rustyline::completion::Completer for Completer {
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
            }).collect();

        Ok((prefix_start, options))
    }
}

/// Gets the full path to where our REPL history should be stored
///
/// This does very little error handling as history is a "nice to have" feature
fn repl_history_path() -> Option<path::PathBuf> {
    let project_dirs = directories::ProjectDirs::from("xyz.arret-lang", "", "arret")?;
    let data_dir = project_dirs.data_dir();

    fs::create_dir_all(data_dir).ok()?;
    Some(data_dir.join("repl-history"))
}

pub fn interactive_loop(cfg: &DriverConfig) {
    use compiler::repl::{EvalKind, EvaledLine};
    use compiler::reporting::report_to_stderr;
    use rustyline;
    use rustyline::error::ReadlineError;

    // Setup our REPL backend
    let mut source_loader = compiler::SourceLoader::new();
    let mut repl_ctx = compiler::repl::ReplCtx::new(&cfg.package_paths, &mut source_loader);
    let mut rl = rustyline::Editor::<Completer>::new();

    // Import [stdlib base] so we have most useful things defined
    let initial_import = "(import [stdlib base])".to_owned();
    if let Err(err) = repl_ctx.eval_line(initial_import, EvalKind::Value) {
        for reportable in err.reports() {
            report_to_stderr(repl_ctx.source_loader(), reportable.as_ref())
        }
    }
    rl.set_completer(Some(Completer::new(repl_ctx.bound_names())));

    // Load our history
    let history_path = repl_history_path();
    if let Some(ref history_path) = history_path {
        let _ = rl.load_history(history_path);
    }

    // Configure our styles
    let prompt_style = Colour::Blue;
    let defs_style = Colour::Purple.bold();
    let expr_arrow_style = Colour::Green.bold();
    let prompt = prompt_style.paint(PROMPT);

    loop {
        let readline = rl.readline(&prompt.to_string());

        match readline {
            Ok(mut line) => {
                if !line.chars().all(char::is_whitespace) {
                    rl.add_history_entry(&line);
                }

                let eval_kind = if line.starts_with(TYPE_ONLY_PREFIX) {
                    line.drain(0..TYPE_ONLY_PREFIX.len());
                    EvalKind::Type
                } else {
                    EvalKind::Value
                };

                match repl_ctx.eval_line(line, eval_kind) {
                    Ok(EvaledLine::EmptyInput) => {}
                    Ok(EvaledLine::Defs) => {
                        // Refresh our completions
                        rl.set_completer(Some(Completer::new(repl_ctx.bound_names())));

                        println!("{}", defs_style.paint("defined"))
                    }
                    Ok(EvaledLine::Expr(value)) => {
                        println!("{} {}", expr_arrow_style.paint("=>"), value);
                    }
                    Err(err) => {
                        for reportable in err.reports() {
                            report_to_stderr(repl_ctx.source_loader(), reportable.as_ref());
                        }
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
