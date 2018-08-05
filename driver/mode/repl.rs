use ansi_term::Colour;
use app_dirs;
use rustyline;

use compiler;

use DriverConfig;

const APP_INFO: app_dirs::AppInfo = app_dirs::AppInfo {
    name: "arret",
    author: "arret",
};

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

pub fn interactive_loop(cfg: &DriverConfig) {
    use compiler::repl::EvaledLine;
    use rustyline;
    use rustyline::error::ReadlineError;

    // Setup our REPL backend
    let mut source_loader = compiler::SourceLoader::new();
    let mut repl_ctx = compiler::repl::ReplCtx::new(&cfg.package_paths, &mut source_loader);
    let mut rl = rustyline::Editor::<Completer>::new();

    // Import [stdlib base] so we have most useful things defined
    let initial_import = "(import [stdlib base])".to_owned();
    if let Err(err) = repl_ctx.eval_line(initial_import) {
        for reportable in err.reports() {
            reportable.report(repl_ctx.source_loader())
        }
    }
    rl.set_completer(Some(Completer::new(repl_ctx.bound_names())));

    // Load our history
    let mut history_path = app_dirs::app_root(app_dirs::AppDataType::UserData, &APP_INFO);
    if let Ok(ref mut history_path) = history_path {
        history_path.push("repl-history");
        let _ = rl.load_history(&history_path);
    }

    // Configure our styles
    let prompt_style = Colour::Blue;
    let defs_style = Colour::Black.bold();
    let prompt = prompt_style.paint("arret> ");

    loop {
        let readline = rl.readline(&prompt.to_string());

        match readline {
            Ok(line) => {
                if !line.chars().all(char::is_whitespace) {
                    rl.add_history_entry(&line);
                }

                match repl_ctx.eval_line(line) {
                    Ok(EvaledLine::EmptyInput) => {}
                    Ok(EvaledLine::Defs(count)) => {
                        // Refresh our completions
                        rl.set_completer(Some(Completer::new(repl_ctx.bound_names())));

                        let defs_noun = if count == 1 {
                            "definition"
                        } else {
                            "definitions"
                        };
                        println!(
                            "processed {} new {}",
                            defs_style.paint(count.to_string()),
                            defs_noun
                        );
                    }
                    Ok(EvaledLine::Expr(value)) => {
                        println!("=> {}", value);
                    }
                    Err(err) => {
                        for reportable in err.reports() {
                            reportable.report(repl_ctx.source_loader());
                        }
                    }
                }
            }
            Err(ReadlineError::Interrupted) | Err(ReadlineError::Eof) => break,
            Err(other) => {
                panic!("Readline error: {:?}", other);
            }
        }

        if let Ok(ref history_path) = history_path {
            let _ = rl.save_history(&history_path);
        }
    }
}