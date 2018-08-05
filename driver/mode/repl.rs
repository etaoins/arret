use ansi_term::Colour;
use app_dirs;

use compiler;

use DriverConfig;

const APP_INFO: app_dirs::AppInfo = app_dirs::AppInfo {
    name: "arret",
    author: "arret",
};

pub fn interactive_loop(cfg: &DriverConfig) {
    use compiler::repl::EvaledLine;
    use rustyline::error::ReadlineError;
    use rustyline::Editor;

    // Setup our REPL backend
    let mut source_loader = compiler::SourceLoader::new();
    let mut repl_ctx = compiler::repl::ReplCtx::new(&cfg.package_paths, &mut source_loader);
    let mut rl = Editor::<()>::new();

    // Import [stdlib base] so we have most useful things defined
    let initial_import = "(import [stdlib base])".to_owned();
    if let Err(err) = repl_ctx.eval_line(initial_import) {
        for reportable in err.reports() {
            reportable.report(repl_ctx.source_loader())
        }
    }

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
