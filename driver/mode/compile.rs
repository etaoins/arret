use std::path;

use compiler;
use compiler::reporting::report_to_stderr;

use crate::DriverConfig;

pub fn compile_input_file(cfg: &DriverConfig, input_path: &path::Path) {
    let mut source_loader = compiler::SourceLoader::new();

    let source_file_id = source_loader
        .load_path(input_path)
        .expect("Unable to read input file");

    let hir = match compiler::lower_program(&cfg.package_paths, &mut source_loader, source_file_id)
    {
        Ok(hir) => hir,
        Err(errors) => {
            for err in errors {
                report_to_stderr(&source_loader, &err);
            }
            return;
        }
    };

    match compiler::infer_program(&hir.pvars, &hir.tvars, hir.module_defs) {
        Ok(inferred_defs) => {
            let mut pcx = compiler::PartialEvalCtx::new();

            for inferred_def in inferred_defs {
                println!("{:?}", inferred_def);
                pcx.consume_def(&hir.tvars, inferred_def);
            }
        }
        Err(errs) => {
            for err in errs {
                report_to_stderr(&source_loader, &err);
            }
            return;
        }
    }
}
