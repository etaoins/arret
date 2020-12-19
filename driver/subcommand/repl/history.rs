use std::{fs, path};

/// Gets the full path to where our REPL history should be stored
///
/// This does very little error handling as history is a "nice to have" feature
pub fn repl_history_path() -> Option<path::PathBuf> {
    let project_dirs = directories_next::ProjectDirs::from("org.arret-lang", "", "arret")?;
    let data_dir = project_dirs.data_dir();

    fs::create_dir_all(data_dir).ok()?;
    Some(data_dir.join("repl-history"))
}
