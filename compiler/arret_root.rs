use std::{env, path};

const ARRET_ROOT_ENV_VAR: &str = "ARRET_ROOT";

fn is_arret_root(path: &path::Path) -> bool {
    path.join("./.arret-root").is_file()
}

pub struct InvalidOptionError {
    invalid_path: path::PathBuf,
}

impl InvalidOptionError {
    /// Path to the invalid Arret root
    pub fn invalid_path(&self) -> &path::Path {
        &self.invalid_path
    }
}

pub struct InvalidEnvVarError {
    invalid_path: path::PathBuf,
}

impl InvalidEnvVarError {
    /// Environment variable that contained the invalid root
    pub fn env_var_name(&self) -> &'static str {
        ARRET_ROOT_ENV_VAR
    }

    /// Path to the invalid Arret root
    pub fn invalid_path(&self) -> &path::Path {
        &self.invalid_path
    }
}

pub enum FindArretRootError {
    /// Explicitly specified option was not an Arret root
    InvalidOption(InvalidOptionError),
    /// Environment variable with the given name is not an Arret root
    InvalidEnvVar(InvalidEnvVarError),
    /// Heuristic search failed
    NotFound,
}

/// Attempts to find the path to Arret root directory
///
/// The search order is:
/// 1. The `arret_root_option` parameter
/// 2. The `ARRET_ROOT` environment variable
/// 3. The path this binary was originally built in and all of its parents
/// 4. The current directory and all of its parents
pub fn find_arret_root(
    arret_root_option: Option<&str>,
) -> Result<path::PathBuf, FindArretRootError> {
    if let Some(arg_root) = arret_root_option {
        let arg_path = path::PathBuf::from(arg_root);
        if !is_arret_root(&arg_path) {
            return Err(FindArretRootError::InvalidOption(InvalidOptionError {
                invalid_path: arg_path,
            }));
        }

        return Ok(arg_path);
    }

    if let Some(env_root) = env::var_os(ARRET_ROOT_ENV_VAR) {
        let env_path = path::PathBuf::from(env_root);
        if !is_arret_root(&env_path) {
            return Err(FindArretRootError::InvalidEnvVar(InvalidEnvVarError {
                invalid_path: env_path,
            }));
        }

        return Ok(env_path);
    }

    if let Some(manifest_dir) = option_env!("CARGO_MANIFEST_DIR") {
        for candidate in path::Path::new(manifest_dir).ancestors() {
            if is_arret_root(candidate) {
                return Ok(candidate.to_owned());
            }
        }
    }

    let current_dir = env::current_dir().expect("Cannot determine current directory");
    for candidate in path::Path::new(&current_dir).ancestors() {
        if is_arret_root(candidate) {
            return Ok(candidate.to_owned());
        }
    }

    Err(FindArretRootError::NotFound)
}
