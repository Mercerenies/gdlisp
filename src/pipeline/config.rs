
use std::path::PathBuf;

#[derive(Debug, Clone)]
pub struct ProjectConfig {
  pub root_directory: PathBuf,
  pub optimizations: bool,
}
