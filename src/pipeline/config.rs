
//! Project-specific configuration data.

use std::path::PathBuf;

/// This structure describes any project-specific settings and
/// information that is necessary throughout the project compilation
/// process.
#[derive(Debug, Clone)]
pub struct ProjectConfig {
  /// The path to the root directory of the Godot project. If we're
  /// compiling a full project, this directory should contain a
  /// `project.godot`. If we're compiling an individual file, this
  /// directory should contain that file.
  pub root_directory: PathBuf,
  /// Whether or not optimizations are turned on. Generally, this
  /// setting defaults to `true`, except for integration testing,
  /// where all optimizations are turned off.
  pub optimizations: bool,
}
