// Copyright 2023 Silvio Mayolo
//
// This file is part of GDLisp.
//
// GDLisp is free software: you can redistribute it and/or modify it
// under the terms of the GNU General Public License as published by
// the Free Software Foundation, either version 3 of the License, or
// (at your option) any later version.
//
// GDLisp is distributed in the hope that it will be useful, but
// WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
// General Public License for more details.
//
// You should have received a copy of the GNU General Public License
// along with GDLisp. If not, see <https://www.gnu.org/licenses/>.

//! Project-specific configuration data.

use crate::runner::version::VersionInfo;

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
  /// The Godot version being used to compile this project.
  pub godot_version: VersionInfo,
}
