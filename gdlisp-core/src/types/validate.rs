// Copyright 2024 Silvio Mayolo
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

use gdextension_serde::to_variant;

use serde::{Serialize, Deserialize};
use godot::prelude::{Dictionary, FromGodot};

use std::fmt::{self, Display};

/// The dictionary returned by the ScriptLanguageExtension::validate
/// implementation.
#[derive(Serialize, Deserialize, Debug, Clone)]
pub struct Validation {
  pub functions: Vec<String>,
  pub errors: Vec<ScriptError>,
  pub warnings: Vec<ScriptWarning>,
  pub safe_lines: Vec<i32>,
  pub valid: bool,
}

#[derive(Serialize, Deserialize, Debug, Clone)]
pub struct ScriptError {
  pub line: i32,
  pub column: i32,
  pub message: String,
}

#[derive(Serialize, Deserialize, Debug, Clone)]
pub struct ScriptWarning {
  pub start_line: i32,
  pub end_line: i32,
  pub leftmost_column: i32,
  pub rightmost_column: i32,
  pub code: i32,
  pub string_code: String,
  pub message: String,
}

impl Validation {
  pub fn into_dictionary(self) -> Dictionary {
    Dictionary::from_variant(&to_variant(&self).unwrap())
  }
}

impl Display for ScriptError {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> Result<(), fmt::Error> {
    write!(f, "line {} column {}: {}", self.line, self.column, self.message)
  }
}

impl Display for ScriptWarning {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> Result<(), fmt::Error> {
    write!(
      f,
      "line {}-{} column {}-{}: [{}] {}",
      self.start_line,
      self.end_line,
      self.leftmost_column,
      self.rightmost_column,
      self.code,
      self.message,
    )
  }
}
