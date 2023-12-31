
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
