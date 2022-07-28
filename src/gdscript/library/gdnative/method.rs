
use super::argument::Argument;

use serde::{Serialize, Deserialize};

#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct Method {
  pub name: String,
  pub return_type: String,
  pub is_editor: bool,
  pub is_noscript: bool,
  pub is_const: bool,
  pub is_reverse: bool,
  pub is_virtual: bool,
  pub has_varargs: bool,
  pub is_from_script: bool,
  pub arguments: Vec<Argument>,
}
