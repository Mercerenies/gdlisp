
use serde::{Serialize, Deserialize};

#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct Argument {
  pub name: String,
  #[serde(rename = "type")]
  pub argument_type: String,
  pub has_default_value: bool,
  pub default_value: String,
}
