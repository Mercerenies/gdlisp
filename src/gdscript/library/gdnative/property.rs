
use serde::{Serialize, Deserialize};

#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct Property {
  pub name: String,
  #[serde(rename = "type")]
  pub property_type: String,
  pub getter: String,
  pub setter: bool,
  pub index: i32,
}
