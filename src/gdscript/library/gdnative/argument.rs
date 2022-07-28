
use serde::{Serialize, Deserialize};

#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct Argument {
  pub name: String,
  #[serde(rename = "type")]
  pub argument_type: String,
  pub has_default_value: bool,
  pub default_value: String,
}

#[cfg(test)]
mod tests {
  use super::*;
  use serde_json;

  #[test]
  fn serialize_roundtrip_argument_test() {
    let argument = Argument {
      name: String::from("mesh"),
      argument_type: String::from("Mesh"),
      has_default_value: false,
      default_value: String::from(""),
    };
    let serialized = serde_json::to_string(&argument).unwrap();
    assert_eq!(serde_json::from_str::<Argument>(&serialized).unwrap(), argument);
  }

}
