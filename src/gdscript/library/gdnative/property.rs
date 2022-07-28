
use serde::{Serialize, Deserialize};

#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct Property {
  pub name: String,
  #[serde(rename = "type")]
  pub property_type: String,
  pub getter: String,
  pub setter: String,
  pub index: i32,
}

#[cfg(test)]
mod tests {
  use super::*;
  use serde_json;

  #[test]
  fn deserialize_property_test() {
    let property_str = r#"{
				"name": "editor_hint",
				"type": "bool",
				"getter": "is_editor_hint",
				"setter": "set_editor_hint",
				"index": -1
			}"#;
    let result: Property = serde_json::from_str(property_str).unwrap();
    assert_eq!(result, Property {
      name: String::from("editor_hint"),
      property_type: String::from("bool"),
      getter: String::from("is_editor_hint"),
      setter: String::from("set_editor_hint"),
      index: -1,
    });
  }

}
