
//! The [`ApiType`] enum, indicating whether a GDScript class is in
//! the core library or intended for editor tooling.

use serde::{Serialize, Deserialize};

#[derive(Clone, Debug, Eq, PartialEq, Serialize, Deserialize)]
#[serde(rename_all = "lowercase")]
pub enum ApiType {
  Core,
  Tools,
}

#[cfg(test)]
mod tests {
  use super::*;
  use serde_json;

  #[test]
  fn serialize_api_type_test() {
    let core = serde_json::to_string(&ApiType::Core).unwrap();
    assert_eq!(core, "\"core\"");
    let tools = serde_json::to_string(&ApiType::Tools).unwrap();
    assert_eq!(tools, "\"tools\"");
  }

  #[test]
  fn deserialize_api_type_test() {
    let core: ApiType = serde_json::from_str("\"core\"").unwrap();
    assert_eq!(core, ApiType::Core);
    let tools: ApiType = serde_json::from_str("\"tools\"").unwrap();
    assert_eq!(tools, ApiType::Tools);
  }

}
