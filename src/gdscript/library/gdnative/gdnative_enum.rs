
use serde::{Serialize, Deserialize};

use std::collections::HashMap;

#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct Enum {
  pub name: String,
  pub values: HashMap<String, i32>,
}

#[cfg(test)]
mod tests {
  use super::*;
  use serde_json;

  #[test]
  fn deserialize_enum_test() {
    let enum_str = r#"{
				"name": "TrackerHand",
				"values": {
					"TRACKER_HAND_UNKNOWN": 0,
					"TRACKER_LEFT_HAND": 1,
					"TRACKER_RIGHT_HAND": 2
				}
			}"#;
    let enum_val = Enum {
      name: String::from("TrackerHand"),
      values: HashMap::from([
        (String::from("TRACKER_HAND_UNKNOWN"), 0),
        (String::from("TRACKER_LEFT_HAND"), 1),
        (String::from("TRACKER_RIGHT_HAND"), 2),
      ]),
    };
    assert_eq!(serde_json::from_str::<Enum>(&enum_str).unwrap(), enum_val);
  }

}
