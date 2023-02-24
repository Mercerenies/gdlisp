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
