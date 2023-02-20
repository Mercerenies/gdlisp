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
