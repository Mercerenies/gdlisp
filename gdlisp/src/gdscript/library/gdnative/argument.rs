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

  #[test]
  fn deserialize_argument_test() {
    let argument_str = r#"{
						"name": "mesh",
						"type": "Mesh",
						"has_default_value": false,
						"default_value": ""
					}"#;
    let argument = Argument {
      name: String::from("mesh"),
      argument_type: String::from("Mesh"),
      has_default_value: false,
      default_value: String::from(""),
    };
    assert_eq!(serde_json::from_str::<Argument>(&argument_str).unwrap(), argument);
  }

}
