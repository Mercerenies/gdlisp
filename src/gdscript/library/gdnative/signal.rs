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

use super::argument::Argument;

use serde::{Serialize, Deserialize};

#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct Signal {
  pub name: String,
  pub arguments: Vec<Argument>,
}

#[cfg(test)]
mod tests {
  use super::*;
  use serde_json;

  #[test]
  fn deserialize_signal_test() {
    let signal_str = r#"{
				"name": "mesh_updated",
				"arguments": [
					{
						"name": "mesh",
						"type": "Mesh",
						"has_default_value": false,
						"default_value": ""
					}
				]
			}"#;
    let result: Signal = serde_json::from_str(signal_str).unwrap();
    assert_eq!(result, Signal {
      name: String::from("mesh_updated"),
      arguments: vec![
        Argument {
          name: String::from("mesh"),
          argument_type: String::from("Mesh"),
          has_default_value: false,
          default_value: String::from(""),
        },
      ],
    });
  }

}
