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
pub struct Method {
  pub name: String,
  pub return_type: String,
  pub is_editor: bool,
  pub is_noscript: bool,
  pub is_const: bool,
  pub is_reverse: bool,
  pub is_virtual: bool,
  pub has_varargs: bool,
  pub is_from_script: bool,
  pub arguments: Vec<Argument>,
}

#[cfg(test)]
mod tests {
  use super::*;
  use serde_json;

  #[test]
  fn deserialize_method_test() {
    let method_str = r#"{
				"name": "add_interface",
				"return_type": "void",
				"is_editor": false,
				"is_noscript": false,
				"is_const": false,
				"is_reverse": false,
				"is_virtual": false,
				"has_varargs": false,
				"is_from_script": false,
				"arguments": [
					{
						"name": "interface",
						"type": "ARVRInterface",
						"has_default_value": false,
						"default_value": ""
					}
				]
			}"#;
    let result: Method = serde_json::from_str(method_str).unwrap();
    assert_eq!(result, Method {
      name: String::from("add_interface"),
      return_type: String::from("void"),
      is_editor: false,
      is_noscript: false,
      is_const: false,
      is_reverse: false,
      is_virtual: false,
      has_varargs: false,
      is_from_script: false,
      arguments: vec![
        Argument {
          name: String::from("interface"),
          argument_type: String::from("ARVRInterface"),
          has_default_value: false,
          default_value: String::from(""),
        },
      ],
    });
  }

}
