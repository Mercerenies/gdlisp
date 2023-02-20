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

use super::api_type::ApiType;
use super::property::Property;
use super::signal::Signal;
use super::method::Method;
use super::gdnative_enum::Enum;

use serde::{Serialize, Deserialize};

use std::collections::HashMap;

#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct Class {
  pub name: String,
  pub base_class: String,
  pub api_type: ApiType,
  pub singleton: bool,
  pub singleton_name: String,
  pub instanciable: bool,
  pub is_reference: bool,
  pub constants: HashMap<String, i32>,
  pub properties: Vec<Property>,
  pub signals: Vec<Signal>,
  pub methods: Vec<Method>,
  pub enums: Vec<Enum>,
}

#[cfg(test)]
mod tests {
  use super::*;
  use serde_json;

  // Note: This is not the actual _Engine class from any GDScript
  // version. It's pared down from the real one but made smaller for
  // testing purposes.
  const SAMPLE_CLASS_JSON: &str = r#"{
		"name": "_Engine",
		"base_class": "Object",
		"api_type": "core",
		"singleton": true,
		"singleton_name": "Engine",
		"instanciable": false,
		"is_reference": false,
		"constants": {
		},
		"properties": [
			{
				"name": "editor_hint",
				"type": "bool",
				"getter": "is_editor_hint",
				"setter": "set_editor_hint",
				"index": -1
			},
			{
				"name": "iterations_per_second",
				"type": "int",
				"getter": "get_iterations_per_second",
				"setter": "set_iterations_per_second",
				"index": -1
			}
		],
		"signals": [
		],
		"methods": [
			{
				"name": "get_author_info",
				"return_type": "Dictionary",
				"is_editor": false,
				"is_noscript": false,
				"is_const": true,
				"is_reverse": false,
				"is_virtual": false,
				"has_varargs": false,
				"is_from_script": false,
				"arguments": [
				]
			},
			{
				"name": "get_copyright_info",
				"return_type": "Array",
				"is_editor": false,
				"is_noscript": false,
				"is_const": true,
				"is_reverse": false,
				"is_virtual": false,
				"has_varargs": false,
				"is_from_script": false,
				"arguments": [
				]
			}
		],
		"enums": [
		]
	}"#;

  #[test]
  fn deserialize_class_test() {
    let class: Class = serde_json::from_str(SAMPLE_CLASS_JSON).unwrap();
    assert_eq!(class.name, String::from("_Engine"));
    assert_eq!(class.base_class, String::from("Object"));
    assert_eq!(class.api_type, ApiType::Core);
    assert_eq!(class.singleton, true);
    assert_eq!(class.singleton_name, String::from("Engine"));
    assert_eq!(class.instanciable, false);
    assert_eq!(class.is_reference, false);
    assert_eq!(class.constants.len(), 0);
    assert_eq!(class.properties.len(), 2);
    assert_eq!(class.signals.len(), 0);
    assert_eq!(class.methods.len(), 2);
    assert_eq!(class.enums.len(), 0);
  }

}
