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

//! Classes for reading the output of `godot
//! --gdnative-generate-json-api`.

pub mod argument;
pub mod api_type;
pub mod class;
pub mod gdnative_enum;
pub mod method;
pub mod property;
pub mod signal;

use class::Class;
use crate::runner::dump_json_api;

use serde_json;

use std::io;
use std::collections::HashMap;

pub const GLOBAL_CONSTANTS_CLASS: &str = "GlobalConstants";

#[derive(Debug, Clone)]
pub struct NativeClasses {
  mapping: HashMap<String, Class>,
}

impl NativeClasses {

  pub fn get_api_from_godot() -> io::Result<NativeClasses> {
    let tempfile = dump_json_api()?;
    let classes: Vec<Class> = serde_json::from_reader(tempfile)?;
    Ok(
      NativeClasses {
        mapping: classes.into_iter().map(|cls| (cls.name.clone(), cls)).collect(),
      }
    )
  }

  pub fn len(&self) -> usize {
    self.mapping.len()
  }

  pub fn is_empty(&self) -> bool {
    self.mapping.is_empty()
  }

  pub fn get(&self, class_name: &str) -> Option<&Class> {
    self.mapping.get(class_name)
  }

  pub fn get_global_constants(&self) -> &Class {
    self.get(GLOBAL_CONSTANTS_CLASS).expect("Could not read global constants from GDNative API")
  }

  pub fn values(&self) -> impl Iterator<Item=&Class> {
    self.mapping.values()
  }

}

#[cfg(test)]
mod tests {
  use super::*;

  #[test]
  fn test_get_api_from_godot() {
    let classes = NativeClasses::get_api_from_godot().unwrap();
    // Assert only a few basic facts about the classes, as we want to
    // maximize compatibility with forked or custom Godot builds that
    // have different classes available.

    // There should exist at least one class.
    assert!(classes.len() > 0);

    // There should be an Object class with the desired properties.
    let object = classes.get("Object").unwrap();
    assert_eq!(object.name, "Object");
    assert_eq!(object.base_class, "");
    assert_eq!(object.api_type, api_type::ApiType::Core);
    assert_eq!(object.singleton, false);
    assert_eq!(object.singleton_name, "");
    assert_eq!(object.instanciable, true);
    assert_eq!(object.is_reference, false);

    // There should be an Reference class with the desired properties.
    let object = classes.get("Reference").unwrap();
    assert_eq!(object.name, "Reference");
    assert_eq!(object.base_class, "Object");
    assert_eq!(object.api_type, api_type::ApiType::Core);
    assert_eq!(object.singleton, false);
    assert_eq!(object.singleton_name, "");
    assert_eq!(object.instanciable, true);
    assert_eq!(object.is_reference, true);

    // There should be an Reference class with the desired properties.
    let object = classes.get("Node").unwrap();
    assert_eq!(object.name, "Node");
    assert_eq!(object.base_class, "Object");
    assert_eq!(object.api_type, api_type::ApiType::Core);
    assert_eq!(object.singleton, false);
    assert_eq!(object.singleton_name, "");
    assert_eq!(object.instanciable, true);
    assert_eq!(object.is_reference, false);

  }

}
