
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

pub fn get_api_from_godot() -> io::Result<Vec<Class>> {
  let tempfile = dump_json_api()?;
  let classes: Vec<Class> = serde_json::from_reader(tempfile)?;
  Ok(classes)
}

#[cfg(test)]
mod tests {
  use super::*;

  #[test]
  fn test_get_api_from_godot() {
    let classes = get_api_from_godot().unwrap();
    // Assert only a few basic facts about the classes, as we want to
    // maximize compatibility with forked or custom Godot builds that
    // have different classes available.

    // There should exist at least one class.
    assert!(classes.len() > 0);

    // There should be an Object class with the desired properties.
    let object = find_class_by_name(&classes, "Object").unwrap();
    assert_eq!(object.name, "Object");
    assert_eq!(object.base_class, "");
    assert_eq!(object.api_type, api_type::ApiType::Core);
    assert_eq!(object.singleton, false);
    assert_eq!(object.singleton_name, "");
    assert_eq!(object.instanciable, true);
    assert_eq!(object.is_reference, false);

    // There should be an Reference class with the desired properties.
    let object = find_class_by_name(&classes, "Reference").unwrap();
    assert_eq!(object.name, "Reference");
    assert_eq!(object.base_class, "Object");
    assert_eq!(object.api_type, api_type::ApiType::Core);
    assert_eq!(object.singleton, false);
    assert_eq!(object.singleton_name, "");
    assert_eq!(object.instanciable, true);
    assert_eq!(object.is_reference, true);

    // There should be an Reference class with the desired properties.
    let object = find_class_by_name(&classes, "Node").unwrap();
    assert_eq!(object.name, "Node");
    assert_eq!(object.base_class, "Object");
    assert_eq!(object.api_type, api_type::ApiType::Core);
    assert_eq!(object.singleton, false);
    assert_eq!(object.singleton_name, "");
    assert_eq!(object.instanciable, true);
    assert_eq!(object.is_reference, false);

  }

  fn find_class_by_name<'a, 'b>(classes: &'a [Class], name: &'b str) -> Option<&'a Class> {
    classes.iter().find(|class| class.name == name)
  }

}
