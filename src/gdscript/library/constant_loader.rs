
use super::gdnative::{NativeClasses, GLOBAL_CONSTANTS_CLASS};

use std::borrow::Borrow;

pub fn get_all_constants(classes: &NativeClasses) -> impl Iterator<Item=&str> {
  let global_constants = classes.get(GLOBAL_CONSTANTS_CLASS).expect("Could not read global constants class from api.json");
  global_constants.constants.keys().map(String::borrow)
}
