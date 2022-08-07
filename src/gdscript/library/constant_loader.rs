
use super::gdnative::NativeClasses;

use std::borrow::Borrow;

pub fn get_all_constants(classes: &NativeClasses) -> impl Iterator<Item=&str> {
  let global_constants = classes.get_global_constants();
  global_constants.constants.keys().map(String::borrow)
}
