
// Helper for storing collections of local variables that need to be
// closed over.

use std::collections::HashMap;
use std::cmp::max;

#[derive(PartialEq, Eq, Debug, Clone)]
pub struct Locals(pub HashMap<String, AccessType>);

#[derive(PartialEq, Eq, PartialOrd, Ord, Debug, Clone, Copy)]
pub enum AccessType { None, Read, RW }

impl Locals {

  pub fn new() -> Locals {
    Locals(HashMap::new())
  }

  pub fn get(&self, name: &str) -> AccessType {
    *self.0.get(name).unwrap_or(&AccessType::None)
  }

  pub fn visited(&mut self, name: &str, access_type: AccessType) {
    self.0.insert(name.to_owned(), max(self.get(name), access_type));
  }

  pub fn remove(&mut self, name: &str) {
    self.0.remove(name);
  }

  pub fn names(&self) -> impl Iterator<Item=&String> {
    self.0.keys()
  }

  pub fn into_names(self) -> impl Iterator<Item=String> {
    self.0.into_iter().map(|x| x.0)
  }

}
