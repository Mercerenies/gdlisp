
// Helper for storing collections of local variables that need to be
// closed over.

use std::collections::HashSet;

#[derive(PartialEq, Eq, Debug)]
pub struct Locals(pub HashSet<String>);

impl Locals {

  pub fn new() -> Locals {
    Locals(HashSet::new())
  }

  pub fn visited(&mut self, name: &str) {
    self.0.insert(name.to_owned());
  }

  pub fn remove(&mut self, name: &str) {
    self.0.remove(name);
  }

  pub fn names(&self) -> impl Iterator<Item=&String> {
    self.0.iter()
  }

  pub fn into_names(self) -> impl Iterator<Item=String> {
    self.0.into_iter()
  }

}
