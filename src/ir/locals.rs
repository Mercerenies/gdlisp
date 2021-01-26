
// Helper for storing collections of local variables that need to be
// closed over.

use std::collections::HashSet;

pub struct Locals(pub HashSet<String>);

impl Locals {

  pub fn new() -> Locals {
    Locals(HashSet::new())
  }

  pub fn visited(&mut self, name: &str) {
    self.0.insert(name.to_owned());
  }

}
