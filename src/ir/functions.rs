
// Helper for storing collections of (possibly local) functions that
// need to be closed over. To be clear, this only includes the
// function namespace, which includes globally defined functions and
// those defined locally with flet, etc. (not implemented yet), but it
// explicitly excludes lambdas assigned to regular variables, which
// are included in the ir::locals::Locals walk.

use std::collections::HashSet;

#[derive(PartialEq, Eq, Debug, Clone, Default)]
pub struct Functions(HashSet<String>);

impl Functions {

  pub fn new() -> Functions {
    Functions::default()
  }

  pub fn from_hashset(map: HashSet<String>) -> Functions {
    Functions(map)
  }

  pub fn contains(&self, name: &str) -> bool {
    self.0.contains(name)
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

  pub fn is_empty(&self) -> bool {
    self.0.is_empty()
  }

  pub fn merge_with(&mut self, b: Functions) {
    self.0.extend(b.into_names());
  }

}
