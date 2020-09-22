
use super::SymbolTable;

use std::collections::HashMap;
use std::collections::hash_map;

pub struct ConcreteTable {
  data: HashMap<String, String>,
}

impl ConcreteTable {
  pub fn new() -> ConcreteTable {
    ConcreteTable { data: HashMap::new() }
  }
}

impl<'a> SymbolTable<'a> for ConcreteTable {

  type Iter = hash_map::Iter<'a, String, String>;

  fn get_var(&mut self, name: &str) -> Option<&str> {
    self.data.get(name).map(|x| x.as_str())
  }

  fn set_var(&mut self, name: String, value: String) -> Option<String> {
    self.data.insert(name, value)
  }

  fn del_var(&mut self, name: &str) {
    self.data.remove(name);
  }

  fn vars(&'a self) -> Self::Iter {
    return self.data.iter();
  }

}
