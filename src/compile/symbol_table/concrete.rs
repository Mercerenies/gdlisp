
use super::SymbolTable;

use std::collections::HashMap;

pub struct ConcreteTable {
  data: HashMap<String, String>,
}

impl ConcreteTable {
  pub fn new() -> ConcreteTable {
    ConcreteTable { data: HashMap::new() }
  }
}

impl SymbolTable for ConcreteTable {

  fn get_var(&self, name: &str) -> Option<&str> {
    self.data.get(name).map(|x| x.as_str())
  }

  fn set_var(&mut self, name: String, value: String) -> Option<String> {
    self.data.insert(name, value)
  }

  fn del_var(&mut self, name: &str) {
    self.data.remove(name);
  }

}
