
use crate::ir::Decl;

use std::collections::HashMap;

#[derive(Clone, Debug, Default)]
pub struct SymbolTable {
  functions: HashMap<String, Decl>,
}

impl SymbolTable {

  pub fn new() -> SymbolTable {
    SymbolTable::default()
  }

  pub fn get(&self, name: &str) -> Option<&Decl> {
    self.functions.get(name)
  }

  pub fn set(&mut self, name: String, value: Decl) {
    self.functions.insert(name, value);
  }

  pub fn del(&mut self, name: &str) {
    self.functions.remove(name);
  }

  pub fn has(&self, name: &str) -> bool {
    self.get(name).is_some()
  }

}
