
use crate::ir::Decl;

use std::collections::HashMap;

#[derive(Clone, Debug, Default)]
pub struct SymbolTable {
  functions: HashMap<String, usize>,
  in_order: Vec<Decl>,
}

impl SymbolTable {

  pub fn new() -> SymbolTable {
    SymbolTable::default()
  }

  pub fn get(&self, name: &str) -> Option<&Decl> {
    self.functions.get(name).map(|idx| &self.in_order[*idx])
  }

  pub fn set(&mut self, name: String, value: Decl) {
    self.functions.insert(name, self.in_order.len());
    self.in_order.push(value);
  }

  pub fn del(&mut self, name: &str) {
    self.functions.remove(name);
  }

  pub fn has(&self, name: &str) -> bool {
    self.get(name).is_some()
  }

}

impl From<SymbolTable> for Vec<Decl> {

  fn from(table: SymbolTable) -> Vec<Decl> {
    table.in_order
  }

}
