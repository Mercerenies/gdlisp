
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

  pub fn filter(&self, condition: impl FnMut(&Decl) -> bool) -> SymbolTable {
    let vec = Vec::from(self.clone());
    let filtered = vec.into_iter().filter(condition).collect::<Vec<_>>();
    filtered.into()
  }

}

impl From<SymbolTable> for Vec<Decl> {

  fn from(table: SymbolTable) -> Vec<Decl> {
    table.in_order
  }

}

impl From<Vec<Decl>> for SymbolTable {

  fn from(decls: Vec<Decl>) -> SymbolTable {
    let mut table = SymbolTable::new();
    for decl in decls {
      table.set(decl.name().to_owned(), decl);
    }
    table
  }

}
