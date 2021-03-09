
use super::Decl;
use super::identifier::Id;

use std::collections::HashMap;

#[derive(Clone, Debug, Default)]
pub struct SymbolTable {
  values: HashMap<Id, usize>,
  in_order: Vec<Decl>,
}

// TODO This should probably be in crate::ir::identifier, not here
#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub enum Namespace {
  Value, Function,
}

impl SymbolTable {

  pub fn new() -> SymbolTable {
    SymbolTable::default()
  }

  pub fn get(&self, name: &str) -> Option<&Decl> {
    let id = Id::build(Namespace::Function, name);
    self.values.get(&*id).map(|idx| &self.in_order[*idx])
  }

  pub fn set(&mut self, name: String, value: Decl) {
    let id = Id::new(Namespace::Function, name);
    let new_idx = self.in_order.len();
    if self.values.contains_key(&id) {
      let idx = *self.values.get(&id).unwrap();
      self.in_order[idx] = value;
    } else {
      self.values.insert(id, new_idx);
      self.in_order.push(value);
    }
  }

  pub fn del(&mut self, name: &str) {
    let id = Id::build(Namespace::Function, name);
    self.values.remove(&*id);
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
