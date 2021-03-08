
use crate::ir::Decl;

use std::collections::HashMap;

#[derive(Clone, Debug, Default)]
pub struct SymbolTable {
  values: HashMap<String, usize>,
  functions: HashMap<String, usize>,
  in_order: Vec<Decl>,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub enum Namespace {
  Value, Function,
}

impl SymbolTable {

  pub fn new() -> SymbolTable {
    SymbolTable::default()
  }

  fn appropriate_map(&self, namespace: Namespace) -> &HashMap<String, usize> {
    match namespace {
      Namespace::Value => &self.values,
      Namespace::Function => &self.functions,
    }
  }

  fn appropriate_map_mut(&mut self, namespace: Namespace) -> &mut HashMap<String, usize> {
    match namespace {
      Namespace::Value => &mut self.values,
      Namespace::Function => &mut self.functions,
    }
  }

  pub fn get(&self, name: &str) -> Option<&Decl> {
    self.appropriate_map(Namespace::Function).get(name).map(|idx| &self.in_order[*idx])
  }

  pub fn set(&mut self, name: String, value: Decl) {
    let new_idx = self.in_order.len();
    if self.appropriate_map_mut(Namespace::Function).contains_key(&name) {
      let idx = *self.appropriate_map_mut(Namespace::Function).get(&name).unwrap();
      self.in_order[idx] = value;
    } else {
      self.appropriate_map_mut(Namespace::Function).insert(name, new_idx);
      self.in_order.push(value);
    }
  }

  pub fn del(&mut self, name: &str) {
    self.appropriate_map_mut(Namespace::Function).remove(name);
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
