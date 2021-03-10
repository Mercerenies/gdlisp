
use super::Decl;
use super::identifier::{Id, IdLike};

use std::collections::HashMap;

#[derive(Clone, Debug, Default)]
pub struct SymbolTable {
  values: HashMap<Id, usize>,
  in_order: Vec<Decl>,
}

impl SymbolTable {

  pub fn new() -> SymbolTable {
    SymbolTable::default()
  }

  pub fn get<'a>(&self, id: &(dyn IdLike + 'a)) -> Option<&Decl> {
    self.values.get(id).map(|idx| &self.in_order[*idx])
  }

  #[allow(clippy::map_entry)] // Using the Entry API would require that value be cloned.
  pub fn set(&mut self, id: Id, value: Decl) {
    let new_idx = self.in_order.len();
    if self.values.contains_key(&id) {
      let idx = *self.values.get(&id).unwrap();
      self.in_order[idx] = value;
    } else {
      self.values.insert(id, new_idx);
      self.in_order.push(value);
    }
  }

  pub fn del<'a>(&mut self, id: &(dyn IdLike + 'a)) {
    self.values.remove(id);
  }

  pub fn has<'a>(&self, id: &(dyn IdLike + 'a)) -> bool {
    self.get(id).is_some()
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
      table.set(decl.to_id(), decl);
    }
    table
  }

}
