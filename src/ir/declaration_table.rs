
use super::Decl;
use super::identifier::{Id, IdLike};

use std::collections::HashMap;

#[derive(Clone, Debug, Default)]
pub struct DeclarationTable {
  values: HashMap<Id, usize>,
  in_order: Vec<Decl>,
}

impl DeclarationTable {

  pub fn new() -> DeclarationTable {
    DeclarationTable::default()
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

  pub fn del<'a>(&mut self, id: &(dyn IdLike + 'a)) -> Option<Decl> {
    if let Some(idx) = self.values.remove(id) {
      let decl = self.in_order.remove(idx);
      for v in self.values.values_mut() {
        // We shifted declarations over, so we need to update all
        // indices that were to the right of the removed one.
        if *v > idx {
          *v -= 1;
        }
      }
      Some(decl)
    } else {
      None
    }
  }

  pub fn has<'a>(&self, id: &(dyn IdLike + 'a)) -> bool {
    self.get(id).is_some()
  }

  pub fn filter(&self, condition: impl FnMut(&Decl) -> bool) -> DeclarationTable {
    let vec = Vec::from(self.clone());
    let filtered = vec.into_iter().filter(condition).collect::<Vec<_>>();
    filtered.into()
  }

}

impl From<DeclarationTable> for Vec<Decl> {

  fn from(table: DeclarationTable) -> Vec<Decl> {
    table.in_order
  }

}

impl From<Vec<Decl>> for DeclarationTable {

  fn from(decls: Vec<Decl>) -> DeclarationTable {
    let mut table = DeclarationTable::new();
    for decl in decls {
      table.set(decl.to_id(), decl);
    }
    table
  }

}
