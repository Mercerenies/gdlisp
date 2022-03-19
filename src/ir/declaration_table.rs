
//! A [`DeclarationTable`] is a sequence of declarations which can be
//! efficiently indexed by [`Id`].

use super::Decl;
use super::identifier::{Id, IdLike, Namespace};
use crate::compile::error::{Error as GDError, ErrorF as GDErrorF};

use std::collections::HashMap;
use std::mem;

/// A `DeclarationTable` stores [`Decl`] declarations, indexed by
/// their names. A `DeclarationTable` should be thought of as a
/// `Vec<Decl>`, but with efficient access indexed by the
/// [identifier](Id) of a declaration. There are `From` and `Into`
/// instances converting from and to `Vec<Decl>`.
///
/// Note that, for indexing purposes, the identifier of a `Decl` is
/// given by [`Decl::to_id`].
#[derive(Clone, Debug, Default)]
pub struct DeclarationTable {
  values: HashMap<Id, usize>,
  in_order: Vec<Decl>,
}

impl DeclarationTable {

  /// Constructs a new, empty `DeclarationTable`.
  pub fn new() -> DeclarationTable {
    DeclarationTable::default()
  }

  /// Gets the declaration with the given identifier.
  pub fn get<'a>(&self, id: &(dyn IdLike<NS=Namespace> + 'a)) -> Option<&Decl> {
    self.values.get(id).map(|idx| &self.in_order[*idx])
  }

  /// Adds a [`Decl`] to the `DeclarationTable`, to be indexed via its
  /// [`Decl::to_id`] value. If a declaration with that ID is already
  /// present in this table, then that declaration is replaced with
  /// `value`. Otherwise, `value` is added at the end of the table.
  #[allow(clippy::map_entry)] // Using the Entry API would require that value be cloned.
  pub fn add(&mut self, value: Decl) -> Option<Decl> {
    let id = value.to_id();
    let new_idx = self.in_order.len();
    if self.values.contains_key(&id) {
      let idx = *self.values.get(&id).unwrap();
      Some(mem::replace(&mut self.in_order[idx], value))
    } else {
      self.values.insert(id, new_idx);
      self.in_order.push(value);
      None
    }
  }

  /// Adds a [`Decl`] to the `DeclarationTable`, if no declaration
  /// with that name already exists. If a declaration with the given
  /// name already exists, an error is reported and the table is
  /// unmodified.
  pub fn add_unless_exists(&mut self, value: Decl) -> Result<(), GDError> {
    if self.get(&*value.id_like()).is_some() {
      Err(
        GDError::new(GDErrorF::DuplicateName(value.namespace().into(), value.name().to_owned()), value.pos),
      )
    } else {
      let result = self.add(value);
      assert!(result.is_none(), "Internal error in DeclarationTable::add_unless_exists");
      Ok(())
    }
  }

  /// Removes the declaration with identifier `id` from the table and
  /// returns it. Returns `None` if no matching declaration exists.
  pub fn del<'a>(&mut self, id: &(dyn IdLike<NS=Namespace> + 'a)) -> Option<Decl> {
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

  /// Checks whether a declaration with the given identifier exists in
  /// the table. Equivalent to `self.get(id).is_some()`.
  pub fn has<'a>(&self, id: &(dyn IdLike<NS=Namespace> + 'a)) -> bool {
    self.get(id).is_some()
  }

  /// Filters the `DeclarationTable` by the predicate `condition`,
  /// returning a new table where only the entries which returned true
  /// under the predicate are kept.
  pub fn filter(&self, condition: impl FnMut(&Decl) -> bool) -> DeclarationTable {
    let vec = Vec::from(self.clone());
    let filtered = vec.into_iter().filter(condition).collect::<Vec<_>>();
    filtered.into()
  }

  /// Iterates over the elements of `self` in order.
  pub fn iter(&self) -> impl Iterator<Item=&Decl> {
    self.in_order.iter()
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
      table.add(decl);
    }
    table
  }

}
