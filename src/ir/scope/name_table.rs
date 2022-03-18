
//! Defines the [`NameTable`] type, for keeping track of names in a
//! given scope and namespace.

use crate::pipeline::source::SourceOffset;
use crate::ir::identifier::IdLike;

use std::collections::HashMap;
use std::hash::Hash;

/// A name table stores an unordered collection of names, tagged with
/// the namespace they belong to and the place in the source code that
/// they were declared.
#[derive(Clone, Debug, Eq, PartialEq)]
pub struct NameTable<NS: Hash + Eq + Clone> {
  data: HashMap<(NS, String), SourceOffset>,
}

impl<NS: Hash + Eq + Clone> NameTable<NS> {

  /// A new, empty `NameTable`.
  pub fn new() -> NameTable<NS> {
    NameTable::default()
  }

  /// Given a namespace and a name, get the position in the source
  /// code where the declaration for that name appears, if one exists.
  pub fn get_source_offset(&self, namespace: NS, name: &str) -> Option<SourceOffset> {
    let key = (namespace, name);
    self.data.get::<dyn IdLike<NS=NS>>((&key) as &(dyn IdLike<NS=NS>)).copied()
  }

  /// Adds a name to the name table, marking it as appearing at the
  /// given source position. If that name already appears as a
  /// declaration somewhere in the source code, this method overwrites
  /// the existing one and returns it. Otherwise, this method inserts
  /// the new name and returns `None`.
  pub fn add_name(&mut self, namespace: NS, name: String, pos: SourceOffset) -> Option<SourceOffset> {
    self.data.insert((namespace, name), pos)
  }

}

impl<NS: Hash + Eq + Clone> Default for NameTable<NS> {

  fn default() -> NameTable<NS> {
    NameTable { data: HashMap::default() }
  }

}
