
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
  pub fn get_name(&self, namespace: NS, name: &str) -> Option<SourceOffset> {
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

  /// The number of names which are known to this name table.
  pub fn len(&self) -> usize {
    self.data.len()
  }

}

impl<NS: Hash + Eq + Clone> Default for NameTable<NS> {

  fn default() -> NameTable<NS> {
    NameTable { data: HashMap::default() }
  }

}

#[cfg(test)]
mod tests {
  use super::*;
  use crate::ir::identifier::Namespace;

  #[test]
  fn empty_table() {
    let table: NameTable<Namespace> = NameTable::new();
    assert_eq!(table.len(), 0);
  }

  #[test]
  fn table_insert_names() {
    let mut table: NameTable<Namespace> = NameTable::new();
    assert_eq!(table.add_name(Namespace::Value, String::from("VALUE"), SourceOffset(20)), None);
    assert_eq!(table.add_name(Namespace::Function, String::from("FUNCTION"), SourceOffset(10)), None);
    assert_eq!(table.get_name(Namespace::Value, "VALUE"), Some(SourceOffset(20)));
    assert_eq!(table.get_name(Namespace::Function, "FUNCTION"), Some(SourceOffset(10)));
    assert_eq!(table.len(), 2);
  }

  #[test]
  fn table_crossed_namespaces() {
    let mut table: NameTable<Namespace> = NameTable::new();
    assert_eq!(table.add_name(Namespace::Value, String::from("VALUE"), SourceOffset(20)), None);
    assert_eq!(table.add_name(Namespace::Function, String::from("FUNCTION"), SourceOffset(10)), None);
    assert_eq!(table.get_name(Namespace::Function, "VALUE"), None);
    assert_eq!(table.get_name(Namespace::Value, "FUNCTION"), None);
    assert_eq!(table.len(), 2);
  }

  #[test]
  fn table_duplicate_name_different_namespace() {
    let mut table: NameTable<Namespace> = NameTable::new();
    assert_eq!(table.add_name(Namespace::Value, String::from("NAME"), SourceOffset(20)), None);
    assert_eq!(table.add_name(Namespace::Function, String::from("NAME"), SourceOffset(10)), None);
    assert_eq!(table.len(), 2);
  }

  #[test]
  fn table_duplicate_name_same_namespace() {
    let mut table: NameTable<Namespace> = NameTable::new();
    assert_eq!(table.add_name(Namespace::Value, String::from("NAME"), SourceOffset(20)), None);
    assert_eq!(table.add_name(Namespace::Value, String::from("NAME"), SourceOffset(10)), Some(SourceOffset(20)));
    assert_eq!(table.len(), 1);
  }

}
