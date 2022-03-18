
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

  pub fn new() -> NameTable<NS> {
    NameTable::default()
  }

  pub fn get_source_offset(&self, namespace: &NS, name: &str) -> Option<SourceOffset> {
    let key = (namespace.clone(), name);
    self.data.get::<dyn IdLike<NS=NS>>((&key) as &(dyn IdLike<NS=NS>)).copied()
  }

}

impl<NS: Hash + Eq + Clone> Default for NameTable<NS> {

  fn default() -> NameTable<NS> {
    NameTable { data: HashMap::default() }
  }

}
