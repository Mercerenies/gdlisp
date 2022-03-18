
//! Defines the [`NameTable`] type, for keeping track of names in a
//! given scope and namespace.

use crate::pipeline::source::SourceOffset;

use std::collections::HashMap;
use std::hash::Hash;

/// A name table stores an unordered collection of names, tagged with
/// the namespace they belong to and the place in the source code that
/// they were declared.
#[derive(Clone, Debug, Eq, PartialEq)]
pub struct NameTable<NS: Hash + Eq> {
  data: HashMap<(NS, String), SourceOffset>,
}

impl<NS: Hash + Eq> NameTable<NS> {

  pub fn new() -> NameTable<NS> {
    NameTable::default()
  }

}

impl<NS: Hash + Eq> Default for NameTable<NS> {

  fn default() -> NameTable<NS> {
    NameTable { data: HashMap::default() }
  }

}
