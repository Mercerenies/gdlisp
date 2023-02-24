// Copyright 2023 Silvio Mayolo
//
// This file is part of GDLisp.
//
// GDLisp is free software: you can redistribute it and/or modify it
// under the terms of the GNU General Public License as published by
// the Free Software Foundation, either version 3 of the License, or
// (at your option) any later version.
//
// GDLisp is distributed in the hope that it will be useful, but
// WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
// General Public License for more details.
//
// You should have received a copy of the GNU General Public License
// along with GDLisp. If not, see <https://www.gnu.org/licenses/>.

//! Builder for a [`NameTable`](super::NameTable).

use super::NameTable;
use crate::ir::scope::error::ScopeError;
use crate::pipeline::source::SourceOffset;

use std::hash::Hash;

/// A [`NameTableBuilder`] builds a [`NameTable`]. If it ever
/// encounters a duplicate name or other error condition, it signals
/// the error via [`ScopeError`].
#[derive(Debug)]
pub struct NameTableBuilder<NS: Hash + Eq + Clone> {
  table: NameTable<NS>,
}

impl<NS: Hash + Eq + Clone> NameTableBuilder<NS> {

  /// A new, empty builder.
  pub fn new() -> NameTableBuilder<NS> {
    NameTableBuilder::default()
  }

  /// Adds a name to the builder. If the name is already present, an
  /// appropriate [`ScopeError::DuplicateName`] error is signaled.
  ///
  /// If this method signals an error, then the builder is left
  /// unmodified.
  pub fn add_name(&mut self, namespace: NS, name: String, pos: SourceOffset) -> Result<(), ScopeError<NS>> {

    // Note: We do the get_name check in advance and assert the result
    // of add_name to ensure that, if the name exists, then the
    // builder is left in a consistent state. If we used the result of
    // add_name to determine if an error had occurred, then we
    // couldn't guarantee that the builder remains in its prior state
    // in case of an error.
    if self.table.get_name(namespace.clone(), &name).is_some() {
      // Duplicate found
      Err(ScopeError::DuplicateName(namespace, name, pos))
    } else {
      let result = self.table.add_name(namespace, name.clone(), pos);
      assert!(result.is_none(), "Internal error in add_name at {}", name);
      Ok(())
    }

  }

  /// Build the [`NameTable`].
  pub fn build(self) -> NameTable<NS> {
    self.table
  }

}

impl<NS: Hash + Eq + Clone> Default for NameTableBuilder<NS> {
  fn default() -> NameTableBuilder<NS> {
    NameTableBuilder { table: NameTable::default() }
  }
}
