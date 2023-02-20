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

//! Provides the [`ImportTable`] type, which maps constant names
//! representing imports to the target pathname.

use std::collections::HashMap;

/// An `ImportTable` is effectively a `HashMap<String, String>`
/// mapping imported variable names to strings representing the target
/// path.
#[derive(Clone, Debug, Default)]
pub struct ImportTable {
  values: HashMap<String, String>,
}

impl ImportTable {

  /// An empty `ImportTable`, equivalent to `ImportTable::default()`.
  pub fn new() -> ImportTable {
    ImportTable::default()
  }

  /// Gets the import path associated to the given variable name.
  pub fn get(&self, name: &str) -> Option<&str> {
    self.values.get(name).map(String::as_ref)
  }

  /// Assigns a path to the given name, replacing any previous path
  /// associated to the name.
  pub fn set(&mut self, name: String, value: String) {
    self.values.insert(name, value);
  }

  /// Removes the path associated to the given name, if it exists.
  pub fn del(&mut self, name: &str) {
    self.values.remove(name);
  }

  /// Converts `self` into a hash map containing the same information.
  pub fn into_hashmap(self) -> HashMap<String, String> {
    self.values
  }

}
