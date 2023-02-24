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

//! The [`MagicTable`] structure for tables of [call magic](super).
//!
//! The compiler maintains a table of known call magic that the user's
//! source code is allowed to link up to during compilation.
//! Currently, this table is read-only and there are no facilities in
//! GDLisp to change it.
//!
//! Note that this functionality is used for bootstrapping and there
//! is *never* a legitimate reason for an end user to ever need to
//! interface with this directly. As such, everything here should be
//! regarded as an implementation detail (like anything else in the
//! `sys/*` namespace).

use super::CallMagic;

use std::collections::HashMap;

/// A `MagicTable` is a table where the keys are strings and the
/// values are [`CallMagic`](super::CallMagic) instances.
///
/// This type can be thought of, in spirit, as `HashMap<String,
/// CallMagic>`.
#[derive(Clone, Debug, Default)]
pub struct MagicTable {
  values: HashMap<String, CallMagic>,
}

impl MagicTable {

  /// An empty `MagicTable`. Equivalent to `MagicTable::default()`.
  pub fn new() -> MagicTable {
    MagicTable::default()
  }

  /// Gets the call magic associated to the given name.
  pub fn get(&self, name: &str) -> Option<&CallMagic> {
    self.values.get(name)
  }

  /// Assigns call magic to the given name, replacing any previous
  /// call magic affiliated with that name.
  pub fn set(&mut self, name: String, value: CallMagic) {
    self.values.insert(name, value);
  }

  /// Removes the call magic associated to the given name, if it
  /// exists.
  pub fn del(&mut self, name: &str) {
    self.values.remove(name);
  }

  /// Converts `self` into a hash map containing the same information.
  pub fn into_hashmap(self) -> HashMap<String, CallMagic> {
    self.values
  }

}

impl From<HashMap<String, CallMagic>> for MagicTable {
  fn from(values: HashMap<String, CallMagic>) -> MagicTable {
    MagicTable { values }
  }
}
