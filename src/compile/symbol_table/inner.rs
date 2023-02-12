
//! Provides the [`InnerSymbolTable`] structure, which behaves mostly
//! like a [`SymbolTable`] but returns global synthetic variables when
//! it goes out of scope, as if by
//! [`SymbolTable::dump_synthetics_to`].

use super::SymbolTable;

use std::ops::{Deref, DerefMut};

/// An `InnerSymbolTable` is a thin wrapper around a [`SymbolTable`].
/// However, `InnerSymbolTable` keeps a (mutable) reference to an
/// enclosing symbol table, and when the `InnerSymbolTable` is
/// dropped, it dumps all global synthetic variables from the current
/// symbol table into the enclosing table, as if by
/// [`SymbolTable::dump_synthetics_to`].
#[derive(Debug)]
pub struct InnerSymbolTable<'a> {
  inner_table: SymbolTable,
  outer_table: &'a mut SymbolTable,
}

impl<'a> InnerSymbolTable<'a> {

  /// Constructs a new `InnerSymbolTable` object wrapping
  /// `inner_table` and pointing to the enclosing table `outer_table`.
  pub fn new(inner_table: SymbolTable, outer_table: &'a mut SymbolTable) -> Self {
    InnerSymbolTable { inner_table, outer_table }
  }

  /// Constructs a new `InnerSymbolTable` with no concrete symbols and
  /// with synthetic symbols inherited from the enclosing table.
  pub fn with_synthetics_from(outer_table: &'a mut SymbolTable) -> Self {
    let inner_table = SymbolTable::with_synthetics_from(outer_table);
    Self::new(inner_table, outer_table)
  }

  /// Constructs a new `InnerSymbolTable` which is a clone of
  /// `outer_table` and points to it.
  pub fn cloned_from(outer_table: &'a mut SymbolTable) -> Self {
    let inner_table = outer_table.clone();
    Self::new(inner_table, outer_table)
  }

}

impl<'a> Deref for InnerSymbolTable<'a> {
  type Target = SymbolTable;

  fn deref(&self) -> &SymbolTable {
    &self.inner_table
  }

}

impl<'a> DerefMut for InnerSymbolTable<'a> {
  fn deref_mut(&mut self) -> &mut SymbolTable {
    &mut self.inner_table
  }
}

impl<'a> Drop for InnerSymbolTable<'a> {

  fn drop(&mut self) {
    self.inner_table.dump_synthetics_to(self.outer_table);
  }

}
