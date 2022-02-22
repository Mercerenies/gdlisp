
//! Provides the [`StdlibUnit`] structure.

use crate::compile::symbol_table::SymbolTable;
use crate::ir::identifier::Id;
use crate::ir::macros::MacroData;
use super::translation_unit::TranslationUnit;

use std::collections::HashMap;

/// A stripped-down version of [`TranslationUnit`] containing the
/// parts of the unit that are necessary to understand the standard
/// library.
pub struct StdlibUnit {
  /// See [`TranslationUnit::table`].
  pub table: SymbolTable,
  /// See [`TranslationUnit::exports`].
  pub exports: Vec<Id>,
  /// See [`TranslationUnit::macros`].
  pub macros: HashMap<Id, MacroData>,
}

impl From<TranslationUnit> for StdlibUnit {

  fn from(unit: TranslationUnit) -> StdlibUnit {
    let TranslationUnit { table, exports, macros, .. } = unit;
    StdlibUnit { table, exports, macros }
  }

}
