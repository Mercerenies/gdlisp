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

//! Provides the [`StdlibUnit`] structure.

use crate::compile::symbol_table::SymbolTable;
use crate::ir::identifier::Id;
use crate::ir::macros::MacroData;
use super::translation_unit::TranslationUnit;

use serde::{Serialize, Deserialize};

use std::collections::HashMap;

/// A stripped-down version of [`TranslationUnit`] containing the
/// parts of the unit that are necessary to understand the standard
/// library.
#[derive(Serialize, Deserialize)]
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
