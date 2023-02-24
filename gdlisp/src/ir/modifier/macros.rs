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

//! Parse rule for modifiers which apply to macros and symbol macros.

use crate::ir::decl::{MacroDecl, SymbolMacroDecl};
use crate::ir::export::Visibility;
use super::ParseRule;
use super::visibility;

/// Modifier for [`MacroDecl`].
#[derive(Clone, Debug, PartialEq, Eq)]
pub enum MacroMod {
  Visibility(Visibility),
}

impl MacroMod {
  /// Apply the modifier.
  pub fn apply(&self, decl: &mut MacroDecl) {
    match self {
      MacroMod::Visibility(vis) => {
        decl.visibility = *vis;
      }
    }
  }
  /// Apply the modifier to a symbol macro.
  pub fn apply_to_symbol_macro(&self, decl: &mut SymbolMacroDecl) {
    match self {
      MacroMod::Visibility(vis) => {
        decl.visibility = *vis;
      }
    }
  }
}

/// Parse rule for macro modifiers.
pub fn parser() -> impl ParseRule<Modifier=MacroMod> {
  visibility::parser().map(MacroMod::Visibility)
}
