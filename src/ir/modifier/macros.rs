
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
