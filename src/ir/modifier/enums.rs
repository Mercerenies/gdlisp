
//! Parse rule for modifiers to `enum` declarations.

use crate::ir::decl::EnumDecl;
use crate::ir::export::Visibility;
use super::ParseRule;
use super::visibility;

/// Modifiers which apply to [`EnumDecl`].
#[derive(Clone, Debug, PartialEq, Eq)]
pub enum EnumMod {
  /// A visibility modifier. See [`super::visibility`].
  Visibility(Visibility),
}

impl EnumMod {
  /// Apply the modifier.
  pub fn apply(&self, decl: &mut EnumDecl) {
    match self {
      EnumMod::Visibility(vis) => {
        decl.visibility = *vis;
      }
    }
  }
}

/// A parse rule for modifiers which apply to [`EnumDecl`].
pub fn parser() -> impl ParseRule<Modifier=EnumMod> {
  visibility::parser().map(EnumMod::Visibility)
}
