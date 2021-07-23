
//! A parse rule for modifiers to `sys/declare` declarations.

use crate::ir::decl::DeclareDecl;
use crate::ir::export::Visibility;
use super::ParseRule;
use super::visibility;

/// Modifiers to compile-time declaration directives.
#[derive(Clone, Debug, PartialEq, Eq)]
pub enum DeclareMod {
  /// A visibility modifier. See [`super::visibility`].
  Visibility(Visibility),
}

impl DeclareMod {
  /// Apply the modifier to `decl`.
  pub fn apply(&self, decl: &mut DeclareDecl) {
    match self {
      DeclareMod::Visibility(vis) => {
        decl.visibility = *vis;
      }
    }
  }
}

/// A parse rule for [`DeclareDecl`].
pub fn parser() -> impl ParseRule<Modifier=DeclareMod> {
  visibility::parser().map(DeclareMod::Visibility)
}
