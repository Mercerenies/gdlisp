
//! Parse rule for modifiers to `object` declarations.

use crate::ir::decl::ObjectDecl;
use crate::ir::export::Visibility;
use super::ParseRule;
use super::visibility;

/// Modifiers which apply to [`ObjectDecl`].
#[derive(Clone, Debug, PartialEq, Eq)]
pub enum ObjectMod {
  /// A visibility modifier. See [`super::visibility`].
  Visibility(Visibility),
}

impl ObjectMod {
  /// Apply the modifier.
  pub fn apply(&self, decl: &mut ObjectDecl) {
    match self {
      ObjectMod::Visibility(vis) => {
        decl.visibility = *vis;
      }
    }
  }
}

/// A parse rule for modifiers which apply to [`EnumDecl`].
pub fn parser() -> impl ParseRule<Modifier=ObjectMod> {
  visibility::parser().map(ObjectMod::Visibility)
}
