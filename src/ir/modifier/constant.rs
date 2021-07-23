
//! Parse rule for modifiers that can be applied to constant declarations.

use crate::ir::decl::ConstDecl;
use crate::ir::export::Visibility;
use super::ParseRule;
use super::visibility;

/// A modifier which applies to a [`ConstDecl`].
#[derive(Clone, Debug, PartialEq, Eq)]
pub enum ConstMod {
  /// A visibility modifier, as per [`super::visibility`].
  Visibility(Visibility),
}

impl ConstMod {
  /// Apply the modifier to `decl`.
  pub fn apply(&self, decl: &mut ConstDecl) {
    match self {
      ConstMod::Visibility(vis) => {
        decl.visibility = *vis;
      }
    }
  }
}

/// A parse rule for constant declarations.
pub fn parser() -> impl ParseRule<Modifier=ConstMod> {
  visibility::parser().map(ConstMod::Visibility)
}
