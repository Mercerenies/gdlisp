
//! Parse rule for modifiers which apply to class declarations.

use crate::ir::decl::ClassDecl;
use crate::ir::export::Visibility;
use super::{ParseRule, Several, Constant};
use super::visibility;

/// A modifier which can be applied to a [`ClassDecl`].
#[derive(Clone, Debug, PartialEq, Eq)]
pub enum ClassMod {
  /// The literal `main` symbol parses as `ClassMod::Main`, which
  /// makes the class into the unique main class for the file.
  Main,
  /// A visibility modifier. See [`super::visibility`].
  Visibility(Visibility),
}

impl ClassMod {
  /// Apply the modifier to `decl`.
  pub fn apply(&self, decl: &mut ClassDecl) {
    match self {
      ClassMod::Main => {
        decl.main_class = true;
      }
      ClassMod::Visibility(vis) => {
        decl.visibility = *vis;
      }
    }
  }
}

/// A parse rule for class declarations.
pub fn parser() -> impl ParseRule<Modifier=ClassMod> {
  Several::new(vec!(
    Box::new(Constant::new("main", ClassMod::Main).unique()),
    Box::new(visibility::parser().map(ClassMod::Visibility)),
  ))
}
