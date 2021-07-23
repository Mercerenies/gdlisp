
//! A parse rule for modifiers which apply specifically to instance
//! methods.

use crate::ir::decl::{ClassFnDecl, ConstructorDecl};
use crate::gdscript::decl::Static;
use crate::compile::error::Error;
use super::{ParseRule, Several, Constant};

/// Modifier for [`ClassFnDecl`].
#[derive(Clone, Debug, PartialEq, Eq)]
pub enum FnMod { // TODO Change the name so it doesn't match the name from super::function
  /// `static` declarations allow the instance method to be called
  /// without an instance of the class available.
  Static,
}

impl FnMod {

  /// Apply the modifier to an instance function declaration.
  pub fn apply(&self, decl: &mut ClassFnDecl) {
    match self {
      FnMod::Static => {
        decl.is_static = Static::IsStatic;
      }
    }
  }

  /// Apply the modifier to a constructor. Some modifiers do not make
  /// sense applied to constructors and will trigger an error if an
  /// attempt is made to do so.
  pub fn apply_to_constructor(&self, _decl: &mut ConstructorDecl) -> Result<(), Error> {
    match self {
      FnMod::Static => {
        Err(Error::StaticConstructor)
      }
    }
  }
}

/// Parse rule for `FnMod`.
pub fn parser() -> impl ParseRule<Modifier=FnMod> {
  Several::new(vec!(
    Box::new(Constant::new("static", FnMod::Static).unique())
  ))
}
