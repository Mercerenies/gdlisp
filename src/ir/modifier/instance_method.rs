
//! A parse rule for modifiers which apply specifically to instance
//! methods.

use crate::ir::decl::{ClassFnDecl, ConstructorDecl};
use crate::gdscript::decl::Static;
use crate::compile::error::{GDError, GDErrorF};
use super::{ParseRule, Several, Constant};

/// Modifier for [`ClassFnDecl`].
#[derive(Clone, Debug, PartialEq, Eq)]
pub enum MethodMod {
  /// `static` declarations allow the instance method to be called
  /// without an instance of the class available.
  Static,
  /// `sys/nullargs` declarations force all formal arguments to the
  /// instance method to be given a default value of `null`.
  Nullargs,
}

impl MethodMod {

  /// Apply the modifier to an instance function declaration.
  pub fn apply(&self, decl: &mut ClassFnDecl) {
    match self {
      MethodMod::Static => {
        decl.is_static = Static::IsStatic;
      }
      MethodMod::Nullargs => {
        decl.is_nullargs = true;
      }
    }
  }

  /// Apply the modifier to a constructor. Some modifiers do not make
  /// sense applied to constructors and will trigger an error if an
  /// attempt is made to do so.
  pub fn apply_to_constructor(&self, decl: &mut ConstructorDecl) -> Result<(), GDError> {
    match self {
      MethodMod::Static => {
        Err(GDError::new(GDErrorF::StaticConstructor, decl.body.pos))
      }
      MethodMod::Nullargs => {
        Err(GDError::new(GDErrorF::NullargsConstructor, decl.body.pos))
      }
    }
  }
}

/// Parse rule for `MethodMod`.
pub fn parser() -> impl ParseRule<Modifier=MethodMod> {
  Several::new(vec!(
    Box::new(Constant::new("static", MethodMod::Static).unique()),
    Box::new(Constant::new("sys/nullargs", MethodMod::Nullargs).unique()),
  ))
}
