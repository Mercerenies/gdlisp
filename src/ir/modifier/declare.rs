
//! A parse rule for modifiers to `sys/declare` declarations.

use crate::ir::decl::{DeclareDecl, DeclareType};
use crate::ir::export::Visibility;
use crate::compile::error::{GDError, GDErrorF};
use crate::pipeline::source::SourceOffset;
use super::{ParseRule, Several, Constant};
use super::visibility;

/// Modifiers to compile-time declaration directives.
#[derive(Clone, Debug, PartialEq, Eq)]
pub enum DeclareMod {
  /// A visibility modifier. See [`super::visibility`].
  Visibility(Visibility),
  /// An indicator that the declared form is a GDScript built-in.
  BuiltinFlag,
}

impl DeclareMod {
  /// Apply the modifier to `decl`.
  pub fn apply(&self, decl: &mut DeclareDecl, pos: SourceOffset) -> Result<(), GDError> {
    match self {
      DeclareMod::Visibility(vis) => {
        decl.visibility = *vis;
      }
      DeclareMod::BuiltinFlag => {
        match &mut decl.declare_type {
          DeclareType::Value | DeclareType::Superglobal => {
            return Err(GDError::new(GDErrorF::NonFunctionMarkedAsBuiltin(decl.name.to_owned()), pos));
          }
          DeclareType::Function(f) => {
            f.is_gdscript_builtin = true;
          }
          DeclareType::SuperglobalFn(f) => {
            f.is_gdscript_builtin = true;
          }
        }
      }
    }
    Ok(())
  }
}

/// A parse rule for [`DeclareDecl`].
pub fn parser() -> impl ParseRule<Modifier=DeclareMod> {
  Several::new(vec!(
    Box::new(Constant::new("gdscript-prim", DeclareMod::BuiltinFlag)),
    Box::new(visibility::parser().map(DeclareMod::Visibility)),
  ))
}
