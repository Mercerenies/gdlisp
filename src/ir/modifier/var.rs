
//! Parse rule for modifiers that can be applied to instance variable
//! declarations.

use crate::ir::decl::ClassVarDecl;
use crate::compile::body::class_initializer::InitTime;
use super::{ParseRule, Constant};

/// A modifier which applies to a [`ClassVarDecl`].
#[derive(Clone, Debug, PartialEq, Eq)]
pub enum VarMod {
  /// An "onready" modifier.
  Onready,
}

impl VarMod {
  /// Apply the modifier to `decl`.
  pub fn apply(&self, decl: &mut ClassVarDecl) {
    match self {
      VarMod::Onready => {
        decl.init_time = InitTime::Ready;
      }
    }
  }
}

/// A parse rule for constant declarations.
pub fn parser() -> impl ParseRule<Modifier=VarMod> {
  Constant::new("onready", VarMod::Onready).unique()
}
