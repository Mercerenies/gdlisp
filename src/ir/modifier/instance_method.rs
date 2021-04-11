
use crate::ir::decl::{ClassFnDecl, ConstructorDecl};
use crate::gdscript::decl::Static;
use crate::compile::error::Error;
use super::{ParseRule, Several, Constant};

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum FnMod {
  Static,
}

impl FnMod {
  pub fn apply(&self, decl: &mut ClassFnDecl) {
    match self {
      FnMod::Static => {
        decl.is_static = Static::IsStatic;
      }
    }
  }
  pub fn apply_to_constructor(&self, _decl: &mut ConstructorDecl) -> Result<(), Error> {
    match self {
      FnMod::Static => {
        Err(Error::StaticConstructor)
      }
    }
  }
}

pub fn parser() -> impl ParseRule<Modifier=FnMod> {
  Several::new(vec!(
    Box::new(Constant::new("static", FnMod::Static))
  ))
}
