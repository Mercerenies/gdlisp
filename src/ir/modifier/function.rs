
use crate::ir::decl::FnDecl;
use crate::ir::export::Visibility;
use super::{ParseRule, Several};
use super::visibility;
use super::magic;

#[derive(Clone)]
pub enum FnMod {
  Visibility(Visibility),
  Magic(String),
}

impl FnMod {
  pub fn apply(&self, decl: &mut FnDecl) {
    match self {
      FnMod::Visibility(vis) => {
        decl.visibility = *vis;
      }
      FnMod::Magic(m) => {
        decl.call_magic = Some(m.clone());
      }
    }
  }
}

pub fn parser() -> impl ParseRule<Modifier=FnMod> {
  Several::new(vec!(
    Box::new(visibility::parser().map(FnMod::Visibility)),
    Box::new(magic::parser().map(FnMod::Magic)),
  ))
}
