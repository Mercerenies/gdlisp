
use crate::ir::decl::FnDecl;
use crate::ir::export::Visibility;
use super::ParseRule;
use super::visibility;

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum FnMod {
  Visibility(Visibility),
}

impl FnMod {
  pub fn apply(&self, decl: &mut FnDecl) {
    match self {
      FnMod::Visibility(vis) => {
        decl.visibility = *vis;
      }
    }
  }
}

pub fn parser() -> impl ParseRule<Modifier=FnMod> {
  visibility::parser().map(FnMod::Visibility)
}
