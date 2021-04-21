
use crate::ir::decl::EnumDecl;
use crate::ir::export::Visibility;
use super::ParseRule;
use super::visibility;

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum EnumMod {
  Visibility(Visibility),
}

impl EnumMod {
  pub fn apply(&self, decl: &mut EnumDecl) {
    match self {
      EnumMod::Visibility(vis) => {
        decl.visibility = *vis;
      }
    }
  }
}

pub fn parser() -> impl ParseRule<Modifier=EnumMod> {
  visibility::parser().map(EnumMod::Visibility)
}
