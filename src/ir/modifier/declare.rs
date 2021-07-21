
use crate::ir::decl::DeclareDecl;
use crate::ir::export::Visibility;
use super::ParseRule;
use super::visibility;

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum DeclareMod {
  Visibility(Visibility),
}

impl DeclareMod {
  pub fn apply(&self, decl: &mut DeclareDecl) {
    match self {
      DeclareMod::Visibility(vis) => {
        decl.visibility = *vis;
      }
    }
  }
}

pub fn parser() -> impl ParseRule<Modifier=DeclareMod> {
  visibility::parser().map(DeclareMod::Visibility)
}
