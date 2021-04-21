
use crate::ir::decl::MacroDecl;
use crate::ir::export::Visibility;
use super::ParseRule;
use super::visibility;

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum MacroMod {
  Visibility(Visibility),
}

impl MacroMod {
  pub fn apply(&self, decl: &mut MacroDecl) {
    match self {
      MacroMod::Visibility(vis) => {
        decl.visibility = *vis;
      }
    }
  }
}

pub fn parser() -> impl ParseRule<Modifier=MacroMod> {
  visibility::parser().map(MacroMod::Visibility)
}
