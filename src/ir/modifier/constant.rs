
use crate::ir::decl::ConstDecl;
use crate::ir::export::Visibility;
use super::ParseRule;
use super::visibility;

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum ConstMod {
  Visibility(Visibility),
}

impl ConstMod {
  pub fn apply(&self, decl: &mut ConstDecl) {
    match self {
      ConstMod::Visibility(vis) => {
        decl.visibility = *vis;
      }
    }
  }
}

pub fn parser() -> impl ParseRule<Modifier=ConstMod> {
  visibility::parser().map(ConstMod::Visibility)
}
