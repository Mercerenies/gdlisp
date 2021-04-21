
use crate::ir::decl::ClassDecl;
use crate::ir::export::Visibility;
use super::{ParseRule, Several, Constant};
use super::visibility;

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum ClassMod {
  Main,
  Visibility(Visibility),
}

impl ClassMod {
  pub fn apply(&self, decl: &mut ClassDecl) {
    match self {
      ClassMod::Main => {
        decl.main_class = true;
      }
      ClassMod::Visibility(vis) => {
        decl.visibility = *vis;
      }
    }
  }
}

pub fn parser() -> impl ParseRule<Modifier=ClassMod> {
  Several::new(vec!(
    Box::new(Constant::new("main", ClassMod::Main).unique()),
    Box::new(visibility::parser().map(ClassMod::Visibility)),
  ))
}
