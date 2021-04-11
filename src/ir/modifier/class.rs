
use crate::ir::decl::ClassDecl;
use super::{ParseRule, Several, Constant};

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum ClassMod {
  Main,
}

impl ClassMod {
  pub fn apply(&self, decl: &mut ClassDecl) {
    match self {
      ClassMod::Main => {
        decl.main_class = true;
      }
    }
  }
}

pub fn parser() -> impl ParseRule<Modifier=ClassMod> {
  Several::new(vec!(
    Box::new(Constant::new("main", ClassMod::Main)),
  ))
}
