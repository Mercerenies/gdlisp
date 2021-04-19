
use crate::ir::export::Visibility;
use super::{ParseRule, Several, Constant};

pub fn parser() -> impl ParseRule<Modifier=Visibility> {
  Several::new(vec!(
    Box::new(Constant::new("public", Visibility::Public)),
    Box::new(Constant::new("private", Visibility::Private)),
  ))
}
