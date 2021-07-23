
//! Parse rule for declaring [`Visibility`] of an identifier.

use crate::ir::export::Visibility;
use super::{ParseRule, Several, Constant};

/// A parse rule which checks for the literal symbols `public` or
/// `private`, to return the appropriate [`Visibility`] values. The
/// entire parse rule is wrapped in [`ParseRule::unique`], hence if
/// the user ever supplies two visibility modifiers (whether they are
/// the same or different), a fatal error will be issued.
pub fn parser() -> impl ParseRule<Modifier=Visibility> {
  Several::new(vec!(
    Box::new(Constant::new("public", Visibility::Public)),
    Box::new(Constant::new("private", Visibility::Private)),
  )).named("visibility").unique()
}
