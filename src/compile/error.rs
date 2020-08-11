
use crate::sxp;
use crate::sxp::ast::AST;

#[derive(Debug)]
pub enum Error {
  DottedListError,
  CannotCall(AST),
  TooFewArgs(String, usize),
  TooManyArgs(String, usize),
}

impl From<sxp::dotted::TryFromDottedExprError> for Error {
  fn from(_: sxp::dotted::TryFromDottedExprError) -> Error {
    Error::DottedListError
  }
}
