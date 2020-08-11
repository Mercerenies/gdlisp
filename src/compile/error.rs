
use crate::sxp;

#[derive(Debug)]
pub enum Error {
  DottedListError,
  CannotCall(String),
  TooFewArgs(String, i32),
  TooManyArgs(String, i32),
}

impl From<sxp::dotted::TryFromDottedExprError> for Error {
  fn from(_: sxp::dotted::TryFromDottedExprError) -> Error {
    Error::DottedListError
  }
}
