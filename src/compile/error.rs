
use crate::sxp;

#[derive(Debug)]
pub enum Error {
  DottedListError,
  CannotCall(String),
}

impl From<sxp::dotted::TryFromDottedExprError> for Error {
  fn from(_: sxp::dotted::TryFromDottedExprError) -> Error {
    Error::DottedListError
  }
}
