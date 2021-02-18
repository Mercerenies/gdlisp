
use crate::sxp;
use crate::sxp::ast::AST;
use crate::ir::arglist::ArgListParseError;

#[derive(Debug)]
pub enum Error {
  DottedListError,
  ArgListParseError(ArgListParseError),
  CannotCall(AST),
  TooFewArgs(String, usize),
  TooManyArgs(String, usize),
  InvalidArg(String, AST, String), // Function, argument, expected
  NoSuchVar(String),
  NoSuchFn(String),
  UnknownDecl(AST),
  InvalidDecl(AST),
}

impl From<sxp::dotted::TryFromDottedExprError> for Error {
  fn from(_: sxp::dotted::TryFromDottedExprError) -> Error {
    Error::DottedListError
  }
}

impl From<ArgListParseError> for Error {
  fn from(err: ArgListParseError) -> Error {
    Error::ArgListParseError(err)
  }
}
