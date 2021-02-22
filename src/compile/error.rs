
use crate::sxp;
use crate::sxp::ast::AST;
use crate::ir::arglist::ArgListParseError;
use crate::ir::import::ImportDeclParseError;

#[derive(Debug)]
pub enum Error {
  DottedListError,
  ArgListParseError(ArgListParseError),
  ImportDeclParseError(ImportDeclParseError),
  CannotCall(AST),
  TooFewArgs(String, usize),
  TooManyArgs(String, usize),
  InvalidArg(String, AST, String), // Function, argument, expected
  NoSuchVar(String),
  NoSuchFn(String),
  UnknownDecl(AST),
  InvalidDecl(AST),
  UnquoteOutsideQuasiquote,
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

impl From<ImportDeclParseError> for Error {
  fn from(err: ImportDeclParseError) -> Error {
    Error::ImportDeclParseError(err)
  }
}
