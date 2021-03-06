
use crate::sxp;
use crate::sxp::ast::AST;
use crate::ir::arglist::ArgListParseError;
use crate::ir::import::ImportDeclParseError;
use crate::runner::path::RPathBuf;

use std::fmt;

// TODO TooFewArgs and TooManyArgs sometimes use the second argument
// to specify expected # of args and sometimes use it to specify the
// given # of args. Standardize this, likely by taking two arguments.

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
  NoSuchFile(RPathBuf),
}

impl fmt::Display for Error {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    match self {
      Error::DottedListError => {
        write!(f, "Unexpected dotted list")
      }
      Error::ArgListParseError(err) => {
        write!(f, "{}", err)
      }
      Error::ImportDeclParseError(err) => {
        write!(f, "{}", err)
      }
      Error::CannotCall(ast) => {
        write!(f, "Cannot make function call on expression {}", ast)
      }
      Error::TooFewArgs(name, _) => {
        write!(f, "Too few arguments to call {}", name)
      }
      Error::TooManyArgs(name, _) => {
        write!(f, "Too many arguments to call {}", name)
      }
      Error::InvalidArg(name, provided, expected) => {
        write!(f, "Invalid argument to {}, given {}, expecting {}", name, provided, expected)
      }
      Error::NoSuchVar(name) => {
        write!(f, "No such variable {}", name)
      }
      Error::NoSuchFn(name) => {
        write!(f, "No such function {}", name)
      }
      Error::UnknownDecl(ast) => {
        write!(f, "Unknown declaration {}", ast)
      }
      Error::InvalidDecl(ast) => {
        write!(f, "Invalid declaration {}", ast)
      }
      Error::UnquoteOutsideQuasiquote => {
        write!(f, "Unquote (,) can only be used inside quasiquote (`)")
      }
      Error::NoSuchFile(p) => {
        write!(f, "Cannot locate file {}", p)
      }
    }
  }
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
