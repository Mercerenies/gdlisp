
use crate::compile::error::{Error as GDError};
use crate::sxp::dotted::TryFromDottedExprError;
use crate::ir::arglist::ArgListParseError;
use crate::ir::modifier::{ParseError as ModifierParseError};

use lalrpop_util::ParseError;

use std::io;
use std::fmt;

#[derive(Debug)]
pub enum Error {
  ParseError(String), // TODO Properly use the lalrpop_util::ParseError type here
  IOError(io::Error),
  GDError(GDError),
}

impl fmt::Display for Error {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    match self {
      Error::ParseError(err) => write!(f, "{}", err),
      Error::IOError(err) => write!(f, "{}", err),
      Error::GDError(err) => write!(f, "{}", err),
    }
  }
}

impl<L : fmt::Display, T : fmt::Display, E : fmt::Display> From<ParseError<L, T, E>> for Error {
  fn from(e: ParseError<L, T, E>) -> Error {
    Error::ParseError(e.to_string())
  }
}

impl From<io::Error> for Error {
  fn from(e: io::Error) -> Error {
    Error::IOError(e)
  }
}

impl From<GDError> for Error {
  fn from(e: GDError) -> Error {
    Error::GDError(e)
  }
}

impl From<TryFromDottedExprError> for Error {
  fn from(e: TryFromDottedExprError) -> Error {
    Error::from(GDError::from(e))
  }
}

impl From<ArgListParseError> for Error {
  fn from(e: ArgListParseError) -> Error {
    Error::from(GDError::from(e))
  }
}

impl From<ModifierParseError> for Error {
  fn from(e: ModifierParseError) -> Error {
    Error::from(GDError::from(e))
  }
}
