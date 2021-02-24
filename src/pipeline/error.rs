
use crate::compile::error::{Error as GDError};
use crate::sxp::dotted::TryFromDottedExprError;

use lalrpop_util::ParseError;

use std::io;
use std::fmt::Display;

#[derive(Debug)]
pub enum Error {
  ParseError(String), // TODO Properly use the lalrpop_util::ParseError type here
  IOError(io::Error),
  GDError(GDError),
}

impl<L : Display, T : Display, E : Display> From<ParseError<L, T, E>> for Error {
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
