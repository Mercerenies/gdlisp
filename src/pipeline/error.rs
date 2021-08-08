
use crate::compile::error::{Error as GDError};
use crate::pipeline::source::SourceOffset;
use crate::sxp::dotted::TryFromDottedExprError;
use crate::ir::arglist::ArgListParseError;
use crate::ir::modifier::{ParseError as ModifierParseError};

use lalrpop_util::ParseError;

use std::io;
use std::fmt;

// TODO Rename this to PError and compile::Error to GDError
// canonically so that we don't have two types with canonical name
// "Error".

#[derive(Debug)]
pub enum Error {
  ParseError(ParseError<SourceOffset, String, String>),
  IOError(IOError),
  GDError(GDError),
}

/// An [`io::Error`] which has a [`SourceOffset`] attached to it.
#[derive(Debug)]
pub struct IOError {
  pub error: io::Error,
  pub pos: SourceOffset,
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

impl fmt::Display for IOError {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    write!(f, "{}", self.error)
  }
}

impl IOError {

  pub fn new(error: io::Error, pos: SourceOffset) -> IOError {
    IOError { error, pos }
  }

}

impl From<IOError> for io::Error {
  fn from(err: IOError) -> io::Error {
    err.error
  }
}

fn _impl_partial_eq_warning(err: Error) {
  // If this function produces exhaustive match warnings, then
  // PartialEq is missing a case.
  match err {
    Error::ParseError(_) => (),
    Error::IOError(_) => (),
    Error::GDError(_) => (),
  }
}

impl PartialEq<Error> for Error {
  fn eq(&self, other: &Error) -> bool {
    match (self, other) {
      (Error::ParseError(a), Error::ParseError(b)) => a == b,
      (Error::IOError(_), Error::IOError(_)) => true, // Best we can do
      (Error::GDError(a), Error::GDError(b)) => a == b,
      (_, _) => false,
    }
  }
}

impl Eq for Error {}

impl<L, T : fmt::Display, E : fmt::Display> From<ParseError<L, T, E>> for Error
where SourceOffset : From<L> {
  fn from(e: ParseError<L, T, E>) -> Error {
    Error::ParseError(e.map_location(SourceOffset::from).map_token(|x| x.to_string()).map_error(|x| x.to_string()))
  }
}

impl From<IOError> for Error {
  fn from(e: IOError) -> Error {
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
