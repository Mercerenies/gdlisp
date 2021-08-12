
use crate::compile::error::{Error as GDError};
use crate::pipeline::source::{SourceOffset, Sourced};
use crate::sxp::dotted::TryFromDottedExprError;
use crate::ir::arglist::ArgListParseError;
use crate::ir::modifier::{ParseError as ModifierParseError};
use crate::ir::decl::DuplicateMainClassError;

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

/// [`Error`] is [`Sourced`] in a somewhat trivial way. It's not a
/// recursive data type, so it doesn't "contain" a separate value in
/// the same sense as other implementors like [`GDError`] (which
/// contains [`GDErrorF`](crate::compile::error::ErrorF)). But An
/// `Error` always has its `SourceOffset` nonetheless.
impl Sourced for Error {
  type Item = Error;

  fn get_source(&self) -> SourceOffset {
    match self {
      Error::ParseError(err) => get_source_from_parse_error(err),
      Error::IOError(err) => err.pos,
      Error::GDError(err) => err.get_source(),
    }
  }

  fn get_value(&self) -> &Error {
    self
  }

}

fn get_source_from_parse_error<T, E>(err: &ParseError<SourceOffset, T, E>) -> SourceOffset {
  match err {
    ParseError::InvalidToken { location } => *location,
    ParseError::UnrecognizedEOF { location, .. } => *location,
    ParseError::UnrecognizedToken { token: (location, _, _), .. } => *location,
    ParseError::ExtraToken { token: (location, _, _), .. } => *location,
    ParseError::User { .. } => SourceOffset::default(), // TODO There's no error location information for this one. Can we get the user error type E to contain that information somehow?
  }
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

impl From<DuplicateMainClassError> for Error {
  fn from(e: DuplicateMainClassError) -> Error {
    Error::from(GDError::from(e))
  }
}
