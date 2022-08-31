
use crate::compile::error::GDError;
use crate::pipeline::source::{SourceOffset, Sourced};
use crate::sxp::dotted::TryFromDottedExprError;
use crate::ir::loops::error::LoopPrimitiveError;
use crate::ir::arglist::error::ArgListParseError;
use crate::ir::modifier::{ParseError as ModifierParseError};
use crate::ir::decl::DuplicateMainClassError;
use crate::ir::scope::error::ScopeError;
use crate::ir::depends::DependencyError;

use lalrpop_util::ParseError;

use std::io;
use std::fmt;
use std::error::Error;

#[derive(Debug)]
pub enum PError {
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

/// [`PError`] is [`Sourced`] in a somewhat trivial way. It's not a
/// recursive data type, so it doesn't "contain" a separate value in
/// the same sense as other implementors like [`GDError`] (which
/// contains [`GDErrorF`](crate::compile::error::GDErrorF)). But a
/// `PError` always has its `SourceOffset` nonetheless.
impl Sourced for PError {
  type Item = PError;

  fn get_source(&self) -> SourceOffset {
    match self {
      PError::ParseError(err) => get_source_from_parse_error(err),
      PError::IOError(err) => err.pos,
      PError::GDError(err) => err.get_source(),
    }
  }

  fn get_value(&self) -> &PError {
    self
  }

}

impl Error for PError {

  fn source(&self) -> Option<&(dyn Error + 'static)> {
    match self {
      PError::ParseError(err) => Some(err),
      PError::IOError(err) => Some(&err.error),
      PError::GDError(err) => Some(err),
    }
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

impl fmt::Display for PError {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    match self {
      PError::ParseError(err) => write!(f, "{}", err),
      PError::IOError(err) => write!(f, "{}", err),
      PError::GDError(err) => write!(f, "{}", err),
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

fn _impl_partial_eq_warning(err: PError) {
  // If this function produces exhaustive match warnings, then
  // PartialEq is missing a case.
  match err {
    PError::ParseError(_) => (),
    PError::IOError(_) => (),
    PError::GDError(_) => (),
  }
}

impl PartialEq<PError> for PError {
  fn eq(&self, other: &PError) -> bool {
    match (self, other) {
      (PError::ParseError(a), PError::ParseError(b)) => a == b,
      (PError::IOError(_), PError::IOError(_)) => true, // Best we can do
      (PError::GDError(a), PError::GDError(b)) => a == b,
      (_, _) => false,
    }
  }
}

impl Eq for PError {}

impl<L, T : fmt::Display, E : fmt::Display> From<ParseError<L, T, E>> for PError
where SourceOffset : From<L> {
  fn from(e: ParseError<L, T, E>) -> PError {
    PError::ParseError(e.map_location(SourceOffset::from).map_token(|x| x.to_string()).map_error(|x| x.to_string()))
  }
}

impl From<IOError> for PError {
  fn from(e: IOError) -> PError {
    PError::IOError(e)
  }
}

impl From<GDError> for PError {
  fn from(e: GDError) -> PError {
    PError::GDError(e)
  }
}

impl From<TryFromDottedExprError> for PError {
  fn from(e: TryFromDottedExprError) -> PError {
    PError::from(GDError::from(e))
  }
}

impl From<ArgListParseError> for PError {
  fn from(e: ArgListParseError) -> PError {
    PError::from(GDError::from(e))
  }
}

impl From<ModifierParseError> for PError {
  fn from(e: ModifierParseError) -> PError {
    PError::from(GDError::from(e))
  }
}

impl From<DuplicateMainClassError> for PError {
  fn from(e: DuplicateMainClassError) -> PError {
    PError::from(GDError::from(e))
  }
}

impl<NS> From<ScopeError<NS>> for PError
where GDError: From<ScopeError<NS>> {
  fn from(e: ScopeError<NS>) -> PError {
    PError::from(GDError::from(e))
  }
}

impl From<LoopPrimitiveError> for PError {
  fn from(e: LoopPrimitiveError) -> PError {
    PError::from(GDError::from(e))
  }
}

impl From<DependencyError> for PError {
  fn from(e: DependencyError) -> PError {
    PError::from(GDError::from(e))
  }
}
