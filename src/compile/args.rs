
//! This module provides convenience functions for checking the length
//! of [`Vec`] vectors of values.
//!
//! This module is *not* a handler for the concept of an "argument
//! list" on a GDLisp function; for that, use [`crate::ir::arglist`]
//! or [`crate::gdscript::arglist`], depending on whether you're
//! concerned about the GDLisp or GDScript representation of an
//! argument list. This module, instead, is for situations where the
//! *compiler* is expecting some predetermined number of arguments,
//! and we'd like a convenient way to check that the argument count is
//! correct and return an error otherwise.

use super::error::{Error, ErrorF};
use crate::pipeline::source::SourceOffset;

use std::fmt;

/// `Expecting` specifies how many arguments are expected. The two
/// bounds are always inclusive. If a function has no upper bound,
/// then [`usize::MAX`] can be used.
///
/// An `Expecting` instance where `maximum < minimum` will accept no
/// argument lists and always produce an error. No effort is made in
/// this module to prevent the construction of such instances.
#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub struct Expecting {
  pub minimum: usize,
  pub maximum: usize,
}

impl Expecting {

  /// Convenience function for constructing general `Expecting`
  /// values.
  pub fn new(minimum: usize, maximum: usize) -> Expecting {
    Expecting { minimum, maximum }
  }

  /// An `Expecting` which demands a specific number of arguments.
  pub fn exactly(value: usize) -> Expecting {
    Expecting::new(value, value)
  }

  /// An `Expecting` with no upper bound.
  pub fn at_least(minimum: usize) -> Expecting {
    Expecting::new(minimum, usize::MAX)
  }

  /// An `Expecting` with no lower bound.
  pub fn at_most(maximum: usize) -> Expecting {
    Expecting::new(0, maximum)
  }

  /// Check that the number of arguments is within the bounds
  /// specified by `self`.
  pub fn contains(&self, args_count: usize) -> bool {
    args_count >= self.minimum && args_count <= self.maximum
  }

  /// Check that `self.contains(args_count)`, and if not, raise an
  /// appropriate [`ErrorF::WrongNumberArgs`] with the given `name`
  /// and `pos`.
  pub fn validate_amount(&self, name: &str, pos: SourceOffset, args_count: usize) -> Result<(), Error> {
    if self.contains(args_count) {
      Ok(())
    } else {
      Err(Error::new(ErrorF::WrongNumberArgs(String::from(name), *self, args_count), pos))
    }
  }

  /// Validate against the length of a slice.
  ///
  /// Equivalent to `self.validate_amount(name, pos, slice.len())`.
  pub fn validate<T>(&self, name: &str, pos: SourceOffset, slice: &[T]) -> Result<(), Error> {
    self.validate_amount(name, pos, slice.len())
  }

}

impl fmt::Display for Expecting {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    if self.minimum > self.maximum {
      write!(f, "(no valid call)")
    } else if self.minimum == self.maximum {
      write!(f, "exactly {}", self.minimum)
    } else if self.maximum == usize::MAX {
      write!(f, "at least {}", self.minimum)
    } else if self.minimum == usize::MIN {
      write!(f, "at most {}", self.maximum)
    } else {
      write!(f, "{} to {}", self.minimum, self.maximum)
    }
  }
}
