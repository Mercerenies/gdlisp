
//! Wrapper struct and helpers for keeping track of positions in the
//! source code.
//!
//! All source positions always refer to the original GDLisp source
//! file, never to the position in an intermediate representation.

use std::fmt;

/// A `SourceOffset` is really just a [`usize`] representing a byte
/// offset into the original file.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
#[repr(transparent)]
pub struct SourceOffset(pub usize);

impl From<usize> for SourceOffset {
  fn from(x: usize) -> SourceOffset {
    SourceOffset(x)
  }
}

impl fmt::Display for SourceOffset {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    write!(f, "{}", self.0)
  }
}
