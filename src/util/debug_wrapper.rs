
//! Wrapper for non-[`Debug`] types that prints a trivial `Debug`
//! representation.

use serde::{Serialize, Deserialize};

use std::fmt::{self, Debug};

/// A `DebugWrapper` is a very thin wrapper around a `T`.
/// `DebugWrapper` is mostly uninteresting, except that it
/// unconditionally implements [`Debug`] for all types. The debug
/// representation for every instance of `DebugWrapper` is always an
/// ellipsis `"..."`.
#[derive(Clone, Eq, PartialEq, Ord, PartialOrd, Serialize, Deserialize)]
#[repr(transparent)]
pub struct DebugWrapper<T>(pub T);

impl<T> Debug for DebugWrapper<T> {

  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    write!(f, "...")
  }

}
