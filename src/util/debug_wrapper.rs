
// Wrapper for non-Debug types that prints a trivial Debug
// representation.

use std::fmt::{self, Debug};

#[derive(Clone, Eq, PartialEq, Ord, PartialOrd)]
#[repr(transparent)]
pub struct DebugWrapper<T>(pub T);

impl<T> Debug for DebugWrapper<T> {

  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    write!(f, "...")
  }

}
