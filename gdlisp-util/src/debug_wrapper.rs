// Copyright 2023, 2024 Silvio Mayolo
//
// This file is part of GDLisp.
//
// GDLisp is free software: you can redistribute it and/or modify it
// under the terms of the GNU General Public License as published by
// the Free Software Foundation, either version 3 of the License, or
// (at your option) any later version.
//
// GDLisp is distributed in the hope that it will be useful, but
// WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
// General Public License for more details.
//
// You should have received a copy of the GNU General Public License
// along with GDLisp. If not, see <https://www.gnu.org/licenses/>.

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

#[cfg(test)]
mod tests {
  use super::*;

  #[test]
  fn test_debug_repr() {
    let value = DebugWrapper(42i64);
    let repr = format!("{:?}", value);
    assert_eq!(repr, String::from("..."));
  }

}
