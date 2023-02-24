// Copyright 2023 Silvio Mayolo
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

//! Provides the [`Recursive`] trait.

/// A `Recursive` data structure is one that (as the name implies) can
/// recursively contain elements of itself. Further, implementations
/// of this trait provide a mechanism for determining the maximum
/// recursion depth of the trait.
pub trait Recursive {

  /// The maximum depth of the recursive data structure. Atomic nodes,
  /// those which do not contain any further instances of the data
  /// structure, should have a depth of 1. Any node which contains
  /// other nodes has depth equal to the maximum of the depths of its
  /// subnodes plus 1.
  fn depth(&self) -> u32;

}
