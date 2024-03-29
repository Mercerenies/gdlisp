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

//! Defines the [`AccessType`] enum, for describing methods of
//! accessing a local variable.

use serde::{Serialize, Deserialize};

use crate::util::lattice::Lattice;

/// `AccessType` describes the different ways we can access a variable.
///
/// Note that, generally speaking, an `AccessType` should be regarded
/// as context-sensitive. That is, an access type of
/// [`None`](AccessType::None) does *not* necessarily mean that the
/// variable is irrelevant to the whole program; it simply means that
/// the variable is never accessed in the scope we're worried about.
///
/// These possibilities do *not* form a total ordering, but they do
/// form a [lattice](crate::util::lattice::Lattice) as follows.
///
/// ```text
///   ClosedRW
///   /      \
///  RW    ClosedRead
///   \      /
///     Read
///      |
///     None
/// ```
///
/// This lattice is realized by the [`AccessType::meet`] and
/// [`AccessType::join`] functions.
///
/// The logic for the lattice is as follows. If a variable is accessed
/// in two ways, `a` and `b` (both `AccessType` values), then we need
/// at least `AccessType::join(a, b)` control over the variable to be
/// able to implement both accesses correctly. For most access types,
/// this is straightforward: if a variable is read (`Read`) and
/// written to (`RW`) but never in a closure, then we need `RW`. If a
/// variable is read in (`ClosedRead`) and out (`Read`) of a closure
/// but never written to, then we need `ClosedRead`. The one slightly
/// more provocative corner case is when a variable is written to
/// outside of a closure (`RW`) and read within one (`ClosedRead`).
/// This is equivalent in power to writing to a variable within a
/// closure (`ClosedRW`), since in both cases we will need an explicit
/// cell wrapper around the variable to implement the read-write
/// correctly.
///
/// Note that, for the purposes of `AccessType`, we only care about
/// actual modifications to the *value* of the variable. Internal
/// mutability, such as modifying the *fields* of an instance stored
/// in a variable, are irrelevant. That is,
///
/// ```text
/// var x = 1
/// x = 2
/// var y = [1]
/// y[0] = 2
/// ```
///
/// In this example, `x` requires `RW` as the variable is directly
/// written to. However, `y` only requires `Read`, because the
/// variable, while internally mutated, it never directly rewritten.
/// We draw this distinction because `AccessType` is intended for
/// determining what sort of closure we need to construct for a
/// variable for lambda purposes, and a closure does not care about
/// internal mutability, only the top-level value and how constant it
/// remains.
#[derive(PartialEq, Eq, Debug, Clone, Copy, Serialize, Deserialize)]
pub enum AccessType {
  /// The variable in question is never accessed in the relevant
  /// scope.
  None,
  /// The variable is read *directly* in the relevant scope.
  Read,
  /// The variable is written to *directly* in the relevant scope.
  ///
  /// This includes direct assignment to the variable itself or to an
  /// instance variable *on* this variable. Nested assignment to an
  /// instance variable of an instance variable (or deeper) is
  /// excluded specifically. This condition is included so as to
  /// ensure that COW data structures such as Vector behave correctly
  /// inside closures. Specifically, COW structures should be wrapped
  /// in cells if a slot on them is ever changed, not just if the
  /// variable itself is assigned to.
  RW,
  /// The variable is read within a closure in the relevant scope.
  ClosedRead,
  /// The variable is written to within a closure in the relevant
  /// scope.
  ClosedRW,
} // TODO We could get PartialOrd on this (not the derived instance but a custom one).

impl AccessType {

  /// Given an access type which occurs inside of a lambda, convert it
  /// to the access type observed from outside the lambda. This method
  /// converts `Read` into `ClosedRead` and `RW` into `ClosedRW`,
  /// leaving all other inputs alone.
  ///
  /// # Examples
  ///
  /// ```
  /// # use gdlisp::ir::access_type::AccessType;
  /// assert_eq!(AccessType::None.closed(), AccessType::None);
  /// assert_eq!(AccessType::Read.closed(), AccessType::ClosedRead);
  /// assert_eq!(AccessType::RW.closed(), AccessType::ClosedRW);
  /// assert_eq!(AccessType::ClosedRead.closed(), AccessType::ClosedRead);
  /// assert_eq!(AccessType::ClosedRW.closed(), AccessType::ClosedRW);
  /// ```
  pub fn closed(&self) -> AccessType {
    match *self {
      AccessType::None => AccessType::None,
      AccessType::Read | AccessType::ClosedRead => AccessType::ClosedRead,
      AccessType::RW | AccessType::ClosedRW => AccessType::ClosedRW,
    }
  }

  /// Returns whether the access type requires a cell.
  ///
  /// Only [`AccessType::ClosedRW`] requires a cell. Any variables
  /// which are never used within a closure are [`AccessType::RW`] or
  /// less, and those variables can simply be written to directly, as
  /// the standard Godot local variable semantics will handle this
  /// case correctly. Any variables which are read-only (possibly
  /// within a closure) are at most [`AccessType::ClosedRead`], and
  /// those variables can likewise be read directly, since they'll
  /// never be changed, so Godot's built-in pointer copying is
  /// harmless.
  ///
  /// However, if a variable is written to within a closure, or if a
  /// variable is written to and (separately) read within a closure,
  /// then the standard process of closing around the variable will
  /// implicitly copy the pointer to the variable, which means that
  /// changes to the original will not be reflected in the closure and
  /// vice versa. Hence, in this situation, we need to wrap the local
  /// variable in a cell. `Cell` is a GDLisp standard library class
  /// which has a single field and no methods. Its only purpose is
  /// adding one layer of indirection to avoid this implicit copy
  /// problem.
  pub fn requires_cell(&self) -> bool {
    *self == AccessType::ClosedRW
  }

  /// Returns whether or not the access type involves modifying the
  /// variable. [`AccessType::ClosedRW`] and [`AccessType::RW`]
  /// involves
  pub fn is_written_to(&self) -> bool {
    *self == AccessType::ClosedRW || *self == AccessType::RW
  }

}

impl Lattice for AccessType {

  /// The least-upper-bound of `a` and `b`, under the lattice
  /// described in [`AccessType`].
  fn join(self, b: AccessType) -> AccessType {
    if self == AccessType::None {
      return b;
    }
    if b == AccessType::None {
      return self;
    }
    if self == AccessType::Read {
      return b;
    }
    if b == AccessType::Read {
      return self;
    }
    if self == b {
      return self;
    }
    AccessType::ClosedRW
  }

  /// The greatest-lower-bound of `a` and `b`, under the lattice
  /// described in [`AccessType`].
  fn meet(self, b: AccessType) -> AccessType {
    if self == AccessType::ClosedRW {
      return b;
    }
    if b == AccessType::ClosedRW {
      return self;
    }
    if self == b {
      return self;
    }
    if self == AccessType::None {
      return self;
    }
    if b == AccessType::None {
      return b;
    }
    AccessType::Read
  }

}
