
//! Helper for storing collections of local variables that need to be
//! closed over.
//!
//! This module defines the [`Locals`] type, which maintains a
//! collection of local variables for the purposes of creating
//! closures. The equivalent module for local functions is
//! [`super::functions`].

use crate::util;

use std::collections::HashMap;

/// A collection of local variables, as well as the broadest
/// [`AccessType`] the variables need.
#[derive(PartialEq, Eq, Debug, Clone, Default)]
pub struct Locals(HashMap<String, AccessType>);

/// `AccessType` describes the different ways we can access a variable.
///
/// Note that, generally speaking, an `AccessType` should be regarded
/// as context-sensitive. That is, an access type of
/// [`None`](AccessType::None) does *not* necessarily mean that the
/// variable is irrelevant to the whole program; it simply means that
/// the variable is never accessed in the scope we're worried about.
///
/// These possibilities do *not* form a total ordering, but they do
/// form a [lattice](https://en.wikipedia.org/wiki/Lattice_(order)) as
/// follows.
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
/// This lattice is realized by the [`AccessType::min`] and
/// [`AccessType::max`] functions.
///
/// The logic for the lattice is as follows. If a variable is accessed
/// in two ways, `a` and `b` (both `AccessType` values), then we need
/// at least `AccessType::max(a, b)` control over the variable to be
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
#[derive(PartialEq, Eq, Debug, Clone, Copy)]
pub enum AccessType {
  /// The variable in question is never accessed in the relevant
  /// scope.
  None,
  /// The variable is read *directly* in the relevant scope.
  Read,
  /// The variable is written to *directly* in the relevant scope.
  RW,
  /// The variable is read within a closure in the relevant scope.
  ClosedRead,
  /// The variable is written to within a closure in the relevant
  /// scope.
  ClosedRW,
} // TODO We could get PartialOrd on this (not the derived instance but a custom one).

impl Locals {

  /// Constructs a new, empty `Locals` object.
  pub fn new() -> Locals {
    Locals::default()
  }

  /// Constructs a `Locals` object from a hashmap mapping variable
  /// names to the broadest access type required.
  pub fn from_hashmap(map: HashMap<String, AccessType>) -> Locals {
    Locals(map)
  }

  /// Gets the access type associated to the variable. If the variable
  /// does not exist in `self`, returns [`AccessType::None`] as a
  /// default.
  pub fn get(&self, name: &str) -> AccessType {
    *self.0.get(name).unwrap_or(&AccessType::None)
  }

  /// Marks the given variable as accessed in the given way. If the
  /// variable was not known to `self` before, it is added to the
  /// table. If the variable was known, then the previous access type
  /// and `access_type` are joined (via [`AccessType::max`]) to
  /// produce the new access type for the variable.
  pub fn visited(&mut self, name: &str, access_type: AccessType) {
    self.0.insert(name.to_owned(), AccessType::max(self.get(name), access_type));
  }

  /// Removes the variable from the table, effectively resetting its
  /// access to [`AccessType::None`] and making the table not aware of
  /// that variable anymore.
  pub fn remove(&mut self, name: &str) {
    self.0.remove(name);
  }

  /// An iterator over all variable names known to the `Locals`
  /// instance, including variables which have been explicitly added
  /// but still have access type [`AccessType::None`].
  pub fn names(&self) -> impl Iterator<Item=&String> {
    self.0.keys()
  }

  /// As [`Locals::names`] but takes ownership over the values in
  /// `self.`
  pub fn into_names(self) -> impl Iterator<Item=String> {
    self.0.into_iter().map(|x| x.0)
  }

  /// Returns whether `self` is devoid of *any* variables.
  pub fn is_empty(&self) -> bool {
    self.0.is_empty()
  }

  /// Adds all of the variables from `b` into `self`. Any variables
  /// which did not exist in `self` are added, and those which did
  /// have the two access types joined via [`AccessType::max`].
  pub fn merge_with(&mut self, b: Locals) {
    util::merge_hashmap_inplace(&mut self.0, b.0, AccessType::max);
  }

  /// Filters the `Locals` instance in-place, retaining only those
  /// keys for which `f` returns true.
  pub fn retain(&mut self, mut f: impl FnMut(&str, AccessType) -> bool) {
    self.0.retain(|k, v| f(k, *v));
  }

  /// Closes over all variables in the `self`, using
  /// [`AccessType::closed`]. This method is appropriate for taking a
  /// `Locals` instance which refers to an inner lambda scope and
  /// converting it to one which describes the outer containing scope.
  pub fn closed(mut self) -> Self {
    for (_, v) in self.0.iter_mut() {
      *v = v.closed();
    }
    self
  }

}

impl AccessType {

  /// Given an access type which occurs inside of a lambda, convert it
  /// to the access type observed from outside the lambda. This method
  /// converts `Read` into `ClosedRead` and `RW` into `ClosedRW`,
  /// leaving all other inputs alone.
  ///
  /// # Examples
  ///
  /// ```
  /// # use gdlisp::ir::locals::AccessType;
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

  /// The least-upper-bound of `a` and `b`, under the lattice
  /// described in [`AccessType`].
  pub fn max(a: AccessType, b: AccessType) -> AccessType {
    if a == AccessType::None {
      return b;
    }
    if b == AccessType::None {
      return a;
    }
    if a == AccessType::Read {
      return b;
    }
    if b == AccessType::Read {
      return a;
    }
    if a == b {
      return a;
    }
    AccessType::ClosedRW
  }

  /// The greatest-lower-bound of `a` and `b`, under the lattice
  /// described in [`AccessType`].
  pub fn min(a: AccessType, b: AccessType) -> AccessType {
    if a == AccessType::ClosedRW {
      return b;
    }
    if b == AccessType::ClosedRW {
      return a;
    }
    if a == b {
      return a;
    }
    if a == AccessType::None {
      return a;
    }
    if b == AccessType::None {
      return b;
    }
    AccessType::Read
  }

}
