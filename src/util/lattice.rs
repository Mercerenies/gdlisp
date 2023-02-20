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

//! Defines the [`Lattice`] trait, for mathematical
//! [lattices](https://en.wikipedia.org/wiki/Lattice_(order)).

/// A [lattice](https://en.wikipedia.org/wiki/Lattice_(order)) is a
/// collection of values together with a join and meet operation.
/// These operations should be associative and commutative, and they
/// should satisfy the following rules, often called the *absorption
/// laws*.
///
/// ```text
/// a.join(a.meet(b)) == a
/// a.meet(a.join(b)) == a
/// ```
///
/// In the future, we may define relationships between this trait,
/// [`PartialOrd`], and [`Ord`], since in order theory every total
/// order is a lattice and every lattice is a partial order. These
/// relations, as of now, are not established in Rust, and this trait
/// is unrelated to the other two. In the spirit of providing an
/// intuitive API, types which implement either `PartialOrd` or `Ord`
/// *and* `Lattice` should make the instances compatible.
pub trait Lattice {

  /// The lattice-theoretic join (or least upper bound) of the two
  /// values.
  fn join(self, other: Self) -> Self;

  /// The lattice-theoretic meet (or greatest lower bound) of the two
  /// values.
  fn meet(self, other: Self) -> Self;

}

#[allow(clippy::unused_unit)]
impl Lattice for () {
  fn join(self, _other: ()) -> () {
    ()
  }
  fn meet(self, _other: ()) -> () {
    ()
  }
}

/// An implementation of the binary product lattice.
impl<A: Lattice, B: Lattice> Lattice for (A, B) {
  fn join(self, other: (A, B)) -> (A, B) {
    (self.0.join(other.0), self.1.join(other.1))
  }
  fn meet(self, other: (A, B)) -> (A, B) {
    (self.0.meet(other.0), self.1.meet(other.1))
  }
}
