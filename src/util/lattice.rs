
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
