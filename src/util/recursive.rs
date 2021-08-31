
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
