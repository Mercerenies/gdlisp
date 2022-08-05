
//! Provides the [`NameGenerator`] trait, whose implementations are
//! capable of generating names.

/// `NameGenerator` represents structures intended to generate new,
/// unique symbolic names.
pub trait NameGenerator {

  /// Generate a new name, beginning with `prefix`.
  fn generate_with(&mut self, prefix: &str) -> String;

  /// Generate a new name, using
  /// [`generate_with`](NameGenerator::generate_with) and
  /// [`DEFAULT_PREFIX`] as the prefix.
  fn generate(&mut self) -> String {
    self.generate_with(DEFAULT_PREFIX)
  }

}

/// The default `prefix` argument to
/// [`NameGenerator::generate_with`].
pub const DEFAULT_PREFIX: &str = "_G";