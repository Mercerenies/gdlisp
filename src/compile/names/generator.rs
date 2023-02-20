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
