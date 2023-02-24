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

//! Provides the [`SpecialRef`] type and associated values.

use super::expr::ExprF;

/// A `SpecialRef` is a type of [`Expr`](super::expr::Expr) which is
/// superficially a literal form but which acquires some state from
/// its surrounding environment or context. That is, a special
/// reference can be thought of as a 0-ary function which is not, in
/// the mathematical sense, a pure function.
///
/// Special references are converted to [`Expr`](super::expr::Expr)
/// either via the explicit constructor
/// [`ExprF::SpecialRef`](super::expr::ExprF::SpecialRef) or via the
/// general-purpose helper
/// [`Expr::from_value`](super::expr::Expr::from_value).
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum SpecialRef {
  /// `ThisFile` will compile into a `load` expression which loads and
  /// returns a reference to the current file, as a GDScript script
  /// object.
  ThisFile,
  /// `ThisFileName` will compile into a string literal expression
  /// which refers to the current (GDScript) filename. If called
  /// during a macro expansion, this will expand to the virtual name
  /// of the macro file.
  ThisFileName,
  /// `ThisTrueFileName` will compile into a string literal expression
  /// which refers to the current (GDScript) filename. If called
  /// during macro expansion, this will expand into the *true* final
  /// name of the file, not the name of the virtual macro file.
  ThisTrueFileName,
  /// `GodotVersion` returns the current Godot version, as an integer
  /// such that later Godot versions compare greater than earlier
  /// ones.
  GodotVersion,
}

impl From<SpecialRef> for ExprF {
  fn from(special_ref: SpecialRef) -> ExprF {
    ExprF::SpecialRef(special_ref)
  }
}
