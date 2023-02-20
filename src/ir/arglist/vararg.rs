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

//! Defines the [`VarArg`] enum, which indicates the types of variable
//! argument tails available to lambda lists.

use serde::{Serialize, Deserialize};

/// The type of "rest" argument which accumulates any extra arguments
/// to a function call.
#[derive(Clone, Copy, Debug, PartialEq, Eq, Serialize, Deserialize)]
pub enum VarArg {
  /// A `&rest` argument accumulates the arguments into a GDLisp list.
  RestArg,
  /// An `&arr` argument accumulates the arguments into a Godot array.
  ArrArg,
}

impl VarArg {

  /// An [`i32`] constant representing no variable argument at all.
  pub const NONE: i32 = 0;

  /// Converts `self` into a numerical value, suitable for
  /// communication with GDScript. This method is guaranteed to never
  /// return [`VarArg::NONE`].
  pub fn into_constant(self) -> i32 {
    match self {
      VarArg::RestArg => 1,
      VarArg::ArrArg => 2,
    }
  }

  /// Returns [`VarArg::NONE`] if `opt` is `None`, or calls
  /// [`VarArg::into_constant`] otherwise.
  pub fn arg_to_const(opt: Option<VarArg>) -> i32 {
    opt.map_or(VarArg::NONE, VarArg::into_constant)
  }

}
