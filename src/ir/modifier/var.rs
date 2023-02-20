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

//! Parse rule for modifiers that can be applied to instance variable
//! declarations.

use crate::ir::decl::ClassVarDecl;
use crate::compile::body::class_initializer::InitTime;
use super::{ParseRule, Constant};

/// A modifier which applies to a [`ClassVarDecl`].
#[derive(Clone, Debug, PartialEq, Eq)]
pub enum VarMod {
  /// An "onready" modifier.
  Onready,
}

impl VarMod {
  /// Apply the modifier to `decl`.
  pub fn apply(&self, decl: &mut ClassVarDecl) {
    match self {
      VarMod::Onready => {
        decl.init_time = InitTime::Ready;
      }
    }
  }
}

/// A parse rule for constant declarations.
pub fn parser() -> impl ParseRule<Modifier=VarMod> {
  Constant::new("onready", VarMod::Onready).unique()
}
