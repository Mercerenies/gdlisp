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

//! A parse rule for modifiers to `sys/declare` declarations.

use crate::ir::decl::DeclareDecl;
use crate::ir::export::Visibility;
use super::ParseRule;
use super::visibility;

/// Modifiers to compile-time declaration directives.
#[derive(Clone, Debug, PartialEq, Eq)]
pub enum DeclareMod {
  /// A visibility modifier. See [`super::visibility`].
  Visibility(Visibility),
}

impl DeclareMod {
  /// Apply the modifier to `decl`.
  pub fn apply(&self, decl: &mut DeclareDecl) {
    match self {
      DeclareMod::Visibility(vis) => {
        decl.visibility = *vis;
      }
    }
  }
}

/// A parse rule for [`DeclareDecl`].
pub fn parser() -> impl ParseRule<Modifier=DeclareMod> {
  visibility::parser().map(DeclareMod::Visibility)
}
