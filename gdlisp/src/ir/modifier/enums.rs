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

//! Parse rule for modifiers to `enum` declarations.

use crate::ir::decl::EnumDecl;
use crate::ir::export::Visibility;
use super::ParseRule;
use super::visibility;

/// Modifiers which apply to [`EnumDecl`].
#[derive(Clone, Debug, PartialEq, Eq)]
pub enum EnumMod {
  /// A visibility modifier. See [`super::visibility`].
  Visibility(Visibility),
}

impl EnumMod {
  /// Apply the modifier.
  pub fn apply(&self, decl: &mut EnumDecl) {
    match self {
      EnumMod::Visibility(vis) => {
        decl.visibility = *vis;
      }
    }
  }
}

/// A parse rule for modifiers which apply to [`EnumDecl`].
pub fn parser() -> impl ParseRule<Modifier=EnumMod> {
  visibility::parser().map(EnumMod::Visibility)
}
