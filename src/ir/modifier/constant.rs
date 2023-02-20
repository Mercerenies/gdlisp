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

//! Parse rule for modifiers that can be applied to constant declarations.

use crate::ir::decl::ConstDecl;
use crate::ir::export::Visibility;
use super::ParseRule;
use super::visibility;

/// A modifier which applies to a [`ConstDecl`].
#[derive(Clone, Debug, PartialEq, Eq)]
pub enum ConstMod {
  /// A visibility modifier, as per [`super::visibility`].
  Visibility(Visibility),
}

impl ConstMod {
  /// Apply the modifier to `decl`.
  pub fn apply(&self, decl: &mut ConstDecl) {
    match self {
      ConstMod::Visibility(vis) => {
        decl.visibility = *vis;
      }
    }
  }
}

/// A parse rule for constant declarations.
pub fn parser() -> impl ParseRule<Modifier=ConstMod> {
  visibility::parser().map(ConstMod::Visibility)
}
