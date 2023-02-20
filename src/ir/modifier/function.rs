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

//! Modifiers which apply to function declarations.
//!
//! This modifier type applies to standalone functions. For the
//! modifier type which applies to instance methods, see
//! [`super::instance_method`].

use crate::ir::decl::FnDecl;
use crate::ir::export::Visibility;
use super::{ParseRule, Several};
use super::visibility;
use super::magic;

/// Modifier type which applies to [`FnDecl`].
#[derive(Clone)]
pub enum FnMod {
  /// Visibility modifier, as per [`super::visibility`].
  Visibility(Visibility),
  /// A magic declaration, as per [`super::magic`].
  Magic(String),
}

impl FnMod {
  /// Apply the modifier.
  pub fn apply(&self, decl: &mut FnDecl) {
    match self {
      FnMod::Visibility(vis) => {
        decl.visibility = *vis;
      }
      FnMod::Magic(m) => {
        decl.call_magic = Some(m.clone());
      }
    }
  }
}

/// A parse rule for function modifiers.
pub fn parser() -> impl ParseRule<Modifier=FnMod> {
  Several::new(vec!(
    Box::new(visibility::parser().map(FnMod::Visibility)),
    Box::new(magic::parser().map(FnMod::Magic)),
  ))
}
