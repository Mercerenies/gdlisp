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

//! Parse rule for declaring [`Visibility`] of an identifier.

use crate::ir::export::Visibility;
use super::{ParseRule, Several, Constant};

/// A parse rule which checks for the literal symbols `public` or
/// `private`, to return the appropriate [`Visibility`] values. The
/// entire parse rule is wrapped in [`ParseRule::unique`], hence if
/// the user ever supplies two visibility modifiers (whether they are
/// the same or different), a fatal error will be issued.
pub fn parser() -> impl ParseRule<Modifier=Visibility> {
  Several::new(vec!(
    Box::new(Constant::new("public", Visibility::Public)),
    Box::new(Constant::new("private", Visibility::Private)),
  )).named("visibility").unique()
}
