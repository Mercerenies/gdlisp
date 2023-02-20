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

//! Provides the [`ScopeError`] type, which marks error during scope
//! resolution.

use crate::pipeline::source::SourceOffset;
use crate::ir::identifier::{Namespace, ClassNamespace};

// TODO Encode the offset of both duplicate names in this error type.

///// Display and error

/// The type of errors during scope resolution.
#[derive(PartialEq, Eq, Debug)]
pub enum ScopeError<NS> {
  /// The same name was declared twice in the same namespace and
  /// scope.
  DuplicateName(NS, String, SourceOffset),
  NameConflictWithMainClass(NS, String, SourceOffset),
}

impl From<ScopeError<Namespace>> for ScopeError<ClassNamespace> {
  fn from(error: ScopeError<Namespace>) -> ScopeError<ClassNamespace> {
    match error {
      ScopeError::DuplicateName(n, s, p) => ScopeError::DuplicateName(n.into(), s, p),
      ScopeError::NameConflictWithMainClass(n, s, p) => ScopeError::NameConflictWithMainClass(n.into(), s, p),
    }
  }
}
