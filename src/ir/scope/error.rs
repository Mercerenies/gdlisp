
//! Provides the [`ScopeError`] type, which marks error during scope
//! resolution.

use crate::pipeline::source::SourceOffset;
use crate::ir::identifier::{Namespace, ClassNamespace};

// TODO Encode the offset of both duplicate names in this error type.

/// The type of errors during scope resolution.
#[derive(PartialEq, Eq, Debug)]
pub enum ScopeError<NS> {
  /// The same name was declared twice in the same namespace and
  /// scope.
  DuplicateName(NS, String, SourceOffset),
}

impl From<ScopeError<Namespace>> for ScopeError<ClassNamespace> {
  fn from(error: ScopeError<Namespace>) -> ScopeError<ClassNamespace> {
    match error {
      ScopeError::DuplicateName(n, s, p) => ScopeError::DuplicateName(n.into(), s, p),
    }
  }
}
