
//! Provides the [`ScopeError`] type, which marks error during scope
//! resolution.

use crate::pipeline::source::SourceOffset;

// TODO Encode the offset of both duplicate names in this error type.

/// The type of errors during scope resolution.
#[derive(PartialEq, Eq, Debug)]
pub enum ScopeError<NS> {
  DuplicateName(NS, String, SourceOffset),
}
