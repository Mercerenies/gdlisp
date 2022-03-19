
//! Functions for determining scope at a high-level.
//!
//! These functions deal with high-level concepts such as classes and
//! declarations and are specifically *not* concerned with lower-level
//! concepts like `let` blocks and local variables.

use super::name_table::NameTable;
use super::error::ScopeError;
use crate::ir::identifier::ClassNamespace;

use std::hash::Hash;

pub trait DeclScope<NS: Hash + Eq + Clone> {

  fn get_scope_names(&self) -> Result<NameTable<NS>, ScopeError<NS>>;

}
