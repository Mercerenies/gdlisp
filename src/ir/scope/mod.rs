
//! Functions for identifying and detailing which names are in scope
//! in the IR.

pub mod decl;
pub mod name_table;
pub mod error;

use crate::ir::decl::TopLevel;
use crate::compile::error::GDError;

/// Convenience method to perform all scope checking on an IR file
/// representation.
///
/// Currently, this simply delegates to
/// [`decl::check_all_decl_scopes`] and broadens the error type, but
/// it will be expanded in the future to perform more detailed checks.
pub fn check_scopes(toplevel: &TopLevel) -> Result<(),  GDError> {
  decl::check_all_decl_scopes(toplevel).map_err(GDError::from)
}
