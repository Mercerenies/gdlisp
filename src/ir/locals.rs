
//! Helper for storing collections of local variables that need to be
//! closed over.
//!
//! This module defines the [`Locals`] type, which maintains a
//! collection of local variables for the purposes of creating
//! closures. The equivalent module for local functions is
//! [`super::functions`].

use super::closure_names::ClosureNames;
use super::access_type::AccessType;

/// A collection of local variables, as well as the broadest
/// [`AccessType`] the variables need.
pub type Locals = ClosureNames<AccessType>;
