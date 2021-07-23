
//! Helper for storing collections of local variables that need to be
//! closed over.
//!
//! This module defines the [`Locals`] type, which maintains a
//! collection of local variables for the purposes of creating
//! closures. The equivalent module for local functions is
//! [`super::functions`].

use crate::util;

use super::access_type::AccessType;

use std::collections::HashMap;

/// A collection of local variables, as well as the broadest
/// [`AccessType`] the variables need.
#[derive(PartialEq, Eq, Debug, Clone, Default)]
pub struct Locals(HashMap<String, AccessType>);

impl Locals {

  /// Constructs a new, empty `Locals` object.
  pub fn new() -> Locals {
    Locals::default()
  }

  /// Constructs a `Locals` object from a hashmap mapping variable
  /// names to the broadest access type required.
  pub fn from_hashmap(map: HashMap<String, AccessType>) -> Locals {
    Locals(map)
  }

  /// Gets the access type associated to the variable. If the variable
  /// does not exist in `self`, returns [`AccessType::None`] as a
  /// default.
  pub fn get(&self, name: &str) -> AccessType {
    *self.0.get(name).unwrap_or(&AccessType::None)
  }

  /// Marks the given variable as accessed in the given way. If the
  /// variable was not known to `self` before, it is added to the
  /// table. If the variable was known, then the previous access type
  /// and `access_type` are joined (via [`AccessType::max`]) to
  /// produce the new access type for the variable.
  pub fn visited(&mut self, name: &str, access_type: AccessType) {
    self.0.insert(name.to_owned(), AccessType::max(self.get(name), access_type));
  }

  /// Removes the variable from the table, effectively resetting its
  /// access to [`AccessType::None`] and making the table not aware of
  /// that variable anymore.
  pub fn remove(&mut self, name: &str) {
    self.0.remove(name);
  }

  /// An iterator over all variable names known to the `Locals`
  /// instance, including variables which have been explicitly added
  /// but still have access type [`AccessType::None`].
  pub fn names(&self) -> impl Iterator<Item=&String> {
    self.0.keys()
  }

  /// As [`Locals::names`] but takes ownership over the values in
  /// `self.`
  pub fn into_names(self) -> impl Iterator<Item=String> {
    self.0.into_iter().map(|x| x.0)
  }

  /// Returns whether `self` is devoid of *any* variables.
  pub fn is_empty(&self) -> bool {
    self.0.is_empty()
  }

  /// Adds all of the variables from `b` into `self`. Any variables
  /// which did not exist in `self` are added, and those which did
  /// have the two access types joined via [`AccessType::max`].
  pub fn merge_with(&mut self, b: Locals) {
    util::merge_hashmap_inplace(&mut self.0, b.0, AccessType::max);
  }

  /// Filters the `Locals` instance in-place, retaining only those
  /// keys for which `f` returns true.
  pub fn retain(&mut self, mut f: impl FnMut(&str, AccessType) -> bool) {
    self.0.retain(|k, v| f(k, *v));
  }

  /// Closes over all variables in the `self`, using
  /// [`AccessType::closed`]. This method is appropriate for taking a
  /// `Locals` instance which refers to an inner lambda scope and
  /// converting it to one which describes the outer containing scope.
  pub fn closed(mut self) -> Self {
    for (_, v) in self.0.iter_mut() {
      *v = v.closed();
    }
    self
  }

}
