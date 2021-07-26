
//! Helper for storing collections of values to be closed over.
//!
//! This module defines the [`ClosureNames`] type, which maintains a
//! collection of names and some reference to how they're used. If a
//! name is "used" multiple times (i.e. [`ClosureNames::visit`] is
//! called multiple times with the same name), then the usage
//! references will be combined via [`Lattice::max`].

use crate::util;
use crate::util::lattice::Lattice;

use std::collections::HashMap;

/// A collection of names and usage information. The usage information
/// shall be an instance of [`Lattice`]. The `Lattice` instance is
/// used for combining duplicate keys in the table.
#[derive(PartialEq, Eq, Debug, Clone)]
pub struct ClosureNames<T : Lattice>(HashMap<String, T>);

impl<T : Lattice> ClosureNames<T> {

  /// Constructs a new, empty `ClosureNames`.
  pub fn new() -> Self {
    ClosureNames::default()
  }

  /// Constructs a `ClosureNames` from a hashmap mapping names to
  /// usage information.
  pub fn from_hashmap(map: HashMap<String, T>) -> Self {
    ClosureNames(map)
  }

  /// Gets the usage information associated to the name, or [`None`]
  /// if the name is unknown to `self`.
  pub fn get(&self, name: &str) -> Option<&T> {
    self.0.get(name)
  }

  /// Marks the given name as being used in the given way. If the name
  /// was not known to `self` before, it is added to the table. If the
  /// variable was known, then the previous usage information and
  /// `usage` are joined via [`Lattice::join`] to produce the new
  /// usage.
  pub fn visit(&mut self, name: String, usage: T) {
    match self.0.remove(&name) {
      None => self.0.insert(name, usage),
      Some(prev_usage) => self.0.insert(name, prev_usage.join(usage)),
    };
  }

  /// Removes the name from the table, returning its usage type if one
  /// existed.
  pub fn remove(&mut self, name: &str) -> Option<T> {
    self.0.remove(name)
  }

  /// An iterator over all names known to the `ClosureNames` instance.
  pub fn names(&self) -> impl Iterator<Item=&str> {
    self.0.keys().map(|x| &x[..])
  }

  /// As [`ClosureNames::names`] but takes ownership over the values
  /// in `self`.
  pub fn into_names(self) -> impl Iterator<Item=String> {
    self.0.into_iter().map(|x| x.0)
  }

  /// Returns whether `self` is devoid of any names.
  pub fn is_empty(&self) -> bool {
    self.0.is_empty()
  }

  /// Adds all of the names from `b` into `self`. Any names which did
  /// not exist in `self` are added, and those which did will have
  /// their two usage values joined via [`Lattice::join`].
  pub fn merge_with(&mut self, b: ClosureNames<T>) {
    util::merge_hashmap_inplace(&mut self.0, b.0, Lattice::join)
  }

  /// Filters the `ClosureNames` instance in-place, retaining only
  /// those values for which `f` returns true.
  pub fn retain(&mut self, mut f: impl FnMut(&str, &mut T) -> bool) {
    self.0.retain(|x, y| f(x, y))
  }

  /// Iterates over the entries in the table.
  pub fn iter(&self) -> impl Iterator<Item=(&str, &T)> {
    self.0.iter().map(|(x, y)| (&x[..], y))
  }

  /// Iterates over the entries in the table, allowing mutation on the
  /// usage information.
  pub fn iter_mut(&mut self) -> impl Iterator<Item=(&str, &mut T)> {
    self.0.iter_mut().map(|(x, y)| (&x[..], y))
  }

}

impl<T : Lattice> Default for ClosureNames<T> {
  fn default() -> Self {
    ClosureNames(HashMap::default())
  }
}
