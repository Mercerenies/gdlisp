
//! Helper for storing collections of values to be closed over.
//!
//! This module defines the [`ClosureNames`] type, which maintains a
//! collection of names and some reference to how they're used. If a
//! name is "used" multiple times (i.e. [`ClosureNames::visit`] is
//! called multiple times with the same name), then the usage
//! references will be combined via [`Lattice::join`].

use crate::util::lattice::Lattice;
use crate::pipeline::source::SourceOffset;

use std::collections::HashMap;
use std::iter::IntoIterator;

/// A collection of names and usage information. The usage information
/// shall be an instance of [`Lattice`]. The `Lattice` instance is
/// used for combining duplicate keys in the table.
#[derive(PartialEq, Eq, Debug, Clone)]
pub struct ClosureNames<T : Lattice>(HashMap<String, (T, SourceOffset)>);

impl<T : Lattice> ClosureNames<T> {

  /// Constructs a new, empty `ClosureNames`.
  pub fn new() -> Self {
    ClosureNames::default()
  }

  /// Constructs a `ClosureNames` from a hashmap mapping names to
  /// usage information.
  pub fn from_hashmap(map: HashMap<String, (T, SourceOffset)>) -> Self {
    // TODO Do we ever use this function? Its signature is kinda... unfortunately complicated.
    ClosureNames(map)
  }

  /// Gets the usage information associated to the name, or [`None`]
  /// if the name is unknown to `self`.
  pub fn get(&self, name: &str) -> Option<&T> {
    self.0.get(name).map(|x| &x.0)
  }

  /// Whether or not the `ClosureNames` contains the given name.
  /// Equivalent to `self.get(name).is_some()`.
  pub fn contains(&self, name: &str) -> bool {
    self.get(name).is_some()
  }

  /// Marks the given name as being used in the given way. If the name
  /// was not known to `self` before, it is added to the table. If the
  /// variable was known, then the previous usage information and
  /// `usage` are joined via [`Lattice::join`] to produce the new
  /// usage.
  ///
  /// In case of a conflict, the minimum of the two source positions
  /// is chosen, since errors reported earlier in the file are
  /// generally more intuitive than those reported later, all other
  /// things being equal.
  pub fn visit(&mut self, name: String, usage: T, pos: SourceOffset) {
    match self.0.remove(&name) {
      None => self.0.insert(name, (usage, pos)),
      Some((prev_usage, prev_pos)) => self.0.insert(name, (prev_usage.join(usage), prev_pos.min(pos))),
    };
  }

  /// Removes the name from the table, returning its usage type if one
  /// existed.
  pub fn remove(&mut self, name: &str) -> Option<T> {
    self.0.remove(name).map(|x| x.0)
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
    for (name, data, pos) in b.into_iter_with_offset() {
      self.visit(name, data, pos);
    }
  }

  /// Filters the `ClosureNames` instance in-place, retaining only
  /// those values for which `f` returns true.
  pub fn retain(&mut self, mut f: impl FnMut(&str, &mut T) -> bool) {
    self.0.retain(|x, (y, _)| f(x, y))
  }

  // TODO "with offset" versions of iter(), iter_mut(), and a "non-with-offset" version of into_iter().

  /// Iterates over the entries in the table.
  pub fn iter(&self) -> impl Iterator<Item=(&str, &T)> {
    self.0.iter().map(|(x, (y, _))| (&x[..], y))
  }

  /// Iterates over the entries in the table, allowing mutation on the
  /// usage information.
  pub fn iter_mut(&mut self) -> impl Iterator<Item=(&str, &mut T)> {
    self.0.iter_mut().map(|(x, (y, _))| (&x[..], y))
  }

  /// Iterates over the entries in the table, taking ownership during iteration.
  pub fn into_iter_with_offset(self) -> impl Iterator<Item=(String, T, SourceOffset)> {
    self.0.into_iter().map(|(s, (t, p))| (s, t, p))
  }

}

impl<T : Lattice> Default for ClosureNames<T> {
  fn default() -> Self {
    ClosureNames(HashMap::default())
  }
}
