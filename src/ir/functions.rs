
//! Helper for storing collections of (possibly local) functions that
//! need to be closed over.
//!
//! This only includes the function namespace, which includes globally
//! defined functions and those defined locally with `flet`, etc., but
//! it explicitly excludes lambdas assigned to regular variables,
//! which are included in the [`super::locals`] structure instead.

use std::collections::HashSet;

/// A collection of functions, either local or global.
///
/// Unlike [`Locals`](super::locals::Locals), `Functions` does not
/// need to keep track of access types, since it is impossible to
/// reassign a function in the function namespace after its been
/// declared. It's possible to shadow functions with local ones, but
/// this doesn't mutate the existing one and a closure around the
/// existing function will still reflect the old value of the name.
#[derive(PartialEq, Eq, Debug, Clone, Default)]
pub struct Functions(HashSet<String>);

impl Functions {

  /// Constructs a new, empty `Functions` instance.
  pub fn new() -> Functions {
    Functions::default()
  }

  /// Constructs a `Functions` instance from a hashset of function
  /// names.
  pub fn from_hashset(map: HashSet<String>) -> Functions {
    Functions(map)
  }

  /// Whether or not the mapping contains the given function.
  pub fn contains(&self, name: &str) -> bool {
    self.0.contains(name)
  }

  /// Adds the given function to `self`. No action is taken if the
  /// function is already present.
  pub fn visited(&mut self, name: &str) {
    self.0.insert(name.to_owned());
  }

  /// Removes the given function from `self`.
  pub fn remove(&mut self, name: &str) {
    self.0.remove(name);
  }

  /// Iterates over all function names known to the `Functions`
  /// instance.
  pub fn names(&self) -> impl Iterator<Item=&String> {
    self.0.iter()
  }

  /// As [`Functions::names`] but takes ownership over the names.
  pub fn into_names(self) -> impl Iterator<Item=String> {
    self.0.into_iter()
  }

  /// Returns whether `self` is devoid of any functions.
  pub fn is_empty(&self) -> bool {
    self.0.is_empty()
  }

  /// Adds all of the variables from `b` into `self`.
  pub fn merge_with(&mut self, b: Functions) {
    self.0.extend(b.into_names());
  }

  /// Filters the `Functions` instance in-place, retaining only those
  /// keys for which `f` returns true.
  pub fn retain(&mut self, mut f: impl FnMut(&str) -> bool) {
    self.0.retain(|k| f(k));
  }

}
