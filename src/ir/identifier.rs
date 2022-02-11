
//! This module defines the [`Id`] type, for namespaced identifiers.

use serde::{Serialize, Deserialize};

use std::borrow::{Borrow, ToOwned};
use std::hash::{Hash, Hasher};

/// An identifier consists of a namespace and a name. Two identifiers
/// which happen to share a name but are in different namespaces are
/// considered unrelated and distinct names.
#[derive(Clone, Debug, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub struct Id {
  /// The identifier namespace.
  pub namespace: Namespace,
  /// The identifier name.
  pub name: String,
}

/// There are two namespaces in GDLisp: the value namespace and the
/// function namespace.
#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub enum Namespace {
  /// The value namespace consists of variables (local and global),
  /// class declarations, enums, and constants.
  Value,
  /// The function namespace consists of functions (local and global)
  /// and macros.
  Function,
}

// This trait is a specialized version of KeyPair from this
// StackOverflow answer. We specialize it to work on &str.
//
// https://stackoverflow.com/a/45795699/2288659

/// [`Id`] is frequently used as the key type in some kind of
/// associative data structure. `Id`, however, requires ownership of
/// its string value, which is not always ideal for simple lookups.
///
/// Similar to how `String` keys in a hashmap can be queried with a
/// `&str`, this trait allows `Id` keys in a hashmap or similar
/// structure to be queried by any `IdLike` implementor, most namely a
/// tuple `(Namespace, &str)`. Any type which has a [`Namespace`] and
/// a `&str` name can implement this trait. The trait object type `dyn
/// IdLike` implements the necessary [`Borrow`] and [`ToOwned`] traits
/// to be used to reference `Id` keys in a hashmap.
pub trait IdLike {
  /// Gets the namespace for `self`. [`Namespace`] is a [`Copy`] type,
  /// so we return by value here, since returning a `&Namespace` is
  /// pointless and verbose.
  fn namespace(&self) -> Namespace;
  /// Gets the name for `self`, by reference.
  fn name(&self) -> &str;
}

impl Id {

  /// A new `Id` value with the given name and namespace.
  pub fn new(namespace: Namespace, name: String) -> Self {
    Id { namespace, name }
  }

  /// A new [`IdLike`] value with the given name and namespace. This
  /// function does not take ownership of `name`, nor does it clone
  /// it.
  pub fn build<'a>(namespace: Namespace, name: &'a str) -> Box<dyn IdLike + 'a> {
    Box::new((namespace, name))
  }

}

impl IdLike for Id {
  fn namespace(&self) -> Namespace {
    self.namespace
  }
  fn name(&self) -> &str {
    &self.name
  }
}

impl IdLike for (Namespace, &str) {
  fn namespace(&self) -> Namespace {
    self.0
  }
  fn name(&self) -> &str {
    self.1
  }
}

impl<'a> Borrow<dyn IdLike + 'a> for Id {
  fn borrow(&self) -> &(dyn IdLike + 'a) {
    self
  }
}

impl<'a> Hash for (dyn IdLike + 'a) {
  fn hash<H : Hasher>(&self, state: &mut H) {
    self.namespace().hash(state);
    self.name().hash(state);
  }
}

impl<'a> PartialEq for (dyn IdLike + 'a) {
  fn eq(&self, other: &Self) -> bool {
    self.namespace() == other.namespace() && self.name() == other.name()
  }
}

impl<'a> Eq for (dyn IdLike + 'a) {}

impl<'a> ToOwned for (dyn IdLike + 'a) {
  type Owned = Id;
  fn to_owned(&self) -> Id {
    Id::new(self.namespace(), self.name().to_owned())
  }
}

#[cfg(test)]
mod tests {
  use super::*;
  use std::collections::{HashMap, HashSet};

  #[test]
  fn test_id_in_hashmap() {
    let mut container: HashMap<Id, i32> = HashMap::new();
    container.insert(Id::new(Namespace::Function, String::from("foobar")), 945);
    assert_eq!(container.get(&*Id::build(Namespace::Function, "foobar")), Some(&945));
  }

  #[test]
  fn test_id_in_hashset() {
    let mut container: HashSet<Id> = HashSet::new();
    container.insert(Id::new(Namespace::Function, String::from("foobar")));
    assert!(container.contains(&*Id::build(Namespace::Function, "foobar")));
  }

}
