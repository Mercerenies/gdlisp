
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

/// While [`Namespace`] suffices for most of GDLisp, there is
/// technically a third scope that sometimes applies, that of signals.
/// Signals cannot be referenced by name at runtime, as they don't
/// actually exist in GDScript in that form. But for the purposes of
/// name resolution, there are occasions where we need to check them
/// for correctness.
#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub enum ClassNamespace {
  /// See [`Namespace::Value`].
  Value,
  /// See [`Namespace::Function`].
  Function,
  /// A signal declaration, i.e. the result of a `defsignal` declaration.
  Signal,
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

  /// The underlying namespace type.
  type NS;

  /// Gets the namespace for `self`. [`Namespace`] is a [`Copy`] type,
  /// so we return by value here, since returning a `&Namespace` is
  /// pointless and verbose.
  fn namespace(&self) -> Self::NS;

  /// Gets the name for `self`, by reference.
  fn name(&self) -> &str;

}

impl From<Namespace> for ClassNamespace {

  fn from(ns: Namespace) -> ClassNamespace {
    match ns {
      Namespace::Value => ClassNamespace::Value,
      Namespace::Function => ClassNamespace::Function,
    }
  }

}

impl Id {

  /// A new `Id` value with the given name and namespace.
  pub fn new(namespace: Namespace, name: String) -> Self {
    Id { namespace, name }
  }

  /// A new [`IdLike`] value with the given name and namespace. This
  /// function does not take ownership of `name`, nor does it clone
  /// it.
  pub fn build<'a, NS: Clone + 'a>(namespace: NS, name: &'a str) -> Box<dyn IdLike<NS=NS> + 'a> {
    Box::new((namespace.clone(), name))
  }

}

impl IdLike for Id {

  type NS = Namespace;

  fn namespace(&self) -> Namespace {
    self.namespace
  }

  fn name(&self) -> &str {
    &self.name
  }

}

impl<NS: Clone> IdLike for (NS, &str) {

  type NS = NS;

  fn namespace(&self) -> NS {
    self.0.clone()
  }

  fn name(&self) -> &str {
    self.1
  }

}

impl<'a> Borrow<dyn IdLike<NS=Namespace> + 'a> for Id {
  fn borrow(&self) -> &(dyn IdLike<NS=Namespace> + 'a) {
    self
  }
}

impl<'a, NS: Hash> Hash for (dyn IdLike<NS=NS> + 'a) {
  fn hash<H : Hasher>(&self, state: &mut H) {
    self.namespace().hash(state);
    self.name().hash(state);
  }
}

impl<'a, NS: PartialEq> PartialEq for (dyn IdLike<NS=NS> + 'a) {
  fn eq(&self, other: &Self) -> bool {
    self.namespace() == other.namespace() && self.name() == other.name()
  }
}

impl<'a, NS: PartialEq> Eq for (dyn IdLike<NS=NS> + 'a) {}

impl<'a> ToOwned for (dyn IdLike<NS=Namespace> + 'a) {
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
