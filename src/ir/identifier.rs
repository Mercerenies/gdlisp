
use super::symbol_table::Namespace;

use std::borrow::Borrow;
use std::hash::{Hash, Hasher};

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct Id {
  pub namespace: Namespace,
  pub name: String,
}

// This trait is a specialized version of KeyPair from this
// StackOverflow answer. We specialize it to work on &str.
//
// https://stackoverflow.com/a/45795699/2288659
pub trait IdLike {
  fn namespace(&self) -> Namespace;
  fn name(&self) -> &str;
}

impl Id {

  pub fn new(namespace: Namespace, name: String) -> Self {
    Id { namespace, name }
  }

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
