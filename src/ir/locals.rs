
// Helper for storing collections of local variables that need to be
// closed over.

use std::collections::HashMap;
use std::cmp::max;

#[derive(PartialEq, Eq, Debug, Clone)]
pub struct Locals(HashMap<String, AccessType>);

/*
 * This requires a bit of explanation. When we look at a variable, we
 * need to be able to tell what we're going to do with that variable.
 * The simplest answer is AccessType::None, which indicates the
 * variable is not used at all. AccessType::Read indicates that the
 * variable may be read but will not be assigned directly to.
 * AccessType::RW indicates that the variable may be read or written,
 * but it will not be written to while it's a closure variable.
 * Finally, AccessType::ClosedRW indicates that the variable may be
 * read / written even as a closure variable. The last distinction is
 * necessary so we know when to use Cell to wrap a variable.
 */
#[derive(PartialEq, Eq, PartialOrd, Ord, Debug, Clone, Copy)]
pub enum AccessType { None, Read, RW, ClosedRW }

impl Locals {

  pub fn new() -> Locals {
    Locals(HashMap::new())
  }

  pub fn from_hashmap(map: HashMap<String, AccessType>) -> Locals {
    Locals(map)
  }

  pub fn get(&self, name: &str) -> AccessType {
    *self.0.get(name).unwrap_or(&AccessType::None)
  }

  pub fn visited(&mut self, name: &str, access_type: AccessType) {
    self.0.insert(name.to_owned(), max(self.get(name), access_type));
  }

  pub fn remove(&mut self, name: &str) {
    self.0.remove(name);
  }

  pub fn names(&self) -> impl Iterator<Item=&String> {
    self.0.keys()
  }

  pub fn into_names(self) -> impl Iterator<Item=String> {
    self.0.into_iter().map(|x| x.0)
  }

}

impl AccessType {

  // Use on a closure variable; if the access type inside of a lambda
  // is RW, then the access type outside the lambda is ClosedRW.
  pub fn closed(&self) -> AccessType {
    if *self == AccessType::RW {
      AccessType::ClosedRW
    } else {
      *self
    }
  }

}
