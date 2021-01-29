
// Helper for storing collections of local variables that need to be
// closed over.

use std::collections::HashMap;

#[derive(PartialEq, Eq, Debug, Clone)]
pub struct Locals(HashMap<String, AccessType>);

/*
 * AccessType describes the different ways we can access a variable.
 *
 * + None - The variable is never accessed.
 * + Read - The variable may be read directly.
 * + RW - The variable may be read and written to directly.
 * + ClosedRead - The variable may be read directly or from within a closure.
 * + ClosedRW - The variable may be read and written to directly or from within a closure.
 *
 * These possibilities form a lattice as follows.
 *
 *   ClosedRW
 *   /      \
 *  RW    ClosedRead
 *   \      /
 *     Read
 *      |
 *     None
 */
#[derive(PartialEq, Eq, Debug, Clone, Copy)]
pub enum AccessType { None, Read, RW, ClosedRead, ClosedRW }

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
    self.0.insert(name.to_owned(), AccessType::max(self.get(name), access_type));
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
  // Likewise, if it's Read, then on the outside it's ClosedRead.
  pub fn closed(&self) -> AccessType {
    match *self {
      AccessType::None => AccessType::None,
      AccessType::Read | AccessType::ClosedRead => AccessType::ClosedRead,
      AccessType::RW | AccessType::ClosedRW => AccessType::ClosedRW,
    }
  }

  pub fn requires_cell(&self) -> bool {
    *self == AccessType::ClosedRW
  }

  pub fn is_written_to(&self) -> bool {
    *self == AccessType::ClosedRW || *self == AccessType::RW
  }

  pub fn max(a: AccessType, b: AccessType) -> AccessType {
    if a == AccessType::None {
      return b;
    }
    if b == AccessType::None {
      return a;
    }
    if a == AccessType::Read {
      return b;
    }
    if b == AccessType::Read {
      return a;
    }
    if a == b {
      return a;
    }
    return AccessType::ClosedRW;
  }

}
