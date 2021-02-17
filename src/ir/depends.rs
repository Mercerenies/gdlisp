
use super::symbol_table::SymbolTable;
use crate::compile::error::Error;

use std::collections::HashSet;
use std::borrow::Borrow;

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct Dependencies {
  pub known: HashSet<String>,
  pub unknown: HashSet<String>,
}

#[derive(Debug)]
pub enum DependencyError {
  UnknownName(String),
}

impl Dependencies {

  pub fn identify(table: &SymbolTable, name: &str) -> Dependencies {
    let mut visited = HashSet::new();
    let mut unknown = HashSet::new();
    match table.get(name) {
      None => {
        // No traversal necessary.
        unknown.insert(name.to_owned());
      }
      Some(initial) => {
        let mut frontier = vec!(initial);
        while let Some(current) = frontier.pop() {
          if visited.contains(current.name()) {
            continue;
          }
          let deps = current.dependencies();
          for dep in deps {
            match table.get(&dep) {
            None => {
              unknown.insert(dep);
            }
              Some(next) => {
                frontier.push(next);
              }
            }
          }
          visited.insert(current.name().to_owned());
      }
      }
    }
    Dependencies {
      known: visited,
      unknown: unknown,
    }
  }

  pub fn purge_unknowns<T, I>(&mut self, purge: I)
  where T : Borrow<str>,
        I : Iterator<Item=T> {
    for s in purge {
      self.unknown.remove(s.borrow());
    }
  }

  pub fn try_into_knowns(self) -> Result<HashSet<String>, DependencyError> {
    if let Some(unknown) = self.unknown.into_iter().next() {
      Err(DependencyError::UnknownName(unknown))
    } else {
      Ok(self.known)
    }
  }

}

impl From<DependencyError> for Error {
  fn from(e: DependencyError) -> Error {
    match e {
      DependencyError::UnknownName(x) => Error::NoSuchFn(x)
    }
  }
}
