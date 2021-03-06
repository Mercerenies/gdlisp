
use super::symbol_table::SymbolTable;
use super::identifier::{IdLike, Id, Namespace};
use crate::compile::error::Error;

use std::collections::HashSet;

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct Dependencies {
  pub known: HashSet<Id>,
  pub imports: HashSet<Id>,
  pub unknown: HashSet<Id>,
}

#[derive(Debug)]
pub enum DependencyError {
  UnknownName(Id),
}

impl Dependencies {

  pub fn identify<'a>(table: &SymbolTable, known_imports: &HashSet<Id>, id: &(dyn IdLike + 'a))
                      -> Dependencies {
    let mut visited = HashSet::new();
    let mut imports = HashSet::new();
    let mut unknown = HashSet::new();
    match table.get(id) {
      None => {
        // No traversal necessary.
        unknown.insert(id.to_owned());
      }
      Some(initial) => {
        let mut frontier = vec!(initial);
        while let Some(current) = frontier.pop() {
          if visited.contains(&*current.id_like()) {
            continue;
          }
          let deps = current.dependencies();
          for dep in deps {
            if let Some(next) = table.get(&dep) {
              frontier.push(next);
            } else if known_imports.contains(&dep) {
              imports.insert(dep);
            } else {
              unknown.insert(dep);
            }
          }
          visited.insert(current.to_id());
      }
      }
    }
    let known = &visited - &imports;
    Dependencies { known, imports, unknown }
  }

  // TODO Rather than purge after the fact, we can use the imports
  // argument above to account for built-ins.
  pub fn purge_unknowns<'a, 'b, I>(&mut self, purge: I)
  where I : Iterator<Item=&'a (dyn IdLike + 'b)>,
        'b : 'a {
    for s in purge {
      self.unknown.remove(s);
    }
  }

  pub fn try_into_knowns(self) -> Result<HashSet<Id>, DependencyError> {
    // Note: We explicitly don't care about imports here. As long as
    // all names are either knowns or imports (and there are no
    // unknowns), then we can safely discard the imports and keep only
    // the knowns, as the imports are already loaded elsewhere.
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
      DependencyError::UnknownName(id) => {
        match id.namespace {
          Namespace::Function => Error::NoSuchFn(id.name),
          Namespace::Value => Error::NoSuchVar(id.name),
        }
      }
    }
  }
}
