
use super::declaration_table::DeclarationTable;
use super::identifier::{IdLike, Id, Namespace};
use crate::pipeline::source::SourceOffset;

use std::collections::{HashSet, HashMap};

// TODO Make these fields private
#[derive(Clone, Debug, Eq, PartialEq)]
pub struct Dependencies {
  pub known: HashMap<Id, SourceOffset>,
  pub imports: HashMap<Id, SourceOffset>,
  pub unknown: HashMap<Id, SourceOffset>,
}

#[derive(Debug)]
pub enum DependencyError {
  UnknownName(Id, SourceOffset),
}

impl Dependencies {

  pub fn identify<'a>(table: &DeclarationTable,
                      known_imports: &HashSet<Id>,
                      id: &(dyn IdLike<NS=Namespace> + 'a),
                      pos: SourceOffset)
                      -> Dependencies {
    let mut visited = HashMap::new();
    let mut imports = HashMap::new();
    let mut unknown = HashMap::new();
    match table.get(id) {
      None => {
        // No traversal necessary.
        unknown.insert(id.to_owned(), pos);
      }
      Some(initial) => {
        let mut frontier = vec!((initial, pos));
        while let Some((current, pos)) = frontier.pop() {
          if visited.contains_key(&*current.id_like()) {
            visited.entry(current.to_id()).and_modify(|old_pos: &mut SourceOffset| {
              *old_pos = (*old_pos).min(pos);
            });
            continue;
          }
          let deps = current.dependencies();
          for (dep, pos) in deps {
            if let Some(next) = table.get(&dep) {
              frontier.push((next, pos));
            } else if known_imports.contains(&dep) {
              imports.insert(dep, pos);
            } else {
              unknown.insert(dep, pos);
            }
          }
          visited.insert(current.to_id(), pos);
      }
      }
    }
    let mut known = visited;
    for imported_name in imports.keys() {
      known.remove(imported_name);
    }
    Dependencies { known, imports, unknown }
  }

  // TODO Rather than purge after the fact, we can use the imports
  // argument above to account for built-ins.
  pub fn purge_unknowns<'a, 'b, I>(&mut self, purge: I)
  where I : Iterator<Item=&'a (dyn IdLike<NS=Namespace> + 'b)>,
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
    if let Some((unknown, pos)) = self.unknown.into_iter().next() {
      Err(DependencyError::UnknownName(unknown, pos))
    } else {
      Ok(self.known.into_iter().map(|(k, _)| k).collect())
    }
  }

}
