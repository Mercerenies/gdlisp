
use super::symbol_table::SymbolTable;

use std::collections::HashSet;

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct Dependencies {
  pub known: HashSet<String>,
  pub unknown: HashSet<String>,
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

}
