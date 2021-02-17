
use super::symbol_table::SymbolTable;
use crate::compile::error::Error;

use std::collections::HashSet;

pub fn transitive_dependencies(table: &SymbolTable, name: &str) -> Result<HashSet<String>, Error> {
  let mut visited = HashSet::new();
  let initial = table.get(name).ok_or_else(|| Error::NoSuchFn(name.to_owned()))?;
  let mut frontier = vec!(initial);
  while let Some(current) = frontier.pop() {
    if visited.contains(current.name()) {
      continue;
    }
    let deps = current.dependencies();
    for dep in deps {
      let next = table.get(&dep).ok_or_else(|| Error::NoSuchFn(dep.to_owned()))?;
      frontier.push(next);
    }
    visited.insert(current.name().to_owned());
  }
  Ok(visited)
}
