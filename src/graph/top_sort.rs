// Copyright 2023 Silvio Mayolo
//
// This file is part of GDLisp.
//
// GDLisp is free software: you can redistribute it and/or modify it
// under the terms of the GNU General Public License as published by
// the Free Software Foundation, either version 3 of the License, or
// (at your option) any later version.
//
// GDLisp is distributed in the hope that it will be useful, but
// WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
// General Public License for more details.
//
// You should have received a copy of the GNU General Public License
// along with GDLisp. If not, see <https://www.gnu.org/licenses/>.

//! Topological sorting of a graph.

use super::*;

use std::collections::HashSet;

/// Error type produced if [`top_sort()`] finds a cycle.
#[derive(Clone, Copy, Eq, PartialEq, Debug)]
pub struct CycleInTopSortError {}

/// Any directed acyclic graph can be topologically sorted. A valid
/// topological sorting of an acyclic graph is a total order of the
/// nodes in that graph such that, for any edge from `u` to `v` in the
/// graph, `u <= v` in the total order.
///
/// This function finds some valid topological sorting of the nodes in
/// the graph. If there is a cycle in the graph, then an error is
/// returned.
pub fn top_sort<T>(graph: &Graph<T>) -> Result<Vec<&T>, CycleInTopSortError>
where T : Eq + Hash {
  let nodes: HashSet<_> = graph.nodes().collect();
  let mut visited: HashSet<&T> = HashSet::new();
  let mut result = Vec::new();
  while visited.len() < nodes.len() {
    match nodes.iter().find(|node| {
      // To be a valid candidate, every outgoing node must be in visited.
      !visited.contains(*node) &&
        graph.outgoing_edges(**node).expect("Node not found").iter().all(|out| {
          visited.contains(out)
        })
    }) {
      None => { return Err(CycleInTopSortError {}) }
      Some(node) => {
        result.push(*node);
        visited.insert(node);
      }
    }
  }
  result.reverse();
  Ok(result)
}

#[cfg(test)]
mod tests {
  use super::*;

  #[test]
  fn test_top_sort_1() {
    // There's only one topological sort possible.
    let graph = Graph::from_edges(vec!((0, 1), (1, 2), (2, 3)).into_iter());
    let result = top_sort(&graph);
    assert_eq!(result, Ok(vec!(&0, &1, &2, &3)));
  }

  #[test]
  fn test_top_sort_2() {
    // There's only one topological sort possible.
    let graph = Graph::from_edges(vec!((0, 1), (1, 2), (0, 2)).into_iter());
    let result = top_sort(&graph);
    assert_eq!(result, Ok(vec!(&0, &1, &2)));
  }

  #[test]
  fn test_top_sort_3() {
    // Two options; as long as it's one of them, we're happy.
    let graph = Graph::from_edges(vec!((0, 1), (1, 2), (0, 3), (3, 2)).into_iter());
    let result = top_sort(&graph).expect("No topological sort found");
    assert_eq!(result[0], &0);
    assert_eq!(result[3], &2);
  }

  #[test]
  fn test_top_sort_4() {
    // No topological sort.
    let graph = Graph::from_edges(vec!((0, 1), (1, 2), (2, 3), (3, 1)).into_iter());
    let result = top_sort(&graph);
    assert_eq!(result, Err(CycleInTopSortError {}));
  }

}
