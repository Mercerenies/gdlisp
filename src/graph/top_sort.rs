
use super::*;

use std::collections::HashSet;

#[derive(Clone, Copy, Eq, PartialEq, Debug)]
pub struct CycleInTopSortError {}

pub fn top_sort<'a, 'b, T>(graph: &'b Graph<'a, T>) -> Result<Vec<&'a T>, CycleInTopSortError>
where T : Eq + Hash,
      'b : 'a {
  let nodes: HashSet<_> = graph.nodes().collect();
  let mut visited: HashSet<_> = HashSet::new();
  let mut result = Vec::new();
  while visited.len() < nodes.len() {
    match nodes.iter().find(|node| {
      // To be a valid candidate, every outgoing node must be in visited.
      !visited.contains(node) &&
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
    let graph = Graph::from_edges(vec!((0, &1), (1, &2), (2, &3)).into_iter());
    let result = top_sort(&graph);
    assert_eq!(result, Ok(vec!(&0, &1, &2, &3)));
  }

  #[test]
  fn test_top_sort_2() {
    // There's only one topological sort possible.
    let graph = Graph::from_edges(vec!((0, &1), (1, &2), (0, &2)).into_iter());
    let result = top_sort(&graph);
    assert_eq!(result, Ok(vec!(&0, &1, &2)));
  }

  #[test]
  fn test_top_sort_3() {
    // Two options; as long as it's one of them, we're happy.
    let graph = Graph::from_edges(vec!((0, &1), (1, &2), (0, &3), (3, &2)).into_iter());
    let result = top_sort(&graph).expect("No topological sort found");
    assert_eq!(result[0], &0);
    assert_eq!(result[3], &2);
  }

  #[test]
  fn test_top_sort_4() {
    // No topological sort.
    let graph = Graph::from_edges(vec!((0, &1), (1, &2), (2, &3), (3, &1)).into_iter());
    let result = top_sort(&graph);
    assert_eq!(result, Err(CycleInTopSortError {}));
  }

}
