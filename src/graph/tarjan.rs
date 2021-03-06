
// Implementation of Tarjan's SCC Algorithm
//
// https://en.wikipedia.org/wiki/Tarjan%E2%80%99s_strongly_connected_components_algorithm

use super::*;

use std::collections::HashMap;
use std::collections::HashSet;
use std::cmp::min;
use std::mem::swap;

#[derive(Clone, Debug)]
pub struct SCC<'a, T>(pub HashSet<&'a T>);

#[derive(Clone, Debug)]
pub struct SCCSummary<'a, T> {
  sccs: Vec<SCC<'a, T>>,
  scc_lookup: HashMap<&'a T, usize>,
}

struct Algorithm<'a, T> {
  graph: &'a Graph<T>,
  sccs: Vec<SCC<'a, T>>,
  indices: HashMap<&'a T, usize>,
  lowlinks: HashMap<&'a T, usize>,
  index: usize,
  stack: Vec<&'a T>,
  current_scc: SCC<'a, T>,
}

impl<'a, T> PartialEq for SCC<'a, T> where T : Eq + Hash {
  fn eq(&self, other: &Self) -> bool {
    self.0 == other.0
  }
}

impl<'a, T> Default for SCC<'a, T> {
  fn default() -> Self {
    SCC(HashSet::new())
  }
}

impl<'a, T> SCCSummary<'a, T>
where T : Eq + Hash {

  pub fn get_scc_by_id(&self, id: usize) -> Option<&SCC<'a, T>> {
    self.sccs.get(id)
  }

  pub fn get_scc_id(&self, node: &'a T) -> Option<usize> {
    self.scc_lookup.get(node).copied()
  }

  pub fn get_scc(&self, node: &'a T) -> Option<&SCC<'a, T>> {
    self.scc_lookup.get(node).map(|idx| &self.sccs[*idx])
  }

  pub fn is_in_same(&self, a: &'a T, b: &'a T) -> bool {
    match self.get_scc(a) {
      None => false,
      Some(SCC(nodes)) => nodes.contains(b),
    }
  }

  pub fn count(&self) -> usize {
    self.sccs.len()
  }

}

impl<'a, T> Algorithm<'a, T>
where T : Eq + Hash {

  fn new(graph: &'a Graph<T>) -> Algorithm<'a, T> {
    Algorithm {
      graph: graph,
      sccs: Vec::new(),
      indices: HashMap::new(),
      lowlinks: HashMap::new(),
      index: 0,
      stack: Vec::new(),
      current_scc: SCC::default(),
    }
  }

  fn strongconnect(&mut self, v: &'a T) {
    self.indices.insert(v, self.index);
    self.lowlinks.insert(v, self.index);
    self.index += 1;
    self.stack.push(v);
    for w in self.graph.outgoing_edges(v).expect("Node not found") {
      if !self.indices.contains_key(w) {
        self.strongconnect(w);
        let a = *self.lowlinks.get(w).expect("No lowlink for w");
        let b = *self.lowlinks.get(v).expect("No lowlink for v");
        self.lowlinks.insert(v, min(a, b));
      } else if self.stack.iter().any(|x| x == &w) {
        let a = *self.indices.get(w).expect("No index for w");
        let b = *self.lowlinks.get(v).expect("No lowlink for v");
        self.lowlinks.insert(v, min(a, b));
      }
    }
    if self.indices.get(v) == self.lowlinks.get(v) {
      while {
        let w = self.stack.pop().expect("Ran out of stack in strongconnect");
        self.current_scc.0.insert(w);
        w != v
      } {}
      let mut curr = SCC::default();
      swap(&mut curr, &mut self.current_scc);
      self.sccs.push(curr);
    }
  }

}

impl<'a, T> From<Algorithm<'a, T>> for SCCSummary<'a, T>
where T : Eq + Hash {

  fn from(alg: Algorithm<'a, T>) -> SCCSummary<'a, T> {
    let sccs = alg.sccs;
    let mut scc_lookup = HashMap::new();
    for (idx, SCC(nodes)) in sccs.iter().enumerate() {
      for node in nodes {
        scc_lookup.insert(*node, idx);
      }
    }
    SCCSummary { sccs, scc_lookup }
  }

}

pub fn find_scc<T>(graph: &Graph<T>) -> SCCSummary<'_, T>
where T : Eq + Hash {
  let mut alg = Algorithm::new(graph);
  for v in graph.nodes() {
    if !alg.indices.contains_key(v) {
      alg.strongconnect(v);
    }
  }
  alg.into()
}

pub fn build_scc_graph<'a, 'b, T>(graph: &'a Graph<T>, summary: &'b SCCSummary<'a, T>) -> Graph<usize>
where T : Eq + Hash {
  let mut new_graph = Graph::from_nodes(0..summary.count());
  for (x, y) in graph.all_edges() {
    let xscc = summary.get_scc_id(x).expect("Node not found in SCCSummary");
    let yscc = summary.get_scc_id(y).expect("Node not found in SCCSummary");
    if xscc != yscc {
      new_graph.add_edge_no_dup(xscc, yscc);
    }
  }
  new_graph
}

#[cfg(test)]
mod tests {
  use super::*;

  #[test]
  fn scc_test_isolated() {
    let graph = Graph::from_nodes(vec!(1, 2, 4, 3).into_iter());
    let result = find_scc(&graph);
    assert_eq!(result.count(), 4);
    assert!(!result.is_in_same(&1, &2));
    assert!(!result.is_in_same(&1, &3));
    assert!(!result.is_in_same(&1, &4));
    assert!(!result.is_in_same(&2, &3));
    assert!(!result.is_in_same(&2, &4));
  }

  #[test]
  fn scc_test_path() {
    let graph = Graph::from_edges(vec!((1, 2), (2, 3), (3, 4)).into_iter());
    let result = find_scc(&graph);
    assert_eq!(result.count(), 4);
    assert!(!result.is_in_same(&1, &2));
    assert!(!result.is_in_same(&1, &3));
    assert!(!result.is_in_same(&1, &4));
    assert!(!result.is_in_same(&2, &3));
    assert!(!result.is_in_same(&2, &4));
  }

  #[test]
  fn scc_test_cycle_rhs() {
    let graph = Graph::from_edges(vec!((1, 2), (2, 3), (3, 4), (4, 2)).into_iter());
    let result = find_scc(&graph);
    assert_eq!(result.count(), 2);
    assert!(!result.is_in_same(&1, &2));
    assert!(!result.is_in_same(&1, &3));
    assert!(!result.is_in_same(&1, &4));
    assert!(result.is_in_same(&2, &3));
    assert!(result.is_in_same(&2, &4));
  }

  #[test]
  fn scc_test_cycle_lhs() {
    let graph = Graph::from_edges(vec!((1, 2), (2, 3), (3, 4), (3, 1)).into_iter());
    let result = find_scc(&graph);
    assert_eq!(result.count(), 2);
    assert!(result.is_in_same(&1, &2));
    assert!(result.is_in_same(&1, &3));
    assert!(!result.is_in_same(&1, &4));
    assert!(result.is_in_same(&2, &3));
    assert!(!result.is_in_same(&2, &4));
  }

  #[test]
  fn scc_test_one_big_cycle() {
    let graph = Graph::from_edges(vec!((1, 2), (2, 3), (3, 4), (4, 1)).into_iter());
    let result = find_scc(&graph);
    assert_eq!(result.count(), 1);
  }

  #[test]
  fn scc_test_two_connected_cycles() {
    let graph = Graph::from_edges(vec!((1, 2), (2, 3), (3, 4), (4, 5), (3, 1), (5, 3)).into_iter());
    let result = find_scc(&graph);
    assert_eq!(result.count(), 1);
  }

}
