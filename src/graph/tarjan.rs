
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

struct Algorithm<'a, 'b, T> {
  graph: &'b Graph<'a, T>,
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

impl<'a, 'b, T> Algorithm<'a, 'b, T>
where T : Eq + Hash {

  fn new(graph: &'b Graph<'a, T>) -> Algorithm<'a, 'b, T> {
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
      } else if self.stack.iter().find(|x| *x == w).is_some() {
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

pub fn find_scc<'a, 'b, T>(graph: &'b Graph<'a, T>) -> Vec<SCC<'a, T>>
where T : Eq + Hash, 'b : 'a {
  let mut alg = Algorithm::new(graph);
  for v in graph.nodes() {
    if !alg.indices.contains_key(v) {
      alg.strongconnect(v);
    }
  }
  alg.sccs
}

#[cfg(test)]
mod tests {
  //use super::*;

}
