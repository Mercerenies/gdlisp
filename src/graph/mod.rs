
pub mod top_sort;
pub mod tarjan;

use std::collections::HashMap;
use std::borrow::Borrow;
use std::hash::Hash;

#[derive(Debug, Clone)]
pub struct Graph<'a, T> {
  edges: HashMap<T, Vec<&'a T>>,
}

impl<'a, T> Graph<'a, T> where T : Eq + Hash {

  pub fn from_edges<I, U>(iter: I) -> Graph<'a, T>
  where I : Iterator<Item=(T, &'a U)>,
        U : Borrow<T>,
        U : 'a,
        T : Clone {
    let mut edges = HashMap::new();
    for (x, y) in iter {
      let vec = edges.entry(x).or_insert_with(Vec::new);
      vec.push(y.borrow());
      // Make sure the other node is present too.
      if !edges.contains_key(y.borrow()) {
        edges.insert((*y.borrow()).clone(), Vec::new());
      }
    }
    Graph { edges }
  }

  pub fn from_nodes<I>(iter: I) -> Graph<'a, T>
  where I : Iterator<Item=T> {
    Graph { edges: iter.map(|x| (x, Vec::new())).collect() }
  }

  pub fn new() -> Graph<'a, T> {
    Graph::default()
  }

  pub fn nodes(&self) -> impl Iterator<Item=&T> {
    self.edges.keys()
  }

  pub fn outgoing_edges<'b>(&'b self, node: impl Borrow<T>) -> Option<&'b Vec<&'a T>> {
    self.edges.get(node.borrow())
  }

  pub fn add_node(&mut self, node: T) {
    self.edges.entry(node).or_insert_with(Vec::new);
  }

  pub fn add_edge<'b, U, V>(&'b mut self, x: &'a U, y: &'a V) where U : Borrow<T>, V : Borrow<T> {
    match self.edges.get_mut(x.borrow()) {
      None => {}
      Some(vec) => { vec.push(y.borrow()) }
    }
  }

  pub fn remove_edge<U, V>(&mut self, x: &U, y: &V) where U : Borrow<T>, V : Borrow<T> {
    if let Some(vec) = self.edges.get_mut(x.borrow()) {
      if let Some(pos) = vec.iter().position(|node| *node == y.borrow()) {
        vec.swap_remove(pos);
      }
    }
  }

  pub fn node_count(&self) -> usize {
    self.edges.len()
  }

}

impl<'a, T> Default for Graph<'a, T> {

  fn default() -> Self {
    Graph { edges: Default::default() }
  }

}
