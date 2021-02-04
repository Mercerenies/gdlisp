
pub mod top_sort;
pub mod tarjan;

use std::collections::HashMap;
use std::borrow::Borrow;
use std::hash::Hash;

// TODO Really, we should be storing the edge sets with some kind of
// numerical index, rather than assuming the type T can be safely
// cloned.
#[derive(Debug, Clone)]
pub struct Graph<T> {
  edges: HashMap<T, Vec<T>>,
}

impl<T> Graph<T> where T : Eq + Hash {

  pub fn from_edges<I>(iter: I) -> Graph<T>
  where T : Clone,
        I : Iterator<Item=(T, T)> {
    let mut edges = HashMap::new();
    for (x, y) in iter {
      // Make sure the other node is present too.
      if !edges.contains_key(&y) {
        edges.insert(y.clone(), Vec::new());
      }
      // Then put an entry for the edge
      let vec = edges.entry(x).or_insert_with(Vec::new);
      vec.push(y);
    }
    Graph { edges }
  }

  pub fn from_nodes<I>(iter: I) -> Graph<T>
  where I : Iterator<Item=T> {
    Graph { edges: iter.map(|x| (x, Vec::new())).collect() }
  }

  pub fn new() -> Graph<T> {
    Graph::default()
  }

  pub fn nodes(&self) -> impl Iterator<Item=&T> {
    self.edges.keys()
  }

  pub fn get_node<U>(&self, x: &U) -> Option<&T> where U : Borrow<T> {
    self.edges.get_key_value(x.borrow()).map(|(k, _)| k)
  }

  pub fn has_node<U>(&self, x: &U) -> bool where U : Borrow<T> {
    self.get_node(x).is_some()
  }

  pub fn all_edges(&self) -> impl Iterator<Item=(&T, &T)> {
    self.edges.iter().flat_map(|(x, ys)| ys.iter().map(move |y| (x, y)))
  }

  pub fn outgoing_edges(&self, node: impl Borrow<T>) -> Option<&Vec<T>> {
    self.edges.get(node.borrow())
  }

  pub fn add_node(&mut self, node: T) {
    self.edges.entry(node).or_insert_with(Vec::new);
  }

  pub fn add_edge(&mut self, x: T, y: T) {
    let entry = self.edges.entry(x).or_insert_with(Vec::new);
    entry.push(y);
  }

  pub fn add_edge_no_dup(&mut self, x: T, y: T) {
    if !self.has_edge(&x, &y) {
      self.add_edge(x, y);
    }
  }

  pub fn has_edge<U, V>(&self, x: &U, y: &V) -> bool where U : Borrow<T>, V : Borrow<T> {
    match self.outgoing_edges(x.borrow()) {
      None => false,
      Some(vec) => vec.iter().find(|node| *node == y.borrow()).is_some(),
    }
  }

  pub fn remove_edge<U, V>(&mut self, x: &U, y: &V) where U : Borrow<T>, V : Borrow<T> {
    if let Some(vec) = self.edges.get_mut(x.borrow()) {
      if let Some(pos) = vec.iter().position(|node| node == y.borrow()) {
        vec.swap_remove(pos);
      }
    }
  }

  pub fn node_count(&self) -> usize {
    self.edges.len()
  }

}

impl<T> Default for Graph<T> {

  fn default() -> Self {
    Graph { edges: HashMap::default() }
  }

}
