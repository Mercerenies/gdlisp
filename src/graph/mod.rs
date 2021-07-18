
//! Functions for working with
//! [graphs](https://en.wikipedia.org/wiki/Graph_(discrete_mathematics)).
//!
//! The [`Graph`] datatype provides the core functionality for
//! representing graphs.

pub mod top_sort;
pub mod tarjan;

use std::collections::HashMap;
use std::borrow::Borrow;
use std::hash::Hash;

// TODO Really, we should be storing the edge sets with some kind of
// numerical index, rather than assuming the type T can be safely
// cloned.

/// A graph consists of nodes and edges. Here, we represent directed
/// unlabeled multigraphs with loopback edges.
///
/// That is, every edge has a head and a tail, there can be multiple
/// edges with the same head and tail, and an edge can point from a
/// node to itself.
///
/// The type `T` of nodes must be [`Eq`] and [`Hash`] in order to
/// perform any of the `Graph` operations on it. Generally, you will
/// want it to have a sane [`Clone`] implementation as well, since a
/// given node will need to be stored several times within the data
/// structure.
#[derive(Debug, Clone)]
pub struct Graph<T> {
  edges: HashMap<T, Vec<T>>,
}

impl<T> Graph<T> where T : Eq + Hash {

  /// Given an iterator of edges, produce a [`Graph`]. Each element of
  /// the iterator shall be a pair, where the first term of the pair
  /// is the head of the edge and the second is the tail.
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

  /// Produce an [empty
  /// graph](https://en.wikipedia.org/wiki/Empty_graph) containing the
  /// given node set.
  ///
  /// In the case of duplicate nodes, only the first will be kept.
  pub fn from_nodes<I>(iter: I) -> Graph<T>
  where I : Iterator<Item=T> {
    Graph { edges: iter.map(|x| (x, Vec::new())).collect() }
  }

  /// Produce an empty graph with no edges or nodes.
  pub fn new() -> Graph<T> {
    Graph::default()
  }

  /// An iterator over the nodes of the graph, in an unspecified
  /// order.
  pub fn nodes(&self) -> impl Iterator<Item=&T> {
    self.edges.keys()
  }

  /// Given a value `x`, get a reference to the unique node in the
  /// graph with is equal to `x`. If there is no such node, returns
  /// [`None`].
  pub fn get_node<U>(&self, x: &U) -> Option<&T> where U : Borrow<T> {
    self.edges.get_key_value(x.borrow()).map(|(k, _)| k)
  }

  /// Returns whether or not the graph contains a node equal to `x`.
  pub fn has_node<U>(&self, x: &U) -> bool where U : Borrow<T> {
    self.get_node(x).is_some()
  }

  /// An iterator over all edges of the graph, in an unspecified
  /// order.
  pub fn all_edges(&self) -> impl Iterator<Item=(&T, &T)> {
    self.edges.iter().flat_map(|(x, ys)| ys.iter().map(move |y| (x, y)))
  }

  /// A vector over all of the outgoing edges from a given node, in an
  /// unspecified order. Returns [`None`] if the node does not exist.
  pub fn outgoing_edges(&self, node: impl Borrow<T>) -> Option<&Vec<T>> {
    self.edges.get(node.borrow())
  }

  /// Add a new node to the graph. If there is already a node equal to
  /// `node` in the graph, this method does nothing.
  pub fn add_node(&mut self, node: T) {
    self.edges.entry(node).or_insert_with(Vec::new);
  }

  /// Add a new edge to the graph. Both `x` and `y` should already be
  /// in the graph, or the result is undefined.
  pub fn add_edge(&mut self, x: T, y: T) {
    let entry = self.edges.entry(x).or_insert_with(Vec::new);
    entry.push(y);
  }

  /// Add a new edge in the graph. If there is already an edge from
  /// `x` to `y`, do not add another. Both `x` and `y` should already
  /// be in the graph, or the result is undefined.
  pub fn add_edge_no_dup(&mut self, x: T, y: T) {
    if !self.has_edge(&x, &y) {
      self.add_edge(x, y);
    }
  }

  /// Check whether the graph has an edge from `x` to `y`.
  pub fn has_edge<U, V>(&self, x: &U, y: &V) -> bool where U : Borrow<T>, V : Borrow<T> {
    match self.outgoing_edges(x.borrow()) {
      None => false,
      Some(vec) => vec.iter().any(|node| node == y.borrow()),
    }
  }

  /// Remove one edge from the graph pointing from `x` to `y`. If no
  /// such edge exists, then the graph is unchanged.
  pub fn remove_edge<U, V>(&mut self, x: &U, y: &V) where U : Borrow<T>, V : Borrow<T> {
    if let Some(vec) = self.edges.get_mut(x.borrow()) {
      if let Some(pos) = vec.iter().position(|node| node == y.borrow()) {
        vec.swap_remove(pos);
      }
    }
  }

  /// The total number of nodes in the graph.
  pub fn node_count(&self) -> usize {
    self.edges.len()
  }

  /// The graph with all edges flipped. The resulting graph will have
  /// the same nodes as `self`, but every edge will have its head and
  /// tail swapped.
  pub fn transpose(&self) -> Graph<T>
  where T : Clone {
    let mut graph = Graph::from_nodes(self.nodes().cloned());
    for (x, y) in self.all_edges() {
      graph.add_edge(y.clone(), x.clone());
    }
    graph
  }

}

impl<T> Default for Graph<T> {

  fn default() -> Self {
    Graph { edges: HashMap::default() }
  }

}
