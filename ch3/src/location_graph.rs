use petgraph::graph::{Graph, NodeIndex, UnGraph};
use crate::location_set::Location;
use indexmap::IndexMap;


pub struct LocationGraph {
  pub graph: UnGraph<Location, ()>,
  pub nodes: IndexMap<Location, NodeIndex>,
}

impl LocationGraph {
  pub fn new() -> Self {
    Self {
      graph: Graph::new_undirected(),
      nodes: IndexMap::new(),
    }
  }

  pub fn insert_node(&mut self, loc: Location) -> NodeIndex {
    if let Some(&node) = self.nodes.get(&loc) {
      node
    } else {
      let node = self.graph.add_node(loc);
      self.nodes.insert(loc, node);
      node
    }
  }

  pub fn get_node(&self, loc: Location) -> Option<NodeIndex> {
    self.nodes.get(&loc).cloned()
  }

  pub fn add_edge(&mut self, n1: NodeIndex, n2: NodeIndex) {
    if !self.graph.contains_edge(n1, n2) {
      self.graph.add_edge(n1, n2, ());
    }
  }
}