use crate::location_set::Location;
use indexmap::IndexMap;
use petgraph::graph::{Graph, NodeIndex as PrivateNodeIndex, UnGraph};
use std::marker::PhantomData;

pub struct LocationGraph<T> {
  graph: UnGraph<Location, ()>,
  nodes: IndexMap<Location, PrivateNodeIndex>,
  _marker: PhantomData<T>,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct NodeIndex<T> {
  index: PrivateNodeIndex,
  _marker: PhantomData<T>,
}

impl<T> LocationGraph<T> {
  pub fn new() -> Self {
    Self {
      graph: Graph::new_undirected(),
      nodes: IndexMap::new(),
      _marker: PhantomData,
    }
  }

  pub fn insert_node(&mut self, loc: Location) -> NodeIndex<T> {
    let index = if let Some(&node) = self.nodes.get(&loc) {
      node
    } else {
      let node = self.graph.add_node(loc);
      self.nodes.insert(loc, node);
      node
    };
    NodeIndex {
      index,
      _marker: PhantomData,
    }
  }

  pub fn node_index(&self, loc: Location) -> Option<NodeIndex<T>> {
    self.nodes.get(&loc).map(|&index| NodeIndex {
      index,
      _marker: PhantomData,
    })
  }

  pub fn node_data(&self, node: NodeIndex<T>) -> Option<Location> {
    self.graph.node_weight(node.index).cloned()
  }

  pub fn add_edge(&mut self, n1: NodeIndex<T>, n2: NodeIndex<T>) {
    if !self.graph.contains_edge(n1.index, n2.index) {
      self.graph.add_edge(n1.index, n2.index, ());
    }
  }

  pub fn node_indices(&self) -> impl Iterator<Item = NodeIndex<T>> {
    self.graph.node_indices().map(|index| NodeIndex {
      index,
      _marker: PhantomData,
    })
  }

  pub fn neighbors(
    &self,
    node: NodeIndex<T>,
  ) -> impl Iterator<Item = NodeIndex<T>> + '_ {
    self.graph.neighbors(node.index).map(|index| NodeIndex {
      index,
      _marker: PhantomData,
    })
  }

  pub fn graph(&self) -> &UnGraph<Location, ()> {
    &self.graph
  }
}
