use crate::location_set::{Location, Var};
use super::interference::{Info as OldInfo, InterferenceGraph};
use asm::{Program, Block, Reg};
use ast::IdxVar;
use indexmap::IndexSet;
use petgraph::graph::NodeIndex;
use std::collections::HashMap;
use std::cmp::Ordering;
use bittyset::BitSet;
use priority_queue::PriorityQueue;

pub struct Info {
  pub locals: IndexSet<IdxVar>,
  /// in bytes
  pub stack_space: usize,
}

pub fn allocate_registers(prog: Program<OldInfo, IdxVar>) -> Program<Info> {
  let conflicts = prog.info.conflicts;
  let blocks = prog.blocks.into_iter()
    .map(|(label, block)| {
      let conflicts = &conflicts[&label];
      (label, allocate_registers_block(block, conflicts))
    })
    .collect();
  Program {
    info: Info {
      locals: prog.info.locals,
      stack_space: 0,
    },
    blocks,
  }
}

fn allocate_registers_block(
  block: Block<IdxVar>,
  conflicts: &InterferenceGraph,
) -> Block {
  todo!()
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
struct Color(i32);

#[derive(Debug, Clone, PartialEq, Eq)]
struct Vertex {
  location: Location,
  color: Option<Color>,
  saturation: BitSet,
}

impl PartialOrd for Vertex {
  fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
    self.saturation.len().partial_cmp(&other.saturation.len())
  }
}

impl Ord for Vertex {
  fn cmp(&self, other: &Self) -> Ordering {
    self.saturation.len().cmp(&other.saturation.len())
  }
}

fn color_graph(
  graph: &InterferenceGraph,
  reg_colors: &HashMap<Reg, Color>,
) -> HashMap<Var, Color> {
  let mut queue = PriorityQueue::<NodeIndex, Vertex>::new();

  for node in graph.graph.node_weights() {
  }

  let graph = graph.graph.map(
    |_, location| {
      Vertex {
        location: *location,
        color: None,
        saturation: BitSet::new(),
      }
    },
    |_, _| (),
  );

  todo!()
}