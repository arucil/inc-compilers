use asm::{Arg, Block, Instr, Label, Program};
use ast::IdxVar;
use ch2::pass::select_instruction::Info as OldInfo;
use ch3::location_set::{LocationSet, VarStore};
use ch4::pass::liveness_analysis::Info as NewInfo;
use indexmap::{IndexMap, IndexSet};
use petgraph::Graph;
use std::collections::HashMap;

pub fn analyze_liveness(
  prog: Program<OldInfo, IdxVar>,
  label_live: HashMap<Label, LocationSet>,
) -> Program<NewInfo, IdxVar> {
}

pub fn analyze_dataflow<N, T, F>(
  g: Graph<N, T>,
  transfer: F,
  bottom: T,
  join: (),
) {
}
