use super::interference::Info;
use crate::{
  location_graph::LocationGraph,
  location_set::{Location, VarStore},
};
use asm::{Arg, Block, Instr, Program};
use ast::IdxVar;

pub fn build_move_graph(
  mut prog: Program<Info, IdxVar>,
) -> Program<Info, IdxVar> {
  let moves = &mut prog.info.moves;
  let var_store = &mut prog.info.var_store;
  for (label, block) in &prog.blocks {
    let moves = moves
      .entry(label.clone())
      .or_insert_with(|| LocationGraph::new());
    build_block_move_graph(&block, moves, var_store);
  }
  prog
}

fn build_block_move_graph(
  block: &Block<IdxVar>,
  moves: &mut LocationGraph,
  var_store: &mut VarStore,
) {
  for instr in &block.code {
    match instr {
      Instr::Mov(src @ Arg::Var(_), dest @ Arg::Var(_)) => {
        let src = moves
          .insert_node(Location::from_arg(src.clone(), var_store).unwrap());
        let dest = moves
          .insert_node(Location::from_arg(dest.clone(), var_store).unwrap());
        moves.add_edge(src, dest);
      }
      _ => {}
    }
  }
}
