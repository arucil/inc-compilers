use super::interference::Info;
use asm::{Arg, Block, Instr, Program};
use ast::IdxVar;
use ch3::pass::interference::Moves;
use ch3::{
  location_graph::LocationGraph,
  location_set::{Location, VarStore},
};

pub fn build_move_graph(
  mut prog: Program<Info, IdxVar>,
) -> Program<Info, IdxVar> {
  let moves = &mut prog.info.moves;
  let var_store = &prog.info.var_store;
  for block in &prog.blocks {
    build_block_move_graph(block, moves, var_store);
  }
  for fun in &mut prog.funs {
    let moves = &mut fun.info.moves;
    let var_store = &fun.info.var_store;
    for block in &fun.blocks {
      build_block_move_graph(block, moves, var_store);
    }
  }
  prog
}

fn build_block_move_graph(
  block: &Block<IdxVar>,
  moves: &mut LocationGraph<Moves>,
  var_store: &VarStore,
) {
  for instr in &block.code {
    if let Instr::Mov {
      src: src @ Arg::Var(_),
      dest: dest @ Arg::Var(_),
    } = instr
    {
      if src != dest {
        let src = moves
          .insert_node(Location::from_arg(src.clone(), var_store).unwrap());
        let dest = moves
          .insert_node(Location::from_arg(dest.clone(), var_store).unwrap());
        moves.add_edge(src, dest);
      }
    }
  }
}
