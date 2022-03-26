use super::liveness_analysis::Info as OldInfo;
use crate::location_graph::LocationGraph;
use crate::location_set::{Location, LocationSet, VarStore};
use asm::{Block, Instr, Program, Reg};
use ast::IdxVar;
use indexmap::IndexSet;
use petgraph::dot::{Config, Dot};
use std::fmt::{self, Debug, Formatter};

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct Interference;

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct Moves;

pub struct Info {
  /// Includes all locals.
  pub locals: IndexSet<IdxVar>,
  /// Nodes include all locals.
  pub conflicts: LocationGraph<Interference>,
  pub moves: LocationGraph<Moves>,
  pub var_store: VarStore,
}

pub fn build_interference(
  prog: Program<OldInfo, IdxVar>,
) -> Program<Info, IdxVar> {
  let var_store = &prog.info.var_store;
  let live = &prog.info.live;
  let mut conflicts = LocationGraph::new();
  for (_, var) in var_store.iter() {
    conflicts.insert_node(Location::from(var.clone()));
  }
  for (label, block) in &prog.blocks {
    build_graph(block, &live[label], var_store, &mut conflicts);
  }

  Program {
    info: Info {
      locals: prog.info.locals,
      conflicts,
      moves: LocationGraph::new(),
      var_store: prog.info.var_store,
    },
    constants: prog.constants,
    blocks: prog.blocks,
  }
}

fn build_graph(
  block: &Block<IdxVar>,
  live_sets: &[LocationSet],
  var_store: &VarStore,
  graph: &mut LocationGraph<Interference>,
) {
  for (instr, live_after) in block.code.iter().zip(live_sets.iter().skip(1)) {
    add_instr_edges(instr, live_after, var_store, graph);
  }
}

fn add_instr_edges(
  instr: &Instr<IdxVar>,
  live_after: &LocationSet,
  var_store: &VarStore,
  graph: &mut LocationGraph<Interference>,
) {
  let mut add = |write_loc: Location| {
    let write_loc_node = graph.insert_node(write_loc);
    for after_loc in live_after {
      if after_loc != write_loc {
        let after_loc_node = graph.insert_node(after_loc);
        graph.add_edge(write_loc_node, after_loc_node);
      }
    }
  };

  match instr {
    Instr::Add { dest, .. }
    | Instr::Sub { dest, .. }
    | Instr::Xor { dest, .. }
    | Instr::Neg(dest)
    | Instr::Pop(dest) => {
      if let Some(dest_loc) = Location::from_arg(dest.clone(), var_store) {
        add(dest_loc);
      }
    }
    Instr::Call { .. } => {
      for reg in Reg::caller_saved_regs() {
        add(reg.into());
      }
    }
    Instr::Mov { src, dest } | Instr::Movzx { src, dest } => {
      if let Some(dest_loc) = Location::from_arg(dest.clone(), var_store) {
        if let Some(src_loc) = Location::from_arg(src.clone(), var_store) {
          let dest_loc_node = graph.insert_node(dest_loc);
          for after_loc in live_after {
            if after_loc != dest_loc && after_loc != src_loc {
              let after_loc_node = graph.insert_node(after_loc);
              graph.add_edge(dest_loc_node, after_loc_node);
            }
          }
        } else {
          add(dest_loc);
        }
      }
    }
    Instr::Cmp { .. }
    | Instr::Push(_)
    | Instr::Ret
    | Instr::Syscall
    | Instr::Jmp(_)
    | Instr::JumpIf { .. } => {}
    _ => todo!(),
  }
}

impl Debug for Info {
  fn fmt(&self, f: &mut Formatter) -> fmt::Result {
    let graph = self
      .conflicts
      .graph()
      .map(|_, var| var.to_arg(&self.var_store), |_, _| ());
    Dot::with_config(&graph, &[Config::EdgeNoLabel]).fmt(f)
  }
}

#[cfg(test)]
mod tests {
  use super::super::*;
  use super::*;
  use asm::Label;
  use ch2::pass::select_instruction::Info as OldOldInfo;
  use indexmap::indexset;
  use insta::assert_snapshot;
  use maplit::hashmap;

  #[test]
  fn example_in_book() {
    use asm::Reg::*;
    let blocks = asm::parse_blocks(
      |s| IdxVar::new(s),
      r#"
    start:
      mov v, i
      mov w, 42
      mov x, v
      add x, 7
      mov y, x
      mov z, x
      add z, w
      mov t, y
      neg t
      mov rax, z
      add rax, t
      jmp conclusion
    "#,
    );
    let label_live = hashmap! {
      Label::Conclusion => LocationSet::regs([Rax, Rsp])
    };
    let prog = Program {
      info: OldOldInfo {
        locals: indexset! {
          IdxVar::new("v"),
          IdxVar::new("i"),
          IdxVar::new("w"),
          IdxVar::new("x"),
          IdxVar::new("y"),
          IdxVar::new("z"),
          IdxVar::new("t")
        },
      },
      constants: Default::default(),
      blocks,
    };
    let prog = liveness_analysis::analyze_liveness(prog, label_live);
    let result = build_interference(prog);

    assert_snapshot!(format!("{:?}", result.info));
  }

  #[test]
  fn call() {
    use asm::Reg::*;
    let blocks = asm::parse_blocks(
      |s| IdxVar::new(s),
      r#"
    start:
      pop rdi
      pop rsi
      push x
      mov w, rbx
      call foo, 3
      add w, rax
      jmp conclusion
    "#,
    );
    let label_live = hashmap! {
      Label::Conclusion => LocationSet::regs([Rax, Rsp])
    };
    let prog = Program {
      info: OldOldInfo {
        locals: indexset! {
          IdxVar::new("x"),
          IdxVar::new("w"),
        },
      },
      constants: Default::default(),
      blocks,
    };
    let prog = liveness_analysis::analyze_liveness(prog, label_live);
    let result = build_interference(prog);

    assert_snapshot!(format!("{:?}", result.info));
  }

  #[test]
  fn mov_same_variables() {
    let blocks = asm::parse_blocks(
      |s| IdxVar::new(s),
      r#"
    start:
      mov t, x
      add t, y
      mov z, t
      add z, w
    "#,
    );
    let label_live = hashmap! {};
    let prog = Program {
      info: OldOldInfo {
        locals: indexset! {
          IdxVar::new("t"),
          IdxVar::new("x"),
          IdxVar::new("y"),
          IdxVar::new("z"),
          IdxVar::new("w"),
        },
      },
      constants: Default::default(),
      blocks,
    };
    let prog = liveness_analysis::analyze_liveness(prog, label_live);
    let result = build_interference(prog);

    assert_snapshot!(format!("{:?}", result.info));
  }
}
